#' Filter Rows and Slice Data Frames
#'
#' @description
#' Functions for subsetting rows based on conditions or by position.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `filter_()` - Keep rows that match conditions
#' * `distinct_()` - Keep only unique/distinct rows based on columns
#' * `slice_()` - Select rows by position (index)
#' * `slice_head_()` - Select first n rows or proportion
#' * `slice_tail_()` - Select last n rows or proportion
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param ... For `filter_()`: conditions as formulas (e.g., `~mpg > 20`).
#'   For `distinct_()`: columns to use for uniqueness.
#'   For `slice_()`: row positions.
#' @param .by A list of names of the columns to use for grouping the data.
#' @param by A list of names of the columns to use for grouping the data.
#' @param .preserve Logical. When `TRUE`, preserve the grouping structure in
#'   the result. When `FALSE` (default), recalculate grouping based on the
#'   filtered data.
#'
#' @return A data frame with filtered/selected rows, maintaining the same class
#'   as the input (data.frame, data.table, or tibble).
#'
#' @note From \{dplyr\}, the `slice_min()`, `slice_max()` and `slice_sample()`
#' functions are not added yet.
#'
#' @seealso
#' [dplyr::filter()], [dplyr::distinct()], [dplyr::slice()],
#' [dplyr::slice_head()], [dplyr::slice_tail()]
#'
#' @examples
#' library(svTidy)
#' data(mtcars)
#'
#' # Filter rows with condition
#' mtcars |> filter_(~mpg > 20)
#'
#' # Multiple conditions (AND logic)
#' mtcars |> filter_(~mpg > 20, ~cyl == 4)
#'
#' # Get distinct values for columns
#' mtcars |> distinct_(~cyl, ~gear)
#'
#' # Distinct with all columns kept
#' mtcars |> distinct_(~cyl, .keep_all = TRUE)
#'
#' # Slice specific rows
#' mtcars |> slice_(1, 5, 10)
#'
#' # Select first 5 rows
#' mtcars |> slice_head_(n = 5)
#'
#' # Select last 10% of rows
#' mtcars |> slice_tail_(prop = 0.1)
#'
#' # Grouped filtering
#' mtcars |>
#'   group_by_(~cyl) |>
#'   filter_(~mpg > mean(~mpg))
#'
#' @name filtering
#' @export
#' @rdname filtering
#' @importFrom collapse fsubset
filter_ <- structure(function(.data = (.), ..., .by = NULL, .preserve = FALSE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Case missing(...), just return the data frame
  if (missing(...))
    return(.data)

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # We do not support .preserve = TRUE
  if (isTRUE(.preserve)) {
    stop(
      "Argument {.code .preserve = TRUE} is not supported in {.fun filter_}.",
      i = "The groups are kept, but they are recalculated on the filtered data frame.")
  }

  # Process dots with formula-masking
  is_grouped <- is_grouped_df(.data)
  if (is_grouped) {
    new_gvars <- fgroup_vars(.data, return = "names")
  } else {# No grouping variables
    new_gvars <- character(0)
  }

  # Apply formula-masking on ...
  no_se_msg <- gettext(
    "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(..., .no.se = is_grouped, .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by)) {
    if (is_grouped)
      stop("Can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    if (!args$are_formulas)
      stop(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    #let_data.table_to_data.trame(.data)
    # TODO: a formula-select "lite" here
    res <- group_by_vars(.data, by = .by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
    new_gvars <- character(0) # Don't keep these groups
  } else {
    res <- .data
  }

  # No input can be named
  dots_names <- names(args$dots)
  if (!is.null(dots_names)) {
    named <- whichv(dots_names, "", invert = TRUE)
    if (length(named)) {
      first_named <- named[1]
      first_name <- dots_names[first_named]
      first_expr <- expr_deparse(sys.call()[[first_name]])
      if (first_expr == "NULL") {# Probably using formula like name ~ expr
        stop("We detected a named input.",
          i = "Did you used formula like {.code name ~ expr}?",
          i = "You must filter with formula having only right member, like {.code ~expr}.")
      } else {# Same error message as dplyr
        msg_expr <- paste(first_name, first_expr, sep = " == ")
        stop("We detected a named input.",
          i = "This usually means that you've used {.code =} instead of {.code ==}.",
          i = "Did you mean {.code {msg_expr}}?")
      }
    }
  }

  filters <- args$dots
  # fsubset() or ss() can use only one subset argument at a time. So, we run it
  # multiple times on each argument to filter_() to mimic filter()
  if (args$are_formulas) {# NSE argument, must us fsubset()
    # fsubset() does not support groups, so, we need to do the job ourselves
    if (is_grouped) {
      subset_onegroup <- function(i, gdata, filters, envir) {
        gdata <- gdata[i, ]
        for (i in 1:length(filters))
          gdata <- do.call(fsubset, list(.x = gdata, filters[[i]]),
            envir = envir)
        gdata
      }
      # Since we drop rows, we cannot use greorder() after the treatment ->
      # we add a temporary sorting variable that we will drop at the end
      res <- add_vars(res, ._order_. = seq_len(nrow(res)))
      res <- rowbind(lapply(gsplit(g = res), subset_onegroup, gdata = res,
        filters = filters, envir = args$env))
      # Can't use this one!
      #res <- res[greorder(1:nrow(res), g = res), ] # Reorder the data
      res <- roworderv(res, cols = '._order_.', decreasing = FALSE)
      # Remove the temporary sorting variable
      res$._order_. <- NULL
      # Group again the results


    } else {# No groups
      for (i in 1:...length())
        res <- do.call(fsubset, list(.x = res, filters[[i]]), envir = args$env)
    }
  } else {# SE arguments, ss() is faster
    filter <- filters[[1L]]
    lfilters <- length(filters)
    if (lfilters > 1L) {
      for (i in 2:lfilters)
        filter <- filter & filters[[i]]
    }
    res <- ss(res, filter)
  }
  if (length(new_gvars)) # (re)set grouping
    res <- group_by_vars(res, by = new_gvars)
  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::filter"))

#' @export
#' @rdname filtering
#' @param .keep_all Logical. For `distinct_()`, if `TRUE`, keep all columns in
#'   the result. If `FALSE` (default), keep only the distinct columns.
#' @param .method The algorithm to use for grouping:  `"radix"`, `"hash"`, or
#'   `"auto"` (by default). `"auto"` chose `"radix"` when `sort = TRUE` and
#'   `"hash"` otherwise.
distinct_ <- structure(function(.data = (.), ..., .keep_all = FALSE,
  .method = "auto") {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  if (missing(...)) {
    args <- list(dots = as.list(names(.data)), are_formulas = FALSE,
      env = parent.frame())
  } else {
    args <- formula_masking(...)
  }

  is_grouped <- is_grouped_df(.data)
  if (missing(...) && !is_grouped) {# use all columns of .data
    # Note: since we use all variables here, .keep_all = TRUE/FALSE is the same
    return(funique(.data, cols = names(.data), method = .method))
  }

  if (is.numeric(.keep_all))
    .keep_all <- as.logical(.keep_all)
  if (!isTRUE(.keep_all) && !isFALSE(.keep_all))
    stop("{.arg .keep_all} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {(.keep_all)}} ({.val {(.keep_all)}}).")

  # If there are named items, we mutate data with these expressions
  dots_names <- names(args$dots)
  if (!is.null(dots_names)) {
    is_expr <- dots_names != ""
    if (any(is_expr)) {
      args2 <- args$dots[is_expr]
      if (!args$are_formulas) # force standard evaluation
        force(args2)
      data2 <- do.call(fmutate, c(list(.data), args2), envir = args$env)
      # Replace the expression by its name ( as a symbol, if formulas)
      args$dots[is_expr] <- dots_names[is_expr]
    } else {
      # If ~pick(...) somewhere in args$dots, use tidyselect::eval_select()
      args$dots <- lapply(args$dots, function(x) {
        if (is.call(x) && x[[1]] == 'pick') {
          x[[1]] <- as.symbol('c')
          x <- eval_select(expr = x, data = .data, env = args$env,
            error_call = stop_top_call())
          names(x) # eval_select() returns indices with names, we use names only
        } else {
          x
        }
      })
      data2 <- .data
    }
  } else {
    data2 <- .data
  }
  argsdots <- unlist(lapply(args$dots, as.character))
  if (is_grouped) {
    argsdots <- funique(c(fgroup_vars(.data, return = "names"), argsdots))
    data2 <- fungroup(data2) # Always ungroup at the end
  }

  res <- funique(data2, cols <- unlist(argsdots), method = .method)

  # if we don't keep all, select the columns used to compute distinct
  if (isFALSE(.keep_all))
    res <- fselect(res, argsdots) # This is faster than the next one.
  #res <- res[, argsdots, drop = FALSE]

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res


}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::distinct"))

#' @export
#' @rdname filtering
slice_ <- structure(function(.data = (.), ..., .by = NULL, .preserve = NULL) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(.preserve))
    warning("`.preserve` argument is not used in `slice_()`. ",
      "SciViews functions, unlike 'dplyr', always ungroup at the end.")

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # Process dots with formula-masking
  is_grouped <- is_grouped_df(.data)
  # No, we always ungroup at the end
  #if (is_grouped) {
  #  new_gvars <- fgroup_vars(.data, return = "names")
  #} else {# No grouping variables
  #  new_gvars <- character(0)
  #}

  # Case missing(...), return the data frame with zero rows
  if (missing(...)) {
    res <- ss(.data, 0L)
    if (is_grouped)
      res <- fungroup(res)
    #if (to_dtrm)
    #  let_data.table_to_data.trame(res)
    return(res)
  }

  # Apply formula-masking on ...
  #no_se_msg <- gettext(
  #  "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(...) #, .no.se = is_grouped, .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by) && length(.by)) {
    if (is_grouped)
      stop("Can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    #if (!args$are_formulas)
    #  stop(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    #let_data.table_to_data.trame(.data)
    # For now, we let group_by_vars return the error
    #if (is.numeric(.by)) {
    #
    #} else if (!is.character(.by)) {
    #
    #} else {# .by is a character vector
    #  absent_by <- !.by %in% names(.data)
    #  if (any(absent_by))
    #    stop("Can't select columns that don't exist.",
    #      x = "Column {.code {(.by[absent_by][1])}} doesn't exist.")
    #}
    # TODO: a formula-select "lite" here
    res <- group_by_vars(.data, by = .by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
    #new_gvars <- character(0) # Don't keep these groups
    is_grouped <- is_grouped_df(res)
  } else {
    res <- .data
  }

  # No input can be named
  dots_names <- names(args$dots)
  if (!is.null(dots_names)) {
    named <- whichv(dots_names, "", invert = TRUE)
    if (length(named)) {
      first_named <- named[1]
      first_name <- dots_names[first_named]
      first_expr <- expr_deparse(sys.call()[[first_name]])
      if (first_expr == "NULL") {# Probably using formula like name ~ expr
        stop("Arguments in {.arg ...} must be passed by position, not name.",
          i = "Did you used formula like {.code name ~ expr}?",
          i = "You must filter with formula having only right member, like {.code ~expr}.")
      } else {# Same error message as dplyr
        msg_expr <- paste(first_name, first_expr, sep = " = ")
        stop("Arguments in {.arg ...} must be passed by position, not name.",
          x = "Problematic argument:",
          `*` = "{.code {msg_expr}}")
      }
    }
  }

  indices <- unlist(args$dots)
  if (!is.numeric(indices)) {
    if (!is.function(indices[[1]]))
      stop("Can't subset elements with {.val {indices}}.",
        i = "You must provide numeric values or a {.cls function}, not {.obj_type_friendly {indices}}.")
    if (length(indices) != 1L) # With a function, cannot use more arguments
      stop("If you supply a {.cls function}, you cannot give more arguments.")
    if (is_grouped) {
      # This one line does the whole grouped slice!
      res <- ss(res, na_rm(unlist(lapply(gsplit(g = res), indices[[1]]))))
      res <- fungroup(res)
    } else {
      res <- ss(res, na_rm(indices[[1]](seq_row(res))))
    }
  } else {# Numeric indices
    indices <- as.integer(indices)
    if (is_grouped) {
      # This one line does the whole grouped slice!
      res <- ss(res, na_rm(unlist(lapply(gsplit(g = res), ss, indices))))
      res <- fungroup(res)
    } else {
      res <- ss(res, indices)
    }
  }

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::slice"))

#' @export
#' @rdname filtering
#' @param n Number of rows to keep
#' @param prop Proportion of rows to keep, between 0 and 1. Provide either `n`,
#'   or `prop` but not both simultaneously. If none is provided, `n = 1` is
#'   used.
#'  @param by A list of names of the columns to use for joining the two data
#' frames.
#' @param sort If `TRUE` largest group will be shown on top.
slice_head_ <- structure(function(.data = (.), ..., n = 1L, prop, by = NULL,
  sort = TRUE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...)) {
    if (...length() == 1L && is.null(...names()))
      stop("{.arg n} must be explicitly named.",
        i = "Did you mean {.code slice_head_(n = {.val {(..1)}})}?")
    check_dots_empty()
  } # ... must be empty

  # Use prop or n indifferently as n in fslice()
  if (missing(n)) {
    if (!missing(prop)) {# We use prop instead of n
      if (!is.numeric(prop) || length(prop) != 1L)
        stop("{.arg prop} must be a single number, not {.obj_type_friendly {prop}} of length {length(prop)}.")
      if (prop < 0)
        stop("Negative {.arg prop} is not supported by {.fun slice_head_}.")
      if (prop >= 1L)
        prop <- 0.999999999999
      n <- prop
    }
  } else {# n non missing
    if (!missing(prop))
      stop("Must supply {.arg n} or {.arg prop}, but not both.")
    if (!is.numeric(n) || length(n) != 1L)
      stop("{.arg n} must be a single round number, not {.obj_type_friendly {n}} of length {length(n)}.")
    if (n < 0)
      stop("Negative {.arg n} is not supported by {.fun slice_head_}.")
    n2 <- as.integer(n)
    if (n2 != n)
      stop("{.arg n} must be a single round number, not {.val {n}}.")
    n <- n2
    if (n > nrow(.data))
      n <- nrow(.data)
  }

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  is_grouped <- is_grouped_df(.data)
  # If by is defined, use these groups, but do not keep them
  if (!missing(by) && length(by)) {
    if (is_grouped)
      stop("Can't supply {.arg by} when {.arg .data} is a grouped data frame.")
    #let_data.table_to_data.trame(.data)
    res <- group_by_vars(.data, by = by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
  } else {
    res <- .data
    if (is_grouped) {
      by <- fgroup_vars(res, return = "names")
    } else {
      by <- NULL
    }
  }

  res <- fslicev(res, cols = by, n = n, how = "first", sort = sort)
  if (is_grouped)
    res <- fungroup(res) # Always ungroup at the end

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::slice_head"))

#' @export
#' @rdname filtering
slice_tail_ <- structure(function(.data = (.), ..., n = 1L, prop, by = NULL,
  sort = TRUE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...)) {
    if (...length() == 1L && is.null(...names()))
      stop("{.arg n} must be explicitly named.",
        i = "Did you mean {.code slice_tail_(n = {.val {(..1)}})}?")
    check_dots_empty()
  } # ... must be empty

  # Use prop or n indifferently as n in fslice()
  if (missing(n)) {
    if (!missing(prop)) {# We use prop instead of n
      if (!is.numeric(prop) || length(prop) != 1L)
        stop("{.arg prop} must be a single number, not {.obj_type_friendly {prop}} of length {length(prop)}.")
      if (prop < 0)
        stop("Negative {.arg prop} is not supported by {.fun slice_tail_}.")
      if (prop >= 1L)
        prop <- 0.999999999999
      n <- prop
    }
  } else {# n non missing
    if (!missing(prop))
      stop("Must supply {.arg n} or {.arg prop}, but not both.")
    if (!is.numeric(n) || length(n) != 1L)
      stop("{.arg n} must be a single round number, not {.obj_type_friendly {n}} of length {length(n)}.")
    if (n < 0)
      stop("Negative {.arg n} is not supported by {.fun slice_tail_}.")
    n2 <- as.integer(n)
    if (n2 != n)
      stop("{.arg n} must be a single round number, not {.val {n}}.")
    n <- n2
    if (n > nrow(.data))
      n <- nrow(.data)
  }

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  is_grouped <- is_grouped_df(.data)
  # If by is defined, use these groups, but do not keep them
  if (!missing(by) && length(by)) {
    if (is_grouped)
      stop("Can't supply {.arg by} when {.arg .data} is a grouped data frame.")
    #let_data.table_to_data.trame(.data)
    res <- group_by_vars(.data, by = by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
  } else {
    res <- .data
    if (is_grouped) {
      by <- fgroup_vars(res, return = "names")
    } else {
      by <- NULL
    }
  }

  res <- fslicev(res, cols = by, n = n, how = "last", sort = sort)
  if (is_grouped)
    res <- fungroup(res) # Always ungroup at the end

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::slice_tail"))
