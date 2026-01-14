#' Summarising and Counting Functions
#'
#' @description
#' Functions for summarising data and counting observations in data frames.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `summarise_()` / `summarize_()` - Compute summary statistics for groups
#' * `reframe_()` - Similar to summarise but always returns ungrouped data
#' * `count_()` - Count observations by group
#' * `tally_()` - Count total observations (wrapper around count_)
#' * `add_count_()` - Add count column to data frame
#' * `add_tally_()` - Add total count column to data frame
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param ... For `summarise_()` and `reframe_()`: name-value pairs of summary
#'   functions. Names are column names in the output; values are expressions to
#'   compute. Use formulas for non-standard evaluation (e.g., `~mean_mpg = mean(~mpg)`).
#'   For `count_()` and `add_count_()`: grouping variables specified as formulas
#'   (e.g., `~cyl`, `~gear`) or character names. Can include named expressions
#'   to create new grouping variables before counting.
#' @param .by Optional temporary grouping variables for per-group computations.
#'   Provide as formulas (e.g., `~group_col`) or character names. Groups are
#'   temporary and not preserved in the output. Cannot be used with grouped
#'   data frames.
#' @param .groups Control grouping of the result. Options:
#'   * `"drop_last"` (default) - Drop the last grouping level
#'   * `"drop"` - Remove all grouping
#'   * `"keep"` - Keep all grouping levels
#'   * `"rowwise"` - Not implemented
#'   For `reframe_()`, only `"drop"` is allowed.
#' @param .keep.group_vars Logical. If `TRUE` (default), keep grouping variables
#'   in the result.
#' @param .cols Optional character vector of column names to operate on. Currently
#'   only `NULL` (default) is implemented.
#' @param wt For `count_()`, `tally_()`, `add_count_()`, and `add_tally_()`:
#'   frequency weights. Can be `NULL` (default, counts rows), a numeric vector,
#'   a column name as character, or a formula (e.g., `~weight_col`).
#' @param name Character string specifying the name of the count column created
#'   in the output. Default is `"n"`.
#' @param sort Logical. If `TRUE`, sort the result by the count column in
#'   decreasing order (or as specified by `decreasing`). Default is `FALSE`.
#' @param decreasing Logical. If `TRUE` (default), sort counts in decreasing
#'   order when `sort = TRUE`.
#' @param .drop Logical. If `TRUE` (default), drop unused factor levels. Note:
#'   `.drop = FALSE` is not yet implemented in `count_()`.
#' @param add Logical. If `TRUE`, add the count column to the original data
#'   frame instead of returning a summary. Default is `FALSE`.
#'
#' @return
#' * `summarise_()` returns a data frame with one row per group (or one row if
#'   ungrouped), containing the summary statistics. Grouping depends on `.groups`.
#' * `reframe_()` returns an ungrouped data frame (can have any number of rows
#'   per group).
#' * `count_()` returns a data frame with one row per unique combination of
#'   grouping variables, plus a count column.
#' * `tally_()` returns a data frame with one row per group showing the count.
#' * `add_count_()` returns the original data with an additional count column.
#' * `add_tally_()` returns the original data with an additional count column.
#'
#' @note
#' The `summarise_()` function does not support `n()` as does [dplyr::summarise()].
#' You can use [svBase::fn()] instead, but then you must give a variable name as
#' argument. The [svBase::fn()] alternative can also be used in [dplyr::summarise()]
#' for homogeneous syntax between the two.
#'
#' @seealso
#' [dplyr::summarise()], [dplyr::reframe()], [dplyr::count()], [dplyr::tally()],
#' [dplyr::add_count()], [dplyr::add_tally()], [collapse::fsummarise()],
#' [collapse::fcount()], [svBase::fn()]
#'
#' @examples
#' library(svTidy)
#' data(mtcars)
#'
#' # Basic summarise - single summary statistic
#' mtcars |> summarise_(mean_mpg = ~mean(mpg))
#'
#' # Multiple summary statistics
#' mtcars |>
#'   summarise_(
#'     mean_mpg = ~mean(mpg),
#'     sd_mpg   = ~sd(mpg),
#'     max_hp   = ~max(hp)
#'   )
#'
#' # Summarise by groups
#' mtcars |>
#'   group_by_(~cyl) |>
#'   summarise_(
#'     mean_mpg = ~mean(mpg),
#'     mean_hp  = ~mean(hp)
#'   )
#'
#' # Use .by for temporary grouping
#' mtcars |>
#'   summarise_(
#'     mean_mpg = ~mean(mpg),
#'     count    = ~length(mpg),
#'     .by = 'cyl'
#'   )
#'
#' # Multiple grouping variables with .by
#' mtcars |>
#'   summarise_(
#'     mean_mpg = ~mean(mpg),
#'     .by = c('cyl', 'gear')
#'   )
#'
#' # Control grouping of result
#' mtcars |>
#'   group_by_(~cyl, ~gear) |>
#'   summarise_(mean_mpg = ~mean(mpg), .groups = "drop")
#'
#' mtcars |>
#'   group_by_(~cyl, ~gear) |>
#'   summarise_(mean_mpg = ~mean(mpg), .groups = "keep")
#'
#' # Using standard evaluation (ungrouped data only)
#' mtcars |> summarise_(mean_mpg = mean(mtcars$mpg))
#'
#' # reframe_() for summaries returning multiple rows per group
#' mtcars |>
#'   group_by_(~cyl) |>
#'   reframe_(quantile_mpg = ~quantile(mpg, c(0.25, 0.5, 0.75)))
#'
#' # Count observations by group
#' mtcars |> count_(~cyl)
#'
#' # Count by multiple variables
#' mtcars |> count_(~cyl, ~gear)
#'
#' # Count with sorting
#' mtcars |> count_(~cyl, sort = TRUE)
#'
#' # Count in increasing order
#' mtcars |> count_(~cyl, sort = TRUE, decreasing = FALSE)
#'
#' # Count with weights
#' mtcars |> count_(~cyl, wt = ~mpg)
#'
#' # Count with computed grouping variable
#' mtcars |> count_(high_mpg = ~mpg > 20)
#'
#' # Combine grouping and computation
#' mtcars |> count_(~cyl, high_hp = ~hp > 150)
#'
#' # tally_() - count rows (optionally by existing groups)
#' mtcars |> tally_()
#'
#' mtcars |>
#'   group_by_(~cyl) |>
#'   tally_()
#'
#' # tally with weights
#' mtcars |>
#'   group_by_(~cyl) |>
#'   tally_(wt = ~hp)
#'
#' # add_count_() - add count column without collapsing
#' mtcars |> add_count_(~cyl)
#'
#' # add_count with custom column name
#' mtcars |> add_count_(~cyl, name = "n_cyl")
#'
#' # add_count by multiple variables
#' mtcars |> add_count_(~cyl, ~gear)
#'
#' # add_tally_() - add total count to each row
#' mtcars |> add_tally_()
#'
#' mtcars |>
#'   group_by_(~cyl) |>
#'   add_tally_()
#'
#' # Chain operations
#' mtcars |>
#'   count_(~cyl, ~gear, sort = TRUE) |>
#'   mutate_(pct = ~n/sum(n) * 100)
#'
#' # Use with filtering
#' mtcars |>
#'   add_count_(~cyl) |>
#'   filter_(~n > 10)
#'
#' @name summarising
#' @export
#' @rdname summarising
summarise_ <- structure(function(.data = (.), ..., .by = NULL,
  .groups = "drop_last", .keep.group_vars = TRUE, .cols = NULL) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # For grouped data, compute the grouping variables that must be returned
  is_grouped <- is_grouped_df(.data)
  if (is_grouped) {
    gvars <- fgroup_vars(.data, return = "names")
    new_gvars <- switch(.groups,
      drop_last = gvars[-length(gvars)],
      drop      = character(0),
      keep      = gvars,
      rowwise   = stop("{.arg .groups} must be {.code \"drop_last\"}, {.code \"drop\"}, or {.code \"keep\"}. {.code \"rowwise\"} is not supported."),
      stop("{.arg .groups} must be {.code \"drop_last\"}, {.code \"drop\"}, or {.code \"keep\"}.")
    )
  } else {# No grouping variables
    new_gvars <- character(0)
  }

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  # Case missing(...)
  # We only allow standard evaluation,... or formulas. They cannot be mixed and
  # standard evaluation is NOT possible for grouped data.
  if (missing(...)) {
    # If data is group7ed, we return the number of observations par level
    if (is_grouped) {
      # We return only the grouping variables, like summarise() does
      # fsummarise() refuses to do the calculation without calculating at least
      # one column... so, we calculate and the drop it
      .zzzz <- .data[[gvars[1]]]
      res <- fsummarise(.data, .zzzz = .zzzz[1])
      res$.zzzz <- NULL
      if (to_dtrm)
        let_data.table_to_data.trame(res)
      # fsummarise() ungroup the result, but not summarise()
      if (length(new_gvars)) {
        return(group_by_vars(res, by = new_gvars))
      } else {# Only one grouping variable, so, we ungroup, OK
        return(res)
      }
    } else {# Return a df with 0 columns and 1 row (like dplyr::summarise())
      if (to_dtrm)
        let_data.table_to_data.trame(.data)
      return(.data[1L, 0L])
    }
  }

  # Process dots with formula-masking
  no_se_msg <- gettext(
    "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(..., .make.names = TRUE, .no.se = is_grouped,
    .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by)) {
    if (is_grouped)
      stop("can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    if (!args$are_formulas)
      abort(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    let_data.table_to_data.trame(.data)
    .data <- group_by_vars(.data, by = .by, sort = FALSE)
    let_data.trame_to_data.table(.data)
    new_gvars <- character(0) # Don't keep these groups
  } else {
    .data <- .data
  }

  res <- do.call(fsummarise, c(list(.data = .data), args$dots,
    list(keep.group_vars = force(.keep.group_vars), .cols = force(.cols))),
    envir = args$env)
  if (length(new_gvars)) # (re)set grouping
    res <- group_by_vars(res, by = new_gvars)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fsummarise"))

#' @export
#' @rdname summarising
summarize_ <- summarise_

#' @export
#' @rdname summarising
reframe_ <- structure(function(.data, ..., .by = NULL, .groups = "drop",
  .keep.group_vars = TRUE, .cols = NULL) {

  .__top_call__. <- TRUE

  # Call summarise_(.groups = "drop")
  if (.groups != "drop")
    stop("{.fun reframe_} only accepts {.code .groups = \"drop\"}.",
      i = "Use {.fun summarise_} instead.")

  call <- sys.call()
  call[[1]] <- as.symbol('summarise_') # Use summarise_() instead
  if (missing(.data) || !is.data.frame(.data))
    return(recall_with_data_dot(call))

  # Use summarise_() with same arguments
  eval_bare(call, env = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fsummarise"))

#' @export
#' @rdname summarising
count_ <- structure(function(.data = (.), ..., wt = NULL, name = "n",
  sort = FALSE, decreasing = TRUE, .drop = TRUE, add = FALSE) {

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
    args <- list(dots = list(), are_formulas = FALSE, env = parent.frame())
  } else {
    args <- formula_masking(...)
  }

  # wt can be NULL, numeric, a character string or a formula like ~name
  if (!is.null(wt) && !is.numeric(wt)) {
    if (is_formula(wt)) {
      if (is_formula(wt, lhs = TRUE))
        stop("{.arg wt} must be a simple formula like {.code ~name}, or a character string.")
      wt <- f_rhs(wt)
      if (!is.symbol(wt))
        stop("{.arg wt} must be a simple formula like {.code ~name}, or a character string.")
      # At the end, wt is always NULL, numeric or a symbol (if formulas) or a
      # character string (if not)
      if (!args$are_formulas)
        wt <- as.character(wt)
    } else if (!is.character(wt)) {

    } else {# It is character
      if (args$are_formulas)
        wt <- as.symbol(wt)
    }
  }

  if (length(name) != 1L || !is.character(name))
    stop("{.arg name} must be a single string, not {.obj_type_friendly {name}} of length {length(name)}.")

  if (!isTRUE(sort) && !isFALSE(sort))
    stop("{.arg sort} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {sort}} ({.val {sort}}).")

  if (!isTRUE(decreasing) && !isFALSE(decreasing))
    stop("{.arg decreasing} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {sort}} ({.val {sort}}).")

  # TODO: .drop = FALSE not implemented yet
  if (isFALSE(.drop)) {
    stop("{.code .drop = FALSE} not implemented yet in {.fun count_}, use {.fun count} instead")
  } else  if (!isTRUE(.drop)) {
    stop("{.arg .drop} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {.drop}} ({.val {.drop}}).")
  }

  if (!isTRUE(add) && !isFALSE(add))
    stop("{.arg add} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {sort}} ({.val {sort}}).")

  is_grouped <- is_grouped_df(.data)
  if (missing(...) && !is_grouped) {# Just return the number of row
    if (is.null(wt)) {
      n <- nrow(.data)
    } else {
      n <- fsum(.data[[as.character(wt)]], na.rm = TRUE)
    }
    if (isTRUE(add)) {
      res <- .data
      res[[name]] <- n
    } else {# Just return n
      res <- .data[1L, 1L]
      names(res) <- name
      res[[name]] <- nrow(.data)
    }
    return(res)
  }

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
      if (args$are_formulas)
        args$dots[is_expr] <- lapply(args$dots[is_expr], as.symbol)
    } else {
      data2 <- .data
    }
  } else {
    data2 <- .data
  }
  if (args$are_formulas) {
    if (is_grouped) {
      gvars <- as.list(fgroup_vars(.data, return = "names"))
      gvars <- lapply(gvars, as.symbol)
      args$dots <- unique(c(gvars, args$dots))
    }
    res <- do.call(fcount, c(args$dots, list(x = data2, w = substitute(wt),
      name = name, add = add, sort = TRUE, decreasing = FALSE)),
      envir = args$env)
  } else {# SE evaluation
    if (is_grouped)
      args$dots <- unique(c(as.list(fgroup_vars(.data, return = "names")),
        args$dots))
    res <- fcountv(data2, cols = unlist(args$dots), w = wt, name = name,
      add = add, sort = TRUE, decreasing = FALSE)
  }

  # sort= argument of dplyr::count sorts the frequency column indeed, not the
  # category column(s)
  if (isTRUE(sort))
    do.call(setorderv, list(res, name, order = if (decreasing) -1 else 1,
      na.last = TRUE))
  #res <- res[order(res[[name]], decreasing = decreasing), ]

  # row.names are always integer indices with dplyr::count()
  if (nrow(res))
    attr(res, "row.names") <- 1:nrow(res)

  if (is_grouped)
    res <- fungroup(res) # Always ungroup at the end

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::count"))

#' @export
#' @rdname summarising
tally_ <- structure(function(.data = (.), wt = NULL, name = "n", sort = FALSE,
  decreasing = TRUE) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data)) {
    call <- sys.call()
    # Directly call count_ here
    call[[1]] <- as.symbol('count_')
    return(recall_with_data_dot(call))
  }
  count_(.data = .data, wt = wt, name = name, sort = sort,
    decreasing = decreasing, add = FALSE)

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::tally"))

#' @export
#' @rdname summarising
add_count_ <- structure(function(.data = (.), ..., wt = NULL, name = "n",
  sort = FALSE, decreasing = TRUE, .drop = TRUE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(.drop))
    warning("the `.drop` argument is deprecated in `add_count_()`")

  do.call('count_', list(.data = .data, ..., wt = substitute(wt), name = name,
    sort = sort, decreasing = decreasing, add = TRUE), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::add_count"))

#' @export
#' @rdname summarising
add_tally_ <- structure(function(.data = (.), wt = NULL, name = "n",
  sort = FALSE, decreasing = TRUE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  do.call('count_', list(.data = .data, wt = substitute(wt), name = name,
    sort = sort, decreasing = decreasing, add = TRUE), envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))

