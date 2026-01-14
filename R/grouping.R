#' Grouping Functions and Group Metadata
#'
#' @description
#' Functions for grouping data frames and accessing group metadata.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `group_by_()` - Group data by one or more variables
#' * `ungroup_()` - Remove grouping variables
#' * `group_vars_()` - Get names or info about grouping variables
#' * `group_rows_()` - Get row indices for each group
#' * `group_data_()` - Get a tibble with grouping data and row indices
#' * `group_indices_()` - Get group index for each row
#' * `group_keys_()` - Get unique values of grouping variables
#' * `groups_()` - Get grouping variables as symbols
#' * `group_size_()` - Get number of rows in each group
#' * `n_groups_()` - Get total number of groups
#' * `as.grouped_df()` / `as_grouped_df()` - Convert to grouped_df object
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param ... For `group_by_()`: grouping variables as formulas (e.g., `~cyl`)
#'   or character names. For `ungroup_()`: optional variables to remove from
#'   grouping (if omitted, removes all grouping).
#' @param .add Logical. If `TRUE`, add new grouping variables to existing ones.
#'   If `FALSE` (default), replace existing grouping.
#' @param .drop Logical. Should unused factor levels be dropped? Default is
#'   `TRUE`, unless the data was previously grouped with `.drop = FALSE`.
#' @param .sort Logical. Should groups be sorted? Default uses the collapse
#'   package setting.
#' @param .decreasing Logical. Should sorting be in decreasing order? Default
#'   is `FALSE`.
#' @param .na.last Logical. Should `NA` values be sorted last? Default is `TRUE`.
#' @param .return.groups Logical. Should group information be stored? Default
#'   is `TRUE`.
#' @param .return.order Logical. Should group order be stored? Default follows
#'   `.sort`.
#' @param .method Character. Method for grouping: `"auto"` (default), `"hash"`,
#'   or `"radix"`.
#' @param return For `group_vars_()`: what to return - `"names"` (default),
#'   `"data"`, `"unique"`, `"indices"`, `"named_indices"`, `"logical"`, or
#'   `"named_logical"`.
#' @param x An object to convert to grouped_df
#'
#' @return
#' * `group_by_()` returns a grouped data frame (GRP_df or grouped_df class)
#' * `ungroup_()` returns the data frame without grouping (or with partial
#'   grouping if specific variables removed)
#' * `group_vars_()` returns names, data, or indices depending on `return` arg
#' * `group_rows_()` returns a list of integer vectors with row indices per
#'   group
#' * `group_data_()` returns a tibble with grouping columns and a `.rows` column
#' * `group_indices_()` returns an integer vector with group ID for each row
#' * `group_keys_()` returns a data frame with unique grouping variable values
#' * `groups_()` returns a list of symbols (grouping variable names)
#' * `group_size_()` returns an integer vector with row counts per group
#' * `n_groups_()` returns a single integer (total number of groups)
#' * `as.grouped_df()` returns a grouped_df object
#'
#' @seealso
#' [dplyr::group_by()], [dplyr::ungroup()], [dplyr::group_vars()],
#' [dplyr::group_rows()], [dplyr::group_data()], [dplyr::group_indices()],
#' [dplyr::group_keys()], [dplyr::groups()], [dplyr::group_size()],
#' [dplyr::n_groups()], [collapse::fgroup_by()]
#'
#' @examples
#' library(svTidy)
#' data(mtcars)
#'
#' # Group by single variable
#' mtcars |> group_by_(~cyl)
#'
#' # Group by multiple variables using formulas
#' mtcars_grouped <- mtcars |> group_by_(~cyl, ~gear)
#'
#' # Group using character names
#' mtcars |> group_by_('cyl', 'gear')
#'
#' # Add grouping variables to existing groups
#' mtcars |>
#'   group_by_(~cyl) |>
#'   group_by_(~gear, .add = TRUE)
#'
#' # Get grouping variable names
#' mtcars_grouped |> group_vars_()
#'
#' # Get number of groups
#' mtcars_grouped |> n_groups_()
#'
#' # Get size of each group
#' mtcars_grouped |> group_size_()
#'
#' # Get group indices for each row
#' mtcars_grouped |> group_indices_()
#'
#' # Get unique grouping keys
#' mtcars_grouped |> group_keys_()
#'
#' # Get row indices for each group
#' mtcars_grouped |> group_rows_()
#'
#' # Get complete group data
#' mtcars_grouped |> group_data_()
#'
#' # Ungroup completely
#' mtcars_grouped |> ungroup_()
#'
#' # Ungroup specific variables
#' mtcars |>
#'   group_by_(~cyl, ~gear, ~am) |>
#'   ungroup_(~gear)
#'
#' # Use with other operations
#' mtcars |>
#'   group_by_(~cyl) |>
#'   summarise_(~mean(mpg), ~mean(hp))
#'
#' @name grouping
#' @export
#' @rdname grouping
group_by_ <- structure(function(.data = (.), ..., .add = FALSE, .drop = NULL,
  .sort = get_collapse("sort"), .decreasing = FALSE, .na.last = TRUE,
  .return.groups = TRUE, .return.order = .sort, .method = "auto") {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # If no grouping variables provided
  if (missing(...)) {
    if (isTRUE(.add)) {# Return unmodified .data
      return(.data)
    } else {# Return an ungrouped data frame
      #let_data.trame_to_data.table(.data)
      #on.exit(let_data.table_to_data.trame(.data))
      return(fungroup(.data))
    }
  }

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  args <- formula_select(..., .fast.allowed.funs = "")

  # Default value for .drop is TRUE, unless previously grouped using FALSE
  if (missing(.drop)) {
    if (is_grouped_df(.data)) {
      .drop <- attr(.data, "groups") |> attr(".drop")
      if (is.null(.drop))
        .drop <- TRUE
    } else {
      .drop <- TRUE
    }
  }

  # There are cases not handled (yet) by collapse::fgroup_by(). So, we use
  # dplyr::group_by() and convert the corresponding object into a GRP_df,
  # also reconverting the tibble into data.frame, data.table, or data.trame...
  # Not very efficient, but the only solution I fould to support those cases.

  # *  .drop = FALSE not implemented yet in fgroup_by() (should be possible,
  #    because converting a grouped_df object into GRP gives the correct groups)
  #    For now, we just use dplyr::group_by() and do the conversion...
  # *  .add = TRUE is also not implemented yet in fgroup_by(), but we have a
  #    solution for group_by_vars() when not using formulas.
  # *  expressions are handled differently by group_by() and fgroup_by(), so,
  #    for any expression found, we automatically switch to group_by().
  if ((isTRUE(.add) && args$are_formulas) || !isTRUE(.drop) || !args$fastselect) {
    # group_by() cannot handle .sort = FALSE, decreasing = TRUE or
    # .na.last = FALSE
    if (isFALSE(.sort) || isTRUE(.decreasing) || isFALSE(.na.last))
      stop("Using {.fun dplyr::group_by_} but it only sorts increasing  with {.code NA} last.",
        i = "So, keep defaults for {.arg .sort}, {.arg .decreasing} and {.arg .na.last}.")

    # Call group_by() and convert the grouped_df object into a GRP_df object
    .__dplyr_error_call__. <- environment() # For correct display of the error
    # Since dplyr::group_by() does not accept character names,
    # we have to transform them into symbols
    args$dots <- lapply(args$dots, function(x) {
      if (is.character(x)) {
        as.symbol(x)
      } else {
        x
      }
    })
    # If we have data already grouped with collapse::fgroup_by(),
    # dplyr::group_by() issues an error, so, we must ungroup it first
    if (inherits(.data, 'GRP_df')) {
      # If we have .add = TRUE; we must handle existing grouping variable too
      if (isTRUE(.add)) {
        gvars <- as.list(fgroup_vars(.data, return = "names"))
        gvars <- lapply(gvars, as.symbol)
        args$dots <- unique(c(gvars, args$dots))
      }
      res0 <- fungroup(.data)
    } else {
      res0 <- .data
    }
    res0 <- do.call(group_by, c(args$dots, list(.data = res0,
      .add = force(.add), .drop = force(.drop))))
    groups <- GRP(res0)
    # We also keep .drop = FALSE attribute
    if (!isTRUE(.drop))
      attr(groups, '.drop') <- FALSE
    res <- .data
    # Add computed group variables into res
    names_newg <- setdiff(names(res0), names(res))
    if (length(names_newg))
      res <- add_vars(res, res0[names_newg])
    attr(res, "groups") <- groups
    # Change the class the same way fgroup_by() does
    class_res <- class(.data)
    class_res <- class_res[!class_res %in% c("grouped_df", "data.frame")]
    class(res) <- c("GRP_df", class_res, "grouped_df", "data.frame")
    if (to_dtrm)
      let_data.table_to_data.trame(res)
    return(res)
  }

  # For all other cases, we can use the faster collapse functions
  if (args$are_formulas) {# Use collapse::fgroup_by()
    res <- do.call(fgroup_by, c(args$dots, list(.X = .data, sort = force(.sort),
      decreasing = force(.decreasing), na.last = force(.na.last),
      return.groups = force(.return.groups),
      return.order = force(.return.order), method = force(.method))))
    # Note: should a problem emerge again with wrong interpretation of formulas
    # We could use this check to detect it:
    ## Check if we are in the case ~c(var1, var2), while we meant ~var1, ~var2
    #ngroups <- length(fgroup_vars(res, return = "names"))
    #largs <- ...length()
    #if (largs != ngroups)
    #  stop("Incorrect grouping formula (you got {.val {ngroups}} groups instead of {.val {largs}}).",
    #    i = "You cannot use {.code ~var1:var2} in {.fun group_by_}.",
    #    i = "and you must use {.code ~var1, ~var2, ...} instead of {.code ~c(var1, var2, ...)}.")

  } else {# Use collapse::groups_by_vars() instead
    gvars <- unlist(args$dots) # Also allows c('var1', 'var2', ...) not in dplyr
    if (isTRUE(.add) && is_grouped_df(.data))
      gvars <- unique(c(fgroup_vars(.data, return = "names"), gvars))
    res <- group_by_vars(.data, by = gvars, sort = .sort,
      decreasing = .decreasing, na.last = .na.last,
      return.groups = .return.groups, return.order = .return.order,
      method = .method)
  }

  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fgroup_by"))

#' @export
#' @rdname grouping
ungroup_ <- structure(function(.data = (.), ..., .na.last = TRUE,
  .method = "auto") {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  if (...length()) {# Ungroup only certain data
    un_gvars <- c(...)
    if (!is.character(un_gvars)) {
      # Try to convert only formulas like ~name into character
      un_gvars <- sapply(un_gvars, function(x) {
        if (is_formula(x)) {
          x <- f_rhs(x)
          if (is.symbol(x))
            as.character(x)
        } else {# Something else, should not happen, so place something wrong
          quote(incorrect_item)
        }
      })
    }
    if (!is.character(un_gvars))
      stop("Incorrect names of the ungrouping variables.",
        i = "Use either {.code ~name} or {.code 'name'} (and don't mix both forms).")

    # Provided variables must be in the dataset
    non_exist <- setdiff(un_gvars, names(.data))
    if (length(non_exist)) {
      stop("Can't select columns that don't exist.",
        x = "Column {.code {non_exist[1]}} doesn't exist.")
    }

    # Consider only variables that are actually in the grouping
    gvars <- setdiff(fgroup_vars(.data, return = "names"), un_gvars)
    # If there remain grouping variables, we keep them
    if (length(gvars)) {
      # If we have a GRP_df object, return a similar object
      if (inherits(.data, "GRP_df")) {
        # Get configuration to apply the same one
        grp <- attr(.data, "groups")
        sort <- isTRUE(grp$ordered[1])
        drop <- attr(grp, ".drop")
        # If drop = FALSE, we must use group_by() and convert... not handled yet!
        if (!isTRUE(drop))
          warning("Regrouping with `.drop = FALSE` is not handled yet, changing to `.drop = TRUE`.")
        decreasing <- isTRUE(grp$ordered[2]) # If sort = TRUE, no matters
        return.groups <- !is.null(grp$groups)
        return.order <- !is.null(grp$order)
        res <- group_by_vars(.data, by = gvars, sort = sort,
          decreasing = decreasing, na.last = .na.last,
          return.groups = return.groups, return.order = return.order,
          method = .method)
        if (to_dtrm)
          let_data.table_to_data.trame(res)
        return(res)

      } else {# A dplyr grouped_df object... for now, don't change it to GRP_df
        drop <- attr(.data, "groups") |> attr(".drop")
        if (is.null(drop)) drop <- TRUE
        res <- group_by(.data, gvars, .drop = drop)
        if (to_dtrm)
          let_data.table_to_data.trame(res)
        return(res)
      }
    }
  }

  # No grouping variables left, so we just ungroup
  res <- fungroup(.data)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fungroup"))

#' @export
#' @rdname grouping
#' @param return What to return: `"data"` or `1`, `"unique"` or `2` for unique
#'   rows of grouping columns, `"names"` or `3` (default) for names of grouping
#'   columns, `"indices"` or `4` for integer indices of grouping columns,
#'   `"named_indices"` or `5` for named indices, `"logicial"` or `6` for logical
#'   selection vector of grouping columns, or `"named_logical"` or `7` for named
#'   logical.
group_vars_ <- function(.data = (.), return = "names") {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    switch(as.character(return),
      "1" =,
      "data" = .data[0L, 0L],
      "2" =,
      "unique" = unique(.data),
      "3" =,
      "names" = character(0),
      "4" =,
      "indices" =,
      "5" =,
      "named_indices" = integer(0),
      "6" =,
      "logical" =,
      "7" =,
      "named_logical" = logical(0),
      stop("Unknown {.arg return} value."))
  } else {
    fgroup_vars(.data, return = return)
  }
}


#' @export
#' @rdname grouping
group_rows_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    # This is to reutrn a <list_of<integer>> like dplyr::group_rows() does
    structure(list(seq_len(nrow(.data))), ptype = integer(0),
      class = c("vctrs_list_of", "vctrs_vctr", "list"))
  } else {
    # Unfortunately, gsplit() skips empty groups, so the hack!
    res <- gsplit(seq_row(.data), GRPid(.data), use.g.names = TRUE)
    ngroups <- length(GRPN(.data, expand = FALSE))
    nms <- as.character(1:ngroups)
    res <- res[nms]
    res <- lapply(res, function(x) if (is.null(x)) integer(0) else x)
    names(res) <- NULL
    structure(res, ptype = integer(0),
      class = c("vctrs_list_of", "vctrs_vctr", "list"))
  }
}


#' @export
#' @rdname grouping
group_data_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    res <- tibble(.rows = 1L)
    res$.rows <- structure(list(seq_row(.data)), ptype = integer(0),
      class = c("vctrs_list_of", "vctrs_vctr", "list"))
    res
  } else {
    res <- as_tibble(fgroup_vars(.data, return = "unique"))
    res$.rows <- group_rows_(.data)
    attr(res, ".drop") <- attr(attr(.data, "groups"), ".drop") %||% TRUE
    res
  }
}


#' @export
#' @rdname grouping
group_indices_ <- function(.data = (.), ...) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...))
    check_dots_empty()

  if (!is_grouped_df(.data)) {
    rep_len(1, nrow(.data))
  } else {
    GRPid(.data)
  }
}


#' @export
#' @rdname grouping
group_keys_ <- function(.data = (.), ...) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...))
    check_dots_empty()

  if (!is_grouped_df(.data)) {
    .data[1L, 0L]
  } else {
    fgroup_vars(.data, return = "unique")
  }
}


#' @export
#' @rdname grouping
groups_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    list()
  } else {
    lapply(as.list(fgroup_vars(.data, return = "names")), as.symbol)
  }
}


#' @export
#' @rdname grouping
group_size_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    nrow(.data)
  } else {
    GRPN(.data, expand = FALSE)
  }
}


#' @export
#' @rdname grouping
n_groups_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    1L
  } else {
    length(GRPN(.data, expand = FALSE))
  }
}


#' @export
#' @rdname grouping
as.grouped_df <- function(x, ...) {
  UseMethod("as.grouped_df")
}


#' @export
#' @rdname grouping
as_grouped_df <- as.grouped_df


#' @export
#' @rdname grouping
as.grouped_df.default <- function(x, ...) {
  stop("Don't know how to convert an object of class `%s` to a grouped_df.",
    class(x)[1])
}


#' @export
#' @rdname grouping
as.grouped_df.grouped_df <- function(x, ...) {
  x # Just return the object
}


#' @export
#' @rdname grouping
as.grouped_df.GRP_df <- function(x, ...) {
  # Construct a grouped_df object from a GRP_df object
  res <- x
  attr(res, "groups") <- group_data_(x)
  classes <- c(class(x))
  setv(classes, "GRP_df", "grouped_df")
  class(res) <- unique(classes)
  res
}


#' @export
#' @rdname grouping
print.grouped_df <- function(x, ...) {
  # We create grouped_df objects that could be something else than tibbles
  # and in this casen groups are NOT printed by default

  if (is_tibble(x)) {
    NextMethod("print")
  } else {
    gvars <- paste(group_vars_(x), collapse = ", ")
    grps <- n_groups_(x)
    mark <- if (identical(getOption("OutDec"), ",")) "." else ","
    grps <- formatC(x, big.mark = mark)
    cat(col_red("# Groups: ", gvars, " [", grps, "]\n", sep = ""))
    print(ungroup_(x))
  }
  invisible(x)
}


