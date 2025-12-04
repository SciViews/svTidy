# TODO: explore Tailcall, Exec and Recall for more efficiently calling functions
# than do.call() (reusing the current environment), e.g., for data_dot mechanism

#' SciViews functions (mainly from collapse and data.table) to manipulate data frames
#'
#' @description A SciViews::R version of the tidyverse functions in \{dplyr\}
#' and \{tidyr\} with standard evaluation, and non-standard evaluation trough
#' formulas. These functions end with an underscore `_`. Avoid mixing tidy,
#' speedy and SciViews functions in the same pipeline.
#'
#' @param .data A data frame (data.frame, data.table or tibble's tbl_df)
#' @param ... Arguments dependent to the context of the function and most of
#'   the time, not evaluated in a standard way (cf. the tidyverse approach).
#' @param .add If `TRUE`, the grouping variables are added to the existing ones.
#' @param .na.last How to treat missing values in groups? Assign them to the last
#'   group by default (`TRUE`).
#' @param .return.groups If `TRUE`, the grouping variables are returned in the GRP
#'   object (default).
#' @param .return.order If `TRUE`, the order of the grouping variables is
#'   returned in the object (by default, same value as `sort=`).
#' @param .method The algorithm to use for grouping:  `"radix"`, `"hash"`, or
#'   `"auto"` (by default). `"auto"` chose `"radix"` when `sort = TRUE` and
#'   `"hash"` otherwise.
#' @param .by A list of names of the columns to use for grouping the data.
#' @param .preserve When data is grouped, do we preserve grouping or recalculate
#'   it according to the new data frame obtained?
#' @param .groups How to treat the grouping variables in the result? Possible
#'   values are `"drop_last"` (default), `"drop"` (no grouping variables),
#'   `"keep"` (keep all grouping variables), or `"rowwise"` (not implemented
#'   yet).
#' @param .keep.group_vars If `TRUE` (by default), the grouping variables are
#'   kept in the result.
#' @param .fn A function to use.
#' @param .cols The list of the column where to apply the transformation. For
#'   the moment, only all existing columns, which means `.cols = everything()`
#'   is implemented
#' @param .keep Which columns to keep. The default is `"all"`, possible values
#' are `"used"`, `"unused"`, or `"none"` (see [mutate()]).
#' @param x A data frame (data.frame, data.table or tibble's tbl_df).
#' @param y A second data frame.
#' @param by A list of names of the columns to use for joining the two data
#' frames. Could also be a join specification created with [dplyr::join_by()],
#' but in this case, calculation is delegated to dplyr's join methods.
#' @param suffix The suffix to the column names to use to differentiate the
#' columns that come from the first or the second data frame. By default it is
#' `c(".x", ".y")`.
#' @param copy This argument is there for compatibility with the "t" matching
#' functions, but it is not used here.
#' @param .id The name of the column for the origin id, either names if all
#' other arguments are named, or numbers.
#' @param .name_repair How should the name be "repaired" to avoid duplicate
#' column names? See [dplyr::bind_cols()] for more details.
#' @param .by_group Logical. If `TRUE` rows are first arranger by the grouping
#' variables in any. `FALSE` by default.
#' @param wt Frequency weights. Can be `NULL` or a variable. Use data masking.
#' @param .sort If `TRUE` groups are sorted.
#' @param sort If `TRUE` largest group will be shown on top.
#' @param .drop Are levels with no observations dropped (`TRUE` by default).
#' @param add Add counts to the data frame (`FALSE` by default).
#' @param decreasing Is sorting done in decreasing order (`FALSE` by default)?
#' @param .decreasing Is sorting done in decreasing order (`FALSE` by default)?
#' @param name The name of the new column in the output (`n` by default, and no
#' existing column must have this name, or an error is generated).4
#' @param var A variable specified as a name, a positive or a negative integer
#'   (counting from the end). The default is `-1` and returns last variable.
#' @param .keep_all If `TRUE` keep all variables in `.data`.
#' @param .before Place new columns before this one.
#' @param .after Place new columns after this one.
#' @param data A data frame, or for `replace_na()` a vector or a data frame.
#' @param replace If `data` is a vector, a unique value to replace `NA`s,
#' otherwise, a list of values, one per column of the data frame.
#' @param cols A selection of the columns using tidy-select syntax,
#'   see[tidyr::pivot_longer()].
#' @param names_to A character vector with the name or names of the columns for
#'   the names.
#' @param values_to A string with the name of the column that receives the
#'   values.
#' @param names_from The column or columns containing the names (use tidy
#'   selection and do not quote the names).
#' @param values_from Idem for the column or columns that contain the values.
#' @param weights A vector of weight to use to "uncount" `data`.
#' @param .remove If `TRUE`, and `weights` is the name of a column, that column
#'   is removed from `data`.
#' @param col The name quoted or not of the new column with united variable.
#' @param sep Separator to use between values for united or separated columns.
#' @param remove If 'TRUE' the columns used to unite are removed.
#' @param na.rm If `TRUE`, `NA`s are eliminated before uniting the values.
#' @param into Name of the new column to put separated variables. Use `NA` for
#'   items to drop.
#' @param remove If `TRUE` the initial columns that are separated are also
#'   removed from `data`.
#' @param convert If `'TRUE` resulting values are converted into numeric,
#' integer or logical.
#' @param .direction Direction in which to fill missing data: `"down"` (by
#'   default), `"up"`, or `"downup"` (first down, then up), `"updown"`
#'   (the opposite).
#'
#' @note The `summarise_()` function does not support `n()` as does
#' [dplyr::summarise()]. You can use [svBase::fn()] instead, but then, you must
#' give a variable name as argument. The [svBase::fn()] alternative can also be
#' used in [dplyr::summarise()] for homogeneous syntax between the two.
#' From \{dplyr\}, the `slice_min()`, `slice_max()` and `splice_sample()`
#' functions are not added yet.
#' From \{tidyr\} [tidyr::expand()], [tidyr::chop()], [tidyr::unchop()],
#' [tidyr::nest()], [tidyr::unnest()], [tidyr::unnest_longer()],
#' [tidyr::unnest_wider()], [tidyr::hoist()], [tidyr::pack()] and
#' [tidyr::unpack()] are not implemented yet.
#'
#' @return See corresponding "non-SciViews" function for the full help page with
#' indication of the return values.
#'
#' @export
#' @name sciviews_functions
#'
#' @examples
#' # TODO...
list_sciviews_functions <- function() {
  c("add_count_", "add_tally_", "anti_join", "arrange_", "bind_cols_",
    "bind_rows_", "count_", "distinct_", "drop_na_", #"extract_", # TODO
    "fill_",
    "filter_", "full_join_", "group_by_", "inner_join_", "join_", "left_join_",
    "mutate_", "pivot_longer_", "pivot_wider_", "pull_", "reframe_", "rename_",
    "rename_with_", "replace_na_", "right_join_", "select_", "semi_join",
    "separate_", #"separate_rows_", # TODO
    "slice_", "summarise_", "summarize_()",
    "tally_", "transmute_", "uncount_", "ungroup_", "unite_")
}

.src_sciviews <- function(src,
  comment = "A SciViews function, see ?sciviews_functions.") {
  attr(comment, "src") <- src
  comment
}

# Basically, all_of() does nothing, it is useless in SciViews functions,
# but we define it here to be compatible with tidy-select and dplyr.
#' @export
#' @rdname sciviews_functions
all_of <- function(x) x

# dplyr and collapse grouped data are different. Worse: collapse::GRP() defines
# its grouped data frame as GRP_df/.../grouped_df/data.frame, but since the
# "groups" attribute is not the same, dplyr::validate_grouped_df() fails!
# Consequently, we cannot use a grouped object obtained from collapse function
# with dplyr functions, but the opposite can be done. Here, we need a function
# to convert a collapse grouped data frame into a dplyr one.
# Also, dplyr has .drop = FALSE, with no collapse equivalent... and it has also
# rowwise() with no collapse equivalent. On the other hand, with collapse, one
# can sort the groups or not, and decide to sort them increasing or decreasing
# order, which is not possible with dplyr. dplyr ALWAYS sort using group_by(),
# and NEVER sorts using the by=/.by= argument. All this is much too complicated.
# How do we sort this out in SciViews??? Let's start with an as.grouped_df()
# method that converts from collapse to dplyr version of the grouped data frame.

#' @export
#' @rdname sciviews_functions
as.grouped_df <- function(x, ...) {
  UseMethod("as.grouped_df")
}

#' @export
#' @rdname sciviews_functions
as_grouped_df <- as.grouped_df

#' @export
#' @rdname sciviews_functions
as.grouped_df.default <- function(x, ...) {
  stop("Don't know how to convert an object of class `%s` to a grouped_df.",
    class(x)[1])
}

#' @export
#' @rdname sciviews_functions
as.grouped_df.grouped_df <- function(x, ...) {
  x # Just return the object
}

#' @export
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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

#' @export
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
n_groups_ <- function(.data = (.)) {
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is_grouped_df(.data)) {
    1L
  } else {
    length(GRPN(.data, expand = FALSE))
  }
}

# TODO: default .drop as in dplyr::group_by()
# TODO: what are those "computations" that one can give to group_by()? See doc
# TODO: also compute the grouping variables in the data frame itself, like
# group_by does (+ an argument to do so or not)
#' @export
#' @rdname sciviews_functions
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
    res0 <- do.call('group_by', c(args$dots, list(.data = res0,
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
    res <- do.call('fgroup_by', c(args$dots, list(.X = .data, sort = force(.sort),
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

# TODO: use something like collapse:::fungroup2 where applicable
# Note: only standard evaluation for ... for now
# Note: in dplyr, it is ungroup(x, ...), but changed x here to .data
#' @export
#' @rdname sciviews_functions
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

# rename_() seems OK
#' @export
#' @rdname sciviews_functions
rename_ <- structure(function(.data = (.), ...) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # If no renaming arguments, return .data
  if (missing(...))
    return(.data)

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  args <- formula_select(..., .must.be.named = TRUE)

  # collapse::frename() uses old_name = new_name, but dplyr::rename() uses
  # new_name = old_name -> make frename() compatible with dplyr::rename()
  # Also, the cols= argument with a function matches rename_with() instead.
  # Transform symbols into names and flatten the list
  dots_flat <- unlist(lapply(args$dots, function(x) {
    if (is.symbol(x)) {
      as.character(x)
    } else {
      x
    }
  }))
  if (is.list(dots_flat))
    stop("Incorrect renaming inputs.",
      i = "Use {.code new_name = <old_name>} with {.code <old_name>} as a single {.cls character} or {.cls symbol}.")
  # If values are numbers, these are indices -> replace then by names
  # (because frename() cannot use indices)
  # Otherwise, invert names and values

  if (is.numeric(dots_flat)) {
    # Apparently, dplyr::rename() ignore both 0 and negative indices
    dots_flat <- dots_flat[dots_flat > 0] # Keep only positive indices
    dots_inv <- names(dots_flat)
    names(dots_inv) <- names(.data)[dots_flat]
  } else {
    dots_inv <- names(dots_flat)
    names(dots_inv) <- dots_flat
  }

  # Check that all old names are in .data
  # Remember: we have now old_name = "new_name" in dots_inv
  missing_cols <- setdiff(names(dots_inv), names(.data))
  if (length(missing_cols))
    stop("Can't rename columns that don't exist.",
      x = "Column {.code {missing_cols[1]}} doesn't exist.")

  res <- do.call('frename', c(list(.x = .data, .nse = FALSE), dots_inv),
    envir = args$env)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::rename"))

#everything_ <- function() {
#  stop("This should not be called directly, see ?mutate_with.")
#}

# The equivalent of rename_with() "feature complete", but as slow as the dplyr version
#' @export
#' @rdname sciviews_functions
rename_with_ <- structure(function(.data = (.), .fn,
  .cols = ~everything(), ...) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Special case .cols = ~everything()
  if (length(.cols) == 2L && substitute(.cols) == quote(~everything())) {
    .cols <- NULL
  } else {# Selection of some columns
    if (is_formula(.cols))
      .cols <- eval_select(f_rhs(.cols), .data)
    if (any(is.na(.cols)))
      stop("Selections can't have missing values.")
    if (is.character(.cols)) {
      all_cols <- seq_along(.data)
      names(all_cols) <- names(.data)
      .cols2 <- .cols
      .cols <- all_cols[.cols]
      if (anyNA(.cols))
        stop("Can't select columns that don't exist.",
          x = "Column {.code {(.cols2[is.na(.cols)][1])}} doesn't exist.")
    } else if (is.numeric(.cols)) {
      wrong_cols <- abs(.cols) > ncol(.data)
      if (any(wrong_cols))
        stop("Can't select columns past the end.",
          i = "Location{?s} {as.character(abs(.cols[wrong_cols]))} {?doesn't/don't} exist.",
          i = "There {?is/are} only {ncol(.data)} column{?s}.")

    } else if (is.logical(.cols)) {
      if (length(.cols) != ncol(.data))
        stop("Logical selection must match columns ({ncol(.data)}).")

      setv(.cols, NA, FALSE) # NA are considered as FALSE here
    } else if (anyNA(.cols)) {
      stop("Selections can't have missing values.")
    }
    if (!length(.cols)) # No column to rename, return .data
      return(.data)
  }

  if (missing(.fn))
    stop("Argument {.arg .fn} is missing, with no default.")
  if (is_formula(.fn)) {
    if (is_formula(.fn, lhs = FALSE)) {# Check for lhs absence
      # The convention being to use `.x` for the argument, we put names in .x
      # and the execute the call on it
      .call <- f_rhs(.fn)
      if (is.null(.cols)) {# Change all columns
        .x <- c(names(.data)) # c() to make sure to have a copy!
        names(.data) <- eval(.call)
      } else {# Change only a selection
        .x <- names(.data)[.cols]
        names(.data)[.cols] <- eval(.call)
      }
      return(.data)
    } else {
      stop("Can't convert {.arg .fn}, a two-sided {.cls formula}, to a {.cls function}.")
    }
  }

  # Now, we are supposed to get the name of a function...
  if (length(.fn) != 1L)
    stop("Can't convert {.arg .fn}, a {.cls {typeof(.fn)}} vector, to a {.cls function}.")
  if (is.character(.fn))
    .fn <- get0(.fn, envir = parent.frame(), mode = 'function')
  if (!is.function(.fn))
    stop("Can't convert {.arg .fn}, a {.cls {typeof(.fn)}} vector, to a {.cls function}.")

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  if (is.null(.cols)) {# Special case .cols = ~everything()
    # cols= missing to apply to all columns
    res <- frename(.data, .fn, ..., .nse = FALSE)
  } else {
    args <- formula_select(.cols)
    res <- frename(.data, .fn, ..., cols = .cols, .nse = FALSE)
  }
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::rename_with"))

#' @export
#' @rdname sciviews_functions
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
    stop("Argument {.code .preserve = TRUE} is not supported in {.fun filter_}.",
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
          gdata <- do.call('fsubset', list(.x = gdata, filters[[i]]),
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
        res <- do.call('fsubset', list(.x = res, filters[[i]]), envir = args$env)
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

# Do we need to transform temporarily data.trame into data.table here?
#' @export
#' @rdname sciviews_functions
select_ <- structure(function(.data = (.), ...) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  # If no selection is provided
  if (missing(...)) # Return a data frame with zero columns
    return(.data[0L, , drop = FALSE])

  # Process dots to args
  args <- formula_select(..., .fast.allowed.funs = c(":", "-", "c", "("))
  if (!args$fastselect) {
    #message(gettextf("Using tidyselect with `%s`",
    #  paste(args$dots, collapse = ", ")))
    eval_select2 <- function(..., data)
      eval_select(expr(c(...)), data = data, error_call = stop_top_call())
    loc <- do.call('eval_select2', c(args$dots, list(data = .data)))
    if (to_dtrm)
      let_data.table_to_data.trame(.data)
    res <- .data[, loc, drop = FALSE]
    names(res) <- names(loc)


  } else {# fastselect with collapse
    if (args$are_formulas) {
      # Other return modes not supported (yet)
      res <- do.call('fselect', c(list(.x = .data, return = "data"), args$dots),
        envir = args$env)
    } else {
      res <- get_vars(.data, unlist(args$dots), rename = TRUE)
    }
  }
  if (to_dtrm)
    let_data.table_to_data.trame(res)

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fselect"))

# TODO: .before and .after
#' @export
#' @rdname sciviews_functions
mutate_ <- structure(function(.data = (.), ..., .by = NULL,
  .keep = "all", .before = NULL, .after = NULL, .cols = NULL) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!is.character(.keep))
    stop("{.arg .keep} must be a string or character vector.")
  if (length(.keep) != 1L)
    stop("{.arg .keep} must be a single string, not a vector of length {.val {length(.keep)}}.")
  .keep <- .keep
  if (fmatch(.keep, c("all", "used", "unused", "none"), 0L) == 0L)
    stop("{.arg .keep} must be one of \"all\", \"used\", \"unused\", or \"none\", not \"{(.keep)}\".")

  is_grouped <- is_grouped_df(.data)

  # Case missing(...), just return the original data frame
  if (missing(...))
    return(.data)

  # Treat data.trames as data.tables (not needed here?)
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

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
    #let_data.table_to_data.trame(.data)
    res <- group_by_vars(.data, by = .by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
  } else {
    res <- .data
  }

  res <- do.call('fmutate', c(list(.data = res), args$dots,
    list(.keep = force(.keep), .cols = force(.cols))), envir = args$env)
  if (!missing(.by))
    res <- fungroup(res)
  if (!missing(.before) || !missing(.after)) {# Reorder the columns
    # TODO: not implemented yet
    stop("{.arg .before} and {.arg .after} are not implemented yet in {.fun mutate_}.",
      i = "Use {.fun mutate} instead.")
    #res_names <- names(res)
    #data_names <- names(.data)
  }
  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
},
  class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fmutate"))

#' @export
#' @rdname sciviews_functions
transmute_ <- structure(function(.data, ...) {
  .__top_call__. <- TRUE
  do.call(mutate_, list(.data, ..., .keep = "none"), envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::transmute"))

# On the contrary to summarise() we always do not warn if summarise() returns
# several rows (but reframe_() is there too)
# Also, .group = "rowwise" is not implemented here
# TODO: across_() that constructs a list of formulas
# TODO: n_() that gets the number of items in the group
#' @export
#' @rdname sciviews_functions
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
    # If data is grouped, we return the number of observations par level
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

  res <- do.call('fsummarise', c(list(.data = .data), args$dots,
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
#' @rdname sciviews_functions
summarize_ <- summarise_

# Reframe_() works unless some groups return a 0-row result. It also does not
# support the return of a data.frame with several columns, nor across()
# The syntax using .cols/.data does not work either
#' @export
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
#' @param .locale The locale to sort character vectors in. If `NULL`(default),
#'   use `"C"` locale.
arrange_ <- structure(function(.data = (.), ..., .by_group = FALSE,
  .locale = "C", .decreasing = FALSE) {

  .__dplyr_error_call__. <- environment()
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Case missing(...), or no rows, or only one row, just return the data frame
  if (missing(...) || nrow(.data) < 2L)
    return(.data)

  # Not necessary?
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # dplyr uses desc(var) and collapse uses -var to sort in descending order
  args <- formula_select(..., .fast.allowed.funs = c("desc", "-"))

  if (is.null(.locale) && !isTRUE(getOption("dplyr.legacy_locale")))
    .locale <- "C" # Default locale for sorting

  if (!is.null(.locale) && .locale == "C" && args$fastselect) {
    # We can use the fast collapse::roworder()/roworderv()
    if (args$are_formulas) {
      if (!missing(.decreasing))
        warning("`.decreasing` is ignored when using formulas in arrange_().")
      # Transform desc(var) into -var, since collapse does not understand desc()
      minus <- as.symbol('-')
      args$dots <- lapply(args$dots, function(x) {
        if (is.call(x) && x[[1]] == 'desc')
          x[[1]] <- minus
        x
      })
    } else {# SE mode: unlist the arguments, and possibly "extract" .decreasing=
      # from the arguments
      ovars <- unlist(args$dots)
      if (!is.character(ovars))
        stop("All arguments in {.code ...} must be column names or formulas.")
      if (missing(.decreasing)) {# Recreate it from the vars (TRUE when '-var')
        .decreasing <- startsWith(ovars, "-")
        ovars[.decreasing] <- substring(ovars[.decreasing], 2L) # Eliminate '-'
      }
    }
    # If this is a grouped_df, we ungroup, sort, and then, recalculate groups
    if (is_grouped_df(.data)) {
      gvars <- fgroup_vars(.data, return = "names")
      grp <- attr(.data, "groups")
      desc <- isTRUE(grp$ordered[2])
      drop <- attr(grp, ".drop")

      if (args$are_formulas) {
        # If we sort by groups, prepend the grouping variables to the list
        if (isTRUE(.by_group)) {
          if (desc)
            gvars <- paste0("-", gvars) # Collapse understands '-var'
          args$dots <- c(as.list(gvars), args$dots)
        }
        res <- do.call('roworder', c(list(X = fungroup(.data)), args$dots),
          envir = args$env)

      } else {# SE arguments
        # If we sort by groups, prepend the grouping variables to the list
        if (isTRUE(.by_group)) {
          ovars <- c(gvars, ovars)
          odesc <- c(rep_along(gvars, desc), rep_along(ovars, .decreasing))
        } else {
          odesc <- .decreasing
        }
        res <- roworderv(fungroup(.data), cols = ovars, decreasing = odesc)
      }

      # Recalculate groups
      # Collect required data to redo a similar grouping after sorting
      is_grp <- inherits(.data, "GRP_df") # either a GRP_df or a grouped_df
      drop <- attr(grp, ".drop")
      # Regroup
      if (is_grp) {
        # If drop = FALSE, we must use group_by() and convert... not handled yet!
        if (isFALSE(drop))
          warning("Regrouping with `.drop = FALSE` is not handled yet, changing to `.drop = TRUE`.")
        sort <- isTRUE(grp$ordered[1])
        return.groups <- !is.null(grp$groups)
        return.order <- !is.null(grp$order)
        # How do I got na.last and method? Better to always keep defaults here?
        res <- group_by_vars(res, by = gvars, sort = sort,
          decreasing = desc, na.last = TRUE, return.groups = return.groups,
          return.order = return.order, method = 'auto')

      } else {# Regroup with dplyr::group_by()
        gvars_sym <- lapply(as.list(gvars), as.symbol)
        res <- do.call('group_by', c(list(.data = res, .drop = drop),
          gvars_sym), envir = args$env)
      }

    } else {# Ungrouped dataset, no particular difficulties
      if (args$are_formulas) {
        res <- do.call('roworder', c(list(X = .data), args$dots),
          envir = args$env)
      } else {# Not using formulas
        res <- roworderv(.data, cols = ovars, decreasing = .decreasing)
      }
    }

  } else {# .locale != "C", or complex data-masking: fall back to dplyr/stringi
    desc <- as.symbol('desc')
    # If we supply .decreasing=, must transform into desc(var)
    if (!missing(.decreasing)) {
      .decrease <- rep_along(args$dots, .decreasing)
      for (i in 1:length(.decrease)) {
        if (isTRUE(.decrease[i])) {
          x <- args$dots[[i]]
          args$dots[[i]] <- as.call(list(desc, as.symbol(x))) # desc(var)
        }
      }
    }
    # Transform -var into desc(var), since dplyr does not understand -var
    args$dots <- lapply(args$dots, function(x) {
      if (is.call(x) && x[[1]] == '-') {
        x[[1]] <- desc
      } else if (is.character(x)) {# arrange() does not support 'var' here!
        # If we have '-var', we must transform it into desc(var)
        if (startsWith(x, "-")) {
          x <- substring(x, 2L) # Eliminate '-'
          x <- as.call(list(desc, as.symbol(x))) # desc(var)
        } else {
          x <- as.symbol(x) # Just a symbol
        }
      }
      x
    })
    # If we have a GRP_df object, we cannot proceed
    if (inherits(.data, "GRP_df")) {
      if (.locale != "C") {
        stop("Cannot use {.fun arrange_} with \"{(.locale)}\" locale on these data.",
          i = "Either ungroup the data first, or use {.fun group_by} for grouping.")
      } else {# Complex data-masking
        stop("Cannot use {.fun arrange_} with complex data masking on these data.",
          i = "Either ungroup the data first, or use {.fun group_by} for grouping.")
      }
    }
    res <- do.call('arrange', c(list(.data), args$dots,
      list(.by_group = .by_group, .locale = .locale)), envir = args$env)
  }

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::arrange"))

# TODO: use default .rownames for name?
#' @export
#' @rdname sciviews_functions
pull_ <- structure(function(.data = (.), var = -1, name = NULL, ...) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  ## For now, we use same function as txxx() counterpart... still must rework
  #if (inherits(.data, c("tbl_db", "dtplyr_step")))
  #  stop("You must collect results before using this function.",
  #    i = "Use {.fun collect_dtx} first.")

  if (!missing(...))
    check_dots_empty()

  if (is_formula(var)) {
    env <- f_env(var)
    var <- f_rhs(var)
    if (is.symbol(var)) {
      var <- as.character(var)
    } else {
      var <- eval(var, envir = env)
    }
  }
  if (is.numeric(var) && var < 0)
    var <- ncol(.data) + 1 + var
  res <- c(.data[[var]])

  if (!is.null(name)) {
    if (is_formula(name)) {
      env <- f_env(name)
      name <- f_rhs(name)
      if (is.symbol(name)) {
        name <- as.character(name)
      } else {
        name <- eval(name, envir = env)
      }
    }
    if (is.numeric(name) && name < 0)
      name <- ncol(.data)  + 1 + name
    names(res) <- c(.data[[name]])
  }
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::pull"))

#' @export
#' @rdname sciviews_functions
#' @param how Can be "full" (default), "inner", "left", "right", "semi", or "anti".
join_ <- function(x, y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", unmatched = "drop", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL, how = "full") {

  .__top_call__. <- TRUE

  if (!missing(...))
    check_dots_empty()

  #if (!missing(copy))
  #  warning("`copy =` is only for compatibility with `right_join()` but it does nothing.")

  na_matches <- na_matches[1]
  if (!(isTRUE(na_matches == "na") || isTRUE(na_matches == "never")))
    stop("{.arg na_matches} must be one of \"na\" or \"never\", not {.obj_type_friendly {na_matches}} (it is {.val {na_matches}}).")

  if (length(multiple) != 1L ||
      fmatch(multiple, c("all", "first", "any", "last"), nomatch = 0L) == 0L)
    stop("{.arg multiple} must be one of \"all\", \"first\", \"any\", or \"last\", not {.obj_type_friendly {multiple}} (it is {.val {multiple}}).")

  if (!(isTRUE(unmatched == "drop") || isTRUE(unmatched == "error") ||
      is.list(unmatched))) # List for collapse::join()
    stop("{.arg unmatched} must be \"drop\", \"error\", or a {.cls list}, not {.obj_type_friendly {unmatched}} (it is {.val {unmatched}}).")

  # Depending on the arguments, we use the fast collapse::join(), or the slower
  # dplyr::xxx_join(), also if a 'dplyr_join_by' object is provided for `by=`
  # Exception for right_join() where collapse::join() does not produce the same
  # result as dplyr::right_join(), for the other, "first" is OK for collapse::join()
  if (how == "right") {
    multi_no_collapse <- c("first", "any", "last")
  } else {
    multi_no_collapse <- c("any", "last")
  }
  if (isTRUE(keep) || na_matches == "never" ||
      multiple %in% multi_no_collapse || inherits(by, 'dplyr_join_by')) {
    # These are not handled by collapse::join()
    # Note: sort, verbose, column and attr not used!
    if (isTRUE(sort) || verbose > 0 || !is.null(column) || !is.null(attr))
      warning("Using dplyr join method: `sort`, `verbose`, `column` and `attr` ignored.")
    if (is.list(unmatched)) {
      warning("`unmatched = \"error\"` (`dplyr::right_joint()` cannot handle a list).")
      unmatched <- "error"
    }
    .__dplyr_error_call__. <- environment()
    dplyr_join <- switch(how,
      "inner" = inner_join,
      "left"  = left_join,
      "right" = right_join,
      "full"  = full_join,
      "semi"  = semi_join,
      "anti"  = anti_join,
      stop("{.arg how} must be one of \"inner\", \"left\", \"right\", \"full\", \"semi\", or \"anti\"."))
    # There are less arguments for semi and anti joins (and full)
    if (how %in% c("semi", "anti")) {
      return(dplyr_join(x, y, by = by, copy = copy, na_matches = na_matches))
    } else if (how == "full") {
      return(dplyr_join(x, y, by = by, copy = copy, suffix = suffix, keep = keep,
        na_matches = na_matches, multiple = multiple,
        relationship = relationship))
    } else {
      return(dplyr_join(x, y, by = by, copy = copy, suffix = suffix, keep = keep,
        na_matches = na_matches, multiple = multiple, unmatched = unmatched,
        relationship = relationship))
    }
  }

  # Otherwise, use collapse::join()
  if (!is.character(suffix) || length(suffix) != 2L)
    stop("{.arg suffix} must be a character vector of length 2, not {.obj_type_friendly {suffix}} of length {length(suffix)}.")

  if (!is.null(keep) && !isFALSE(keep))
    stop("{.arg keep} must be {.code TRUE}, {.code FALSE}, or {.code NULL}, not {.obj_type_friendly {keep}} (it is {.val {keep}}).")

  if (is.null(relationship)) {
    validate <- "m:m"
  } else if (length(relationship) != 1L || !is.character(relationship)) {
    stop("{.arg relationship} must be a single string, not {.obj_type_friendly {relationship}} of length {length(relationship)}.")
  } else {
    validate <- switch(relationship,
      "one-to-one" = "1:1",
      "one-to-many" = "1:m",
      "many-to-one" = "m:1",
      "many-to-many" = "m:m",
      stop("{.arg relationship} must be one of \"one-to-one\", \"one-to-many\", \"many-to-one\", or \"many-to-many\"."))
  }

  mult <- multiple == "all"

  if (length(verbose) != 1 || fmatch(verbose, c(0, 1, 2), nomatch = 0L) == 0L)
    stop("{.arg verbose} must be one of 0, 1, or 2, not {.obj_type_friendly {verbose}} (it is {.val {verbose}}).")

  if (is.list(unmatched)) {
    require <- unmatched
  } else if (unmatched == "error") {
    require <- list(x = 1, y = 1, fail = "error")
  } else {# Must be unmatched == "drop"
    require = NULL
  }
  if (!is.null(require) && verbose == 0)
    verbose <- 1

  if (!isTRUE(sort) && !isFALSE(sort))
    stop("{.arg sort} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {sort}} (it is {.val {sort}}).")

  # Better manage errors returned by collapse::join()
  .temp_env <- temp_env()
  .temp_env$.stop_top_call_env <- stop_top_call(1L)

  err <- function(e) {
    call_env = svMisc::get_temp(".stop_top_call_env")
    svMisc::rm_temp(".stop_top_call_env")
    msg <- e$message
    msg2 <- svBase::process_error_msg(msg, list(
      "x must be a list" = c("{msg}",
        i = "Provide a valid {.cls data.frame} for {.arg x}."),
      "y must be a list" = c("{msg}",
        i = "Provide a valid {.cls data.frame} for {.arg y}."),
      "Unknown x columns" = c("{msg}",
        i = "All {.arg by} columns must be present in {.arg x}."),
      "Unknown y columns" = c("{msg}",
        i = "All {.arg by} columns must be present in {.arg y}."),
      "Join is not 1:1" = c("{msg}",
        i = "Each row in {.arg x} must match exactly 1 row in {.arg y}."),
      "Join is not 1:m" = c("{msg}",
        i = "Each row in {.arg y} must match at most 1 row in {.arg x}."),
      "Join is not m:1" = c("{msg}",
        i = "Each row in {.arg x} must match at most 1 row in {.arg y}.")
    ), fixed = TRUE)
    cli::cli_abort(c(msg2,
      `*` = sprintf("{.emph from: {.code {expr_text(e$call, nlines = 3L)}}}")),
      call = call_env, .frame = call_env)
  }
  res <- withCallingHandlers(error = err, expr =
      do.call('join', list(substitute(x), substitute(y), on = by, how = how,
        suffix = suffix, validate = validate,
        multiple = mult, sort = sort, keep.col.order = TRUE,
        drop.dup.cols = FALSE, verbose = verbose, require = require,
        column = column, attr = attr), envir = parent.frame()))
  .temp_env$.stop_top_call_env <- NULL

  res
}


# TODO: it should be possible to recreate what dplyr's keep = TRUE produces,
# starting from collapse's column = TRUE.
#' @export
#' @rdname sciviews_functions
#' @param keep Should the join keys from both `x` and `y` be preserved in the
#' output? If `NULL`, the default, joins on equality retain only the keys from
#' `x`, while joins on inequality retain the keys from both inputs. If `TRUE`,
#' all keys from both inputs are retained. If `FALSE`, only keys from `x` are
#' retained. For right and full joins, the data in key columns corresponding to
#' rows that only exist in `y` are merged into the key columns from `x`. Can't
#' be used when joining on inequality conditions. If `keep = TRUE`, calculation
#' is delegated to dplyr join methods.
#' @param na_matches Should two `NA` or two `NaN` values match? `"na"`, the
#' default, treats two `NA` or two `NaN` values as equal, like `%in%`,
#' `match()`, `and merge()`. `"never"` treats two `NA` or two `NaN` values as
#' different, and will never match them together or to any other values. This is
#' similar to joins for database sources and to
#' `base::merge(incomparables = NA)`. If `"never"`, calculation is delegated to
#' dplyr join methods.
#' @param multiple Handling of rows in `x` with multiple matches in `y`. For
#' each row of `x`: `"all"`, the default, returns every match detected in `y`.
#' This is the same behavior as SQL. `"any"` returns one match detected in `y`,
#' with no guarantees on which match will be returned. It is often faster than
#' `"first"` and `"last"` in dplyr, but avoid it here. `"first"` returns the
#' first match detected in `y`. `"last"` returns the last match detected in `y`.
#' For `"any"` and `"last"`, calculation is delegated to dplyr join methods, and
#' in the case of right join, also for `"first"`..
#' @param unmatched How should unmatched keys that would result in dropped rows
#' be handled? `"drop"` drops unmatched keys from the result. `"error"` throws
#' an error if unmatched keys are detected. Also, a named list of the form
#' `list(x = 1, y = 0.5, fail = "warning")`can be used when calculation is
#' **not** delegated to dplyr. The first two elements are the proportions that
#' must match, and the third element is `"message"`, `"warning"`, or `"error"`.
#' @param relationship Handling of the expected relationship between the keys of
#' `x` and `y`. If the expectations chosen from the list below are invalidated,
#' an error is thrown. `NULL`, the default, doesn't expect there to be any
#' relationship between `x` and `y`. However, for equality joins it will check
#' for a many-to-many relationship (which is typically unexpected) and will warn
#' if one occurs, encouraging you to either take a closer look at your inputs or
#' make this relationship explicit by specifying "many-to-many". `"one-to-one"`
#' expects: Each row in x matches at most 1 row in y. Each row in y matches at
#' most 1 row in x. `"one-to-many"` expects:  Each row in y matches at most 1
#' row in x. "many-to-one" expects: Each row in x matches at most 1 row in y.
#' `"many-to-many"` doesn't perform any relationship checks, but is provided to
#' allow you to be explicit about this relationship if you know it exists.
#' `relationship` doesn't handle cases where there are zero matches. For that,
#' see `unmatched`.
#' @param verbose integer. Prints information about the join. One of `0` (off),
#' `1` (default) or `2` (additionally prints the classes of the `by` columns).
#' @param column name for an extra column to generate in the output indicating
#' which dataset a record came from. `TRUE` calls this column `".join"`, or give
#' another name.
#' @param attr name for attribute providing information about the join performed
#' (including the output of [collapse::fmatch()]) to the result. `TRUE` calls
#' this attribute `"join.match"` or give your own name. Note: this also invokes
#' the count argument to [collapse::fmatch()].
right_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", unmatched = "drop", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, suffix = suffix, keep = keep, na_matches = na_matches,
    multiple = multiple, unmatched = unmatched, relationship = relationship,
    sort = sort, verbose = verbose, column = column, attr = attr,
    how = "right"), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::right_join"))

#' @export
#' @rdname sciviews_functions
full_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, suffix = suffix, keep = keep, na_matches = na_matches,
    multiple = multiple, relationship = relationship,
    sort = sort, verbose = verbose, column = column, attr = attr,
    how = "full"), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::full_join"))

#' @export
#' @rdname sciviews_functions
left_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", unmatched = "drop", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, suffix = suffix, keep = keep, na_matches = na_matches,
    multiple = multiple, unmatched = unmatched, relationship = relationship,
    sort = sort, verbose = verbose, column = column, attr = attr,
    how = "left"), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::left_join"))

#' @export
#' @rdname sciviews_functions
inner_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", unmatched = "drop", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, suffix = suffix, keep = keep, na_matches = na_matches,
    multiple = multiple, unmatched = unmatched, relationship = relationship,
    sort = sort, verbose = verbose, column = column, attr = attr,
    how = "inner"), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::inner_join"))

# Note: for semi_join, multiple = FALSE must be used in collapse::join()
#       -> multiple = "first"
#' @export
#' @rdname sciviews_functions
semi_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE, ...,
  na_matches = c("na", "never"), sort = FALSE, verbose = 0, column = NULL,
  attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, na_matches = na_matches, multiple = "first", sort = sort,
    verbose = verbose, column = column, attr = attr, how = "semi"),
    envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::semi_join"))

# Note: for anti_join, multiple = FALSE or TRUE are OK in collapse::join()
#       -> multiple = "first"
#' @export
#' @rdname sciviews_functions
anti_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE, ...,
  na_matches = c("na", "never"), sort = FALSE, verbose = 0, column = NULL,
  attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Deleguate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, na_matches = na_matches, multiple = "first", sort = sort,
    verbose = verbose, column = column, attr = attr, how = "anti"),
    envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::anti_join"))

# TODO: a mechanism to interpret and translate rbindlist() errors, see .join_()
# TODO: mixing character & factor -> charaxcter in dplyr, but factor in data.table
# TODO: check and find a solution for different units
# Note that this cannot be a data-dot function, because
# bind_rows_(a = df1, b = df2) should be OK without adding .data = .!
#' @export
#' @rdname sciviews_functions
#' @param .use_names If `TRUE` (default), bind by matching names, if `FALSE`, bind by
#'   position. If `NULL`, warns if  all items do not have the same name in the
#'   same order, and then proceeds as if `FALSE` (but will be as if `TRUE` in
#'   the future).
#' @param .fill If `TRUE` (default), fills missing columns with `NA` or `NULL`
#'   for missing list columns, if `FALSE`, do not fill.
bind_rows_ <- structure(function(..., .id = NULL, .use_names = TRUE,
  .fill = TRUE) {

  .__top_call__. <- TRUE

  # If nothing provided, return a 0 x 0 data.trame
  if (missing(...))
    return(data.trame())

  # If ..1 is a list and ...length == 1, use it, otherwise, use list(...)
  if ((is.list(..1) && !is.data.frame(..1)) && ...length() == 1L) {
    if (!length(..1))
      return(data.trame())
    arg_list <- ..1
  } else {
    arg_list <- list(...)
  }
  if (length(arg_list) == 1L)
    warning("Binding only one data frame is a nonsense. Did you forgot the `.` (not a data-dot function)?")
  # Ungroup if it is grouped (note: dplyr::bind_rows don't do that!)
  # This is because dplyr::bind_rows() does not support data.frames grouped
  # with collapse::fgroup_by()
  first_arg <- arg_list[[1]]
  if (is_grouped_df(first_arg))
    arg_list[[1]] <- first_arg <- fungroup(first_arg)

  # If the list contains something else than data frames (could be named
  # vectors, or lists), bind_cols() uses as_tibble() instead and ignore other
  # arguments. In this case, we currently delegate to dplyr:bind_rows().
  if (!all(sapply(arg_list, is.data.frame)))
    return(do.call('bind_rows', c(arg_list, list( .id = .id)),
      envir = parent.frame()))

  # If .id is not NULL, check it
  if (!is.null(.id) && (length(.id) != 1 || !is.character(.id)))
    stop("{.arg .id} must be a single string, not {.obj_type_friendly {(.id)}} of length {length(.id)}.")

  # .fill can be NULL, TRUE or FALSE
  if (!isTRUE(.fill) && !isFALSE(.fill))
    stop("{.arg .fill} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {(.fill)}}.")

  # .use_names can be NULL, TRUE, FALSE
  if (!is.null(.use_names) && !isTRUE(.use_names) && !isFALSE(.use_names))
    stop("{.arg .use_names} must be {.code TRUE}, {.code FALSE}, or {.code NULL}, not {.obj_type_friendly {(.use_names)}} (it is {.val {(.use_names)}}).")

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(first_arg)
  if (to_dtrm) {
    let_data.trame_to_data.table(first_arg)
    on.exit(let_data.table_to_data.trame(first_arg))
  }
  # rbindlist() always return a data.table, but we may desire a data.frame or a
  # tibble as well
  to_dtf <- is_dtf(first_arg)
  to_dtbl <- is_dtbl(first_arg)

  if (is.null(.use_names)) {# Use the default that is "check" in rbindlist()
    res <- rbindlist(arg_list, fill = .fill, idcol = .id)
  } else {
    res <- rbindlist(arg_list, use.names = .use_names, fill = .fill, idcol = .id)
  }

  # bind_rows() always returns characters for .id, but rbindlist() sometimes
  # returns integers
  if (!is.null(.id))
    res[[.id]] <- as.character(res[[.id]])

  if (to_dtrm) {
    let_data.table_to_data.trame(res)
  } else if (to_dtf) {
    res <- as_dtf(res)
  } else if (to_dtbl) {
    res <- as_dtbl(res)
  }
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::bind_rows"))

#' @export
#' @rdname sciviews_functions
bind_cols_ <- structure(function(...,
  .name_repair = c("unique", "universal", "check_unique", "minimal")) {

  .__top_call__. <- TRUE

  # If nothing provided, return a 0 x 0 data.trame
  if (missing(...))
    return(data.trame())

  # Ungroup if it is grouped (note: dplyr::bind_rows don't do that!)
  # This is because dplyr::bind_rows() does not support data.frames grouped
  # with collapse::fgroup_by()
  arg_list <- list(...)
  if (length(arg_list) == 1L)
    warning("Binding only one data frame is a nonsense. Did you forgot the `.` (not a data-dot function)?")
  first_arg <- arg_list[[1]]
  if (is_grouped_df(first_arg))
    arg_list[[1]] <- fungroup(first_arg)

  # For now, we use dplyr::bind_cols() internally, but we convert
  # into the correct data frame object at the end
  do.call('bind_cols', c(arg_list, list( .name_repair = .name_repair)),
    envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::bind_cols"))

#' @export
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
#' @param n Number of rows to keep
#' @param prop Proportion of rows to keep, between 0 and 1. Provide either `n`,
#'   or `prop` but not both simultaneously. If none is provided, `n = 1` is used.
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
#' @rdname sciviews_functions
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

# TODO: this still needs some work!
# @export
# @rdname sciviews_functions
# slice_min_ <- structure(function(.data = (.), order_by = NULL, ..., n = 1L,
#     prop, by = NULL, with_ties = TRUE, na_rm = FALSE, sort = TRUE) {
#
#   if (!prepare_data_dot(.data))
#     return(recall_with_data_dot())
#
#   if (!missing(...)) {
#     if (...length() == 1L && is.null(...names()))
#       stop("{.arg n} must be explicitly named.",
#         i = "Did you mean {.code slice_min_(n = {.val {(..1)}})}?")
#     check_dots_empty()
#   } # ... must be empty
#
#   if (!isTRUE(na_rm) && !isFALSE(na_rm))
#     stop("{.arg na_rm} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {na_rm}} ({.val {na_rm}}).")
#
#   if (!isTRUE(with_ties) && !isFALSE(with_ties))
#     stop("{.arg with_ties} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {with_ties}} ({.val {with_ties}}).")
#   if (isTRUE(with_ties) && (n != 1L || isTRUE(sort)))
#     stop("{.arg with_ties} `TRUE` only supported with {.code n= 1}, and {.code sort = FALSE}.")
#
#   # Use prop or n indifferently as n in fslice()
#   if (missing(n)) {
#     if (!missing(prop)) {# We use prop instead of n
#       if (!is.numeric(prop) || length(prop) != 1L)
#         stop("{.arg prop} must be a single number, not {.obj_type_friendly {prop}} of length {length(prop)}.")
#       if (prop < 0)
#         stop("Negative {.arg prop} is not supported by {.fun slice_tail_}.")
#       if (prop >= 1L)
#         prop <- 0.999999999999
#       n <- prop
#     }
#   } else {# n non missing
#     if (!missing(prop))
#       stop("Must supply {.arg n} or {.arg prop}, but not both.")
#     if (!is.numeric(n) || length(n) != 1L)
#       stop("{.arg n} must be a single round number, not {.obj_type_friendly {n}} of length {length(n)}.")
#     if (n < 0)
#       stop("Negative {.arg n} is not supported by {.fun slice_tail_}.")
#     n2 <- as.integer(n)
#     if (n2 != n)
#       stop("{.arg n} must be a single round number, not {.val {n}}.")
#     n <- n2
#     if (n > nrow(.data))
#       n <- nrow(.data)
#   }
#
#   # Apparently, I don't need to transform a data.trame into a data.table here
#   # Treat data.trames as data.tables
#   #to_dtrm <- is.data.trame(.data)
#   #if (to_dtrm) {
#   #  let_data.trame_to_data.table(.data)
#   #  on.exit(let_data.table_to_data.trame(.data))
#   #}
#
#   is_grouped <- is_grouped_df(.data)
#   # If by is defined, use these groups, but do not keep them
#   if (!missing(by) && length(by)) {
#     if (is_grouped)
#       stop("Can't supply {.arg by} when {.arg .data} is a grouped data frame.")
#     #let_data.table_to_data.trame(.data)
#     res <- group_by_vars(.data, by = by, sort = FALSE)
#     #let_data.trame_to_data.table(.data)
#   } else {
#     res <- .data
#     if (is_grouped) {
#       by <- fgroup_vars(res, return = "names")
#     } else {
#       by <- NULL
#     }
#   }
#
#   res <- fslicev(res, cols = by, order.by = order_by, na_rm = na_rm,
#     with.ties = with_ties, n = n, how = "min", sort = sort)
#   if (is_grouped)
#     res <- fungroup(res) # Always ungroup at the end
#
#   #if (to_dtrm)
#   #  let_data.table_to_data.trame(res)
#   res
#
# }, class = c("function", "sciviews_fn"),
#   comment = .src_sciviews("dplyr::slice_min"))

#' @export
#' @rdname sciviews_functions
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
      data2 <- do.call('fmutate', c(list(.data), args2), envir = args$env)
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
    res <- do.call('fcount', c(args$dots, list(x = data2, w = substitute(wt),
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
    do.call('setorderv', list(res, name, order = if (decreasing) -1 else 1,
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
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
#' @rdname sciviews_functions
add_tally_ <- structure(function(.data = (.), wt = NULL, name = "n",
  sort = FALSE, decreasing = TRUE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  do.call('count_', list(.data = .data, wt = substitute(wt), name = name,
    sort = sort, decreasing = decreasing, add = TRUE), envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))


# Note: this does not work... but could be useful to rapidly count unique items
# by groups
# @export
# @rdname sciviews_functions
# ndistinct_ <- structure(function(.data = (.), ..., .keep_all = FALSE,
#     .op = NULL, .na.rm = FALSE) {
#
#   if (!prepare_data_dot(.data))
#     return(recall_with_data_dot())
#
#   # Apparently, I don't need to transform a data.trame into a data.table here
#   # Treat data.trames as data.tables
#   #to_dtrm <- is.data.trame(.data)
#   #if (to_dtrm) {
#   #  let_data.trame_to_data.table(.data)
#   #  on.exit(let_data.table_to_data.trame(.data))
#   #}
#
#   if (missing(...)) {
#     args <- list(dots = list(), are_formulas = FALSE, env = parent.frame())
#   } else {
#     args <- formula_masking(...)
#   }
#
#     if (is.numeric(.keep_all))
#       .keep_all <- as.logical(.keep_all)
#   if (!isTRUE(.keep_all) && !isFALSE(.keep_all))
#     stop("{.arg .keep_all} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {(.keep_all)}} ({.val {(.keep_all)}}).")
#
#   is_grouped <- is_grouped_df(.data)
#   if (missing(...) && !is_grouped) {# use all columns of .data
#     return(fndistinct(.data, g = .data, TRA = .op, na.rm = .na.rm,
#       use.g.names = TRUE, drop = TRUE))
#   }
#
#   # If there are named items, we mutate data with these expressions
#   dots_names <- names(args$dots)
#   if (!is.null(dots_names)) {
#     is_expr <- dots_names != ""
#     if (any(is_expr)) {
#       args2 <- args$dots[is_expr]
#       if (!args$are_formulas) # force standard evaluation
#         force(args2)
#       data2 <- do.call('fmutate', c(list(.data), args2), envir = args$env)
#       # Replace the expression by its name ( as a symbol, if formulas)
#       args$dots[is_expr] <- dots_names[is_expr]
#       if (args$are_formulas)
#         args$dots[is_expr] <- lapply(args$dots[is_expr], as.symbol)
#     } else {
#       data2 <- .data
#     }
#   } else {
#     data2 <- .data
#   }
#   if (args$are_formulas) {
#     if (is_grouped) {
#       gvars <- as.list(fgroup_vars(.data, return = "names"))
#       args$dots <- unique(c(gvars, lapply(args$dots, as.symbol)))
#     }
#   } else {# SE evaluation
#     if (is_grouped)
#       args$dots <- unique(c(as.list(fgroup_vars(.data, return = "names")),
#         args$dots))
#   }
#   if (is_grouped)
#     data2 <- fungroup(data2) # Always ungroup at the end
#
#   res <- fndistinct(data2, g = args$dots, TRA = .op, na.rm = .na.rm,
#     use.g.names = TRUE, drop = FALSE)
#
#   #if (to_dtrm)
#   #  let_data.table_to_data.trame(res)
#   res
#
#
# }, class = c("function", "sciviews_fn"),
#   comment = .src_sciviews("collapse::fndistinct"))

#' @export
#' @rdname sciviews_functions
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
      data2 <- do.call('fmutate', c(list(.data), args2), envir = args$env)
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


# tidyr verbs -------------------------------------------------------------

# Note: tidyr uses data=, but here we use .data= for coherence with select()
#' @export
#' @rdname sciviews_functions
#' @param .na.attr logical. `TRUE` adds an attribute containing the removed
#' cases. For compatibility reasons this is exactly the same format as
#' [na.omit()], i.e. the attribute is called "na.action" and of class **omit**
#' @param .prop numeric. The proportion missing values in each case for the case
#' to be considered as missing required to keep a
drop_na_ <- structure(function(.data = (.), ..., .na.attr = FALSE, .prop = 0) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # Similar to select_()...
  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }
  if (!isTRUE(.na.attr) && !isFALSE(.na.attr))
    stop("{.arg .na.attr} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {(.na.attr)}} ({.val {(.na.attr)}}).")

  if (!is.numeric(.prop) || length(.prop) != 1L || .prop < 0 || .prop > 1)
    stop("{.arg .prop} must be a single number [0, 1], not {.obj_type_friendly {(.prop)}} ({.val {(.prop)}}).")

  # If no selection is provided
  if (missing(...)) # Drop NA using columns
    return(na_omit(.data, na.attr = .na.attr, prop = .prop))

  # Process dots to args
  args <- formula_select(..., .fast.allowed.funs = c(":", "-", "c"))

  # Arguments cannot be named
  dots_names <- names(args$dots)
  has_names <- dots_names != ""
  if (!is.null(dots_names) && any(has_names)) {
    if (sum(has_names) == 1L) {
      msg <- gettext("Problematic argument:")
    } else {# Several named items
      msg <- gettext("Problematic arguments:")
    }
    # Get correct expressions
    exprs <- lapply(list(...)[has_names], expr_text)
    exprs <- paste(names(exprs), exprs, sep = " = ")
    # Must still rework ' = expr' in 'expr'
    exprs <- sub("^ = ", "", exprs)
    exprs <- sub("([^ ])~([^ ])", "\\1 ~ \\2", exprs) # x~y -> x ~ y
    names(exprs) <- rep_along(exprs, "*")
    msgs <- c(gettext(
      "Arguments in {.code ...} cannot be named or use naming formulas like {.code name ~ expr}."),
      x = msg, exprs)
    cli_abort(msgs, call = stop_top_call())
  }

  if (args$are_formulas) {
    #message(gettextf("Using tidyselect with `%s`",
    #  paste(args$dots, collapse = ", ")))
    eval_select2 <- function(..., data)
      eval_select(expr(c(...)), data = data, allow_rename = FALSE,
        error_call = stop_top_call())
    loc <- do.call('eval_select2', c(args$dots, list(data = .data)))
    res <- na_omit(.data, cols = loc, na.attr = .na.attr, prop = .prop)
    #names(res) <- names(loc)

  } else {# no formulas with collapse
    res <- na_omit(.data, cols = unlist(args$dots), na.attr = .na.attr,
      prop = .prop)
    # Should we rename here?
  }
  if (to_dtrm)
    let_data.table_to_data.trame(res)

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::drop_na"))

# There is tidyr::replace_na and collapse::replace_na
# Make sure we use the correct one
repl_na <- collapse::replace_na

# TODO: replace NULL list elements in a data frame
# (see last example of ?tidyr::replace_na)
# data= is renamed .data= for consistency
# To allow the data-dot mechanism, .data= could only be a data frame. However,
# tidyr::replace_na() also accepts a vector. Here, you must indicate v= ...
# in that case!
#' @export
#' @rdname sciviews_functions
#' @param v a vector where to replace NAs.
replace_na_ <- structure(function(.data = (.), replace, ..., v = NULL) {

  .__top_call__. <- TRUE

  if (!missing(v)) {# This is the replacement in a vector
    # If nothing to replace, return the unmofdifed object
    if (missing(replace) && !missing(.data))
      replace <- .data
    if (is.null(replace) || length(replace) == 0) # Nothing to replace
      return(v)
    if (is.list(replace))
      stop("Replacement for vector {.code v} cannot be a {.cls list}.")
    if (length(replace) != 1L)
      stop("Replacement for vector {.code v} must be length 1, not length {length(replace)}.")
    return(repl_na(v, value = replace, ...))
  }

  # Now, the case of a data frame
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot(abort_msg = gettext(
      "`.data` must be a `data.frame` or a use `v = vector`.")))

  # If nothing to replace, return the unmofdifed object
  if (missing(replace) || length(replace) == 0)
    return(.data)

  if (!is.list(replace))
    stop("{.arg replace} for {.cls data.frame} must be a {.cls list} not {.obj_type_friendly {replace}} ({.val {replace}}).")

  # Faster alternative: a list with a single item named everywhere -> apply to all
  rep_cols <- names(replace)
  if (is.null(rep_cols)) # Nothing is named!
    return(.data)

  if (length(replace) == 1L && rep_cols[1] == ".everywhere") {
    replace <- replace[[1]]
    if (length(replace) != 1L)
      stop("Replacement for {.code data} must a {.cls list} with length 1 items, not length {length(replace)}.")
    return(repl_na(.data, value = replace, ...))
  }

  # This is not very efficient, but OK for now (for loop)
  # Note: variables not in .data or not named are silently ignored!
  res <- .data
  dat_cols <- names(.data)
  for (i in 1:length(rep_cols)) {# >= 1, because tested for 0 above!
    col <- rep_cols[i]
    if (anyv(dat_cols, col)) {# Ignore wrong columns, like tidyr::replace_na
      val <- replace[[i]]
      if (length(val) != 1)
        stop("Replacement for {.code data${col}} must be a {.cls list} with length 1 items, not length {length(val)}.")
      res <- repl_na(res, cols = col, value = val, ...)
    }
  }

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::replace_na"))

# TODO: collapse::pivot_longer() is monothread currently, see data.table::melt()
# or {tidytable} for an alternative.
# TODO: manage the arguments names_sep, names_pattern, names_ptypes,
# values_ptypes, names_transform, values_transform, and names_repair!
#' @export
#' @rdname sciviews_functions
#' @param cols_vary character. Either "fastest" or "slowest". If "fastest"
#'   (default), keep individual rows from `cols` close together. If "slowest",
#'   keeps individual columns from `cols' close together.
#' @param names_prefix character. A regular expression used to remove matching text from the start of each variable name.
#' @param values_drop_na logical. If `TRUE`, drop rows with only `NA`s in the
#'   `values_to` column.
#' @param factor logical. If `TRUE`, convert the names and labels into factors,
#'   if `FALSE` (default) leave then as character strings (but slower for
#'   subsequent filtering).
pivot_longer_ <- structure(function(.data = (.) , cols, ...,
  cols_vary = "fastest", names_to = "name", names_prefix = NULL,
  values_to = "value", values_drop_na = FALSE, factor = FALSE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (cols_vary != "fastest" && cols_vary != "slowest")
    stop("{.arg cols_vary} must be \"fastest\" or \"slowest\", not {.obj_type_friendly {cols_vary}} ({.val {cols_vary}}).")

  # TODO: handle the case where names_to is 0; or NULL or length > 1
  if (!is.character(names_to))
    stop("{.arg names_to} must be a character vector or `NULL`, not {.obj_type_friendly {names_to}} ({.val {names_to}}).")
  if (length(names_to) != 1L)
    stop("{.fun pivot_longer_} only accepts {.arg names_to} of length 1 not of length {length(names_to)}.")

  if (!is.character(values_to))
    stop("{.arg values_to} must be a character vector or `NULL`, not {.obj_type_friendly {values_to}} ({.val {values_to}}).")
  if (length(values_to) != 1L)
    stop("{.arg values_to} must have size 1 not size {length(values_to)}.")

  if (isTRUE(factor))
    factor <- c("names", "labels") # This the value expected by collapse::pivot()

  # cols is tidy selected if a formula
  if (is_formula(cols))
    cols <- eval_select(f_rhs(cols), data = .data, allow_rename = FALSE,
      error_call = stop_top_call())

  # Rework cols names if names_prefix is provided
  if (!missing(names_prefix)) {
    if (!is.character(names_prefix) || length(names_prefix) != 1L)
      stop("{.arg names_prefix} must be a single string, not {.obj_type_friendly {names_prefix}} ({.val {names_prefix}}).")
    if (!startsWith(names_prefix, "^"))
      names_prefix <- paste0("^", names_prefix)
    names(.data)[cols] <- sub(names_prefix, "", names(.data)[cols])
  }

  res <- pivot(.data, values = cols, names = list(names_to, values_to),
    how = "longer",
    na.rm = if (cols_vary == "slowest") values_drop_na else FALSE,
    factor = factor)

  # Change order if cols_vary == "fastest"
  if (cols_vary == "fastest") {
    ncols <- length(cols)
    seqcolsm1 <- 0:(ncols - 1)
    nrows <- nrow(.data)
    seqrows <- 1:nrows
    res <- res[rep(seqrows, each = ncols) + rep(seqcolsm1 * nrows, nrows), ]
    if (isTRUE(values_drop_na))
      res <- na_omit(res, values_to)
  }

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::pivot_longer"))

# This is needed for R CMD check otherwise it will complain
name <- NULL
value <- NULL

#' @export
#' @param id_cols A set of columns that uniquely identify each observation.
#' @param id_expand logical. If `TRUE`, expand the `id_cols`.
#' @param names_vary character. How the various column names are made: "fastest"
#' (default), "slowest", "transpose", or "slowtranspose".
#' @param values_fill Optionally, a scalar value to use for missing values.
#' @param values_fn Either the name of an internal function (as a string) :
#' "first", "last" (default), "count", "sum", "mean", "min", or "max". Could
#' also be a formula calling an external function with first argument being `.x`
#' like `~fmedian(.x, na.rm = TRUE)`.`
#' @param drop Drop unused factor levels or not.
#' @rdname sciviews_functions
pivot_wider_ <- structure(function(.data = (.), ..., id_cols = NULL,
  id_expand = FALSE, names_from = name, names_prefix = "",
  names_vary = "fastest", values_from = value, values_fill = NULL,
  values_fn = "last", drop = TRUE, sort = FALSE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...))
    check_dots_empty()

  transpose <- switch(names_vary, # Equivalent arguments in dplyr vs collapse
    fastest = FALSE,
    slowest = "columns",
    transpose = "names", # Not in dplyr
    slowtranspose = TRUE, # Not in dplyr
    stop("{.arg names_vary} must be \"fastest\", \"slowest\", \"transpose\" or \"slowtranspose\", not {.obj_type_friendly {names_vary}} ({.val {names_vary}})."))

  if (!isTRUE(drop) && !isFALSE(drop))
    stop("{.arg drop} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {drop}} ({.val {drop}}).")

  if (!isTRUE(sort) && !isFALSE(sort))
    stop("{.arg sort} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {sort}} ({.val {sort}}).")

  if (!is.character(names_from))
    stop("{.arg names_from} must be a character vector, not {.obj_type_friendly {names_from}} ({.val {names_from}}).")
  #if (length(names_from) != 1L)
  #  stop("{.fun pivot_wider_} only accepts {.arg names_from} of length 1 not of length {length(names_from)}.")

  if (!is.character(values_from))
    stop("{.arg values_from} must be a character vector, not {.obj_type_friendly {values_from}} ({.val {values_from}}).")
  #if (length(values_from) != 1L)
  #  stop("{.arg values_from} must have size 1 not size {length(values_from)}.")

  # id_cols is tidy selected if a formula
  if (is_formula(id_cols))
    id_cols <- eval_select(f_rhs(id_cols), data = .data, allow_rename = FALSE,
      error_call = stop_top_call())

  if (is.character(values_fn)) {
    if (length(values_fn) != 1L || is.na(values_fn) || values_fn == "")
      stop("{.arg values_fn} must be a single string or a formula, not {.obj_type_friendly {values_fn}} ({.val {values_fn}}).")
    FUN <- values_fn
    FUN.args <- NULL
  } else if (is_formula(values_fn)) {# Rework into FUN and FUN.args
    expr_fn <- f_rhs(values_fn)
    if (length(expr_fn) < 2 && expr_fn[[2]] != ".x")
      stop("When {.arg values_fn} is a formula it must call a function {.code ~fun(.x, ...)}.",
        i = "example: {.code ~fmean(.x, na.rm = TRUE)}.")
    fname <- as.character(expr_fn[[1]])
    FUN <- get0(fname, envir = parent.frame(), mode = "function",
      inherits = TRUE) # Get the function in the parent frame
    if (is.null(FUN))
      stop("When {.arg values_fn} is a formula it must call a function {.code ~fun(.x, ...)}.",
        i = "example: {.code ~fmean(.x, na.rm = TRUE)}.",
        x = " The function {.fun {fname}} is not found, or is not a function.")
    FUN.args <- as.list(expr_fn[-(1:2)]) # Remove the two first element (.x))
  } else {# Anything else...
    stop("{.arg values_fn} must be a single string or a formula, not {.obj_type_friendly {values_fn}} ({.val {values_fn}}).")
  }

  # TODO: rework col names if names_prefix is provided

  res <- pivot(.data, ids = id_cols, values = values_from, names = names_from,
    how = "wider", fill = values_fill, transpose = transpose, FUN = FUN,
    FUN.args = FUN.args, drop = drop, sort = sort)

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::pivot_wider"))

#' @export
#' @rdname sciviews_functions
uncount_ <- structure(function(.data = (.), weights, ..., .remove = TRUE,
  .id = NULL) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...))
    check_dots_empty()

  if (is_formula(weights)) {
    weights <- f_rhs(weights)
    if (length(weights) != 1L)
      stop("When {.arg weights} is a formula, it must be like {.code ~name}.")
    weights <- as.character(weights)
  }

  if (is.character(weights)) {
    if (length(weights) != 1 || is.na(weights) || !any(weights == names(.data)))
      stop("{.arg weights} must be a single string (name of a column of .data), not {.obj_type_friendly {weights}} ({.val {weights}}).")
    weights_is_col <- TRUE
    weights_val <- .data[[weights]]
  } else if (is.numeric(weights)) {
    weights_is_col <- FALSE
    if (length(weights) == 1L) {
      weights_val <- rep_len(weights, nrow(.data))
    } else {
      weights_val <- weights
    }
    if (length(weights) != 1 && length(weights) != nrow(.data))
      stop("{.arg weights} must be a single number or a vector of numbers with the same length as {.code .data}, not {.obj_type_friendly {weights}} ({.val {weights}}).")
  } else {# It is supposed to be a vector of weights
    stop("{.arg weights} must be a single string (name of a column of .data) or numeric values, not {.obj_type_friendly {weights}} ({.val {weights}}).")
  }
  if (anyNA(weights_val))
    stop("{.arg weights} cannot contain {.val NA} values.")
  if (any(weights_val < 0))
    stop("{.arg weights} must be a vector of positive numbers.")
  if (!is_integerish(weights_val))
    stop("{.arg weights} must be a vector of round numbers, not {.obj_type_friendly {weights}} ({.val {weights}}).")

  if (!isTRUE(.remove) && !isFALSE(.remove))
    stop("{.arg .remove} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {(.remove)}} ({.val {(.remove)}}).")

  ind <- rep.int(1:nrow(.data), weights_val)
  res <- .data[ind, ]
  if (.remove && weights_is_col)
    res[[weights]] <- NULL

  if (!is.null(.id)) {
    if (!is.character(.id) || length(.id) != 1L)
      stop("{.arg .id} must be a single string, not {.obj_type_friendly {(.id)}} ({.val {(.id)}}).")
    seqind <- seq_along(ind)
    res[[.id]] <- seqind - seqind[!duplicated(ind)][ind] + 1L
  }

  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::uncount"))

#' @export
#' @rdname sciviews_functions
unite_ <- structure(function(.data = (.), col, ..., sep = "_", remove = TRUE,
  na.rm = FALSE) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  # col could be a string, or a ~name formula
  if (missing(col))
    stop("{.arg col} is absent but must be supplied.")
  if (is_formula(col)) {
    col_name <- f_rhs(col)
    if (length(col_name) != 1L)
      stop("When {.arg col} is a formula, it must be like {.code ~var}.")
    col_name <- as.character(col_name)
  } else {
    col_name <- col
  }
  if (!is.character(col_name) || length(col_name) != 1 || is.na(col_name))
    stop("{.arg col} must be a single string (name of a column of .data) or a {.code ~var} formula.",
      i = "It is {.obj_type_friendly {col}} ({.val {col}}).")

  # If the data frame has no columns, return it unchanged
  if (ncol(.data) == 0L)
    return(.data)

  if (missing(...)) {# If no columns provided, used them all
    cols <- seq_along(.data)
  } else {
    # Use tidyselect to select columns with ...
    args <- formula_select(..., .fast.allowed.funs = c(":", "-", "c", "("))
    if (!args$fastselect) {
      #message(gettextf("Using tidyselect with `%s`",
      #  paste(args$dots, collapse = ", ")))
      eval_select2 <- function(..., data)
        eval_select(expr(c(...)), data = data, error_call = stop_top_call())
      cols <- do.call('eval_select2', c(args$dots, list(data = .data)))

    } else {# fastselect with collapse
      if (args$are_formulas) {
        cols <- do.call('fselect', c(list(.x = .data, return = "indices"),
          args$dots), envir = args$env)
      } else {
        cols <- get_vars(.data, unlist(args$dots), return = "indices")
      }
    }
    if (length(cols) == 0L)
      stop("No columns selected with {.arg ...}.")
  }

  if (!is.character(sep) || length(sep) != 1L || is.na(sep))
    stop("{.arg sep} must be a single string, not {.obj_type_friendly {sep}} ({.val {sep}}).")

  if (isTRUE(na.rm)) {# This one is hard, because paste() does not ignore NAs!
    # Replace NAs by a placeholder
    tmp_data <- replace_na(.data[, cols], "@@@@@")
    col_data <- do.call(paste, c(tmp_data, list(sep = sep)))
    # Replace the placeholder by nothing, including sep
    col_data <- gsub(paste0(sep, "@@@@@"), "", col_data, fixed = TRUE)
    col_data <- sub(paste0("^", "@@@@@", sep, "?"), "", col_data, perl = TRUE)
  } else if (isFALSE(na.rm)) {
    col_data <- do.call(paste, c(.data[, cols], list(sep = sep)))
  } else {
    stop("{.arg na.rm} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {na.rm}} ({.val {na.rm}}).")
  }
  # New variable before first old one
  add_vars_args <- list(x = .data, col_data, pos = cols[1])
  names(add_vars_args)[2] <- col_name
  res <- do.call(add_vars, add_vars_args)

  if (isTRUE(remove)) {
    res <- res[, -(cols + 1), drop = FALSE] # Remove columns
  } else if (!isFALSE(remove)) {
    stop("{.arg remove} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {remove}} ({.val {remove}}).")
  }

  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::unite"))

# TODO: .direction of same length as number of vars?
#' @export
#' @rdname sciviews_functions
fill_ <- structure(function(.data = (.), ..., .direction = "down") {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  check_dots_unnamed()

  if (length(.direction) != 1L || !is.character(.direction))
    stop("{.arg .direction} must be a single string, not {.obj_type_friendly {(.direction)}} ({.val {(.direction)}}).")
  dir <- switch(.direction[1],
    down = "locf",
    up = "focb",
    downup = c("locf", "focb"),
    updown = c("focb", "locf"),
    stop("{.arg .direction} must be one of \"down\", \"up\", \"downup\" or \"updown\", not {.obj_type_friendly {(.direction)}} ({.val {(.direction)}})."))

  if (missing(...)) # No variable to fill
    return(.data)

  args <- formula_select(..., .fast.allowed.funs = "")
  if (!args$fastselect) {# Use tidyselect
    eval_select2 <- function(..., data)
      eval_select(expr(c(...)), data = data, error_call = stop_top_call())
    cols <- do.call('eval_select2', c(args$dots, list(data = .data)))
  } else {
    cols <- unlist(args$dots)
  }
  if (is.list(cols)) # Thus happens when one provides formulas
    cols <- sapply(cols, as.character)
  if (is.numeric(cols)) {
    if (max(cols) > ncol(.data)) {
      wrong_indices <- cols[cols > ncol(.data)]
      stop("Column indices cannot be larger than the number of columns in {.code .data}.",
        x = "Problematic indice(s):",
        `*` = "{.val {wrong_indices}}")
    }
  } else if (is.character(cols)) {
    if (!all(cols %in% names(.data)))
      stop("Can't select columns that don't exist.",
        x = "Missing column(s):",
        `*` = "{.val {cols[!cols %in% names(.data)]}}")
  } else {
    stop("{.arg ...} must be a character vector, numeric indices, or formulas not {.obj_type_friendly {cols}} ({.val {cols}}).")
  }

  if (is_grouped_df(.data)) {
    fill_onegroup <- function(i, .data, cols, type) {
      res <- repl_na(.data[i, ], cols = cols, type = type[1])
      if (length(dir) > 1L) # Second direction
        res <- repl_na(res, cols = cols, type = type[2])
      res
    }
    res <- rbindlist(lapply(gsplit(g = .data), fill_onegroup, .data = .data,
      cols = cols, type = dir))
    res <- res[greorder(1:nrow(.data), g = .data), ] # Reorder the data
    # Since rbindlist() produces a data.table, we eventually have to convert back
    if (is_dtrm(.data)) {
      let_data.table_to_data.trame(res)
    } else if (is_dtbl(.data)) {
      res <- qTBL(res)
    } else if (is_dtf(.data)) {
      res <- qDF(res)
    }
  } else {# Ungrouped data
    res <- repl_na(.data, cols = cols, type = dir[1])
    if (length(dir) > 1L) # Second direction
      res <- repl_na(res, cols = cols, type = dir[2])
  }

  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::fill"))

# TODO: separate with numbers using iotools::dstrfw() = mostly c code
#' @export
#' @param extra When `sep` is a character vector what happens when there are too
#'   many pieces: `"warn"` (default) issue a warning and drop extra items,
#'   `"drop"` does the same without warning and `"merge"` merges the extra items
#'   with the last one.
#' @param fill When `sep` is a character vector what happens when there are not
#'   enough pieces: `"warn"` (default) issue a warning and fill with `NA`s at
#'   right, so does without warning `"right"`, and `"left"` fills with `NA`s at
#'   left.
#' @param fixed logical. If `TRUE`, `sep` is a fixed string, otherwise it is a
#'   (perl) regular expression.
#' @rdname sciviews_functions
separate_ <- structure(function(.data = (.), col, into, sep = "[^[:alnum:]]+",
  remove = TRUE, convert = FALSE, extra = "warn", fill = "warn",
  fixed = FALSE, ...) {

  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())

  if (!missing(...))
    check_dots_empty()

  if (!isTRUE(fixed) && !isFALSE(fixed))
    stop("{.arg fixed} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {fixed}} ({.val {fixed}}).")

  if (missing(col))
    stop("{.arg col} is absent but must be supplied.")
  if (is_formula(col)) {
    col_name <- f_rhs(col)
    if (length(col_name) != 1L)
      stop("When {.arg col} is a formula, it must be like {.code ~var}.")
    if (!is.numeric(col_name))
      col_name <- as.character(col_name)
  } else {
    col_name <- col
  }
  col_num <- NULL
  if (is.numeric(col_name)) {
    col_num <- col_name
    if (!is_integerish(col_num) || length(col_num) != 1L)
      stop("{.arg col} is a column number, but it must be a single integer, not {.obj_type_friendly {col_name}} ({.val {col_name}}).")
    if (col_num < 1L || col_num > ncol(.data))
      stop("{.arg col} is a column number, but it is not in the range [1, {ncol(.data)}].",
        i = "It is {.val {col_num}}.")
    col_name <- names(.data)[col_num]
  } else if (!is.character(col_name) || length(col_name) != 1 || is.na(col_name))
    stop("{.arg col} must be a single string (name of a column of .data) or a {.code ~var} formula.",
      i = "It is {.obj_type_friendly {col}} ({.val {col}}).")
  if (!any(col_name == names(.data)))
    stop("{.code col = \"{col_name}\"} does not exist in {.code .data}.",
      i = "Available columns are: {.val {names(.data)}}.")
  if (is.null(col_num))
    col_num <- (1:ncol(.data))[names(.data) == col_name]
  cdata <- .data[[col_name]] # Column data

  if (!is.character(into) || length(into) < 2L)
    stop("{.arg into} must be a character vector of length >= 2, not {.obj_type_friendly {into}} ({.val {into}}).")
  l_into <- length(into)
  l_new_cols <- sum(!is.na(into))
  # This is not done in separate(), but it is crazy to keep NO columns!
  if (l_new_cols == 0)
    stop("{.arg into} cannot contain only missing values (NA)",
      i = "To get rid of the column {.val {col_name}}, use {.code select_()} instead.")

  if (length(extra) != 1L || !is.character(extra) || is.na(extra) ||
      !any(extra == c("warn", "drop", "merge")))
    stop("{.arg extra} must be \"warn\", \"drop\" or \"merge\", not {.obj_type_friendly {extra}} ({.val {extra}}).")

  if (length(fill) != 1L || !is.character(fill) || is.na(fill) ||
      !any(fill == c("warn", "right", "left")))
    stop("{.arg fill} must be \"warn\", \"right\" or \"left\", not {.obj_type_friendly {fill}} ({.val {fill}}).")

  # sep determines how we work (character vs numeric)
  if (is.character(sep)) {# Use regular expression to split the column
    if (length(sep) != 1L || is.na(sep))
      stop("{.arg sep} must be a single string, not {.obj_type_friendly {sep}} ({.val {sep}}).")

    if (extra == "merge") {# We cannot use strsplit() unfortunately
      matches <- gregexpr(sep, cdata, fixed = fixed, perl = !fixed)
      # Drop matches past l_into and extend length of last match
      l_matches <- vlengths(matches)
      if (any(l_matches > l_into - 1L)) {
        matches <- lapply(matches, function(x) {
          x2 <- x
          if (length(x) > l_into - 1L) {
            x2 <- x[1:(l_into - 1L)]
            attributes(x2) <- attributes(x)
            attr(x2, "match.length") <- attr(x, "match.length")[1:(l_into - 1)]
            x2
          }
          x2
        })
      }
      col_split <- regmatches(cdata, matches, invert = TRUE)
    } else {# Just use strsplit()
      col_split <- strsplit(cdata, sep, fixed = fixed, perl = !fixed)
    }
    split_lengths <- vlengths(col_split)
    # Where there is NA in cdata, we don't care if there is less or more items
    # (like for dplyr::separate())
    split_lengths[is.na(cdata)] <- l_into
    if (any(split_lengths > l_into)) {
      col_split <- switch(extra,
        warn  = {
          wrong_cols <- (1:length(split_lengths))[split_lengths > l_into]
          if (length(wrong_cols) > 20L)
            wrong_cols <- c(wrong_cols[1:20], "...")
          warning(gettextf("Expected %d pieces. Additional pieces discarded in %d rows [%s].",
            l_into, sum(split_lengths > l_into), paste(wrong_cols, collapse = ", ")))
          lapply(col_split, function(x) x[1:l_into])
        },
        drop =  lapply(col_split, function(x) x[1:l_into]),
        merge = col_split # already managed above
      )
    }
    if (any(split_lengths < l_into)) {
      col_split <- switch(fill,
        warn  = {
          wrong_cols <- (1:length(split_lengths))[split_lengths < l_into]
          if (length(wrong_cols) > 20L)
            wrong_cols <- c(wrong_cols[1:20], "...")
          warning(gettextf("Expected %d pieces. Missing pieces filled with `NA` in %d rows [%s].",
            l_into, sum(split_lengths < l_into), paste(wrong_cols, collapse = ", ")))
          col_split
        },
        right = col_split, # Done automatically
        left  = lapply(col_split, function(x) {
          l <- length(x)
          if (l == l_into) x else
            c(rep(NA_character_, l_into - l), x)
        })
      )
    }
    cols <- unlist2d(col_split, idcols = FALSE)
    # If there are less columns than l_into, create missing ones with NA
    if (ncol(cols) < l_into) {
      cols <- add_vars(cols, as.data.frame(matrix(NA_character_,
        nrow = nrow(cols), ncol = l_into - ncol(cols))), pos = "end")
    }
    names(cols) <- into
    # drop columns corresponding to into being NA
    cols <- cols[!is.na(into)]

  } else if (is.numeric(sep)) {# Separate by position
    # Length of the vector must be length of into -1
    length_sep <- length(into) - 1L
    if (length(sep) != length_sep)
      stop("When {.arg sep} is numeric, it must contain one less item than {.arg {into}}",
        i = "It should be {length_sep} and it is {length(sep)}.")
    # Note: negative numbers start from far right
    stop("splitting with numbers not implemented yet...")

  } else {# Wrong sep
    stop("{.arg sep} must be a single string or a vector of numbers, not {.obj_type_friendly {sep}} ({.val {sep}}).")
  }

  if (isTRUE(convert)) {
    cols <- type.convert(cols, as.is = TRUE) # TODO: add an argument to convert chars into factors
  } else if (!isFALSE(convert)) {
    stop("{.arg convert} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {convert}} ({.val {convert}}).")
  }

  # New variables are added after col_num
  pos <- if (col_num == ncol(.data)) "end" else
    (col_num + 1L):(col_num + l_new_cols)
  add_vars_args <- list(x = .data, cols, pos = pos)
  res <- do.call(add_vars, add_vars_args)

  if (isTRUE(remove)) {
    res <- res[, -col_num, drop = FALSE]
  } else if (!isFALSE(remove)) {
    stop("{.arg remove} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {remove}} ({.val {remove}}).")
  }


  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::separate"))

# TODO...
# @export
# @rdname sciviews_functions
# separate_rows_ <- structure(function(.data = (.), ..., sep = "[^[:alnum:].]+",
#     convert = FALSE) {
#
#   if (!prepare_data_dot(.data))
#     return(recall_with_data_dot())
#
#   # For now, we use same function as txxx() counterpart... still must rework
#   if (inherits(.data, c("tbl_db", "dtplyr_step")))
#     stop("You must collect results from a tidy function before using a sciviews one.")
#
#   # Sometimes groups are kept, sometimes not... for now, we do not care
#   # (we always ungroup).
#   is_x_dtf <- is_dtf(.data)
#   is_x_dtt <- is_dtt(.data)
#   res <- separate_rows(.data = as_dtbl(fungroup(.data)), ..., sep = sep,
#     convert = convert)
#   if (is_x_dtf)
#     res <- as_dtf(res)
#   if (is_x_dtt)
#     res <- as_dtt(collect(res))
#   res
# }, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::unite"))

# TODO...
# @export
# @rdname sciviews_functions
# extract_ <- structure(function(.data = (.), col, into, regex = "([[:alnum:]]+)",
#   remove = TRUE, convert = FALSE, ...) {
#
#   if (!prepare_data_dot(.data))
#     return(recall_with_data_dot())
#
#   # For now, we use same function as txxx() counterpart... still must rework
#   if (inherits(.data, c("tbl_db", "dtplyr_step")))
#     stop("You must collect results from a tidy function before using a sciviews one.")
#
#   if (inherits(.data, "GRP_df")) {
#     is_x_grp_df <- TRUE
#     gvars <- fgroup_vars(.data, return = "names")
#     gvars <- lapply(gvars, as.name)
#   } else {
#     is_x_grp_df <- FALSE
#   }
#   if (is_dtt(.data)) {
#
#     res <- do.call('extract', list(as_dtbl(.data), col = substitute(col),
#       into = into, regex = regex, remove = remove, convert = convert, ...))
#     res <- as_dtt(collect(res))
#   } else {
#     res <- do.call('extract', list(.data, col = substitute(col), into = into,
#       regex = regex, remove = remove, convert = convert, ...))
#   }
#   if (is_x_grp_df)
#     res <- do.call('fgroup_by', c(list(.X = res), gvars))
#   res
# }, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::extract"))
