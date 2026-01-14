#' Mutating Functions
#'
#' @description
#' Functions for creating new columns or modifying existing columns in a data
#' frame. These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `mutate_()` - Add new columns or modify existing ones
#' * `transmute_()` - Create new columns and drop all others
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param ... Name-value pairs specifying new columns or modifications. Names
#'   are column names; values are expressions to compute. Use formulas for
#'   non-standard evaluation (e.g., `new_col = ~mean(old_col)`), or provide
#'   character names and expressions for standard evaluation. New columns can
#'   refer to columns just created. Within formulas, you can provide column
#'   names use like this: `'new_col_name' ~ median(old_col)`. You can also use
#'   the name of an object that contains the column name. For instance,
#'   `col_names <- c("new_col1", "new_col2")`, then in `mutate_()`, use
#'   `col_name[1] ~ median(old_col)`.
#' @param .by Optional grouping variables for per-group computations. Provide
#'   as formulas (e.g., `~group_col`) or character names. Groups are temporary
#'   and not preserved in the output. Cannot be used with grouped data frames.
#' @param .keep Control which columns to keep in the output:
#'   * `"all"` (default) - Keep all existing columns plus new ones
#'   * `"used"` - Keep columns used to make new columns, and new columns
#'   * `"unused"` - Keep columns not used to make new columns, and new columns.
#'   This is useful if you do not need the columns that are used to create the
#'   new ones.
#'   * `"none"` - Keep only new columns (same as `transmute_()`)
#' @param .before Optional column name or position to place new columns before.
#'   Not yet implemented - use dplyr::mutate() instead.
#' @param .after Optional column name or position to place new columns after.
#'   Not yet implemented - use dplyr::mutate() instead.
#' @param .cols Optional character vector of column names to operate on. If
#'   provided, only these columns are modified or created.
#'
#' @return
#' A data frame of the same type as `.data` with modified or new columns.
#' * `mutate_()` returns all columns (by default), including new/modified ones
#' * `transmute_()` returns only the newly created columns
#'
#' @seealso
#' [dplyr::mutate()], [dplyr::transmute()], [collapse::fmutate()]
#'
#' @examples
#' library(svTidy)
#' data(mtcars)
#'
#' # Create new columns using formulas
#' mtcars |> mutate_(hp_per_cyl = ~hp/cyl)
#'
#' # Multiple new columns
#' mtcars |>
#'   mutate_(
#'     hp_per_cyl = ~hp/cyl,
#'     wt_kg = ~wt * 453.592
#'   )
#'
#' # Modify existing column
#' mtcars |> mutate_(mpg = ~mpg * 1.5)
#'
#' # Reference newly created columns
#' mtcars |>
#'   mutate_(
#'     hp_per_cyl = ~hp/cyl,
#'     hp_per_cyl_scaled = ~hp_per_cyl * 100
#'   )
#'
#' # Use column name in a variable
#' col_name <- "power_ratio"
#' mtcars |> mutate_(col_name ~ hp/wt)
#'
#' # Group-wise computations with .by
#' mtcars |>
#'   mutate_(
#'     mpg_centered = ~mpg - mean(mpg),
#'     .by = 'cyl'
#'   )
#'
#' # Multiple grouping variables
#' mtcars |>
#'   mutate_(
#'     hp_rank = ~rank(hp),
#'     .by = c('cyl', 'gear')
#'   )
#'
#' # Control which columns to keep
#' mtcars |>
#'   mutate_(
#'     hp_per_cyl = ~hp/cyl,
#'     .keep = "used"
#'   )
#'
#' mtcars |>
#'   mutate_(
#'     efficiency = ~mpg/hp,
#'     .keep = "unused"
#'   )
#'
#' # transmute_() keeps only new columns
#' mtcars |>
#'   transmute_(
#'     car = ~rownames(mtcars),
#'     hp_per_cyl = ~hp/cyl,
#'     efficiency = ~mpg/wt
#'   )
#'
#' # Conditional mutations
#' mtcars |>
#'   mutate_(
#'     performance = ~ifelse(hp > 150, "high", "normal")
#'   )
#'
#' # Use with grouped data
#' mtcars |>
#'   group_by_(~cyl) |>
#'   mutate_(mpg_ratio = ~mpg/mean(mpg))
#'
#' # Complex transformations
#' mtcars |>
#'   mutate_(
#'     log_hp = ~log(hp),
#'     sqrt_wt = ~sqrt(wt),
#'     hp_wt_interaction = ~hp * wt
#'   )
#'
#' @name mutating
#' @export
#' @rdname mutating
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

  res <- do.call(fmutate, c(list(.data = res), args$dots,
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
#' @rdname mutating
transmute_ <- structure(function(.data, ...) {
  .__top_call__. <- TRUE
  do.call(mutate_, list(.data, ..., .keep = "none"), envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::transmute"))
