#' Joining Functions
#'
#' @description
#' Functions for joining two data frames based on matching values in key columns.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `left_join_()` - Keep all rows from x, add matching columns from y
#' * `right_join_()` - Keep all rows from y, add matching columns from x
#' * `inner_join_()` - Keep only rows with matches in both x and y
#' * `full_join_()` - Keep all rows from both x and y
#' * `semi_join_()` - Keep rows in x that have a match in y (no columns from y)
#' * `anti_join_()` - Keep rows in x that do NOT have a match in y
#' * `join_()` - Generic join function with `how` parameter
#'
#' @param x A data frame (data.frame, data.table, or tibble)
#' @param y A data frame to join with x
#' @param by A character vector of column names to join by. If `NULL`, uses all
#'   columns with common names across x and y. Can also be a named character
#'   vector to join by different column names (e.g., `c("a" = "b")` joins x$a to
#'   y$b), or a `dplyr::join_by()` object for complex joins.
#' @param copy If x and y are not from the same data source, and copy is `TRUE`,
#'   then y will be copied into the same source as x. This allows you to join
#'   tables across data sources, but is potentially expensive.
#' @param suffix Character vector of length 2 specifying suffixes to append to
#'   duplicate column names. Default is `c(".x", ".y")`.
#' @param ... Additional arguments (currently unused, for compatibility)
#' @param keep Should the join keys from both x and y be preserved in the output?
#'   If `NULL` (default), keeps join keys from x only for equality joins, but
#'   keeps keys from both for inequality joins. If `TRUE`, all keys from both
#'   inputs are retained. If `FALSE`, only keys from x are retained. Note: when
#'   `keep = TRUE`, calculation is delegated to dplyr join methods.
#' @param na_matches Should two `NA` or two `NaN` values match? `"na"` (default)
#'   treats two `NA` or two `NaN` values as equal, like `%in%`, `match()`, and
#'   `merge()`. `"never"` treats two `NA` or two `NaN` values as different, and
#'   will never match them together or to any other values. When `"never"`,
#'   calculation is delegated to dplyr join methods.
#' @param multiple Handling of rows in x with multiple matches in y. Options:
#'   `"all"` (default) returns every match detected in y (SQL behavior),
#'   `"first"` returns the first match detected in y,
#'   `"last"` returns the last match detected in y,
#'   `"any"` returns one match (faster but non-deterministic).
#'   For `"any"` and `"last"` (and `"first"` for right joins), calculation is
#'   delegated to dplyr join methods.
#' @param unmatched How should unmatched keys that would result in dropped rows
#'   be handled? `"drop"` (default) drops unmatched keys from the result.
#'   `"error"` throws an error if unmatched keys are detected. Can also be a
#'   named list like `list(x = 1, y = 0.5, fail = "warning")` specifying the
#'   proportions that must match and the action (`"message"`, `"warning"`, or
#'   `"error"`). Not available for `full_join_()`, `semi_join_()`, and
#'   `anti_join_()`.
#' @param relationship Expected relationship between the keys of x and y:
#'   `NULL` (default) has no expectations but warns for many-to-many,
#'   `"one-to-one"` expects each row in x matches at most 1 row in y and vice versa,
#'   `"one-to-many"` expects each row in y matches at most 1 row in x,
#'   `"many-to-one"` expects each row in x matches at most 1 row in y,
#'   `"many-to-many"` has no restrictions (explicit about relationship).
#' @param sort Logical. If `TRUE`, the result is sorted by the grouping variables.
#'   Default is `FALSE`.
#' @param verbose Integer controlling information printed about the join:
#'   `0` means no output (default), `1` prints join information, and
#'   `2` additionally prints the classes of the `by` columns.
#'   Note: This parameter is ignored when using dplyr join methods.
#' @param column Name for an extra column to generate in the output indicating
#'   which dataset a record came from. `TRUE` creates a column named `".join"`,
#'   or provide a custom name as a character string. `NULL` (default) does not
#'   add this column. Note: This parameter is ignored when using dplyr join methods.
#' @param attr Name for an attribute providing information about the join
#'   performed (including output of [collapse::fmatch()]) attached to the result.
#'   `TRUE` creates an attribute named `"join.match"`, or provide a custom name.
#'   `NULL` (default) does not add this attribute. Note: This parameter is
#'   ignored when using dplyr join methods.
#' @param how Character string specifying the join type for `join_()`:
#'   `"full"` (default), `"inner"`, `"left"`, `"right"`, `"semi"`, or `"anti"`.
#'
#' @return
#' A data frame of the same type as `x``. The order of the rows and columns of
#' `x` is preserved as much as possible.
#' * `left_join_()` returns all rows from `x`, and all columns from `x` and `y`
#' * `right_join_()` returns all rows from `y`, and all columns from `x` and `y`
#' * `inner_join_()` returns only rows with matches in both `x` and `y`
#' * `full_join_()` returns all rows from both `x` and `y`
#' * `semi_join_()` returns rows from `x` (no columns added from `y`)
#' * `anti_join_()` returns rows from `x` with no match in `y` (no columns from
#'   `y`)
#'
#' @name joining
#' @export
#' @seealso
#' [dplyr::left_join()], [dplyr::right_join()], [dplyr::inner_join()],
#' [dplyr::full_join()], [dplyr::semi_join()], [dplyr::anti_join()],
#' [collapse::join()], [dplyr::join_by()]
#'
#' @examples
#' library(svTidy)
#'
#' # Create example datasets
#' band_members <- data.frame(
#'   name = c("Mick", "John", "Paul"),
#'   band = c("Stones", "Beatles", "Beatles")
#' )
#' band_instruments <- data.frame(
#'   name = c("John", "Paul", "Keith"),
#'   plays = c("guitar", "bass", "guitar")
#' )
#'
#' # Left join - keep all rows from x
#' band_members |> left_join_(band_instruments, by = "name")
#'
#' # Right join - keep all rows from y
#' band_members |> right_join_(band_instruments, by = "name")
#'
#' # Inner join - keep only matching rows
#' band_members |> inner_join_(band_instruments, by = "name")
#'
#' # Full join - keep all rows from both
#' band_members |> full_join_(band_instruments, by = "name")
#'
#' # Semi join - filter x to rows matching y (no columns from y)
#' band_members |> semi_join_(band_instruments, by = "name")
#'
#' # Anti join - filter x to rows NOT matching y
#' band_members |> anti_join_(band_instruments, by = "name")
#'
#' # Join by different column names
#' band_instruments2 <- data.frame(
#'   artist = c("John", "Paul", "Keith"),
#'   plays = c("guitar", "bass", "guitar")
#' )
#' band_members |> left_join_(band_instruments2, by = c("name" = "artist"))
#'
#' # Add suffix to duplicate columns
#' df1 <- data.frame(id = 1:3, value = c("a", "b", "c"))
#' df2 <- data.frame(id = 2:4, value = c("B", "C", "D"))
#' df1 |> full_join_(df2, by = "id", suffix = c("_x", "_y"))
#'
#' # Control handling of multiple matches
#' df_x <- data.frame(key = c(1, 1, 2), x = c("a", "b", "c"))
#' df_y <- data.frame(key = c(1, 1, 2), y = c("A", "B", "C"))
#' df_x |> left_join_(df_y, by = "key", multiple = "all")
#' df_x |> left_join_(df_y, by = "key", multiple = "first")
#'
#' # Validate relationships
#' df_one <- data.frame(id = 1:3, val = c("a", "b", "c"))
#' df_many <- data.frame(id = c(1, 1, 2), val = c("A", "B", "C"))
#' \dontrun{
#' # This will error - expects one-to-one but is one-to-many
#' df_one |> left_join_(df_many, by = "id", relationship = "one-to-one")
#' }
#' # This works - explicitly one-to-many
#' df_one |> left_join_(df_many, by = "id", relationship = "one-to-many")
#'
#' # Add indicator column showing source
#' band_members |>
#'   full_join_(band_instruments, by = "name", column = "source")
#'
#' # Use generic join_() with how parameter
#' band_members |> join_(band_instruments, by = "name", how = "inner")
#' band_members |> join_(band_instruments, by = "name", how = "left")
#'
#' # Handle unmatched keys
#' \dontrun{
#' # Error if any keys don't match
#' band_members |>
#'   inner_join_(band_instruments, by = "name", unmatched = "error")
#' }
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
      do.call(join, list(substitute(x), substitute(y), on = by, how = how,
        suffix = suffix, validate = validate,
        multiple = mult, sort = sort, keep.col.order = TRUE,
        drop.dup.cols = FALSE, verbose = verbose, require = require,
        column = column, attr = attr), envir = parent.frame()))
  .temp_env$.stop_top_call_env <- NULL

  res
}

#' @export
#' @rdname joining
left_join_ <- structure(function(x = (.), y, by = NULL, copy = FALSE,
  suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"),
  multiple = "all", unmatched = "drop", relationship = NULL, sort = FALSE,
  verbose = 0, column = NULL, attr = NULL) {

  if (!prepare_data_dot2(x, y))
    return(recall_with_data_dot2())

  if (!missing(...))
    check_dots_empty()

  # Delegate to .join_()
  do.call('join_', list(x = substitute(x), y = substitute(y), by = by,
    copy = copy, suffix = suffix, keep = keep, na_matches = na_matches,
    multiple = multiple, unmatched = unmatched, relationship = relationship,
    sort = sort, verbose = verbose, column = column, attr = attr,
    how = "left"), envir = parent.frame())

}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::left_join"))

#' @export
#' @rdname joining
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
#' @rdname joining
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

#' @export
#' @rdname joining
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
#' @rdname joining
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

#' @export
#' @rdname joining
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
