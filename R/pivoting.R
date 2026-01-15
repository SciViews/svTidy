#' Pivoting Functions
#'
#' @description
#' Functions for pivoting data between long and wide formats.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' **Functions:**
#' * `pivot_longer_()` - Convert data from wide to long format
#' * `pivot_wider_()` - Convert data from long to wide format
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param cols For `pivot_longer_()`: columns to pivot into longer format. Use
#'   tidy-select syntax with formulas (e.g., `~col1:col5`, `~starts_with("x")`),
#'   or provide column positions or names as a vector.
#' @param ... Additional arguments (currently unused, for compatibility)
#' @param cols_vary Character. Either `"fastest"` (default) or `"slowest"`. If
#'   `"fastest"`, keeps individual rows from `cols` close together in the output.
#'   If `"slowest"`, keeps individual columns from `cols` close together.
#' @param names_to Character string specifying the name of the column to create
#'   from the column names being pivoted. Default is `"name"`.
#' @param names_prefix Character. A regular expression used to remove matching
#'   text from the start of each variable name before creating the names column.
#' @param values_to Character string specifying the name of the column to create
#'   from the cell values. Default is `"value"`.
#' @param values_drop_na Logical. If `TRUE`, rows containing only `NA` values in
#'   the `values_to` column are dropped from the result. Default is `FALSE`.
#' @param factor Logical. If `TRUE`, convert the names and values columns to
#'   factors. If `FALSE` (default), leave them as character strings.
#' @param id_cols For `pivot_wider_()`: columns that uniquely identify each
#'   observation. Use tidy-select syntax or `NULL` (default) to use all columns
#'   except `names_from` and `values_from`.
#' @param id_expand Logical. If `TRUE`, expand the `id_cols` to include all
#'   possible combinations. Default is `FALSE`.
#' @param names_from For `pivot_wider_()`: column(s) containing the names for
#'   the new columns. Provide as unquoted names or character vector. Default is
#'   `name`.
#' @param names_vary Character. How column names are constructed when multiple
#'   `names_from` or `values_from` columns are provided: `"fastest"` (default),
#'   `"slowest"`, `"transpose"`, or `"slowtranspose"`.
#' @param values_from For `pivot_wider_()`: column(s) containing the values for
#'   the new columns. Provide as unquoted names or character vector. Default is
#'   `value`.
#' @param values_fill Optional scalar value to use for missing combinations.
#'   Default is `NULL` (use `NA`).
#' @param values_fn Function to apply when there are multiple values for a cell.
#'   Can be a string naming an internal function (`"first"`, `"last"` (default),
#'   `"count"`, `"sum"`, `"mean"`, `"min"`, `"max"`), or a formula calling an
#'   external function with first argument `.x` (e.g., `~fmedian(.x, na.rm = TRUE)`).
#' @param drop Logical. Should unused factor levels be dropped? Default is `TRUE`.
#' @param sort Logical. If `TRUE`, sort the result so the largest groups are shown
#'   on top. Default is `FALSE`.
#'
#' @return
#' A data frame of the same type as `.data` in the pivoted format.
#' `pivot_longer_()` returns a data frame with more rows and fewer columns.
#' `pivot_wider_()` returns a data frame with fewer rows and more columns.
#'
#' @seealso
#' [tidyr::pivot_longer()], [tidyr::pivot_wider()], [collapse::pivot()]
#'
#' @examples
#' library(svTidy)
#'
#' # Create sample wide data
#' wide_data <- data.frame(
#'   id = 1:3,
#'   year = c(2020, 2021, 2022),
#'   q1 = c(100, 110, 120),
#'   q2 = c(105, 115, 125),
#'   q3 = c(110, 120, 130),
#'   q4 = c(115, 125, 135)
#' )
#'
#' # Pivot from wide to long format
#' wide_data |>
#'   pivot_longer_(~q1:q4, names_to = "quarter", values_to = "sales")
#'
#' # Use tidy-select helpers
#' wide_data |>
#'   pivot_longer_(~starts_with("q"), names_to = "quarter", values_to = "sales")
#'
#' # Remove prefix from column names
#' wide_data |>
#'   pivot_longer_(
#'     ~q1:q4,
#'     names_to = "quarter",
#'     values_to = "sales",
#'     names_prefix = "q"
#'   )
#'
#' # Control row ordering with cols_vary
#' wide_data |>
#'   pivot_longer_(~q1:q4, cols_vary = "slowest")
#'
#' # Drop NA values
#' wide_na <- wide_data
#' wide_na$q3[2] <- NA
#' wide_na |>
#'   pivot_longer_(~q1:q4, values_drop_na = TRUE)
#'
#' # Convert to factors
#' wide_data |>
#'   pivot_longer_(~q1:q4, factor = TRUE)
#'
#' # Create sample long data
#' long_data <- data.frame(
#'   id = rep(1:3, each = 4),
#'   year = rep(c(2020, 2021, 2022), each = 4),
#'   quarter = rep(c("q1", "q2", "q3", "q4"), 3),
#'   sales = c(100, 105, 110, 115, 110, 115, 120, 125, 120, 125, 130, 135)
#' )
#'
#' # Pivot from long to wide format
#' long_data |>
#'   pivot_wider_(names_from = "quarter", values_from = "sales")
#'
#' # Specify id columns explicitly
#' long_data |>
#'   pivot_wider_(
#'     id_cols = ~c(id, year),
#'     names_from = "quarter",
#'     values_from = "sales"
#'   )
#'
#' # Add prefix to new column names
#' long_data |>
#'   pivot_wider_(
#'     names_from = "quarter",
#'     values_from = "sales",
#'     names_prefix = "sales_"
#'   )
#'
#' # Fill missing values
#' long_data |>
#'   pivot_wider_(
#'     names_from = "quarter",
#'     values_from = "sales",
#'     values_fill = 0
#'   )
#'
#' # Handle multiple values with aggregation
#' long_dup <- rbind(long_data, long_data[1:3, ])
#' long_dup |>
#'   pivot_wider_(
#'     names_from = "quarter",
#'     values_from = "sales",
#'     values_fn = "mean"
#'   )
#'
#' # Use custom aggregation function
#' long_dup |>
#'   pivot_wider_(
#'     names_from = "quarter",
#'     values_from = "sales",
#'     values_fn = ~sum(.x)
#'   )
#'
#' # Multiple names_from and values_from
#' long_multi <- data.frame(
#'   id = rep(1:2, each = 4),
#'   metric = rep(c("sales", "profit"), 4),
#'   quarter = rep(c("q1", "q2"), each = 2, times = 2),
#'   value = c(100, 20, 105, 22, 110, 24, 115, 26)
#' )
#' long_multi |>
#'   pivot_wider_(
#'     names_from = c("quarter", "metric"),
#'     values_from = "value"
#'   )
#'
#' # Round-trip: wide -> long -> wide
#' wide_data |>
#'   pivot_longer_(~q1:q4, names_to = "quarter", values_to = "sales") |>
#'   pivot_wider_(names_from = "quarter", values_from = "sales")
#'
#' @name pivoting
#' @export
#' @rdname pivoting
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

#' @export
#' @rdname pivoting
pivot_wider_ <- structure(function(.data = (.), ..., id_cols = NULL,
    id_expand = FALSE, names_from = "", names_prefix = "",
    names_vary = "fastest", values_from = "", values_fill = NULL,
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
