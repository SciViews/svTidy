#' Selecting Functions
#'
#' @description
#' Functions for selecting, renaming, and extracting columns from a data frame.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' **Functions:**
#' * `select_()` - Select columns by name, position, or using tidy-select
#'   helpers
#' * `pull_()` - Extract a single column as a vector
#' * `rename_()` - Rename columns using new_name = old_name pairs
#' * `rename_with_()` - Rename columns using a function
#' * `all_of()` - Helper for selecting all specified columns (errors if missing)
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param ... For `select_()` and `rename_()`: column names, positions, or
#'   tidy-select expressions. Use formulas for non-standard evaluation
#'   (e.g., `~starts_with("x")`). For `rename_()`, provide pairs as
#'   `new_name = old_name` or `new_name = ~old_name`. For `rename_with_()`:
#'   additional arguments passed to `.fn`.
#' @param var For `pull_()`: the column to extract. Can be a column name
#'   (as character), a formula with column name on RHS (e.g., `~mpg`),
#'   a positive integer for position from left, or a negative integer for
#'   position from right. Default is `-1` (last column).
#' @param name For `pull_()`: optional column to use as names for the resulting
#'   vector. Specified the same way as `var`.
#' @param .fn For `rename_with_()`: a function to apply to column names, or a
#'   formula expression using `.x` as the column names (e.g., `~toupper(.x)`).
#' @param .cols For `rename_with_()`: columns to rename. Use tidy-select syntax
#'   with formulas. Default is `~everything()` (all columns).
#' @param x For `all_of()`: a character vector of column names. All must exist
#'   or an error is raised.
#'
#' @return
#' * `select_()` returns a data frame with only the selected columns
#' * `pull_()` returns a vector (named or unnamed depending on `name` parameter)
#' * `rename_()` returns the data frame with renamed columns
#' * `rename_with_()` returns the data frame with renamed columns
#' * `all_of()` returns the input vector (used inside select/rename functions)
#'
#' @seealso
#' [dplyr::select()], [dplyr::pull()], [dplyr::rename()],
#' [dplyr::rename_with()], [dplyr::all_of()], [tidyselect::starts_with()],
#' [tidyselect::ends_with()], [tidyselect::contains()], [tidyselect::matches()],
#' [tidyselect::everything()], [collapse::fselect()]
#'
#' @examples
#' library(svTidy)
#' data(mtcars)
#'
#' # Select specific columns by name
#' mtcars |> select_(~mpg, ~cyl, ~hp)
#'
#' # Select columns by position
#' mtcars |> select_(1, 3, 5)
#'
#' # Select range of columns
#' mtcars |> select_(~mpg:hp)
#'
#' # Use tidy-select helpers
#' mtcars |> select_(~starts_with("d"))
#' mtcars |> select_(~ends_with("p"))
#' mtcars |> select_(~contains("a"))
#'
#' # Exclude columns with minus
#' mtcars |> select_(~-c(mpg, cyl))
#'
#' # Select all numeric columns
#' mtcars |> select_(~where(is.numeric))
#'
#' # Combine multiple selection methods
#' mtcars |> select_(~mpg, ~starts_with("d"), ~hp)
#'
#' # Use all_of() for programmatic selection
#' cols <- c("mpg", "cyl", "hp")
#' mtcars |> select_(~all_of(cols))
#'
#' # Pull a column as a vector (by name)
#' mtcars |> pull_(~mpg)
#'
#' # Pull by position (last column)
#' mtcars |> pull_(-1)
#'
#' # Pull first column
#' mtcars |> pull_(1)
#'
#' # Pull with names from another column
#' mtcars |> pull_(~mpg, name = ~cyl)
#'
#' # Rename columns with new_name = old_name
#' mtcars |> rename_(miles_per_gallon = ~mpg, cylinders = ~cyl)
#'
#' # Rename using column positions
#' mtcars |> rename_(miles_per_gallon = 1, cylinders = 2)
#'
#' # Rename multiple columns
#' mtcars |>
#'   rename_(
#'     miles_per_gallon = ~mpg,
#'     cylinders = ~cyl,
#'     horsepower = ~hp
#'   )
#'
#' # Rename all columns with a function
#' mtcars |> rename_with_(toupper)
#'
#' # Rename using a formula with .x
#' mtcars |> rename_with_(~paste0("var_", .x))
#'
#' # Rename with string manipulation
#' mtcars |> rename_with_(~tolower(.x))
#' mtcars |> rename_with_(~gsub("_", ".", .x))
#'
#' # Rename only selected columns
#' mtcars |> rename_with_(toupper, .cols = ~starts_with("d"))
#'
#' # Rename specific columns by name
#' mtcars |> rename_with_(toupper, .cols = c("mpg", "cyl", "hp"))
#'
#' # Chain operations
#' mtcars |>
#'   select_(~mpg, ~cyl, ~hp, ~wt) |>
#'   rename_(efficiency = ~mpg, weight = ~wt) |>
#'   arrange_(~cyl)
#'
#' # Use in data pipeline
#' mtcars |>
#'   select_(~where(is.numeric)) |>
#'   rename_with_(tolower) |>
#'   filter_(~cyl > 4) |>
#'   pull_(~mpg)
#'
#' @name selecting
#' @export
#' @rdname selecting
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
    return(.data[, 0L, drop = FALSE])

  # Process dots to args
  args <- formula_select(..., .fast.allowed.funs = c(":", "-", "c", "("))
  if (!args$fastselect) {
    #message(gettextf("Using tidyselect with `%s`",
    #  paste(args$dots, collapse = ", ")))
    eval_select2 <- function(..., data)
      eval_select(expr(c(...)), data = data, error_call = stop_top_call())
    loc <- do.call(eval_select2, c(args$dots, list(data = .data)))
    if (to_dtrm)
      let_data.table_to_data.trame(.data)
    res <- .data[, loc, drop = FALSE]
    names(res) <- names(loc)


  } else {# fastselect with collapse
    if (args$are_formulas) {
      # Other return modes not supported (yet)
      res <- do.call(fselect, c(list(.x = .data, return = "data"), args$dots),
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

#' @export
#' @rdname selecting
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
#' @rdname selecting
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

  res <- do.call(frename, c(list(.x = .data, .nse = FALSE), dots_inv),
    envir = args$env)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::rename"))

#' @export
#' @rdname selecting
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
#' @rdname selecting
all_of <- function(x) x
