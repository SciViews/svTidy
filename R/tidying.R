#' Tidying Functions
#'
#' @description
#' Functions for tidying data by separating, uniting, filling, and handling missing values.
#'
#' These are SciViews::R versions of tidyverse functions with standard
#' evaluation and formula-based non-standard evaluation (ending with underscore
#' `_`). They work with data.frame, data.table, and tibbles.
#'
#' @section Functions:
#' * `separate_()` - Separate one column into multiple columns by splitting on a separator
#' * `unite_()` - Unite multiple columns into one by pasting strings together
#' * `fill_()` - Fill missing values using previous or next non-missing value
#' * `drop_na_()` - Drop rows containing missing values
#' * `replace_na_()` - Replace missing values with a specified value
#' * `uncount_()` - Duplicate rows according to a weighting variable
#'
#' @param .data A data frame (data.frame, data.table, or tibble)
#' @param col For `separate_()`: the column to separate. Can be a column name
#'   as character, or a formula (e.g., `~col_name`). For `unite_()`: the name
#'   of the new united column (character string or formula).
#' @param into For `separate_()`: names of new variables to create as a character
#'   vector. Use `NA` to omit a variable in the output.
#' @param sep For `separate_()` and `unite_()`: separator between columns. For
#'   `separate_()`, can be a character vector, a numeric vector of positions to
#'   split at, or a regular expression pattern. Default is `"[^[:alnum:]]+"` for
#'   `separate_()` and `"_"` for `unite_()`.
#' @param ... For `separate_()` and `unite_()`: additional arguments (currently
#'   unused). For `fill_()` and `drop_na_()`: columns to fill or check for NAs.
#'   Use formulas (e.g., `~col1`, `~col2`) or column names. If not provided,
#'   uses all columns.
#' @param remove Logical. If `TRUE` (default), remove input columns from output.
#' @param convert For `separate_()`: logical. If `TRUE`, attempts to convert
#'   new columns to appropriate types. Default is `FALSE`.
#' @param extra For `separate_()` when `sep` is a character: what to do when
#'   there are too many pieces. Options: `"warn"` (default, warn and drop),
#'   `"drop"` (drop without warning), or `"merge"` (merge extra pieces with last).
#' @param fill For `separate_()` when `sep` is a character: what to do when
#'   there are not enough pieces. Options: `"warn"` (default, warn and fill right
#'   with NA), `"right"` (fill right without warning), or `"left"` (fill left).
#' @param fixed For `separate_()`: logical. If `TRUE`, `sep` is a fixed string.
#'   If `FALSE` (default), `sep` is a (perl) regular expression.
#' @param na.rm For `unite_()`: logical. If `TRUE`, `NA` values are removed
#'   before uniting. Default is `FALSE`.
#' @param .direction For `fill_()`: direction to fill missing values. Options:
#'   `"down"` (default, last observation carried forward), `"up"` (next
#'   observation carried backward), `"downup"` (first down then up), or `"updown"`
#'   (first up then down).
#' @param .na.attr For `drop_na_()`: logical. If `TRUE`, adds an attribute
#'   containing removed cases (compatible with [na.omit()] format). Default is
#'   `FALSE`.
#' @param .prop For `drop_na_()`: numeric proportion threshold. Only drop rows
#'   where the proportion of missing values is at least this value. Must be in
#'   [0, 1]. Default is `0` (drop if any NA).
#' @param replace For `replace_na_()`: replacement value(s). For a vector `v`,
#'   provide a single value. For a data frame, provide a named list where names
#'   are column names and values are replacement values.
#' @param v For `replace_na_()`: a vector where NAs should be replaced. If
#'   provided, `.data` is used as the `replace` argument.
#' @param weights For `uncount_()`: a vector of weights for duplication. Can be
#'   a numeric vector, a column name as character, or a formula (e.g., `~wt_col`).
#'   Values must be non-negative integers.
#' @param .remove For `uncount_()`: logical. If `TRUE` (default) and `weights`
#'   is a column name, remove that column from the output.
#' @param .id For `uncount_()`: optional name for a new column that will give
#'   a unique identifier for each row created. Default is `NULL` (no ID column).
#'
#' @return
#' A data frame of the same type as `.data` with the transformation applied.
#' * `separate_()` returns a data frame with the specified column split into
#'   multiple columns
#' * `unite_()` returns a data frame with specified columns combined into one
#' * `fill_()` returns a data frame with missing values filled
#' * `drop_na_()` returns a data frame with rows containing NAs removed
#' * `replace_na_()` returns a data frame or vector with NAs replaced by specified values
#' * `uncount_()` returns a data frame with rows duplicated according to weights
#'
#' @seealso
#' [tidyr::separate()], [tidyr::unite()], [tidyr::fill()], [tidyr::drop_na()],
#' [tidyr::replace_na()], [tidyr::uncount()], [collapse::na_omit()],
#' [collapse::replace_na()]
#'
#' @examples
#' library(svTidy)
#'
#' # separate_() - split one column into multiple
#' df <- data.frame(x = c("a_b_c", "d_e_f", "g_h_i"))
#' df |> separate_(~x, into = c("A", "B", "C"), sep = "_")
#'
#' # Use character name instead of formula
#' df |> separate_("x", into = c("A", "B", "C"), sep = "_")
#'
#' # Drop a column with NA in into
#' df |> separate_(~x, into = c("A", NA, "C"), sep = "_")
#'
#' # Keep original column
#' df |> separate_(~x, into = c("A", "B", "C"), sep = "_", remove = FALSE)
#'
#' # Separate by numeric positions is not implemented yet
#' #df2 <- data.frame(date = c("20201231", "20210115", "20220601"))
#' #df2 |> separate_(~date, into = c("year", "month", "day"), sep = c(4, 6))
#'
#' # Handle too many pieces
#' df3 <- data.frame(x = c("a_b_c", "d_e_f_g", "h_i"))
#' df3 |> separate_(~x, into = c("A", "B"), extra = "drop")
#' df3 |> separate_(~x, into = c("A", "B"), extra = "merge")
#'
#' # Handle too few pieces
#' df3 |> separate_(~x, into = c("A", "B", "C"), fill = "right")
#'
#' # unite_() - combine multiple columns into one
#' df4 <- data.frame(year = 2020:2022, month = 1:3, day = 10:12)
#' df4 |> unite_(~date, ~year, ~month, ~day, sep = "-")
#'
#' # Keep original columns
#' df4 |> unite_(~date, ~year, ~month, ~day, sep = "-", remove = FALSE)
#'
#' # Handle NAs in unite
#' df5 <- data.frame(x = c("a", "b", NA), y = c("d", NA, "f"))
#' df5 |> unite_(~z, ~x, ~y)
#' df5 |> unite_(~z, ~x, ~y, na.rm = TRUE)
#'
#' # fill_() - fill missing values
#' df6 <- data.frame(
#'   group = c(1, 1, 1, 2, 2, 2),
#'   value = c(10, NA, NA, 20, NA, 30)
#' )
#' df6 |> fill_(~value)
#'
#' # Fill upward
#' df6 |> fill_(~value, .direction = "up")
#'
#' # Fill down then up
#' df6 |> fill_(~value, .direction = "downup")
#'
#' # Fill specific columns
#' df7 <- data.frame(x = c(1, NA, 3), y = c(NA, 2, NA), z = c(1, 2, 3))
#' df7 |> fill_(~x, ~y, .direction = "down")
#'
#' # Fill with grouped data
#' df6 |>
#'   group_by_(~group) |>
#'   fill_(~value)
#'
#' # drop_na_() - remove rows with missing values
#' df8 <- data.frame(x = c(1, 2, NA), y = c("a", NA, "c"), z = 1:3)
#' df8 |> drop_na_()
#'
#' # Drop NAs from specific columns only
#' df8 |> drop_na_(~x)
#' df8 |> drop_na_(~x, ~y)
#'
#' # Use proportion threshold
#' df9 <- data.frame(x = c(1, NA, NA), y = c(NA, 2, NA), z = c(NA, NA, 3))
#' df9 |> drop_na_(.prop = 0.5)  # Drop rows with >= 50% NAs
#'
#' # Keep track of removed rows
#' result <- df8 |> drop_na_(.na.attr = TRUE)
#' attr(result, "na.action")
#'
#' # replace_na_() - replace NAs with a value
#' df10 <- data.frame(x = c(1, 2, NA), y = c(NA, "b", "c"))
#' df10 |> replace_na_(list(x = 0, y = "missing"))
#'
#' # Replace in a single vector
#' vec <- c(1, 2, NA, 4, NA)
#' replace_na_(v = vec, replace = 0)
#'
#' # Replace all NAs with same value (not standard tidyr)
#' df10 |> replace_na_(list(everywhere = 999))
#'
#' # uncount_() - duplicate rows according to weights
#' df11 <- data.frame(x = c("a", "b", "c"), n = c(1, 2, 3))
#' df11 |> uncount_(~n)
#'
#' # Keep the weight column
#' df11 |> uncount_(~n, .remove = FALSE)
#'
#' # Add ID column to track original rows
#' df11 |> uncount_(~n, .id = "id")
#'
#' # Use numeric weights vector
#' df12 <- data.frame(x = c("a", "b", "c"))
#' df12 |> uncount_(weights = c(2, 1, 3))
#'
#' @name tidying
#' @export
#' @rdname tidying
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

#' @export
#' @rdname tidying
#' @param na.rm If `TRUE`, `NA`s are eliminated before uniting the values.
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
      cols <- do.call(eval_select2, c(args$dots, list(data = .data)))

    } else {# fastselect with collapse
      if (args$are_formulas) {
        cols <- do.call(fselect, c(list(.x = .data, return = "indices"),
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

#' @export
#' @rdname tidying
#' @param .direction Direction in which to fill missing data: `"down"` (by
#'   default), `"up"`, or `"downup"` (first down, then up), `"updown"`
#'   (the opposite).
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
    cols <- do.call(eval_select2, c(args$dots, list(data = .data)))
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

#' @export
#' @rdname tidying
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
    loc <- do.call(eval_select2, c(args$dots, list(data = .data)))
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

#' @export
#' @rdname tidying
#' @param v a vector where to replace NAs.
#' @param replace If `.data` is a vector, a unique value to replace `NA`s,
#'   otherwise, a list of values, one per column of the data frame.
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

#' @export
#' @rdname tidying
#' @param weights A vector of weight to use to "uncount" `.data`.
#' @param .remove If `TRUE` (default), and `weights` is the name of a column,
#'   that column is removed from `.data`.
#' @param .id The name of the column for the origin id, either names if all
#'   other arguments are named, or numbers.
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
