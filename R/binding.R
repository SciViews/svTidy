#' Binding Functions
#'
#' @description Functions for binding data frames by rows or columns.
#' These are SciViews::R versions with standard evaluation
#' and formula-based non-standard evaluation (ending with underscore `_`).
#'
#' #' **Functions:**
#' * `bind_rows_()` - Stack two or more data frames one on top of the other
#' * `bind_cols_()` - Stack two or more data frames side by side
#'
#' @export
#' @rdname binding
#' @param ... Data frames to bind.
#' @param .id The name of the column for the origin id, either names if all
#' other arguments are named, or numbers.
#' @param .use_names If `TRUE` (default), bind by matching names, if `FALSE`,
#'   bind by position. If `NULL`, warns if  all items do not have the same name
#'   in the same order, and then proceeds as if `FALSE` (but will be as if
#'   `TRUE` in the future).
#' @param .fill If `TRUE` (default), fill missing columns with `NA` or `NULL`
#'   for missing list columns, if `FALSE`, do not fill.
#' @return A data frame of the same type as the first one provided in `...`.
#' @seealso [dplyr::bind_rows()], [dplyr::bind_cols()]
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
    return(do.call(bind_rows, c(arg_list, list( .id = .id)),
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
#' @rdname binding
#' @param .name_repair How should the name be "repaired" to avoid duplicate
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
  do.call(bind_cols, c(arg_list, list( .name_repair = .name_repair)),
    envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::bind_cols"))
