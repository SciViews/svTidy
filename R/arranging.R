#' Arranging Functions
#'
#' @description Functions for arranging (sorting) rows.
#' These are SciViews::R versions with standard evaluation
#' and formula-based non-standard evaluation (ending with underscore `_`).
#'
#' @export
#' @rdname arranging
#' @param .data A data frame (data.frame, data.table or tibble's tbl_df). If not
#'   provided, the data-dot mechanism injects `.` as `.data=` automatically.
#' @param ... Either standard (quoted) column names of `data`. of formulas like
#' `~col_name` (formula-masking).
#' @param .by_group Logical. If `TRUE` rows are first arranged by the grouping
#' variables if any (applies only to grouped data frames). `FALSE` by default.
#' @param .locale The locale to sort character vectors in. If `NULL`(default),
#'   use the `"dplyr.legacy_locale"` option (same one as [dplyr::arrange()]),
#' and if not specified, it uses a `"C"` locale.
#' @param .decreasing  Sort in decreasing order (no, `FALSE`, by default)?
#' @details
#'   For the way missing data are handled, see [dplyr::arrange()].
#' @return A similar object as `.data` with all columns, all attributes and
#' groups preserved, but row rearranged according to the specified order.
#' @seealso [dplyr::arrange()]
#' @importFrom collapse roworder roworderv
#' @examples
#' library(svTidy)
#' data(mtcars, package = 'datasets')
#' mtcars <- data.trame::as.data.trame(mtcars)
#' # Standard evaluation (provide quoted names of the columns to sort)
#' # You cannot use desc(col) here, but must specify what you want in the
#' # .decreasing argument
#' arrange_(mtcars, 'cyl', 'disp', .decreasing = c(FALSE, TRUE))
#' # With formula masking, you can use desc()
#' arrange_(mtcars, ~cyl, ~desc(disp))
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
        res <- do.call(roworder, c(list(X = fungroup(.data)), args$dots),
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
        res <- do.call(group_by, c(list(.data = res, .drop = drop),
          gvars_sym), envir = args$env)
      }

    } else {# Ungrouped dataset, no particular difficulties
      if (args$are_formulas) {
        res <- do.call(roworder, c(list(X = .data), args$dots),
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
    res <- do.call(arrange, c(list(.data), args$dots,
      list(.by_group = .by_group, .locale = .locale)), envir = args$env)
  }

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::arrange"))
