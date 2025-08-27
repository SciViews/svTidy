#.onload <- function(lib, pkg) {# nocov start
#  # Nothing to do for now
#}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Need this for R CMD check to pass
. <- NULL

# Internal options
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
stop <- svMisc::stop_
warning <- svMisc::warning_

# For a better handling of formula masking, we could use checkmate::qtest()
# and annotate names like this:
# .xxxxYYY..name where .xxxx is the supposed origin of the variable with:
# - .data, .env0, .env1, .env2, .env3 ... for the function environment and its
#   parents and .call for the caller environment as fro the first formula env
# - YYY is the specification of the vairable as of qtest()
# - May be soe information on the form of the item (name, symbol, formula,
#   expression...)
# - .. is the separator, followed by the actual name of the variable
# Then formula_masking check each variable (e.g., are .data variables present in
# the data frame and of the right type? Are values or formulas provided for
# variables correct?) If not, it issues a meaningful error message. If
# everything is OK, it manages replacement of the variables to obtain a valid
# and working expression that is called in the .call environment.

# Process ... -> could be a list of formulas, formulas, or SE expressions
formula_masking <- function(..., .max.args = NULL, .must.be.named = FALSE,
  .make.names = FALSE, .no.se = FALSE,
  .no.se.msg = gettext("Standard evaluation is not allowed."),
  .envir = parent.frame(2L), .frame = parent.frame(),
  .verbose = .op$verbose) {

  .__top_call__. <- TRUE
  .make.names <- isTRUE(.make.names)

  # If we have a list of formulas, use it instead of ...
  if (is.list(..1) && is_formula(..1[[1]])) {
    if (...length() > 1L)
      stop("If you provide a list of formulas, you cannot provide more.")
    dots <- ..1
    first_item <- ..1[[1]]
    ldots <- length(dots)

  } else {# Use the regular ... arguments
    dots <- list(...) # Use substitute(...()) instead for not evaluating ...
    first_item <- ..1
    ldots <- ...length()
  }

  # Check number or arguments
  if (!is.null(.max.args)) {
    if (!is.numeric(.max.args) || length(.max.args) != 1 || is.na(.max.args) ||
        .max.args < 1)
      stop("{.arg .max.args} must be a single positive {.cls integer}, not {.max.args}.")
    # Check that the number of arguments is not too large
    if (ldots > .max.args)
      stop("You provided {ldots} arguments, but max allowed is {.max.args}.")
  }

  are_formulas <- is_formula(first_item)

  if (are_formulas) {# Everything is supposed to be formulas
    # The environment where to expand macros is .envir if it contains
    # .__top_call__. == TRUE, otherwise, no macro expansion is done.
    if (isTRUE(.envir$.__top_call__.)) {
      macro_env <- .envir
      all_vars <- names(macro_env)
      nvars <- length(all_vars)
    } else {# No macro expansion)
      nvars <- 0L
    }

    # The environment where to evaluate them is extracted from the first formula
    .envir <- f_env(first_item)

    # Extract expressions from the rhs of the formulas
    # Formulas are converted into expressions in extracting the right-hand side
    # (and if there are left-hand side, they become the name)
    f_to_expr <- function(x, env) {
      if (is_formula(x)) {
        if (nvars) {# Perform macro expansion now
          vars <- all_vars[fmatch(all.vars(x), all_vars,
            nomatch = 0L)] # Same as intersect(all.vars(x), all_vars) but faster
          macros <- list()
          for (var in vars) {
            macro <- macro_env[[var]]
            if (is_formula(macro)) {# TODO: also deal with lhs!!!
              macros[[var]] <- f_rhs(macro)
              #} else {
              #  macros[[var]] <- macro
            }
            if (length(macros)) {
              x <- do.call(substitute, list(x, macros)) # Do macro expansion
              if (isTRUE(.verbose))
                message("Macro expanded expression: ", expr_text(x))
            }
          }
        }
        f_rhs(x)

      } else {# Not a formula
        stop("You cannot mix standard evaluation and formulas.")
      }
    }
    if (ldots == 1) {
      args <- list(f_to_expr(first_item, env = parent.frame(3L)))
    } else {
      args <- lapply(dots, f_to_expr, env = parent.frame(4L))
    }

    # Check names and possibly get names also from the formulas
    names_args <- names(args) <- names(dots)
    if (is.null(names_args))
      names_args <- rep("", ldots)
    for (i in 1:ldots) {
      dots_i <- dots[[i]]
      lhs <- f_lhs(dots_i)
      # TODO: also a replacement mechanism here ?
      if (!is.null(lhs) && names_args[i] == "")
        names_args[i] <- eval_bare(lhs, env = .envir)

      # Do we need everything named?
      if (isTRUE(.must.be.named) && anyv(names_args, ""))
        stop("All inputs must be named.")

      # Functions like fsummarise() do not support empty names!
      # In this case, construct a label from rhs of the formula
      # Note: special case: if across() is used, do not name it!
      if (.make.names && names_args[i] == "" &&
          !anyv(expr_funs(dots_i), "across"))
        names_args[i] <- f_name(dots_i)
    }
    names(args) <- names_args

  } else {# Standard evaluation
    if (isTRUE(.no.se))
      stop(.no.se.msg, i = "Use formulas instead.")

    if (any(sapply(dots, is_formula)))
      stop("You cannot mix standard evaluation and formulas.")

    # Not needed, already done!?
    #args <- lapply(dots, force) # Force SE of the arguments
    args <- dots
  }

  # Return args and a little bit of info in a list
  list(dots = args, env = .envir, are_formulas = are_formulas)
}

# formula_select is the equivalent of tidy-select, but using formulas
# There is a fast subset of tidy-select handled by {collapse}, but for more
# complex cases, we rely on tidyselect::eval_select().
formula_select <- function(..., .fast.allowed.funs = NULL, .max.args = NULL,
  .must.be.named = FALSE, .make.names = FALSE, .no.se = FALSE,
  .no.se.msg = gettext("Standard evaluation is not allowed."),
  .envir = parent.frame(2L), .frame = parent.frame()) {

  .__top_call__. <- TRUE

  # Process the arguments with formula_masking()
  args <- formula_masking(..., .max.args = .max.args,
    .must.be.named = .must.be.named,.make.names = .make.names,
    .no.se = .no.se, .no.se.msg = .no.se.msg, .envir = .envir)

  # For SE, always OK to use fast-select
  if (!args$are_formulas || !length(.fast.allowed.funs)) {
    args$fastselect <- TRUE
  } else {# For formulas, check that all arguments are fast-selectable
    is_arg_ok <- function(arg, .fast.allowed.funs)
      !length(expr_funs(arg, exclude.names = .fast.allowed.funs))

    if (length(args$dots) == 1L) {
      args$fastselect <- is_arg_ok(args$dots[[1]],
        .fast.allowed.funs = .fast.allowed.funs)
    } else {
      args$fastselect <- all(sapply(args$dots, is_arg_ok,
        .fast.allowed.funs = .fast.allowed.funs))
    }
  }
  args
}

# Same as dplyr::is_grouped_df()
is_grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
