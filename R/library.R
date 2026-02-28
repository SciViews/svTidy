#' Load \{dplyr\} or \{tidyr\} without functions ending with an underscore
#'
#' @description
#' This is the same as `library(dplyr)` or `library(tidyr)`, but excluding all
#' functions that end with an underscore and that may conflict with \{svTidy\}
#' corresponding ones. Note that these functions are deprecated in recent
#' versions of \{dplyr\} and \{tidyr\}, and they are even defunct in version
#' 1.2.0 or greater of \{dplyr\}.
#'
#' @param ... Further arguments passed to [base::library()]
#' @param exclude A list of functions to exclude. Leave this argument missing to
#'   exclude all underscore functions from the package by default.
#'
#' @returns The list of attached packages invisibly, or `TRUE`/`FALSE` to
#'   indicate success if `logical.return = TRUE` is indicated.
#' @export
#' @seealso [base::library()]
#'
#' @examples
#' library_dplyr()
#' library_tidyr()
#' search()
#' # However, the functions with underscore are not directly accessible
#' # (unless you make library(svTidy) of course)
#' get0('mutate_')
library_dplyr <- function(..., exclude) {
  if (missing(exclude))
    exclude <- c("add_count_", "add_tally_", "arrange_", "count_", "distinct_",
      "do_", "filter_", "funs_", "group_by_", "group_indices_", "mutate_",
      "mutate_each_", "rename_", "rename_vars_", "select_", "select_vars_",
      "slice_", "summarise_", "summarise_each_", "summarize_",
      "summarize_each_", "tally_", "transmute_")
  suppressWarnings(.library('dplyr', exclude = exclude, ...))
}

#' @rdname library_dplyr
#' @export
library_tidyr <- function(..., exclude) {
  if (missing(exclude))
    exclude <- c("complete_", "crossing_", "drop_na_", "expand_", "extract_",
      "fill_", "gather_", "nest_", "nesting_", "separate_", "separate_rows_",
      "spread_", "unite_", "unnest_")
  suppressWarnings(.library('tidyr', exclude = exclude, ...))
}

.library <- library # To avoid a warning in R CMD check
