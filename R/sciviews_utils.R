#' List SciViews Functions
#'
#' @description Give a comprehensive list of the SciViews functions (ending with
#'   `_` and similar to their dplyr or tidyr equivalents). There are two major
#'   differences from the original dplyr and tidyr functions: (1) the data-dot
#'   mechanism replaces `.` as `.data=` in case it is not provided, and (2) they
#'   use a formula-masking mechanism instead of data-masking or tidy-select. In
#'   this case, you can either use standard evaluation, specifying `df$var` like
#'   for many base R function, of a formula, like `~var`.
#'
#' @export
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
