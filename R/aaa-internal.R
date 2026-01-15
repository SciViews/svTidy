# Helper function for attribution
.src_sciviews <- function(src, # nocov start
  comment = "A SciViews function, see respective help pages.") {
  attr(comment, "src") <- src
  comment
} # nocov end

# This is the collapse replace__na() to avoid clash with tidyr::replace_na()
repl_na <- collapse::replace_na

# Same as dplyr::is_grouped_df()
is_grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
