# Helper function for attribution
.src_sciviews <- function(src,
  comment = "A SciViews function, see respective help pages.") {
  attr(comment, "src") <- src
  comment
}

# This is the collapse replace__na() to avoid clash with tidyr::replace_na()
repl_na <- collapse::replace_na
roworder <- collapse::roworder
