#.onload <- function(lib, pkg) {# nocov start
#  # Nothing to do for now
#}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Need this for R CMD check to pass
. <- NULL

# We use our own stop_() and warning_(), but renamed
stop <- svBase::stop_
warning <- svBase::warning_

# Same as dplyr::is_grouped_df()
is_grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
