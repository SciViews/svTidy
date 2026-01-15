#.onload <- function(lib, pkg) {# nocov start
#  # Nothing to do for now
#}

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Need this for R CMD check to pass
. <- NULL

# We use our own stop_() and warning_(), but renamed
stop <- svBase::stop_
warning <- svBase::warning_ # nocov end
