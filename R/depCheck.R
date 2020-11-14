# depCheck.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbose Logical. If TRUE, print messages to consolde. Note: messages are always printed
#' if dependencies are missing. Setting this to FALSE stops it from printing
#' "Dependencies check passed". Default = TRUE
#' @return TRUE if dependencies can be loaded, otherwise FALSE
#' @author Efstathios D. Gennatas
#' @export

depCheck <- function(..., verbose = TRUE) {

  ns <- as.list(c(...))
  err <- !sapply(ns, function(i) requireNamespace(i, quietly = TRUE))

  if (any(err)) {
    msg("Please install the following missing dependencies:", as.message = TRUE)
    message(paste("    *", ns[err], collapse = "\n"))
    return(FALSE)
    # stop("Please install dependencies and try again") # Error appears in Traceback; not pretty in RStudio
  } else {
    if (verbose) msg("Dependencies check passed", as.message = FALSE)
    return(TRUE)
  }

} # rtemis::depCheck
