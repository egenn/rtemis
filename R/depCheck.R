# depCheck.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbose Logical. Defaults to TRUE, disable to eliminate unwanted console output
#' @return TRUE if dependencies can be loaded, otherwise FALSE
#' @author Efstathios D. Gennatas
#' @export

depCheck <- function(..., verbose = TRUE) {

  ns <- as.list(c(...))
  err <- vector()

  for (i in 1:length(ns)) {
    if (requireNamespace(ns[[i]], quietly = TRUE)) {
      err <- c(err, 0)
    } else {
      err <- c(err, 1)
    }
  }

  if (sum(err) > 0) {
    msg("Dependencies missing:", as.message = TRUE)
    message(paste("    ", ns[as.logical(err)], collapse = "\n"))
    return(FALSE)
    # stop("Please install dependencies and try again") # Error appears in Traceback; not pretty in RStudio
  } else {
    if (verbose) msg("Dependencies check passed", as.message = FALSE)
    return(TRUE)
  }

} # rtemis::depCheck
