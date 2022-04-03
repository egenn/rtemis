# depCheck.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbose Logical. If TRUE, print messages to consolde. Note: messages are always printed
#' if dependencies are missing. Setting this to FALSE stops it from printing
#' "Dependencies check passed". Default = TRUE
#' @return TRUE if dependencies can be loaded, otherwise FALSE
#' @author E.D. Gennatas
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


dependency_check <- function(..., verbose = FALSE) {
    
    ns <- as.list(c(...))
    err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))

    if (any(err)) {
        stop(
            "Please install the following ", ngettext(sum(err), "dependency", "dependencies"), ":\n",
            pastels(ns[err], bullet = "    -")
        )
    } else {
        if (verbose) msg("Dependency check passed", as.message = FALSE)
    }
    
} # rtemis::depCheck2
