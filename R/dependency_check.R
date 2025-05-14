# dependency_check.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org
# replaced depCheck

#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbose Logical. If TRUE, print messages to consolde.
#' Note: An error will always printed if dependencies are missing.
#' Setting this to FALSE stops it from printing
#' "Dependencies check passed".
#'
#' @author E.D. Gennatas
#' @export

dependency_check <- function(..., verbose = FALSE) {
  ns <- as.list(c(...))
  err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))
  if (any(err)) {
    stop(
      "Please install the following ",
      ngettext(sum(err), "dependency", "dependencies"),
      ":\n",
      pastels(ns[err], bullet = "    -")
    )
  } else {
    if (verbose) msg2("Dependency check passed", as.message = FALSE)
  }
} # rtemis::dependency_check
