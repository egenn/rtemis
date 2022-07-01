# timeProc.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Time a process
#'
#' \code{timeProc} measures how long it takes for a process to run
#'
#' @param ... Command to be timed. (Will be converted using \code{as.expression})
#' @param verbose Logical: If TRUE, print messages to console
#' @author E.D. Gennatas
#' @export

timeProc <- function(..., verbose = TRUE) {

  expr <- as.expression(...)
  start <- proc.time()
  eval(expr)
  total <- proc.time() - start
  if (verbose) msg("Completed in", as.numeric(total[3]), "seconds")
  return(total)

} # rtemis::timeProc
