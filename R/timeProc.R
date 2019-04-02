# timeProc.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Time a process
#'
#' \code{timeProc} measures how long it takes for a process to run
#'
#' @param ... Command to be timed. (Will be converted using \code{as.expression})
#' @author Efstathios D. Gennatas
#' @export

timeProc <- function(...) {

  expr <- as.expression(...)
  start <- proc.time()
  eval(expr)
  total <- proc.time() - start
  return(total)

} # rtemis::timeProc
