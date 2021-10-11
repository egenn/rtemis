# survError.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Survival Analysis Metrics
#'
#' @param true Vector, numeric: True survival times
#' @param estimated Vector, numeric: Estimated survival times
#' @author E.D. Gennatas
#' @export

survError <- function(true, estimated) {

  # Dependencies ====
  if (!depCheck("survival", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  if (!survival::is.Surv(true)) stop("true must be Survival object")
  out <- survival::survConcordance(true ~ estimated)
  class(out) <- c("survError", "survConcordance")
  out

} # rtemis::survError


#' Print \link{survError}
#'
#' @param x Object of type \link{survError}
#' @param decimal.place Integer: Number of decimal places to print. Default = 4
#' @param ... Not used
#' @author E.D. Gennatas
#' @export

print.survError <- function(x, decimal.places = 4, ...) {

  printdf1(data.frame(N = x$n,
                      N_Corcordant = x$stats[1],
                      N_Discordant = x$stats[2],
                      Tied_risk = x$stats[3],
                      Tied_time = x$stats[4],
                      Std_error = ddSci(x$std.err, decimal.places),
                      row.names = 1))
  cat("   Concordance : ", rtHighlight$bold(x$concordance), "\n")

} # rtemis::print.survError
