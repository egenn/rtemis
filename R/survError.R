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
  out <- survival::concordancefit(true, estimated)
  names(out) <- labelify(names(out), capitalize.strings = "n")
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

  # printdf1(data.frame(N = x$n,
  #                     N_Corcordant = x$stats[1] |> null2na(),
  #                     N_Discordant = x$stats[2] |> null2na(),
  #                     Tied_risk = x$stats[3] |> null2na(),
  #                     Tied_time = x$stats[4] |> null2na(),
  #                     Std_error = ddSci(x$std.err |> null2na(), decimal.places),
  #                     row.names = 1))
  cat("              N =", ddSci(x$N), "\n")
  cat("            var =", ddSci(x$Var), "\n")
  cat("           cvar =", ddSci(x$Cvar), "\n")
  cat("   Concordance : ", rtHighlight$bold(x$Concordance), "\n")

} # rtemis::print.survError

# print.survError.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Print \code{survError} object
#'
#' \code{print} \code{survError} object
#'
#' @method print survError
#' @param x \code{survError} object
#' @author E.D. Gennatas

# print.survError <- function(x, ...) {
#
#   obj <- x
#   cat("    Concordance =", ddSci(obj$concordance), "\n")
#   # cat("             SE =", ddSci(obj$std.err), "\n")
#   cat("              N =", ddSci(obj$n), "\n")
#   cat("            var =", ddSci(obj$var), "\n")
#   cat("           cvar =", ddSci(obj$cvar), "\n")
#   # cat("   N concordant =", ddSci(obj$stats[[1]]), "\n")
#   # cat("   N discordant =", ddSci(obj$stats[[2]]), "\n")
#   # cat("      Tied Risk =", ddSci(obj$stats[[3]]), "\n")
#   # cat("      Tied Time =", ddSci(obj$stats[[4]]), "\n")
#   # cat("      Std (c-d) =", ddSci(obj$stats[[5]]), "\n")
#   cat("\n")
#
# } # rtemis::print.survError
