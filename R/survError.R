# surv_error.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Survival Analysis Metrics
#'
#' @param true Vector, numeric: True survival times
#' @param estimated Vector, numeric: Estimated survival times
#'
#' @author E.D. Gennatas
#' @export

surv_error <- function(true, estimated) {
  # Dependencies ----
  dependency_check("survival")

  if (!survival::is.Surv(true)) stop("true must be Survival object")
  out <- survival::concordancefit(true, estimated)
  names(out) <- labelify(names(out), capitalize.strings = "n")
  class(out) <- c("surv_error", "survConcordance")
  out
} # rtemis::surv_error


#' Print [surv_error]
#'
#' @param x Object of type [surv_error]
#' @param decimal.places Integer: Number of decimal places to print. Default = 4
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

print.surv_error <- function(x, decimal.places = 4, ...) {
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
  cat("   Concordance : ", hilite(x$Concordance), "\n")
  invisible(x)
} # rtemis::print.surv_error

# print.surv_error.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Print `surv_error` object
#'
#' `print` `surv_error` object
#'
#' @method print surv_error
#' @param x `surv_error` object
#' @author E.D. Gennatas

# print.surv_error <- function(x, ...) {
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
# } # rtemis::print.surv_error
