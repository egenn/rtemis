# checkpoint_earlystop.R
# ::rtemis::
# 2018 Efstathios D Gennatas egenn.github.io

#' Early stopping check
#'
#' Returns list with relative variance over n.steps, absolute.threshold, last value,
#' and logical "stop", if conditions are met and training should stop.
#' The final stop decision is:
#' \code{check.thresh | (check.rthresh & check.rvar)} if \code{combine.relative.thresholds = "AND"}
#' or
#' \code{check.thresh | (check.rthresh | check.rvar)} if \code{combine.relative.thresholds = "OR"}
#'
#' @param x Float, vector: Input - this would normally be the loss at each iteration
#' @param absolute.threshold Float: If set and the last value of \code{x} is less than or equal to this
#' (if \code{minimize = TRUE}) or greater than or equal to this (if \code{minimize = FALSE}), then return
#' \code{stop = TRUE}. See output under Value. Default = NA
#' @param relative.threshold Float: If set, checks if the relative change from the first to last value of \code{x}
#' exceeds this number. i.e. if set to .9 and \code{minimize = TRUE}, if there is a 90\% drop from \code{x[1]} to
#' \code{x[length(x)]}, then the function returns \code{stop = TRUE}. If \code{minimize = FALSE}, then checks if there
#' is a 90\% increase, accordingly.
#' @param minimize Logical: See \code{absolute.threshold}. Default = TRUE
#' @param relativeVariance.threshold Float: If relative variance over last \code{n.steps} is less than or equal to this,
#' return \code{stop = TRUE}. See output under Value
#' @param n.steps Integer; > 1: Calculate relative variance over this many last values of \code{x}
#' @param combine.relative.thresholds Character: "AND" or "OR": How to combine the criteria \code{relative.threshold} and
#' \code{relativeVariance.threshold}. Default = "AND", which means both must be TRUE to stop. The scenario is you might
#' want to check relastiveVariance threshold only after a certain amount of learning has taken place, which you
#' can't predict with \code{min.steps} but would rather quantify with \code{relative.threshold}.
#' @param min.steps Integer: Do not calculate relativeVariance unless \code{x} is at least this length
#' @param na.response Character: "stop" or "continue": what should happen if the last value of \code{x} is \code{NA}
#' @param verbose Logical: If TRUE, print messages to output
#' @return List with the following items:
#' \item{last.value}{Float: Last value of \code{x}}
#' \item{relativeVariance}{Float: relative variance of last \code{n.steps}}
#' \item{check.thresh}{Logical: TRUE, if absolute threshold was reached}
#' \item{check.rvar}{Logical: TRUE, if relative variance threshold was reached}
#' \item{stop}{Logical: TRUE, if either criterion was met - absolute threshold or relativeVariance.threshold}
#' @author Efstathios D. Gennatas
#' @export

checkpoint_earlystop <- function(x,
                      absolute.threshold = NA,
                      relative.threshold = NA,
                      minimize = TRUE,
                      relativeVariance.threshold = NA,
                      n.steps = 10,
                      combine.relative.thresholds = "AND",
                      min.steps = 50,
                      na.response = c("stop", "continue"),
                      verbose = TRUE) {

  na.response <- match.arg(na.response)
  last.value <- x[length(x)]
  if (is.na(last.value) & na.response == "stop") {
    return(list(absolute.threshold = absolute.threshold,
                minimize = minimize,
                last.value = last.value,
                check.thresh = TRUE,
                relative.change = NA,
                relative.threshold = relative.threshold,
                check.rthresh = NA,
                relativeVariance.threshold = relativeVariance.threshold,
                relativeVariance = NA,
                check.rvar = NA,
                stop = TRUE,
                restart = TRUE))
  }

  # Check absolute threshold ====
  if (!is.na(absolute.threshold)) {

    if (is.na(last.value)) {
      warning("Last value was NA; this may mean trouble")
      check.thresh <- FALSE
    } else {
      check.thresh <- if (minimize) {
        ifelse(last.value <= absolute.threshold, TRUE, FALSE)
      } else {
        ifelse(last.value >= absolute.threshold, TRUE, FALSE)
      }
    }

  } else {
    check.thresh <- FALSE
  }


  # Check relative threshold ====
  if (!is.na(relative.threshold)) {
    relative.threshold <- if (minimize)  -(abs(relative.threshold)) else abs(relative.threshold)
    first.value <- x[1]
    relative.change <- (last.value - first.value) / first.value
    if (is.na(relative.change)) {
      warning("Relative change was NA; setting to 0")
      relative.change <- 0
    }
    if (minimize) {
      if (relative.change >= 0) check.rthresh <- FALSE else check.rthresh <- relative.change <= relative.threshold
    } else {
      if (relative.change <= 0) check.rthresh <- FALSE else check.rthresh <- relative.change >= relative.threshold
    }
  } else {
    relative.change <- NA
    check.rthresh <- FALSE
  }

  # Check relative variance ====
  min.steps <- max(n.steps, min.steps)
  if (!is.na(relativeVariance.threshold) & length(x) >= min.steps) {
    last.n.steps <- rev(x)[seq(n.steps)]
    relativeVariance <- var(last.n.steps, na.rm = TRUE)/mean(last.n.steps)
    if (is.na(relativeVariance)) {
      warning("Relative variance was NA; setting to 0")
      relativeVariance <- 0
    }
    check.rvar <- relativeVariance <= relativeVariance.threshold
  } else {
    relativeVariance <- NA
    check.rvar <- FALSE
  }

  # Out ====
  if (!is.na(relative.threshold)) {
    if (combine.relative.thresholds == "AND") {
      .stop <- check.thresh | (check.rthresh & check.rvar)
    } else {
      .stop <- check.thresh | (check.rthresh | check.rvar)
    }
  } else {
    .stop <- check.thresh | check.rvar
  }

  if (verbose) {
    # msg0("Last value = ", ddSci(last.value), "(thresh = ", absolute.threshold, ") | ",
    #      "Pct change = ", ddSci(relative.change * 100), "(thresh = ", relative.threshold * 100, ") | ",
    #      "Relative variance over last ", n.steps, " steps = ", ddSci(relativeVariance),
    #      "(thresh = ", relativeVariance.threshold, ")")

    # msg0("Last: ", ddSci(last.value), " (", absolute.threshold, ") | ",
    #      "Pct: ", ddSci(relative.change * 100), " (", relative.threshold * 100, ") | ",
    #      "Rvar", n.steps, ": ", ddSci(relativeVariance),
    #      " (", relativeVariance.threshold, ")")
    # msg("--")
    # .msg <- list(`Last value` = paste0(ddSci(last.value), " (", absolute.threshold, ")"),
    #              `Relative change` = paste0(ddSci(relative.change * 100), " (", relative.threshold * 100, ")"),
    #              `Relative variance` = paste0(ddSci(relativeVariance), " (", relativeVariance.threshold, ")"),
    #              Minimize = minimize,
    #              Stop = .stop)
    # printls(.msg)
    msg0("Last: ", ddSci(last.value), "; RelDelta: ", ddSci(relative.change * 100),
         "; RelVar: ", ddSci(relativeVariance), "; Min:", minimize, "; Stop: ", .stop)
    #
    # msg0("Last: ", ddSci(last.value), " (", absolute.threshold, ")\n",
    #      "Pct: ", ddSci(relative.change * 100), " (", relative.threshold * 100, ")\n",
    #      "Rvar", n.steps, ": ", ddSci(relativeVariance),
    #      " (", relativeVariance.threshold, ")")
  }

  list(absolute.threshold = absolute.threshold,
       minimize = minimize,
       last.value = last.value,
       check.thresh = check.thresh,
       relative.change = relative.change,
       relative.threshold = relative.threshold,
       check.rthresh = check.rthresh,
       relativeVariance.threshold = relativeVariance.threshold,
       relativeVariance = relativeVariance,
       check.rvar = check.rvar,
       stop = .stop,
       restart = FALSE)

} # rtemis::checkpoint_earlystop
