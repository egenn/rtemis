# earlystop.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Early stopping
#'
#' Check loss vector for early stopping criteria:
#'     - either total percent decrease from starting error (e.g. if predictions started at expectation)
#'     - or minimum percent decrease (relative to the first value of the vector) over a window of last n steps
#'
#' If the first loss value was set to be the loss when yhat = mean(y) (e.g. in boosting), then
#' `total_decrease_pct_max` corresponds to R-squared and `window_decrease_pct_min` to percent R-squared
#' improvement over `window` last steps.
#'
#' @param x Numeric vector: loss at each iteration
#' @param window Integer: Number of steps to consider
#' @param window_decrease_pct_min Float: Stop if improvement is less than this percent over last `window` steps
#' @param total_decrease_pct_max Float: Stop if improvement from first to last step exceeds this percent. If defined, overrides `window_decrease_pct_min`
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @author E.D. Gennatas
#' @export

earlystop <- function(
  x,
  window = 10,
  window_decrease_pct_min = 0.01,
  total_decrease_pct_max = NULL,
  verbose = TRUE
) {
  nsteps <- length(x)

  if (nsteps <= window) {
    return(FALSE)
  } else {
    if (!is.null(total_decrease_pct_max)) {
      total_decrease_pct <- (x[1] - x[nsteps]) / x[1]
      if (total_decrease_pct >= total_decrease_pct_max) {
        verbose &&
          msg20(
            "Total decrease pct max threshold (",
            total_decrease_pct_max,
            ") reached in ",
            nsteps,
            " steps: ",
            total_decrease_pct
          )
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      # window decrease
      window_decrease <- (x[nsteps - window] - x[nsteps]) / x[1]
      if (window_decrease < window_decrease_pct_min) {
        if (verbose) {
          msg20(
            window,
            "-step decrease % threshold (",
            window_decrease_pct_min * 100,
            "%) reached at ",
            nsteps,
            " steps: "
          )
          cat("                                ")
          cat("[", nsteps - window, "] ", sep = "")
          cat(sprintf("%4.4f", x[nsteps - window]), " => ", sep = "")
          cat("[", nsteps, "] ", sep = "")
          cat(sprintf("%4.4f", x[nsteps]), ": ", sep = "")
          cat(hilite(sprintf("%4.4f", window_decrease * 100), "%\n", sep = ""))
        }
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
} # rtemis::earlystop
