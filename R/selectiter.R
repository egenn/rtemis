# selectiter.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Select N of learning iterations based on loss
#'
#' @param loss.valid Float, vector: Validation loss. Can be NULL
#' @param loss.train Float, vector: Training loss
#' @param smooth Logical: If TRUE, smooth loss before finding minimum.
#' @param plot Logical: If TRUE, plot loss curve.
#' @param verbose Logical: If TRUE, print messages to console.
#'
#' @author E.D. Gennatas
#' @export

selectiter <- function(
  loss.valid,
  loss.train, # for plotting
  smooth = TRUE,
  plot = FALSE,
  verbose = FALSE
) {
  loss <- if (is.null(loss.valid)) loss.train else loss.valid
  nsteps <- length(loss)
  loss.smooth <- if (smooth) {
    supsmu(seq(nsteps), loss)$y
  } else {
    NULL
  }

  if (plot) {
    dat <- list(
      Training = loss.train,
      Validation = loss.valid,
      `Smoothed Validation` = loss.smooth
    )
    if (is.null(loss.valid)) names(dat)[3] <- "Smoothed Training"
    dat <- Filter(Negate(is.null), dat)
    mplot3_xy(
      seq(nsteps),
      dat,
      type = "l",
      group.adj = .95,
      line.col = c(ucsfCol$teal, ucsfCol$orange, ucsfCol$red),
      vline = c(which.min(loss), which.min(loss.smooth)),
      vline.col = c(ucsfCol$orange, ucsfCol$red),
      xlab = "N iterations",
      ylab = "Loss"
    )
  }

  list(
    best.nsteps = which.min(if (smooth) loss.smooth else loss),
    loss.smooth = loss.smooth
  )
} # rtemis::selectiter
