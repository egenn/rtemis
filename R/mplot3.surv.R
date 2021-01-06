# mplot3.surv.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.lambdamd.org

#' \code{mplot3}: Survival Plots
#'
#' Plots survival step functions using \link{mplot3.xy}
#'
#' @inheritParams mplot3.xy
#' @param x Survival object / list of Survival objects created using \code{survival::Surv}
#' @param lty Integer: Line type. Default = 1. See \code{par("lty")}
#' @param lwd Float: Line width. Default = 2
#' @param alpha Float: Alpha for lines. Default = 1
# @param normalize Logical: If TRUE: \link{drange} \code{x} to 0:100
#' @param ... Additional arguments to pass to \link{mplot3.xy}
#' @author Efstathios D. Gennatas
#' @export

mplot3.surv <- function(x,
                        lty = 1,
                        lwd = 2,
                        alpha = 1,
                        col = NULL,
                        # normalize = TRUE,
                        cex = 1.2,
                        xlab = "Time",
                        ylab = "Survival",
                        main = "Kaplan-Meier estimate with 95% CI",
                        plot.error = TRUE,
                        error.lty = 2,
                        error.alpha = .5,
                        group.legend = NULL,
                        group.title = "",
                        group.names = NULL,
                        group.side = 3,
                        group.adj = .98,
                        group.padj = 2,
                        group.at = NA,
                        par.reset = TRUE, ...) {

  # [ DATA ] ====
  if (class(x)[1] != "list") x <- list(x)
  # x <- lapply(1:length(x), function(i) as.numeric(as.matrix(x[[i]])[, 1]))
  # if (class(x)[[1]] != "Surv") stop("At least first object must be of type Survival")
  for (i in seq(x)) {
    if (class(x[[i]]) != "Surv") {
      x[[i]] <- survival::Surv(x[[i]], rep(1, length(x[[i]])))
    }
  }

  # [ Kaplan-Meier Estimate ] ====
  .survfit <- lapply(x, function(i) survival::survfit(i ~ 1))

  # # [ Normalize ] ====
  # if (normalize) {
  #   x.sorted <- lapply(x.sorted, function(x) drange(x, 0, 100))
  # }

  # [ LIMITS ] ====
  xl <- lapply(.survfit, function(i) i$time)
  xlim <- range(unlist(xl))
  yl <- lapply(.survfit, function(i) i$surv)

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  cols <- if (is.null(col)) ucsfCol else col


  mplot3.xy(x = xl,
            y = yl,
            ylim = c(0, 1),
            xlim = xlim,
            type = 's', lwd = 2, lty = lty,
            marker.col = col,
            line.alpha = 1,
            main = main,
            xlab = "Time", ylab = "Survival",
            group.legend = FALSE, zerolines = FALSE, par.reset = FALSE, ...)

  # pointwise errors
  if (plot.error) {
    for (i in seq(xl)) {
      lines(.survfit[[i]]$time, .survfit[[i]]$upper,
            lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
      lines(.survfit[[i]]$time, .survfit[[i]]$lower,
            lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
    }
  }

  # [ GROUP LEGEND ] ====
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    if (!is.null(names(x))) {
      group.names <- c(group.title, names(x))
    } else {
      group.names <- c(group.title, paste(" ", toupper(letters[1:length(x)])) )
    }
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(x) > 1, TRUE, FALSE)

  if (group.legend) {
    mtext(group.names,
          col = c("black", unlist(cols))[1:(length(x) + 1)], # change black depending on theme
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = cex,
          padj = seq(group.padj, group.padj + 1.5 * length(x), 1.5))
  }

} # rtemis::mplot3.surv
