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
#' @param normalize Logical: If TRUE: \link{drange} \code{x} to 0:100
#' @param ... Additional arguments to pass to \link{mplot3.xy}
#' @author Efstathios D. Gennatas
#' @export

mplot3.surv <- function(x,
                        lty = 1,
                        lwd = 2,
                        alpha = 1,
                        col = NULL,
                        normalize = TRUE,
                        cex = 1.2,
                        xlab = "Time",
                        ylab = "Survival",
                        main = "Kaplan-Meier estimate with 95% CI",
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
  for (i in 1:length(x)) {
    if (class(x[[i]]) != "Surv") {
      x[[i]] <- survival::Surv(x[[i]], rep(1, length(x[[i]])))
    }
  }

  # [ Kaplan-Meier Estimate ] ====
  .survfit <- lapply(x, function(x) survival::survfit(x ~ 1))
  lapply(1:length(x), function(i) survival::survfit(x[[i]] ~ 1))

  # # [ Normalize ] ====
  # if (normalize) {
  #   x.sorted <- lapply(x.sorted, function(x) drange(x, 0, 100))
  # }

  # [ LIMITS ] ====
  xlim <- c(0, max(.survfit[[1]]$time))

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  cols <- if (is.null(col)) ucsfCol else col

  # with pointwise errors:
  mplot3.xy(x = .survfit[[1]]$time,
            y = list(.survfit[[1]]$surv, Upper = .survfit[[1]]$upper, Lower = .survfit[[1]]$lower),
            ylim = c(0, 1), xlim = xlim,
            type = 's', lwd = 2, lty = c(1, 3, 3),
            marker.col = c(cols[[1]], colorAdjust(cols[[1]], .66), colorAdjust(cols[[1]], .66)),
            line.alpha = 1,
            main = main,
            xlab = "Time", ylab = "Survival",
            group.legend = FALSE, zerolines = FALSE, par.reset = FALSE, ...)
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      mplot3.xy(x = .survfit[[i]]$time,
                y = list(.survfit[[i]]$surv, Upper = .survfit[[i]]$upper, Lower = .survfit[[i]]$lower),
                ylim = c(0, 1), xlim = xlim,
                type = 's', lwd = 2, lty = c(1, 3, 3),
                marker.col = c(cols[[i]], colorAdjust(cols[[i]], .66), colorAdjust(cols[[i]], .66)),
                line.alpha = 1,
                main = "",
                xlab = "Time", ylab = "Survival Function",
                group.legend = FALSE, zerolines = FALSE, par.reset = FALSE,
                axes.visible = FALSE, new = TRUE, ...)
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


