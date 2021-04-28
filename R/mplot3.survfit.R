# mplot3.surv.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' \code{mplot3}: Survival Plots
#'
#' Plots survival step functions using \link{mplot3.xy}
#'
#' @inheritParams mplot3.xy
#' @param x survfit object (output of \code{survival::survfit}
#' @param lty Integer: Line type. Default = 1. See \code{par("lty")}
#' @param lwd Float: Line width. Default = 2
#' @param alpha Float: Alpha for lines. Default = 1
#' @param ... Additional arguments to pass to \link{mplot3.xy}
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' lung <- survival::lung
#' sf <- survival::survfit(Surv(time, status) ~ sex, data = lung)
#' mplot3.survfit(sf)
#' # with N at risk table
#' mplot3.survfit(sf, nrisk.table = TRUE)
#' }

mplot3.survfit <- function(x,
                           lty = 1,
                           lwd = 1.5,
                           alpha = 1,
                           col = NULL,
                           mark.censored = FALSE,
                           censor.mark = TRUE,
                           censor.col = NULL,
                           censor.alpha = .5,
                           censor.pch = "I",
                           censor.cex = .8,
                           nrisk.table = FALSE,
                           plot.height = 5,
                           table.height = 1.5,
                           table.pad = 0,
                           table.font = 2,
                           time.at = NULL,
                           xlim = NULL,
                           # normalize.time = FALSE,
                           cex = 1.2,
                           xlab = "Time",
                           ylab = "Survival",
                           main = "Kaplan-Meier curve",
                           theme = getOption("rt.theme", "lightgrid"),
                           palette = getOption("rt.palette", "rtCol1"),
                           plot.error = FALSE,
                           error.lty = 2,
                           error.alpha = .5,
                           group.legend = NULL,
                           group.title = "",
                           group.names = NULL,
                           group.side = 3,
                           group.adj = .98,
                           group.padj = 2,
                           group.at = NA,
                           mar = NULL,
                           par.reset = TRUE, ...) {

  # [ Data ] ====
  if (class(x) != "survfit") stop("Input must be of class 'survfit'")
  nstrata <- if (is.null(x$strata)) 1 else length(x$strata)
  if (nstrata > 1) {
    .group <- unlist(sapply(seq_len(nstrata), function(i) rep(i, x$strata[i])))
  } else {
    .group <- rep(1, length(x$time))
  }

  # [ Arguments ] ====
  if (mark.censored) censor.mark <- FALSE

  # [ Theme ] ====
  if (is.null(col)) {
    if (is.character(palette)) palette <- rtPalette(palette)
    col <- palette
  }

  .theme <- theme
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # [ Limits ] ====
  # xl <- lapply(x, function(i) i$time)
  # if (normalize.time) xl <- lapply(xl, drange)
  if (is.null(xlim)) xlim <- c(0, max(x$time, na.rm = TRUE))
  if (is.null(ylim)) ylim <- c(0, 1)
  # yl <- lapply(x, function(i) i$surv)

  # x.axis.at ====
  if (nrisk.table && is.null(time.at)) time.at <- "auto"
  if (!is.null(time.at) && time.at[1] == "auto") {
    time.at <- floor(seq(xlim[1], xlim[2], length.out = 6))
    time.at <- round(time.at, digits = -(nchar(as.character(xlim[2])) - 2))
  }

  # [ Plot ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (is.null(mar)) {
    mar <- c(2, 3, 2, 1)
  }

  # if (is.null(xlab)) {
  #   xlab <- if (normalize.time) "Normalized Time" else "Time"
  # }

  if (nrisk.table) layout(matrix(c(1, 2), 2),
                          heights = c(plot.height, table.height))
  mplot3.xy(x = split(x$time, .group),
            y = split(x$surv, .group),
            xlim = xlim,
            ylim = ylim,
            type = 's',
            x.axis.at = time.at,
            lwd = lwd, lty = lty,
            theme = .theme,
            palette = palette,
            marker.alpha = 0,
            marker.col = col,
            line.alpha = 1,
            main = main,
            xlab = xlab, ylab = ylab,
            group.legend = FALSE,
            zerolines = FALSE,
            mar = mar,
            par.reset = FALSE)

  lines(x, mark.time = mark.censored,
        col = unlist(palette),
        lwd = lwd,
        conf.int = FALSE)


  # Censoring markers ====

  if (censor.mark) {
    if (is.null(censor.col)) censor.col <- adjustcolor(theme$fg, censor.alpha)
    .index <- x$n.censor == 1
    points(x$time[.index], x$surv[.index],
           pch = censor.pch,
           col = censor.col,
           cex = censor.cex)
  }

  # pointwise errors ====
  if (plot.error) {
    lines(x$time, x$upper,
          lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
    lines(x$time, x$lower,
          lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
  }

  # [ GROUP LEGEND ] ====
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    group.names <- c(group.title, names(x$strata))
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(x) > 1, TRUE, FALSE)

  if (group.legend) {
    nstrata <- length(x$strat)
    mtext(group.names,
          col = c(theme$fg, unlist(col))[seq(nstrata + 1)],
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = cex,
          padj = seq(group.padj, group.padj + 1.5 * nstrata, 1.5),
          family = theme$font.family)
  }

  # nrisk table ====
  if (nrisk.table) {
    sfs <- summary(x, times = time.at)
    nrisk <- split(sfs$n.risk, sfs$strata)
    nperstratum <- table(sfs$strata)
    strata <- names(nperstratum)
    nriskmat <- matrix(0, nstrata, max(nperstratum))
    for (i in seq_len(nstrata)) {
      nriskmat[i, seq_len(nperstratum[i])] <- nrisk[[i]]
    }
    colnames(nriskmat) <- time.at
    rownames(nriskmat) <- strata
    plot(NULL, NULL, xlim = xlim,
         # ylim = seq_len(nstrata),
         ylim = c(1, nstrata + table.pad * nstrata),
         xlab = "", ylab = "",
         ann = FALSE,
         axes = FALSE)
    # axis(2, at = seq_len(nstrata), labels = strata)
    abline(h = c(seq(nstrata - 1) + .5), col = adjustcolor(theme$fg, .5))
    for (i in seq_len(nstrata)) {
      # text(x = 0, y = i, adj = 1.7, labels = strata[i], col = palette[[i]], xpd = TRUE)
      text(x = time.at, y = i,
           labels = as.character(nriskmat[i, ]),
           # col = theme$fg,
           col = palette[[i]],
           font = table.font,
           xpd = TRUE,
           family = theme$font.family)
    }
    # abline(h = c(seq(nstrata - 1) + .5), col = adjustcolor(theme$bg, .5))
    mtext("Number at risk", col = theme$fg, adj = 0, line = .8,
          cex = theme$cex,
          family = theme$font.family)
  }

} # rtemis::mplot3.surv
