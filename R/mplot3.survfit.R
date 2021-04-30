# mplot3.survfit.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org
# todo: autoplace group.name based on min survival
# todo: table height by nstrata

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
#' # Get the lung dataset
#' data(cancer, package = "survival")
#' sf1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = lung)
#' mplot3.survfit(sf1)
#' sf2 <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
#' mplot3.survfit(sf2)
#' # with N at risk table
#' mplot3.survfit(sf2, nrisk.table = TRUE)
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
                           table.height = .25,
                           table.pad = 0,
                           table.font = 2,
                           time.at = NULL,
                           time.by = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           # normalize.time = FALSE,
                           xlab = "Time",
                           ylab = "Survival",
                           main = NULL,
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
                           group.padj = NULL,
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

  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # [ Limits ] ====
  # if (normalize.time) xl <- lapply(xl, drange)
  if (is.null(xlim)) xlim <- c(0, max(x$time, na.rm = TRUE))
  if (is.null(ylim)) ylim <- c(0, 1)

  # x.axis.at ====
  if (!is.null(time.by)) {
    time.at <- seq(xlim[1], xlim[2], by = time.by)
    if (time.at[length(time.at)] * 1.04 < xlim[2]) {
      time.at <- c(time.at, time.at[length(time.at)] + time.by)
    }
  }
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
    mar <- if (nrisk.table) c(1, 3, 2, 1) else c(2.5, 3, 2, 1)
  }

  # if (is.null(xlab)) {
  #   xlab <- if (normalize.time) "Normalized Time" else "Time"
  # }

  # todo adjust table.height by nstrata
  if (nrisk.table) layout(matrix(c(1, 2), 2),
                          heights = c(plot.height, .9 + table.height * nstrata))
  mplot3.xy(x = split(x$time, .group),
            y = split(x$surv, .group),
            xlim = xlim,
            ylim = ylim,
            type = 's',
            x.axis.at = time.at,
            lwd = lwd,
            lty = lty,
            theme = theme,
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

  # [ Group Legend ] ====
  # todo: extract group.title from strata
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    group.names <- c(group.title, names(x$strata))
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(x) > 1, TRUE, FALSE)

  if (is.null(group.padj)) {
    group.padj <- if (min(x$surv) < .5) 1 else 19
  }
  if (group.legend) {
    mtext(group.names,
          col = c(theme$fg, unlist(col))[seq(nstrata + 1)],
          side = group.side,
          adj = group.adj,
          at = group.at,
          cex = theme$cex,
          padj = seq(group.padj, group.padj + 1.5 * nstrata, 1.5),
          family = theme$font.family)
  }

  # nrisk table ====
  if (nrisk.table) {
    sfs <- summary(x, times = time.at)
    if (nstrata > 1) {
      nrisk <- split(sfs$n.risk, sfs$strata)
      nperstratum <- table(sfs$strata)
      strata <- names(nperstratum)
      nriskmat <- matrix(0, nstrata, length(time.at))
      for (i in seq_len(nstrata)) {
        nriskmat[i, seq_len(nperstratum[i])] <- nrisk[[i]]
      }
      rownames(nriskmat) <- strata
    } else {
      nriskmat <- matrix(sfs$n.risk, 1)
    }
    colnames(nriskmat) <- time.at
    plot(NULL, NULL,
         xlim = xlim,
         # ylim = c(1, nstrata + table.pad * nstrata),
         ylim = c(0, 0),
         xlab = "", ylab = "",
         ann = FALSE,
         axes = FALSE)
    # mtext("Number at risk",
    #       side = 3,
    #       col = theme$fg, adj = 0, line = 0,
    #       cex = theme$cex,
    #       family = theme$font.family)
    mtext("Number\nat risk   ",
          side = 2,
          col = theme$fg, adj = NA, line = 1,
          cex = theme$cex,
          family = theme$font.family)
    # axis(2, at = seq_len(nstrata), labels = strata)
    # abline(h = c(seq(nstrata - 1) + .5), col = adjustcolor(theme$fg, .5))
    for (i in seq_len(nstrata)) {
      # text(x = time.at, y = i,
      #      labels = as.character(nriskmat[i, ]),
      #      # col = theme$fg,
      #      col = palette[[i]],
      #      font = table.font,
      #      xpd = TRUE,
      #      family = theme$font.family)
      mtext(text = as.character(nriskmat[i, ]),
            side = 3,
            line = -i,
            at = time.at,
            col = palette[[i]],
            font = table.font,
            xpd = TRUE,
            family = theme$font.family)
    }
    # abline(h = c(seq(nstrata - 1) + .5), col = adjustcolor(theme$bg, .5))

  }

} # rtemis::mplot3.surv
