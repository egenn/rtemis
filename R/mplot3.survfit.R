# mplot3.survfit.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org
# todo: try oma for nrisk.table

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
                           draw.median = FALSE,
                           median.lty = 3,
                           median.col = theme$fg,
                           median.alpha = .5,
                           censor.mark = TRUE,
                           censor.col = NULL,
                           censor.alpha = .4,
                           censor.pch = "I",
                           censor.cex = .8,
                           nrisk.table = FALSE,
                           nrisk.pos = "below",
                           nrisk.spacing = .9,
                           table.font = 1,
                           time.at = NULL,
                           time.by = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           xlab = "Time",
                           ylab = "Survival",
                           main = "Kaplan-Meier Estimate",
                           theme = getOption("rt.theme", "lightgrid"),
                           palette = getOption("rt.palette", "rtCol1"),
                           # plot.error = FALSE,
                           error.lty = 2,
                           error.alpha = .5,
                           autonames = TRUE,
                           group.legend = NULL,
                           group.names = NULL,
                           group.title = NULL,
                           group.line = NULL,
                           group.side = NULL,
                           group.adj = .98,
                           group.at = NA,
                           mar = c(2.5, 3, 2, 1),
                           oma = NULL,
                           par.reset = TRUE, ...) {

  # [ Data ] ====
  if (class(x)[1] != "survfit") stop("Input must be of class 'survfit'")
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

  # todo: autoestimate time.by instead of length.out which can give unequal intervals
  # if (!is.null(time.at) && time.at[1] == "auto") {
  #   time.at <- floor(seq(xlim[1], xlim[2], length.out = 6))
  #   time.at <- round(time.at, digits = -(nchar(as.character(ceiling(xlim[2]))) - 2))
  #   if (time.at[length(time.at)] * 1.04 < xlim[2]) {
  #     time.at <- c(time.at, time.at[length(time.at)] + time.by)
  #   }
  # }

  if (!is.null(time.at) && time.at[1] == "auto") {
    time.by <- floor(diff(xlim) / 4)
    time.at <- seq(xlim[1], xlim[2], by = time.by)
    if (time.at[length(time.at)] * 1.04 < xlim[2]) {
      time.at <- c(time.at, time.at[length(time.at)] + time.by)
    }
  }

  # [ Plot ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (is.null(oma)) {
    oma <- if (nrisk.table) {
      if (nrisk.pos == "above") {
        c(0, 0, nstrata - .3, 0)
      } else {
        # c(if (nstrata > 1) 1 + nstrata * .8 else 2.2, 0, 0, 0)
        c(1.7 + nstrata * nrisk.spacing, 0, 0, 0)
      }
    } else rep(0, 4)
  }

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
            oma = oma,
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

  # Median survival line(s) ====
  if (draw.median) {
    .median <- if (nstrata == 1) {
      survival:::survmean(x, scale = 1, rmean = "none")$matrix["median"]
    } else {
      survival:::survmean(x, scale = 1, rmean = "none")$matrix[, "median"]
    }
    for (i in .median) {
      lines(x = c(i, i), y = c(.5, 0),
            lty = median.lty,
            col = adjustcolor(median.col, median.alpha))
    }
    lines(x = c(0, max(.median)), y = c(.5, .5),
          lty = median.lty,
          col = adjustcolor(median.col, median.alpha))
  }

  # pointwise errors ====
  # if (plot.error) {
  #   lines(x$time, x$upper,
  #         lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
  #   lines(x$time, x$lower,
  #         lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
  # }

  # Autonames ====
  if (autonames && nstrata > 1) {
    group.title <- paste(
      gsub("=.*", "",
           strsplit(names(x$strata)[1], ", ")[[1]]),
      collapse = ", ")
    group.names <- unlist(
      lapply(strsplit(names(x$strata), ", "), function(i)
        paste(gsub(".*=", "", i), collapse = ", "))
    )
  }
  # [ Group Legend ] ====

  if (is.null(group.names)) group.names <- names(x$strata)

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(x) > 1, TRUE, FALSE)

  if (is.null(group.line)) {
    group.line <- if (min(x$surv) < .5) {
      if (is.null(group.title)) {
        -seq(1, 1 + (nrisk.spacing*(nstrata - 1)), nrisk.spacing)
      } else {
        -seq(1, 1 + (nrisk.spacing * nstrata ), nrisk.spacing)
      }
    } else {
      if (is.null(group.title)) {
        -seq(nrisk.spacing*(nstrata + 1), 1, -nrisk.spacing)
      } else {
        -seq(nrisk.spacing*(nstrata + 2), 1, -nrisk.spacing)
      }
    }
  }
  if (is.null(group.side)) {
    group.side <- if (min(x$surv) < .5) 3 else 1
  }

  if (group.legend) {
    col <- if (!is.null(group.title)) {
       c(adjustcolor(theme$fg, .5), unlist(col)[seq(nstrata)])
    } else {
      unlist(col)[seq(nstrata)]
    }
    mtext(text = c(group.title, group.names),
          side = group.side,
          line = group.line,
          col = col,
          adj = group.adj,
          cex = theme$cex,
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

    if (nrisk.pos == "above") {
      mtext("Number at risk   ",
            side = 3,
            line = (nstrata + 1) * nrisk.spacing + .1,
            # outer = TRUE,
            col = adjustcolor(theme$fg, .5),
            adj = 0,
            cex = theme$cex,
            family = theme$font.family)
      for (i in seq_len(nstrata)) {
        mtext(text = as.character(nriskmat[i, ]),
              side = 3,
              line = ((nstrata + nrisk.spacing) - i) * nrisk.spacing,
              at = time.at,
              col = palette[[i]],
              font = table.font,
              xpd = TRUE,
              family = theme$font.family)
      }
    } else {
      mtext("Number at risk",
            side = 1,
            line = 2.5,
            # outer = TRUE,
            col = adjustcolor(theme$fg, .5),
            # at = 0,
            adj = 0,
            cex = theme$cex,
            family = theme$font.family, xpd = TRUE)
      for (i in seq_len(nstrata)) {
        mtext(text = as.character(nriskmat[i, ]),
              side = 1,
              line = (1 + nrisk.spacing) + (1 + i) * nrisk.spacing,
              at = time.at,
              col = palette[[i]],
              font = table.font,
              xpd = TRUE,
              family = theme$font.family)
      }
    }

  }

} # rtemis::mplot3.surv
