# mplot3_survfit.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' `mplot3`: Plot `survfit` objects
#'
#' Plots survival step functions using [mplot3_xy]
#'
#' @param x survfit object (output of `survival::survfit`)
#' @param lty Integer: Line type. See `par("lty")`
#' @param lwd Float: Line width.
#' @param alpha Float: Alpha for lines.
#' @param col Color, vector: Color(s) to use for survival curves and annotations. If NULL,
#' taken from `palette`
#' @param plot.median Logical: If TRUE, draw lines at 50 percent median survival.
#' @param group.median Logical: If TRUE, include median survival times with group legend
#' @param median.lty Integer: Median survival line type
#' @param median.lwd Float: Median line width.
#' @param median.col Color for median survival lines
#' @param median.alpha Float, (0, 1): Transparency for median survival lines.
#' @param censor.mark Logical: If TRUE, mark each censored case.
#' @param censor.col Color to mark censored cases if `censor.mark = TRUE`
#' @param censor.alpha Transparency for `censor.col`.
#' @param censor.pch Character: Point character for censored marks.
#' @param censor.cex Float: Character expansion factor for censor marks.
#' @param mark.censored Logical: This is an alternative to `censor.mark` which whill mark
#' censored cases using the same color as the survival curve. It can be harder to distinguish the
#' censoring marks from the curve itself, therefore not preferred.
#' @param nrisk.table Logical: If TRUE, print Number at risk table.
#' @param nrisk.pos Character: "above" or "below": where to place `nrisk.table`
#' @param nrisk.spacing Float: Determines spacing between `nrisk.table` rows.
#' @param table.font Integer: 1: regular font, 2: bold.
#' @param time.at Float, vector: x-axis positions to place tickmarks and labels as well as n at risk
#' values if `nrisk.table = TRUE`
#' @param time.by Float: Divide time by this amount to determine placing of tickmarks
#' @param xlim Float, vector, length 2: x-axis limits
#' @param ylim Float, vector, length 2: y-axis limits
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param main Character: main title
#' @param theme Character: Run `themes()` for available themes
#' @param palette Vector of colors, or Character defining a builtin palette - get options with
#' `rtpalette()`
# @param error.lty
# @param error.alpha
#' @param autonames Logical: If TRUE, extract grouping variable names and level labels from `x`
#' and use for legend. It is best to give informative level labels, like female, male instead of
#' 0, 1 when using this.
#' @param group.legend Logical: If TRUE, include group legend
#' @param group.names Character, vector: Group names to use. If NULL, extracted from `x`
#' @param group.title Character: Group legend title
#' @param group.line Float, vector: Lines to print group legend using `mtext`
#' @param group.side Integer: Side to print group legend. Default is determined by survival curves,
#' to avoid overlap of legend with curves.
#' @param mar Float, vector, length 4: Margins. See `par("mar")`
#' @param oma Float, vector, length 4: Outer margins. See `par("oma")`
#' @param par.reset Logical: If TRUE, reset par to initial values before exit
#' @param ... Additional arguments to pass to theme
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # Get the lung dataset
#' data(cancer, package = "survival")
#' sf1 <- survival::survfit(survival::Surv(time, status) ~ 1, data = lung)
#' mplot3_survfit(sf1)
#' sf2 <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
#' mplot3_survfit(sf2)
#' # with N at risk table
#' mplot3_survfit(sf2, nrisk.table = TRUE)
#' }
mplot3_survfit <- function(
  x,
  lty = 1,
  lwd = 1.5,
  alpha = 1,
  col = NULL,
  plot.median = FALSE,
  group.median = FALSE,
  median.lty = 3,
  median.lwd = 2,
  median.col = theme$fg,
  median.alpha = .5,
  censor.mark = TRUE,
  censor.col = NULL,
  censor.alpha = .4,
  censor.pch = "I",
  censor.cex = .8,
  mark.censored = FALSE,
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
  main = "", # "Kaplan-Meier Estimate"
  theme = rtTheme,
  palette = rtPalette,
  plot.error = FALSE,
  # error.lty = 2,
  error.alpha = .33,
  autonames = TRUE,
  group.legend = NULL,
  group.legend.type = c("legend", "mtext"),
  group.names = NULL,
  group.title = NULL,
  group.line = NULL,
  group.side = NULL, # for group.legend.type "mtext"
  legend.x = NULL, # for group.legend.type "legend"
  mar = c(2.5, 3, 2, 1),
  oma = NULL,
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  filename = NULL,
  ...
) {
  group.legend.type <- match.arg(group.legend.type)

  # Data ----
  if (class(x)[1] != "survfit") stop("Input must be of class 'survfit'")
  nstrata <- if (is.null(x$strata)) 1 else length(x$strata)
  if (nstrata > 1) {
    .group <- unlist(sapply(seq_len(nstrata), function(i) rep(i, x$strata[i])))
  } else {
    .group <- rep(1, length(x$time))
  }

  # Arguments ----
  if (mark.censored) censor.mark <- FALSE

  # Theme ----
  if (is.null(col)) {
    if (is.character(palette)) palette <- rtpalette(palette)
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

  # Limits ----
  if (is.null(xlim)) xlim <- c(0, max(x$time, na.rm = TRUE))
  if (is.null(ylim)) ylim <- c(0, 1)

  # x.axis.at ----
  if (!is.null(time.by)) {
    time.at <- seq(xlim[1], xlim[2], by = time.by)
    if (time.at[length(time.at)] * 1.04 < xlim[2]) {
      time.at <- c(time.at, time.at[length(time.at)] + time.by)
    }
  }
  if (nrisk.table && is.null(time.at)) time.at <- "auto"
  if (!is.null(time.at) && time.at[1] == "auto") {
    time.by <- floor(diff(xlim) / 4)
    time.at <- seq(xlim[1], xlim[2], by = time.by)
    if (time.at[length(time.at)] * 1.04 < xlim[2]) {
      time.at <- c(time.at, time.at[length(time.at)] + time.by)
    }
  }

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Plot ----
  if (!is.null(filename)) {
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  }
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (is.null(oma)) {
    oma <- if (nrisk.table) {
      if (nrisk.pos == "above") {
        c(0, 0, nstrata - .3, 0)
      } else {
        c(1.7 + nstrata * nrisk.spacing, 0, 0, 0)
      }
    } else {
      rep(0, 4)
    }
  }

  mplot3_xy(
    x = split(x$time, .group),
    y = split(x$surv, .group),
    xlim = xlim,
    ylim = ylim,
    type = "s",
    x.axis.at = time.at,
    lwd = lwd,
    lty = lty,
    theme = theme,
    palette = palette,
    marker.alpha = 0,
    marker.col = col,
    line.alpha = 1,
    main = main,
    xlab = xlab,
    ylab = ylab,
    group.legend = FALSE,
    zerolines = FALSE,
    mar = mar,
    oma = oma,
    par.reset = FALSE
  )

  lines(
    x,
    mark.time = mark.censored,
    col = unlist(palette),
    lwd = lwd,
    conf.int = FALSE
  )

  # Censoring markers ----
  if (censor.mark) {
    if (is.null(censor.col)) censor.col <- adjustcolor(theme$fg, censor.alpha)
    .index <- x$n.censor == 1
    points(
      x$time[.index],
      x$surv[.index],
      pch = censor.pch,
      col = censor.col,
      cex = censor.cex
    )
  }

  # Median survival line(s) ----
  if (plot.median) {
    survmean <- getFromNamespace("survmean", "survival")
    .median <- if (nstrata == 1) {
      survmean(x, scale = 1, rmean = "none")$matrix["median"]
    } else {
      survmean(x, scale = 1, rmean = "none")$matrix[, "median"]
    }
    for (i in .median) {
      lines(
        x = c(i, i),
        y = c(.5, 0),
        lty = median.lty,
        lwd = median.lwd,
        col = adjustcolor(median.col, median.alpha)
      )
    }
    lines(
      x = c(0, max(.median)),
      y = c(.5, .5),
      lty = median.lty,
      lwd = median.lwd,
      col = adjustcolor(median.col, median.alpha)
    )
  }

  # pointwise errors ----

  # if (plot.error) {
  #   lines(x$time, x$upper,
  #         lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
  #   lines(x$time, x$lower,
  #         lty = error.lty, col = colorAdjust(col[[i]], error.alpha))
  # }

  if (plot.error) {
    par(pty = "s")
    sfs <- summary(x)
    .time <- split(sfs$time, sfs$strata)
    .upper <- split(sfs$upper, sfs$strata)
    .lower <- split(sfs$lower, sfs$strata)

    for (i in seq_len(nstrata)) {
      .exclude <- is.na(.upper[[i]]) | is.na(.lower[[i]])
      .time1 <- .time[[i]][!.exclude]
      .upper1 <- .upper[[i]][!.exclude]
      .lower1 <- .lower[[i]][!.exclude]
      revlower <- rev(.lower1)
      polygon(
        c(.time1, xlim[2], xlim[2], rev(.time1)),
        c(.upper1, rev(.upper1)[1], revlower[1], revlower),
        col = colorAdjust(col[[i]], error.alpha),
        border = NA
      )
    }
  }

  # Autonames ----
  if (autonames && nstrata > 1) {
    if (is.null(group.title)) {
      group.title <- paste(
        sub(
          "=.*",
          "",
          strsplit(names(x$strata)[1], ", ")[[1]]
        ),
        collapse = ", "
      )
    }
    if (is.null(group.names)) {
      group.names <- unlist(
        lapply(
          strsplit(
            gsub(" *", "", names(x$strata)),
            ","
          ),
          function(i) {
            paste(sub(".*?=", "", i), collapse = ", ")
          }
        )
      )
    }
  }
  if (!is.null(group.title) && group.median) {
    group.title <- paste(group.title, "(median)")
  }

  # Group Legend ----
  if (is.null(group.legend)) group.legend <- nstrata > 1
  if (group.legend && is.null(group.names)) group.names <- names(x$strata)

  if (group.legend && group.median) {
    group.names <- paste0(group.names, " (", ddSci(.median), ")")
  }

  if (is.null(group.line)) {
    group.line <- if (min(x$surv) < .5) {
      if (is.null(group.title)) {
        -seq(1, 1 + (nrisk.spacing * (nstrata - 1)), nrisk.spacing)
      } else {
        -seq(1, 1 + (nrisk.spacing * nstrata), nrisk.spacing)
      }
    } else {
      if (is.null(group.title)) {
        -seq(nrisk.spacing * (nstrata + 1), 1, -nrisk.spacing)
      } else {
        -seq(nrisk.spacing * (nstrata + 2), 1, -nrisk.spacing)
      }
    }
  }

  if (is.null(group.side)) {
    group.side <- if (min(x$surv) < .5) 3 else 1
  }

  if (is.null(legend.x)) {
    legend.x <- if (min(x$surv) < .5) "topright" else "bottomright"
  }

  if (group.legend) {
    col <- if (!is.null(group.title)) {
      c(theme$fg, unlist(col)[seq(nstrata)])
    } else {
      unlist(col)[seq(nstrata)]
    }
    if (group.legend.type == "mtext") {
      mtext(
        text = c(group.title, group.names),
        side = group.side,
        line = group.line,
        col = col,
        adj = 1,
        at = xlim[2],
        cex = theme$cex,
        family = theme$font.family
      )
    } else {
      legend(
        "topright",
        title = group.title,
        legend = group.names,
        text.col = theme$labs.col,
        fill = col[-1],
        border = NA,
        bg = theme$bg,
        box.lwd = 0
      )
    }
  }

  # nrisk table ----
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
      mtext(
        "Number at risk   ",
        side = 3,
        line = (nstrata + 1) * nrisk.spacing + .1,
        # outer = TRUE,
        col = adjustcolor(theme$fg, .5),
        adj = 0,
        cex = theme$cex,
        family = theme$font.family
      )
      for (i in seq_len(nstrata)) {
        mtext(
          text = as.character(nriskmat[i, ]),
          side = 3,
          line = ((nstrata + nrisk.spacing) - i) * nrisk.spacing,
          at = time.at,
          col = palette[[i]],
          font = table.font,
          xpd = TRUE,
          cex = theme$cex,
          family = theme$font.family
        )
      }
    } else {
      mtext(
        "Number at risk",
        side = 1,
        line = 2.1,
        # outer = TRUE,
        col = adjustcolor(theme$fg, .5),
        # at = 0,
        adj = 0,
        cex = theme$cex,
        family = theme$font.family,
        xpd = TRUE
      )
      for (i in seq_len(nstrata)) {
        mtext(
          text = as.character(nriskmat[i, ]),
          side = 1,
          line = (.6 + nrisk.spacing) + (1 + i) * nrisk.spacing,
          at = time.at,
          col = palette[[i]],
          font = table.font,
          xpd = TRUE,
          cex = theme$cex,
          family = theme$font.family
        )
      }
    }
  }

  if (!is.null(filename)) dev.off()
  invisible(list(group.names = group.names))
} # rtemis::mplot3_surv
