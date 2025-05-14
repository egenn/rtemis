# mplot_missing.R
# :: rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Plot missingness
#'
#' @inheritParams mplot3_x
#' @param x Data matrix or data.frame
#' @param feat.names Character: Feature names. Defaults to `colnames(x)`
#' @param case.names Character: Case names. Defaults to `rownames(x)`
#' @param main Character: Main title
#' @param col.missing Color for missing cases.
#' @param show Character: "percent" or "total". Show percent missing or total missing
#' per column on the x-axis
#' @param names.srt Numeric: Angle of feature names in degrees.
#' @param case.names.x Numeric: x position of case names
#' @param case.names.every Numeric: Show case names every this many cases
#' @param alpha Numeric: Multiply theme's `fg` color by this amount
#'
#' @examples
#' \dontrun{
#' dat <- iris
#' dat[c(1, 5, 17:20, 110, 115, 140), 1] <-
#' dat[c(12, 15, 55, 73, 100:103), 2] <-
#' dat[sample(1:150, 25), 4] <- NA
#' mplot_missing(dat)
#' }
#'
#' @export

mplot3_missing <- function(
  x,
  feat.names = NULL,
  case.names = NULL,
  main = NULL,
  col.missing = "#FE4AA3",
  show = c("percent", "total"),
  names.srt = 90,
  # x.tick.labs.adj = 0,
  case.names.x = .25,
  case.names.every = NULL,
  # y.srt = 0,
  theme = rtTheme,
  alpha = 1,
  mar = c(3, 3.5, 5.5, 1),
  oma = c(.5, .5, .5, .5),
  par.reset = TRUE,
  ...
) {
  # Arguments ----
  if (is.null(feat.names)) feat.names <- colnames(x)
  if (is.null(case.names)) case.names <- rownames(x)
  if (is.null(main)) main <- paste("Missing Data in", deparse(substitute(x)))
  if (is.null(case.names.every)) case.names.every <- ceiling(NROW(x) * .1)
  show <- match.arg(show)

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(par(par.orig))
  par(
    mar = mar,
    oma = oma,
    cex = theme$cex,
    bg = theme$bg,
    family = theme$font.family
  )

  .nrows <- NROW(x)
  xnai <- matrix(as.integer(is.na(x)), .nrows)

  x <- seq_len(NCOL(xnai))
  y <- seq_len(NROW(xnai))
  z <- t(apply(xnai, 2, rev))

  ylim <- c(.5, max(y) + .5)
  xlim <- c(.5, max(x) + .5)

  if (alpha < 1) {
    col.present <- colorAdjust(theme$fg, alpha)
  } else {
    .col <- mean(col2rgb(theme$fg)) / 255
    if (.col > .5) {
      .col <- .col - .85
    } else if (.col < .5) {
      .col <- .col + .85
    }
    col.present <- rgb(.col, .col, .col)
  }

  # Image ----
  image(
    x,
    y,
    data.matrix(z),
    col = c(col.present, col.missing),
    zlim = c(0, 1),
    asp = NULL,
    ann = FALSE,
    axes = FALSE,
    mar = c(0, 0, 0, 0),
    oma = c(0, 0, 0, 0)
  )

  # Annotations ----
  # title below image
  mtext(
    main,
    side = 1,
    font = 2,
    line = 2,
    # cex = theme$cex,
    col = theme$labs.col
  )

  # column names above image
  text(
    x,
    ylim[2] + diff(range(y)) * .04,
    labels = feat.names,
    adj = 0,
    offset = 0,
    xpd = TRUE,
    srt = names.srt,
    # cex = theme$cex,
    col = theme$labs.col
  )

  # row index left of image
  indl <- logical(case.names.every)
  indl[case.names.every] <- TRUE
  text(
    x = xlim[1] - diff(range(x)) * .02,
    y[rev(indl)],
    labels = rev(case.names)[rev(indl)],
    adj = 1,
    xpd = TRUE,
    # cex = theme$cex,
    col = theme$labs.col
  )

  # ylab
  mtext("Case (row nr.)", 2, line = 2.5, cex = theme$cex, col = theme$labs.col)

  # Column counts below image
  lo.y <- ylim[1] - diff(ylim) * .06
  # polygon(c(0.5, 0.5, xlim[2], xlim[2]),
  #         c(lo.y, 0.5, 0.5, lo.y),
  #         # col = "black",
  #         col = colorAdjust(theme$fg, .2),
  #         border = NA,
  #         xpd = TRUE)
  NAperColumn <- apply(xnai, 2, sum)
  text(
    x = x,
    mean(c(lo.y, .5)),
    adj = c(.5, .5),
    labels = if (show == "percent") ddSci(NAperColumn / .nrows * 100, 1) else
      NAperColumn,
    col = ifelse(NAperColumn > 0, col.missing, theme$labs.col),
    xpd = TRUE,
    cex = theme$cex
  )
  if (show == "percent") {
    text(
      xlim[2],
      .5 * (lo.y + 0.5),
      "%",
      adj = 0,
      # cex = theme$cex,
      col = theme$labs.col,
      xpd = TRUE
    )
  }
} # rtemis:: mplot3_missing
