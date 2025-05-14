# mplot3_guitar.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' `mplot3`: Guitar Fretboard
#'
#' Draw color-coded notes on a guitar fretboard for standard E-A-D-G-B-e tuning
#'
#' Plot is very wide and short. Adjust plot window accordingly.
#' Practice every day.
#' @param theme Character: "light" or "dark"
#' @param useSharps Logical: If TRUE, draw sharp instead of flat notes. Default = FALSE
#' @param strings.col Color for strings
#' @param frets.col Color for frets
#' @param inlays Logical: Draw fretboard inlays. Default = TRUE
#' @param inlays.col Color for inlays
#' @param inlays.cex Numeric: Character expansion factor for inlays. Default = 2
#' @param par.reset Logical: If TRUE, reset par before exit
#' @param ... Additional arguments to theme
#'
#' @author E.D. Gennatas
#' @export

mplot3_fret <- function(
  theme = rtTheme,
  useSharps = FALSE,
  strings.col = "auto",
  frets.col = "auto",
  inlays = TRUE,
  inlays.col = "auto",
  inlays.cex = 2,
  par.reset = TRUE,
  ...
) {
  # [ Theme ] ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (strings.col == "auto") strings.col <- adjustcolor(theme$fg, .8)
  if (frets.col == "auto") frets.col <- adjustcolor(theme$fg, .8)
  if (inlays.col == "auto") inlays.col <- adjustcolor(theme$fg, .35)

  if (par.reset) {
    par.orig <- par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(par.orig)))
  }

  # Plot
  mplot3_xy(
    rep(0, 22),
    rep(0, 22),
    theme = theme,
    scatter = FALSE,
    grid = FALSE,
    pty = "m",
    xlim = c(-0.2, 21.5),
    ylim = c(0.5, 6.5),
    xlab = "",
    ylab = "",
    par.reset = FALSE,
    axes.visible = FALSE,
    zerolines = FALSE,
    xaxs = "i",
    mar = c(1, 2, 1, 1)
  )

  # Strings
  abline(h = 1:6, col = strings.col)

  # Frets
  abline(v = 0:21, lwd = c(2.5, rep(1, 21)), col = frets.col)

  # Note Colors
  cols <- list(
    E = pennCol$lighterPurple,
    F = "gray50",
    Gb = pennCol$lighterGreen,
    G = pennCol$green,
    Ab = pennCol$lightestBlue,
    A = pennCol$lighterBlue,
    Bb = pennCol$yellow,
    B = pennCol$orange,
    C = theme$fg,
    Db = pennCol$lighterRed,
    D = pennCol$red,
    Eb = pennCol$lightestPurple
  )

  cols <- as.character(cols)
  if (useSharps) {
    cols <- cols[c(2:12, 1)]
  }

  # Notes
  if (!useSharps) {
    Notes <- factor(c(
      "E",
      "F",
      "Gb",
      "G",
      "Ab",
      "A",
      "Bb",
      "B",
      "C",
      "Db",
      "D",
      "Eb"
    ))
  } else {
    Notes <- factor(c(
      "E",
      "F",
      "F#",
      "G",
      "G#",
      "A",
      "A#",
      "B",
      "C",
      "C#",
      "D",
      "D#"
    ))
  }

  # White background for Note Names
  points(
    x = rep(0:21 - .5, 6),
    y = rep(1:6, times = rep(22, 6)),
    pch = 15,
    col = theme$bg,
    xpd = TRUE,
    cex = 3.5
  )

  # Note Names
  text(
    x = 0:21 - .5,
    y = 1,
    rep(Notes, length.out = 22),
    xpd = T,
    col = cols,
    font = 2
  )
  text(
    x = 0:21 - .5,
    y = 2,
    rep(rep(Notes, 5)[6:27], length.out = 22),
    xpd = T,
    col = rep(cols, 3)[6:27],
    font = 2
  )
  text(
    x = 0:21 - .5,
    y = 3,
    rep(rep(Notes, 5)[11:32], length.out = 22),
    xpd = T,
    col = rep(cols, 3)[11:32],
    font = 2
  )
  text(
    x = 0:21 - .5,
    y = 4,
    rep(rep(Notes, 5)[4:25], length.out = 22),
    xpd = T,
    col = rep(cols, 3)[4:25],
    font = 2
  )
  text(
    x = 0:21 - .5,
    y = 5,
    rep(rep(Notes, 5)[8:29], length.out = 22),
    xpd = T,
    col = rep(cols, 3)[8:29],
    font = 2
  )
  text(
    x = 0:21 - .5,
    y = 6,
    rep(Notes, length.out = 22),
    xpd = T,
    col = cols,
    font = 2
  )

  # Fretboard inlays
  if (inlays) {
    inlay.x <- c(3, 5, 7, 9, 12, 12, 15, 17, 19, 21) - .5
    inlay.y <- c(3.5, 3.5, 3.5, 3.5, 2.5, 4.5, 3.5, 3.5, 3.5, 3.5)
    points(inlay.x, inlay.y, pch = 16, col = inlays.col, cex = inlays.cex)
  }
} # rtemis::mplot3_fret
