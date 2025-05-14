#' Plot AGGTEobj object
#'
#' Plot AGGTEobj object from the \pkg{did} package.
#'
#' @inheritParams mplot3_xy
#' @param x AGGTEobj object
#'
#' @author E.D. Gennatas
#' @export

mplot_AGGTEobj <- function(
  x,
  x.factor = 1,
  y.factor = 1,
  error = c("se", "95%ci"),
  main = "Average Effect by Length of Exposure",
  legend.title = "",
  group.names = c("Pre", "Post"),
  xlab = NULL,
  ylab = NULL,
  mar = c(2.5, 3.5, 2, 7),
  theme = rtTheme,
  font.family = "Helvetica",
  col = c("#EC1848", "#18A3AC"),
  filename = NULL,
  file.width = 6.5,
  file.height = 5.5,
  par.reset = TRUE,
  ...
) {
  error <- match.arg(error)

  if (par.reset) {
    par.orig <- par(no.readonly = TRUE)
    on.exit(par(par.orig))
  }

  if (!is.null(filename)) {
    pdf(filename, width = file.width, height = file.height)
  }

  if (error == "se") {
    errory <- x$se.egt
    footer = "(error: 1 S.E.)"
  } else {
    errory <- x$crit.val.egt * x$se.egt
    footer = "(error: 95% C.I.)"
  }

  group <- as.integer(x$egt >= 0) + 1

  lim <- mplot3_xy(
    split(x$egt * x.factor, group),
    split(x$att.egt * y.factor, group),
    error.y = split(errory, group),
    theme = theme,
    marker.col = col,
    group.legend = FALSE,
    par.reset = FALSE,
    mar = mar,
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  mlegend(
    lim,
    title = legend.title,
    group.names = group.names,
    col = col,
    footer = footer,
    font.family = font.family
  )

  if (!is.null(filename)) dev.off()
} # rtemis::mplot_AGGTEobj
