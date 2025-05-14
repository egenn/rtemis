#' Add legend to `mplot3` plot
#'
#' @param lims List with plot limits in the form list(xlim = xlim, ylim = ylim)
#' e.g. as returned by [mplot3_xy]
#' @param title Character: Legend title
#' @param group.names Character: group names
#' @param title.col Title color
#' @param col Color vector
#' @param horiz.pad Numeric: Proportion of plot width to pad by
#' @param footer Character: Footer annotation
#' @param font 1 or 2 for regular and bold
#' @param font.family Character: Font family to use
#'
#' @author E.D. Gennatas
#' @export

mlegend <- function(
  lims,
  title = NULL,
  group.names,
  title.col = "black",
  col = rtpalette("rtCol1"),
  horiz.pad = .04,
  footer = NULL,
  font = 1,
  font.family = "Helvetica Neue"
) {
  n.groups <- length(group.names)
  x <- max(lims[[1]]) + abs(diff(lims[[1]])) * horiz.pad

  yline.height <- abs(diff(lims[[2]])) * .06
  if (!is.null(title)) {
    group.names <- c(title, paste("  ", group.names))
    .cols <- c(title.col, unlist(col))

    y <- max(lims[[2]]) - c(0, seq(n.groups) * yline.height)
    points(
      rep(x, n.groups),
      y[-1] - (.3 * yline.height),
      col = .cols[-1],
      xpd = TRUE,
      pch = 16
    )
    text(
      x,
      y,
      group.names,
      xpd = TRUE,
      adj = c(0, 1),
      col = .cols,
      font = font,
      family = font.family
    )
  }

  if (!is.null(footer)) {
    text(
      x,
      lims[[2]][1],
      footer,
      xpd = TRUE,
      adj = c(0, 0),
      family = font.family
    )
  }
} # rtemis::mlegend


mtextlegend <- function(
  labels,
  # title = NULL,
  # title.col = "black",
  # side = 3,
  line = NULL,
  outer = FALSE,
  col = rtpalette("rtCol1"),
  horiz.pad = .04,
  padj.spacing = 1.4,
  font = 1,
  font.family = "Helvetica Neue",
  ...
) {
  nlabels <- length(labels)
  label.lines <- -1:-nlabels
  mtext(
    text = labels,
    at = xright(horiz.pad),
    adj = 0,
    side = 3,
    line = label.lines,
    col = unlist(col)[seq_len(nlabels)],
    font = font,
    family = font.family
  )
} # rtemis::mtextlegend
