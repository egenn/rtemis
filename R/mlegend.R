#' Add legend to \code{mplot3} plot
#'
#' @param lims List with plot limits, as returned e.g. by \code{mplot3.xy}
#'
#' @author E.D. Gennatas
#' @export

mlegend <- function(lims, title = NULL, group.names,
                    title.col = "black",
                    col = rtPalette("rtCol1"),
                    horiz.pad = .04,
                    footer = NULL,
                    font = 1,
                    font.family = "Helvetica Neue", ...) {

  n.groups <- length(group.names)
  x <- max(lims[[1]]) + abs(diff(lims[[1]])) * horiz.pad

  yline.height <- abs(diff(lims[[2]])) * .06
  if (!is.null(title)) {

    group.names <- c(title, paste("  ", group.names))
    .cols <- c(title.col, unlist(col))

    y <- max(lims[[2]]) - c(0, seq(n.groups) * yline.height)
    points(rep(x, n.groups), y[-1] - (.3 * yline.height),
           col = .cols[-1], xpd = TRUE, pch = 16)
    text(x, y, group.names, xpd = TRUE, adj = c(0, 1), col = .cols,
         font = font, family = font.family)
  }

  if (!is.null(footer)) {
    text(x, lims[[2]][1], footer, xpd = TRUE, adj = c(0, 0),
         family = font.family)
  }

} # rtemis::mlegend
