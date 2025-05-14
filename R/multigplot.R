# multigplot.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Multipanel **ggplot2** plots
#'
#' Plot a panel of **gplot2** plots
#'
#' @param plots List of ggplot2 plots
#' @param nrows Integer: number of rows for panel arrangement. Defaults to number of rows required
#' to plot 2 plots per row
#' @param byrow Logical: If TRUE, draw plots in order provided by row, otherwise by column.
#' Default = TRUE
#'
#' @author E.D. Gennatas
#' @export

multigplot <- function(plots = NULL, nrows = NULL, byrow = TRUE) {
  nplots <- length(plots)
  if (is.null(nrows)) nrows <- ceiling(nplots / 2)
  ncols <- ceiling(nplots / nrows)
  layout <- matrix(seq(nrows * ncols), nrows, byrow = byrow)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))

  for (i in seq(nplots)) {
    id <- which(layout == i, arr.ind = TRUE)
    print(
      plots[[i]],
      vp = grid::viewport(layout.pos.row = id[1], layout.pos.col = id[2])
    )
  }
} # rtemis::multigplot
