# rtRandom.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Random \pkg{rtemis} art
#'
#' Draw random shapes and colors
#'
#' @param pch Point character to use
#' @param col Colors to use
#' @param text Text to print
#' @param text.col Color for text
#' @author E.D. Gennatas
#' @export

rtrandom <- function(pch = sample(15:18, 1),
                     col = rtCol,
                     text = "rtemis",
                     text.col = "gray50",
                     text.as.legend = FALSE,
                     legend.bg = NULL,
                     legend.alpha = 1,
                     random.bg = TRUE) {
  # legend = "rtemis",
  # legend.cex = 3,
  # legend.bg = "white",
  # legend.alpha = .5

  x <- runif(100, 1, 50)
  y <- runif(100, 1, 50)
  # color <- runif(100, 1, 100)
  # color <- col
  alpha <- runif(100, .1, 1)
  color.alpha <- sapply(1:length(col), function(x) adjustcolor(col[[x]], alpha[[x]]))
  # if (random.bg) bg <- runif(100, 1, 100) else bg <- col
  bg <- col
  alpha <- runif(length(bg), .1, 1)
  bg.alpha <- sapply(1:length(bg), function(x) adjustcolor(bg[[x]], alpha[[x]]))
  cex <- runif(length(col), 1, 10)
  par.orig <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(par.orig)))
  par(mar = c(0, 0, 0, 0))
  plot(x, y,
       pch = pch,
       col = color.alpha,
       bg = bg.alpha,
       cex = cex, ann = F, axes = F, lwd = 3)

  if (!text.as.legend) {
    text(x = sample(10:40, 1), y = sample(10:40, 1),
         labels = text, cex = 3, col = text.col)
  } else {
    legend(x = sample(10:40, 1), y = sample(10:40, 1),
           text,
           text.col = sample(col, 1),
           box.lty = 0,
           bg = adjustcolor(legend.bg, legend.alpha),
           cex = 2)
  }

} # rtemis::rtemis.random

# plot(1:100, 1:100); legend(x = 5, y = 80, legend = "rtemis", bty = "o", bg = "black", text.col = "blue", adj = .25, cex = 3)
