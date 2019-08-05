# dplot3.varimp.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Plot variable importance using \code{plotly}
#' 
#' A simple \code{plotly} wrapper to plot horizontal barplots, sorted by value, which can be used
#' to visualized variable importance, model coefficients, etc.
#' @inheritParams mplot3.xy
#' @param varimp Vector, float: Variable importance of features
#' @param names Vector, string: Names of features
#' @param col Vector, colors: Single value, or multiple values to define bar (feature) color(s)
#' @param mar Vector, numeric, length 4: Plot margins in pixels (NOT inches).
#' Default = c(50, 110, 50, 50)
#' @param pad Integer: Pad plot by this many pixels. Default = 10
#' @param font.family Character: Font to use. Default = "Open Sans"
#' @param font.size Integer: Overall font size to use (essentially for the title at this point).
#' Default = 14
#' @param axis.font.size Integer: Font size to use for axis labels and tick labels
#' (Seems not to be in same scale as \code{font.size} for some reason. Experiment!)
#' @param font.color Color for all text
#' @param showlegend Logical: If TRUE, show legend
#' @author Efstathios D. Gennatas
#' @export

dplot3.varimp <- function(varimp, names = NULL,
                          main = "Variable Importance",
                          xlab = "",
                          ylab = "",
                          col = colorAdjust(colorGrad(length(names), "penn"), .8),
                          mar = c(50, 110, 50, 50),
                          pad = 10,
                          font.family = "Open Sans",
                          font.size = 14,
                          axis.font.size = 18,
                          font.color = "000",
                          showlegend = TRUE) {
  
  # [ DEPENDENCIES ] ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }
  
  # [ DATA ] ====
  index <- order(varimp)
  x <- varimp[index]
  y <- factor(names[index], levels = names[index])
  if (is.null(names)) {
    names <- if (!is.null(names(varimp))) names(varimp) else paste0("Feature", 1:length(varimp))
  }
  
  # [ AES ] ====
  f <- list(family = font.family,
            size = axis.font.size,
            color = font.color)
  
  p <- plotly::plot_ly(x = ~x, y = ~y, type = 'bar', orientation = 'h',
                       marker = list(color = col),
                       showlegend = showlegend) %>%
    plotly::layout(title = main,
                   margin = list(l = mar[2],
                                 t = mar[3],
                                 r = mar[4],
                                 b = mar[1],
                                 pad = pad),
                   font = list(family = font.family,
                               size = font.size,
                               color = font.color),
                   xaxis = list(title = xlab,
                                titlefont = f,
                                tickfont = f),
                   yaxis = list(title = ylab,
                                titlefont = f,
                                tickfont = f))
  p
} # dplot3.varimp
