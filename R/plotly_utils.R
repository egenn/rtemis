# plotly_utils
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

# plotly_vline calls plotly_vline1 to create a list for one or more vertical 
# lines, to be passed to plotly::layout's shapes argument
plotly_vline1 <- function(x, 
                          color = "#F48024",
                          width = 1,
                          dash = "dot") {
  list(
    type = "line",
    x0 = x, x1 = x,
    y0 = 0, y1 = 1, yref = "paper",
    line = list(color = color,
                width = width,
                dash = dash)
  )
}

plotly_vline <- function(x, 
                         color = "#F48024",
                         width = 1,
                         dash = "dot") {
  color <- recycle(color, x)
  width <- recycle(width, x)
  dash <- recycle(dash, x)
  mapply(plotly_vline1, x, color = color, width = width, dash = dash,
         SIMPLIFY = FALSE)
  
}

plotly_hline1 <- function(y, 
                          color = "#F48024",
                          width = 1,
                          dash = "dot") {
  list(
    type = "line",
    x0 = 0, x1 = 1,
    y0 = y, y1 = y, xref = "paper",
    line = list(color = color,
                width = width,
                dash = dash)
  )
}

plotly_hline <- function(y,
                         color = "#F48024",
                         width = 1,
                         dash = "dot") {
    color <- recycle(color, y)
    width <- recycle(width, y)
    dash <- recycle(dash, y)
    mapply(plotly_hline1, y,
        color = color, width = width, dash = dash,
        SIMPLIFY = FALSE
    )
}
