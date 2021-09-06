# dplot3.box.R
# ::rtemis::
# 201-21 E.D. Gennatas lambdamd.org
# add support for multiple vars + group + time bin

#' Interactive Boxplots & Violin plots
#'
#' Draw interactive boxplots or violin plots using \pkg{plotly}
#'
#' For multiple box plots, the recommendation is:
#' - `x=dat[, columnindex]` for multiple variables of a data.frame
#' - `x=list(a=..., b=..., etc.)` for multiple variables of potentially different length
#' - `x=split(var, group)` for one variable with multiple groups: group names appear below boxplots
#' - `x=dat[, columnindex], group = factor` for grouping multiple variables: group names appear in legend
#'
#' If \code{orientation = "h"}, \code{xlab} is applied to y-axis and vice versa. Similarly,
#' \code{x.axist.type} applies to y-axis - this defaults to "category" and would not normally need
#' changing.
#' @param x Vector or List of vectors: Input
#' @param main Character: Plot title. Default = NULL
#' @param xlab Character: x-axis label. Default = NULL
#' @param ylab  Character: y-axis label. Default = NULL
#' @param col Color, vector: Color for boxes. Default NULL, which will draw colors from \code{palette}
#' @param alpha Float (0, 1]: Transparency for box colors. Default = .8
#' @param bg Color: Background color. Default = "white"
#' @param plot.bg Color: Background color for plot area. Default = "white"
#' @param theme Character: THeme to use: "light", "dark", "lightgrid", "darkgrid". Default = "lightgrid"
#' @param palette Character: Name of \pkg{rtemis} palette to use. Default = "rtCol1". Only used if \code{col = NULL}
#' @param quartilemethod Character: "linear", "exclusive", "inclusive"
#' @param boxpoints Character or FALSE: "all", "suspectedoutliers", "outliers"
#' See \url{https://plotly.com/r/box-plots/#choosing-the-algorithm-for-computing-quartiles}
#' @param xnames Character, vector, length = NROW(x): x-axis names. Default = NULL, which
#' tries to set names appropriately
#' @param order.by.fn Function: If defined, order boxes by increasing value of this function
#' (e.g. median). Default = NULL
#' @param feature.names Character, vector, length = NCOL(x): Feature names. Default = NULL, which uses
#' \code{colnames(x)}
#' @param font.size  Float: Font size for all labels. Default = 16
#' @param legend Logical: If TRUE, draw legend. Default = TRUE
#' @param legend.col Color: Legend text color. Default = NULL, determined by theme
#' @param legend.xy Float, vector, length 2: Relative x, y position for legend. Default = NULL, which places
#' the legend top right beside the plot area. For example, c(0, 1) places the legend top left within the plot area
#' @param xaxis.type Character: "linear", "log", "date", "category", "multicategory"
#' Default = "category"
#' @param margin Named list: plot margins. Default = \code{list(t = 35)}
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # A.1 Box plot of 4 variables
#' dplot3.box(iris[, 1:4])
#' # A.2 Grouped Box plot
#' dplot3.box(iris[, 1:4], group = iris$Species)
#' # B. Boxplot split by time periods
#' # Synthetic data with an instantenous shift in distributions
#' set.seed(2021)
#' dat1 <- data.frame(alpha = rnorm(200, 0), beta = rnorm(200, 2), gamma = rnorm(200, 3))
#' dat2 <- data.frame(alpha = rnorm(200, 5), beta = rnorm(200, 8), gamma = rnorm(200, -3))
#' x <- rbind(dat1, dat2)
#' startDate <- as.Date("2019-12-04")
#' endDate <- as.Date("2021-03-31")
#' time <- seq(startDate, endDate, length.out = 400)
#' dplot3.box(x, time, "year")
#' dplot3.box(x, time, "quarter")
#' dplot3.box(x, time, "month")
#' # (Note how the boxplots widen when the period includes data from both dat1 and dat2)
#' }

# DEV: horizontal gives wrong data => wrong plots, wrong medians, etc

dplot3.box <- function(x,
                       time = NULL,
                       time.bin = c("year", "quarter", "month", "day"),
                       type = c("box", "violin"),
                       group = NULL,
                       main = NULL,
                       xlab = "",
                       ylab = NULL,
                       col = NULL,
                       alpha = .6,
                       bg = NULL,
                       plot.bg = NULL,
                       theme = getOption("rt.theme", "lightgrid"),
                       palette = getOption("rt.palette", "rtCol1"),
                       boxpoints = "outliers",
                       quartilemethod = "linear",
                       # width = 0,
                       violin.box = TRUE,
                       orientation = "v",
                       xnames = NULL,
                       labelify = TRUE,
                       order.by.fn = NULL,
                       font.size = 16,
                       legend = NULL,
                       legend.col = NULL,
                       legend.xy = NULL,
                       legend.orientation = "v",
                       xaxis.type = "category",
                       margin = list(t = 35, pad = 0),
                       automargin.x = TRUE,
                       automargin.y = TRUE,
                       boxgap = NULL,
                       boxgroupgap = NULL,
                       hovertext = NULL,
                       displayModeBar = TRUE,
                       filename = NULL,
                       file.width = 500,
                       file.height = 500,
                       print.plot = TRUE, ...) {

  # Dependencies ====
  if (!depCheck("plotly", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  type <- match.arg(type)
  main <- paste0("<b>", main, "</b>")
  # if (!is.list(x)) x <- list(x)
  # Convert vector or matrix to list
  if (!is.list(x)) {
    # x is vector
    if (is.numeric(x)) {
      .names <- deparse(substitute(x))
      x <- list(x)
      names(x) <- .names
    } else {
      # x is data.frame or matrix
      .names <- colnames(x)
      x <- lapply(seq(NCOL(x)), function(i) x[, i])
      names(x) <- .names
    }
  }
  horizontal <- orientation == "h"

  # Order by fn ====
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    if (is.null(time)) {
      if (is.list(x)) {
        .order <- order(sapply(x, order.by.fn, na.rm = TRUE))
        if (is.data.frame(x)) {
          x <- x[, .order]
        } else {
          x <- x[names(x)[.order]]
        }
      }
      if (!is.null(xnames)) xnames <- xnames[.order]
    } else {
      warning("Ignoring order.by.fn with time data")
      order.by.fn <- NULL
    }
  }

  # Remove non-numeric vectors
  # which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  # if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  if (!is.null(group)) group <- factor(group)
  n.groups <- if (is.null(group)) length(x) else length(levels(group))
  .xnames <- xnames
  if (is.null(.xnames)) {
    .xnames <- names(x)
    if (is.null(.xnames)) .xnames <- paste0("Feature", seq(n.groups))
    if (labelify) .xnames <- labelify(.xnames)
  }


  # Colors ====
  if (is.character(palette)) palette <- rtPalette(palette)
  if (is.null(col)) col <- recycle(palette, seq(n.groups))[seq(n.groups)]
  if (!is.null(order.by.fn) && order.by.fn != "none") {
    col <- col[.order]
  }

  # Theme ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  bg <- plotly::toRGB(theme$bg)
  plot.bg <- plotly::toRGB(theme$plot.bg)
  grid.col <- plotly::toRGB(theme$grid.col)
  tick.col <- plotly::toRGB(theme$tick.labels.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  main.col <- plotly::toRGB(theme$main.col)
  # axes.col <- plotly::toRGB(theme$axes.col)

  # Derived
  if (is.null(legend.col)) legend.col <- labs.col

  if (is.null(time)) {

    if (is.null(group)) {
      # A.1 Single and multiple boxplots ====
      if (is.null(legend)) legend <- FALSE
      args <- if (horizontal) {
        list(x = x[[1]], y = NULL)
      } else {
        list(x = NULL, y = x[[1]])
      }
      args <- c(args,
                list(
                   type = type,
                   name = .xnames[1],
                   line = list(color = plotly::toRGB(col[1])),
                   fillcolor = plotly::toRGB(col[1], alpha),
                   marker = list(color = plotly::toRGB(col[1], alpha))
                   # width = width
      ))
      if (!is.null(hovertext) && n.groups == 1) {
        hovertext <- list(hovertext)
      }
      if (type == "box") {
        args <- c(args, list(quartilemethod = quartilemethod,
                             boxpoints = boxpoints))
        if (!is.null(hovertext)) args$text <- hovertext[[1]]
      }
      if (type == "violin") args$box <- list(visible = violin.box)
      plt <- do.call(plotly::plot_ly, args)
      if (n.groups > 1) {
        for (i in seq_len(n.groups)[-1]) {
          plt <- plotly::add_trace(plt,
                                   x = if (horizontal) x[[i]] else NULL,
                                   y = if (horizontal) NULL else x[[i]],
                                   name = .xnames[i],
                                   line = list(color = plotly::toRGB(col[i])),
                                   fillcolor = plotly::toRGB(col[i], alpha),
                                   marker = list(color = plotly::toRGB(col[i], alpha)),
                                   text = if (!is.null(hovertext)) hovertext[[i]] else NULL)
        }
      }
    } else {
      # A.2 Grouped boxplots with [group] ====
      if (is.null(legend)) legend <- TRUE
      dt <- cbind(data.table::as.data.table(x), group = group)
      dtlong <- data.table::melt(dt[, ID := seq(nrow(dt))], id.vars = c("ID", "group"))
      if (is.null(ylab)) ylab <- ""
      args <- list(data = dtlong,
                   type = type,
                   x = if (horizontal) ~value else ~variable,
                   y = if (horizontal) ~variable else ~value,
                   color = ~group,
                   colors = col2hex(col)
                   # width = width
      )
      if (type == "box") {
        args <- c(args, list(quartilemethod = quartilemethod,
                             boxpoints = boxpoints,
                             alpha = alpha))
        if (!is.null(hovertext)) {
          dtlong <- merge(dtlong, cbind(dt[, .(ID)], hovertext))
          args$text <- dtlong$hovertext
        }
      }
      if (type == "violin") args$box <- list(visible = violin.box)
      cataxis <- list(tickvals = 0:(NCOL(dt) - 2),
                      ticktext = .xnames)
      plt <- do.call(plotly::plot_ly, args) %>%
        plotly::layout(boxmode = "group",
                       xaxis = if (horizontal) NULL else cataxis,
                       yaxis = if (horizontal) cataxis else NULL)
    }

  } else {
    # B. Time-binned boxplots ====
    time.bin <- match.arg(time.bin)
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- ""
    if (is.null(legend)) legend <- TRUE

    dt <- data.table::as.data.table(x)
    dt[, timeperiod := factor(switch(time.bin,
                                     year = data.table::year(time),
                                     quarter = paste(data.table::year(time), quarters(time)),
                                     month = paste(data.table::year(time), months(time, TRUE)),
                                     day = time,
    ))]

    ## Long data ====
    dtlong <- data.table::melt(dt[, ID := seq(nrow(dt))], id.vars = c("ID", "timeperiod"))
    if (!is.null(hovertext)) {
      dtlong <- merge(dtlong, cbind(dt[, .(ID)], hovertext))
    }

    # group by
    if (!is.null(group)) {
      group <- factor(group)
      grouplevels <- levels(group)
      transforms <- list(
        list(
          type = 'groupby',
          groups = group,
          styles =
            lapply(seq_along(grouplevels), function(i) {
              list(target = grouplevels[i], value = list(line = list(color = plotly::toRGB(col[i])),
                                                         fillcolor = plotly::toRGB(col[i], alpha),
                                                         marker = list(color = plotly::toRGB(col[i], alpha)))
              )
            })
        )
      )
    } else {
      transforms <- NULL
    }

    if (is.null(group)) {
      args <- list(data = dtlong,
                   type = type,
                   x = if (horizontal) ~value else ~timeperiod,
                   y = if (horizontal) ~timeperiod else ~value,
                   color = ~variable,
                   colors = col2hex(col))
    } else {
      args <- list(data = dtlong,
                   type = type,
                   x = if (horizontal) ~value else ~timeperiod,
                   y = if (horizontal) ~timeperiod else ~value,
                   transforms = transforms)
    }
    # args$width <- width
    if (!is.null(hovertext)) args$text <- dtlong$hovertext

    if (type == "box") {
      args <- c(args, list(quartilemethod = quartilemethod,
                           boxpoints = boxpoints))
    }
    if (type == "violin") args$box <- list(visible = violin.box)

    plt <- do.call(plotly::plot_ly, args) %>%
      plotly::layout(boxmode = "group")
  }

  # layout ====
  f <- list(family = theme$font.family,
            size = font.size,
            color = labs.col)
  tickfont <- list(family = theme$font.family,
                   size = font.size,
                   color = tick.col)
  .legend <- list(x = legend.xy[1],
                  y = legend.xy[2],
                  font = list(family = theme$font.family,
                              size = font.size,
                              color = legend.col),
                  orientation = legend.orientation)

  plt <- plotly::layout(plt,
                        yaxis = list(title = if (horizontal) xlab else ylab,
                                     type = if (horizontal) xaxis.type else NULL,
                                     titlefont = f,
                                     showgrid = theme$grid,
                                     gridcolor = grid.col,
                                     gridwidth = theme$grid.lwd,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     zeroline = FALSE,
                                     automargin = automargin.y),
                        xaxis = list(title = if (horizontal) ylab else xlab,
                                     type = if (horizontal) NULL else xaxis.type,
                                     titlefont = f,
                                     showgrid = FALSE,
                                     tickcolor = grid.col,
                                     tickfont = tickfont,
                                     automargin = automargin.x),
                        title = list(text = main,
                                     font = list(family = theme$font.family,
                                                 size = font.size,
                                                 color = main.col),
                                     xref = 'paper',
                                     x = theme$main.adj),
                        paper_bgcolor = bg,
                        plot_bgcolor = plot.bg,
                        margin = margin,
                        showlegend = legend,
                        legend = .legend,
                        boxgap = boxgap,
                        boxgroupgap = boxgroupgap)

  # Config
  plt <- plotly::config(plt,
                        displaylogo = FALSE,
                        displayModeBar = displayModeBar)

  # Write to file ====
  if (!is.null(filename)) {
    filename <- file.path(filename)
    plotly::plotly_IMAGE(plt, width = file.width, height = file.height,
                         format = tools::file_ext(file), out_file = filename)
  }

  if (print.plot) suppressWarnings(print(plt))
  invisible(plt)

} # rtemis::dplot3.box.R
