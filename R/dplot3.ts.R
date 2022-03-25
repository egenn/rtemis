# dplot3.ts.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

# => recalc limits for fn = "sum"

#' Interactive Timeseries Plots
#'
#' Draw interactive timeseries plots using \code{plotly}
#'
#' @param x Numeric vector of values to plot or list of vectors
#' @param time Numeric or Date vector of time corresponding to values of \code{x}
#' @param window Integer: apply \code{roll.fn} over this many units of time
#' @param roll.fn Character: "mean", "median", "max", or "sum": Function to apply on rolling windows of \code{x}
#' @param roll.col Color for rolling line
#' @param roll.alpha Numeric: transparency for rolling line
#' @param roll.lwd Numeric: width of rolling line
#' @param align Character: "center", "right", or "left"
#' @param use Character: "data.table" or "zoo": which package to use to apply
#' rolling function
#' @param xlab Character: x-axis label
#' @param n.xticks Integer: number of x-axis ticks to use (approximately)
#' @param x.showspikes Logical: If TRUE, show x-axis spikes on hover
#' @param displayModeBar Logical: If TRUE, display plotly's modebar
#' @param theme Character: theme name or list of theme parameters
#' @param palette Character: palette name, or list of colors
#' @param filename Character: filename to save plot to
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' time <- sample(seq(as.Date("2020-03-01"), as.Date("2020-09-23"), length.out = 140))
#' x1 <- rnorm(140)
#' x2 <- rnorm(140, 1, 1.2)
#' # Single timeseries
#' dplot3.ts(x1, time)
#' # Multiple timeseries input as list
#' dplot3.ts(list(Alpha = x1, Beta = x2), time)
#' # Multiple timeseries input as single vector grouped by group
#' x <- c(x1, x2)
#' group <- rep(c("Alpha", "Beta"), each = 140)
#' dplot3.ts(x, time, group = group)
#' }
#'
dplot3.ts <- function(x, time,
                      window = 7,
                      group = NULL,
                      roll.fn = c("mean", "median", "max", "none"),
                      roll.col = NULL,
                      roll.alpha = 1,
                      roll.lwd = 2,
                      roll.name = NULL,
                      alpha = NULL,
                      align = "center",
                      # use = "data.table",
                      xlab = "Time",
                      n.xticks = 12,
                      tickmode = "array",
                      scatter.type = "scatter",
                      legend = TRUE,
                      x.showspikes = TRUE,
                      y.showspikes = FALSE,
                      spikedash = "solid",
                      spikemode = "across",
                      spikesnap = "hovered data",
                      spikecolor = NULL,
                      spikethickness = 1,
                      displayModeBar = TRUE,
                      theme = getOption("rt.theme"),
                      palette = getOption("rt.palette", "rtCol1"),
                      filename = NULL, ...) {

    # Arguments ====
    roll.fn <- match.arg(roll.fn)
    if (roll.fn == "none") window <- NULL

    # Timeseries ====
    if (!is.null(group)) {
        x <- split(x, group)
        time <- split(time, group)
    }
    
    if (is.data.frame(x)) {
        x <- as.list(x)
    } 
    
    if (!is.list(x)) {
        x <- list(x)
    }

    if (is.data.frame(time)) {
        time <- as.list(time)
    }
    
    if (!is.list(time)) {
        time <- list(time)
    }

    idx <- lapply(time, order)
    time <- lapply(seq_along(time), \(i) time[[i]][idx[[i]]])
    if (length(time) < length(x)) {
        time <- rep(time, length(x) / length(time))
        idx <- rep(idx, length(x) / length(idx))
    }
    x <- lapply(seq_along(x), \(i) x[[i]][idx[[i]]])
    # xtl <- lapply(seq_along(x), \(i) zoo::zoo(x[[i]], time[[i]]))

    if (!is.null(window) && window > 0) {
        avg_line <- switch(roll.fn,
            mean = lapply(x, \(xt) data.table::frollmean(xt, window, align = align)),
            median = lapply(x, \(xt) data.table::frollapply(xt, window, median, align = align)),
            max = lapply(x, \(xt) data.table::frollapply(xt, window, max, align = align)),
            sum = lapply(x, \(xt) data.table::frollsum(xt, window, align = align))
        )
    }

    # Palette ====
    if (is.character(palette)) palette <- rtPalette(palette)
    if (is.null(roll.col)) roll.col <- palette[seq_along(x)]

    # dplot3.xy ====
    plt <- dplot3.xy(time, x,
        xlab = xlab,
        theme = theme,
        palette = palette,
        alpha = alpha,
        legend = legend,
        scatter.type = scatter.type,
        x.showspikes = x.showspikes,
        y.showspikes = y.showspikes,
        spikedash = spikedash,
        spikemode = spikemode,
        spikesnap = spikesnap,
        spikecolor = spikecolor,
        spikethickness = spikethickness, ...
    )

    # Rolling function line ====
    if (is.null(roll.name)) {
        roll.name <- paste0("Rolling ", roll.fn, " (window=", window, ")")
    }

    if (!is.null(window)) {
        for (i in seq_along(x)) {
            plt |> plotly::add_trace(
                x = time[[i]], y = avg_line[[i]],
                type = "scatter",
                mode = "lines",
                line = list(
                    color = plotly::toRGB(roll.col[[i]], alpha = roll.alpha),
                    width = roll.lwd
                ),
                name = roll.name
            ) -> plt
        }
    }

    # Config
    plt <- plotly::config(plt,
        displaylogo = FALSE,
        displayModeBar = displayModeBar
    )

    # Write to file ====
    if (!is.null(filename)) {
        filename <- file.path(filename)
        plotly::plotly_IMAGE(plt,
            width = file.width, height = file.height,
            format = tools::file_ext(filename), out_file = filename
        )
    }

    plt
} # rtemis::dplot3.ts
