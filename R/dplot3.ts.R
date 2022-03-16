# dplot3.ts.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Interactive Timeseries Plots
#' 
#' Draw interactive timeseries plots using \code{plotly}
#' 
#' @param x Numeric vector of values to plot
#' @param time Numeric or Date vector of time corresponding to values of \code{x}
#' @param roll.fn Character: "mean", "median", "max", or "sum": Function to apply on rolling windows of \code{x}
#' @param roll.col Color for rolling line
#' @param roll.alpha Numeric: transparency for rolling line
#' @param roll.lwd Numeric: width of rolling line
#' @param align Character: "center", "right", or "left"
#' @param use Character: "data.table" or "zoo": which package to use to apply
#' rolling function
#' @param xlab Character: x-axis label
#' @param n.xticks Integer: number of x-axis ticks to use (approximately)
#' @param theme Character: theme name or list of theme parameters
#' @param palette Character: palette name, or list of colors
#' 
#' @author E.D. Gennatas
#' @export

dplot3.ts <- function(x, time,
                window = 7,
                roll.fn = c("mean", "median", "max", "sum"),
                roll.col = NULL,
                roll.alpha = 1,
                roll.lwd = 2,
                roll.name = NULL,
                alpha = NULL,
                align = "center",
                use = "data.table",
                xlab = "Time",
                n.xticks = 12,
                tickmode = "array",
                scatter.type = "scattergl",
                legend = TRUE,
                theme = getOption("rt.theme"),
                palette = getOption("rt.palette", "rtCol1"), ...) {
    
    # Arguments ====
    roll.fn <- match.arg(roll.fn)

    # Palette ====
    if (is.character(palette)) palette <- rtPalette(palette)
    if (is.null(roll.col)) roll.col <- palette[[1]]

    # Timeseries ====
    xt <- zoo::zoo(x, time)
    if (!is.null(window)) {
        if (use == "data.table") {
            avg_line <- switch(roll.fn,
                mean = data.table::frollmean(xt, window, align = align),
                median = data.table::frollapply(xt, window, median, align = align),
                max = data.table::frollapply(xt, window, max, align = align),
                sum = data.table::frollsum(xt, window, align = align))
        } else {
            avg_line <- switch(roll.fn,
                mean = zoo::rollmean(xt, window, align = align),
                median = zoo::rollmedian(xt, window, align = align),
                max = zoo::rollmax(xt, window, align = align),
                sum = zoo::rollsum(xt, window, align = align))
        }
    }
    
    # dplot3.xy ====
    linefmt <- list(color = plotly::toRGB(roll.col, alpha = roll.alpha),
                width = roll.lwd)
    plt <- dplot3.xy(seq_along(time), x, 
                xlab = xlab,
                theme = theme, 
                palette = palette,
                alpha = alpha,
                legend = legend,
                scatter.type = scatter.type, ...)
    
    # Rolling function line ====
    if (is.null(roll.name)) {
        roll.name <- paste0(window, "-unit rolling ", roll.fn)
    }
    if (!is.null(window)) {
        if (use == "data.table") {
            plt |> plotly::add_trace(x = seq_along(x), y = avg_line,
                    type = "scatter",
                    mode = "lines",
                    line = linefmt,
                    name = roll.name) -> plt
        } else {
            x_avg <- match(index(avg_line), index(xt))
            plt |> plotly::add_trace(x = x_avg, y = avg_line,
                    type = "scatter",
                    mode = "lines",
                    line = linefmt,
                    name = roll.name)  -> plt
        }
    }
    
    # Tick labels
    # ticks not in idx will not show correct labels on hover
    idx <- c(TRUE, rep(FALSE, floor(length(x)/n.xticks)))
    plt |> plotly::layout(
              xaxis = list(
                  ticktext = index(xt)[idx],
                  tickvals = seq_along(xt)[idx],
                  tickmode = tickmode)) -> plt
    # cannot (?) show only select ticks / ticks every some interval, 
    # dtick doesn't work with custom ticktext and tickvals
    # plt |> plotly::layout(
    #             xaxis = list(
    #                 # dtick = floor(length(xt) / n.xticks),
    #                 dtick = 100,
    #                 ticktext = index(xt),
    #                 tickvals = seq_along(xt),
    #                 # tickmode = "array"
    #                 tickmode = tickmode
    #             )) -> plt
    plt

} # rtemis::dplot3.ts
