# mplot3_surv.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' `mplot3`: Survival Plots
#'
#' Plots survival step functions using [mplot3_xy]
#'
#' @inheritParams mplot3_xy
#' @param x Survival object / list of Survival objects created using `survival::Surv`
#' @param normalize.time Logical: If TRUE, convert each input's time to 0-1 range. This is useful
#' when survival estimates are not provided in original time scale. Default = FALSE.
#' @param lty Integer: Line type. Default = 1. See `par("lty")`
#' @param lwd Float: Line width. Default = 2
#' @param alpha Float: Alpha for lines. Default = 1
#' @param ... Additional arguments to pass to [mplot3_xy]
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' library(survival)
#' mplot3_surv(Surv(time = lung$time, event = lung$status))
#' }

mplot3_surv <- function(
  x,
  lty = 1,
  lwd = 2,
  alpha = 1,
  col = NULL,
  mark.censored = TRUE,
  normalize.time = FALSE,
  cex = 1.2,
  xlab = NULL,
  ylab = "Survival",
  main = "Kaplan-Meier curve",
  theme = rtTheme,
  palette = rtPalette,
  plot.error = FALSE,
  error.lty = 2,
  error.alpha = .5,
  group.legend = NULL,
  group.title = "",
  group.names = NULL,
  group.side = 3,
  group.adj = .98,
  group.padj = 2,
  group.at = NA,
  par.reset = TRUE,
  ...
) {
  # Data ----
  if (class(x)[1] != "list") x <- list(x)
  # x <- lapply(1:length(x), function(i) as.numeric(as.matrix(x[[i]])[, 1]))
  # if (class(x)[[1]] != "Surv") stop("At least first object must be of type Survival")
  for (i in seq(x)) {
    # if (class(x[[i]]) != "Surv") {
    if (!inherits(x[[i]], "Surv")) {
      # Adding 1's assuming time to event for all
      x[[i]] <- survival::Surv(x[[i]], rep(1, length(x[[i]])))
    }
  }

  # Theme ----
  if (is.null(col)) {
    if (is.character(palette)) palette <- rtpalette(palette)
    col <- palette
  }

  # Kaplan-Meier Estimate ----
  .survfit <- lapply(x, function(i) survival::survfit(i ~ 1))

  # Limits ----
  xl <- lapply(.survfit, function(i) i$time)
  if (normalize.time) xl <- lapply(xl, drange)
  xlim <- range(unlist(xl))
  yl <- lapply(.survfit, function(i) i$surv)

  # Plot ----
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  if (is.null(xlab)) {
    xlab <- if (normalize.time) "Normalized Time" else "Time"
  }

  mplot3_xy(
    x = xl,
    y = yl,
    ylim = c(0, 1),
    xlim = xlim,
    type = 's',
    lwd = 2,
    lty = lty,
    theme = theme,
    palette = palette,
    marker.alpha = alpha,
    marker.col = col,
    line.alpha = 1,
    main = main,
    xlab = xlab,
    ylab = ylab,
    group.legend = FALSE,
    zerolines = FALSE,
    par.reset = FALSE,
    ...
  )

  # Censoring markers
  if (mark.censored) {
    for (i in seq_along(xl)) {
      .index <- .survfit[[i]]$n.censor == 1
      points(xl[[i]][.index], yl[[i]][.index], pch = 3, col = palette[[i]])
    }
  }

  # pointwise errors
  if (plot.error) {
    for (i in seq(xl)) {
      lines(
        .survfit[[i]]$time,
        .survfit[[i]]$upper,
        lty = error.lty,
        col = colorAdjust(col[[i]], error.alpha)
      )
      lines(
        .survfit[[i]]$time,
        .survfit[[i]]$lower,
        lty = error.lty,
        col = colorAdjust(col[[i]], error.alpha)
      )
    }
  }

  # Group Legend ----
  if (!is.null(group.names)) {
    group.names <- c(group.title, group.names)
  } else {
    if (!is.null(names(x))) {
      group.names <- c(group.title, names(x))
    } else {
      group.names <- c(group.title, paste(" ", toupper(letters[seq_along(x)])))
    }
  }

  # If not defined, group legend defaults to TRUE, if more than one group
  if (is.null(group.legend)) group.legend <- ifelse(length(x) > 1, TRUE, FALSE)

  if (group.legend) {
    mtext(
      group.names,
      col = c("black", unlist(col))[1:(length(x) + 1)], # change black depending on theme
      side = group.side,
      adj = group.adj,
      at = group.at,
      cex = cex,
      padj = seq(group.padj, group.padj + 1.5 * length(x), 1.5)
    )
  }
} # rtemis::mplot3_surv
