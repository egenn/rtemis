# utils_color.R
# ::rtemis::
# 2016- EDG

#' Simple Color Operations
#'
#' Invert a color or calculate the mean of two colors in HSV or RGB space.
#' This may be useful in creating colors for plots
#'
#' The average of two colors in RGB space will often pass through gray,
#' which is likely undesirable. Averaging in HSV space, better for most applications.
#' @param col Input color(s)
#' @param fn Character: "invert", "mean": Function to perform
#' @param space Character: "HSV", "RGB": Colorspace to operate in - for
#' averaging only
#'
#' @return Color
#' @author EDG
#' @export

color_op <- function(col,
                     fn = c("invert", "mean"),
                     space = c("HSV", "RGB")) {
  # Arguments ----
  fn <- match.arg(fn)
  space <- match.arg(space)

  # Colors ----
  col <- as.list(col)
  col.rgb <- col2rgb(col, alpha = TRUE)

  if (fn == "invert") {
    inverted <- apply(col.rgb, 2, \(i) 255 - i)
    # maintain alpha
    inverted[4, ] <- col.rgb[4, ]
    invertedl <- lapply(seq_len(NCOL(inverted)), \(i) {
      rgb(inverted[1, i],
        inverted[2, i],
        inverted[3, i],
        inverted[4, i],
        maxColorValue = 255
      )
    })
    if (!is.null(names(col))) names(invertedl) <- paste0(names(col), ".invert")
    return(invertedl)
  } else if (fn == "mean") {
    if (length(col) < 2) stop("Need at least two colors to average")
    if (space == "RGB") {
      averaged <- rowMeans(col.rgb)
      averaged <- rgb(averaged[1], averaged[2], averaged[3], averaged[4], maxColorValue = 255)
      return(list(average = averaged))
    } else if (space == "HSV") {
      # Convert HSV to RGB
      col.hsv <- rgb2hsv(col.rgb[1:3, ])
      # Get mean HSV values
      averaged <- rowMeans(col.hsv)
      # Get mean alpha from RGB
      alpha <- mean(col.rgb[4, ])
      # Turn to hex
      averaged <- hsv(averaged[1], averaged[2], averaged[3], alpha / 255)
      return(averaged)
    }
  }
} # rtemis::color_op

#' Squared Color Distance
#'
#' Get the squared RGB distance between two colors
#'
#' @param x Color
#' @param y Color
#'
#' @author EDG
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' color_sqdist("red", "green")
#' color_sqdist("#16A0AC", "#FA6E1E")
#' }
color_sqdist <- function(x, y) {
  x.rgb <- col2rgb(x)
  y.rgb <- col2rgb(y)

  sum((x.rgb - y.rgb)^2)
} # rtemis::color_sqdist

#' Order colors
#'
#' Order colors by RGB distance
#'
#' @param x Vector of colors
#' @param start_with Integer: Which color to output in first position
#' @param order_by Character: "similarity" or "dissimilarity"
#'
#' @author EDG
#' @keywords internal
color_order <- function(x, start_with = 1, order_by = c("similarity", "dissimilarity")) {
  order_by <- match.arg(order_by)
  if (!is.integer(start_with)) start_with <- which(x == start_with)
  fn <- switch(order_by,
    similarity = which.min,
    dissimilarity = which.max
  )
  out <- x[start_with]
  x <- x[-start_with]
  while (length(x) > 1) {
    id <- fn(sapply(x, \(i) color_sqdist(rev(out)[1], i)))
    out <- c(out, x[id])
    x <- x[-id]
  }
  c(out, x)
} # rtemis::color_order

#' Separate colors
#'
#' Separate colors by RGB distance
#'
#' Starting with the first color defined by `start_with`, the next color
#' is chosen to be max distance from all preceding colors
#'
#' @param x Vector of colors
#' @param start_with Integer: Which color to output in first position
#'
#' @author EDG
#' @export

color_separate <- function(x, start_with = 1) {
  if (!is.integer(start_with)) start_with <- which(x == start_with)
  out <- start_with
  dist <- outer(x, x, Vectorize(color_sqdist))
  colnames(dist) <- seq_along(x)
  out <- c(out, as.numeric(colnames(dist)[which.max(dist[out, ])]))
  dist <- dist[, -out, drop = FALSE]
  while (length(out) < length(x)) {
    id <- which.max(colSums(dist[out, , drop = FALSE]))
    out <- c(out, as.numeric(colnames(dist)[id]))
    dist <- dist[, -id, drop = FALSE]
  }
  x[out]
}

#' Color to Grayscale
#'
#' Convert a color to grayscale
#'
#' Uses the NTSC grayscale conversion:
#' 0.299 * R + 0.587 * G + 0.114 * B
#'
#' @param x Color to convert to grayscale
#' @param what Character: "color" returns a hexadecimal color,
#' "decimal" returns a decimal between 0 and 1
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' col2grayscale("red")
#' col2grayscale("red", "dec")
#' }
col2grayscale <- function(x,
                          what = c("color", "decimal")) {
  what <- match.arg(what)
  col <- col2rgb(x)
  gs <- (0.299 * col[1, ] + 0.587 * col[2, ] + 0.114 * col[3, ]) / 255
  if (what == "color") {
    gray(gs)
  } else {
    gs
  }
} # col2grayscale


#' Palettize colors
#'
#' Filter and order a set of colors to produce a palette suitable for
#' multicolor plots
#'
#' @param x Color vector
#' @param grayscale_hicut Numeric: exclude colors whose grayscale equivalent
#' is greater than this value
#' @param start_with Integer or color: For integer, start with this color out
#' of `x`, otherwise find color `x` closer to this color and place it
#' first
#' @param order_by Character: "separation", "dissimilarity", "similarity"
#'
#' @author EDG
#' @export
palettize <- function(x,
                      grayscale_hicut = .8,
                      start_with = "#16A0AC",
                      order_by = c("separation", "dissimilarity", "similarity")) {
  order_by <- match.arg(order_by)
  x <- unlist(x)
  if (!is.integer(start_with)) {
    start_with <- x[which.min(Vectorize(color_sqdist)(x, start_with))]
  }
  if (!is.null(grayscale_hicut)) {
    xgray <- col2grayscale(x, "dec")
    xf <- x[xgray < grayscale_hicut]
  } else {
    xf <- x
  }

  switch(order_by,
    separation = color_separate(xf, start_with),
    dissimilarity = color_order(xf, start_with, "dissimilarity"),
    similarity = color_order(xf, start_with, "similarity")
  )
} # palettize

#' Invert Color in RGB space
#'
#' @param x Color, vector
#'
#' @returns Inverted colors using hexadecimal notation #RRGGBBAA
#' 
#' @author EDG
#' @export
#' 
#' @examples
#' \dontrun{
#' cols <- c("red", "green", "blue")
#' previewcolor(cols)
#' cols |>
#'   color_invertRGB() |>
#'   previewcolor()
#' }
color_invertRGB <- function(x) {
  col <- as.list(x)
  col_rgb <- col2rgb(col, alpha = TRUE)
  inverted <- apply(col_rgb, 2, \(i) 255 - i)
  # maintain alpha
  inverted[4, ] <- col_rgb[4, ]
  invertedl <- sapply(seq_len(NCOL(inverted)), \(i) {
    rgb(inverted[1, i],
      inverted[2, i],
      inverted[3, i],
      inverted[4, i],
      maxColorValue = 255
    )
  })
  if (!is.null(names(col))) names(invertedl) <- paste0(names(col), ".invert")
  invertedl
} # rtemis::color_invertRGB



#' Average colors
#'
#' @param x Color vector
#' @param space Character: RGB  or HSV; space to average in
#'
#' @author EDG
#' @export
#' 
#' @examples
#' \dontrun{
#' color_mean(c("red", "blue")) |> previewcolor()
#' color_mean(c("red", "blue"), "HSV") |> previewcolor()
#' }
color_mean <- function(x,
                       space = c("RGB", "HSV")) {
  if (length(x) < 2) stop("Need at least two colors to average")
  space <- match.arg(space)

  col <- as.list(x)
  col.rgb <- col2rgb(x, alpha = TRUE)

  if (space == "RGB") {
    averaged <- rowMeans(col.rgb)
    averaged <- rgb(
      averaged[1], averaged[2], averaged[3], averaged[4],
      maxColorValue = 255
    )
  } else if (space == "HSV") {
    # Convert HSV to RGB
    col.hsv <- rgb2hsv(col.rgb[1:3, ])
    # Get mean HSV values
    averaged <- rowMeans(col.hsv)
    # Get mean alpha from RGB
    alpha <- mean(col.rgb[4, ])
    # Turn to hex
    averaged <- hsv(averaged[1], averaged[2], averaged[3], alpha / 255)
  }
  averaged
} # rtemis::color_mean

#' Fade color towards target
#'
#' @param x Color source
#' @param to Target color
#' @param pct Numeric (0, 1) fraction of the distance in RGBA space between
#' `x` and `to` to move. e.g. .5 gets the mean RGBA value of the two
#'
#' @returns Color in hex notation
#' @author EDG
#' @export

color_fade <- function(x, to = "#000000", pct = .5) {
  col <- col2rgb(x, alpha = TRUE)
  col2 <- col2rgb(to, alpha = TRUE)
  d <- (col2 - col) * pct
  colf <- (col + d) / 255
  rgb(colf[1], colf[2], colf[3], colf[4])
}

#' Pastelify a color (make a color more pastel)
#'
#' Lower a color's saturation by a given percent in the HSV color system
#'
#' @param color Color, vector: Color(s) to operate on
#' @param s Float: Decrease saturation by this fraction. Default = .3, which means if saturation of given color is 1,
#' it will become .7
#'
#' @return List of adjusted colors
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' color <- c("red", "green", "blue")
#' color.p <- desaturate(color)
#' }
desaturate <- function(color, s = .3) {
  # Infer color names, if available
  if (is.character(color)) {
    .names <- color
  } else if (!is.null(names(color))) {
    .names <- names(color)
  } else {
    .names <- NULL
  }

  x <- as.list(color)
  x <- lapply(x, col2rgb)
  x <- lapply(x, rgb2hsv)
  xp <- lapply(x, function(i) {
    .s <- i[2]
    i[2] <- .s - (.s * s)
    hsv(i[1], i[2], i[3])
  })

  names(xp) <- .names
  xp
} # rtemis::desaturate



#' Convert R color to hexadecimal code
#'
#' Convert a color that R understands into the corresponding hexadecimal code
#'
#' @param color Color(s) that R understands
#'
#' @author EDG
#' @export
#' 
#' @examples
#' \dontrun{
#' col2hex(c("gray50", "skyblue"))
#' }
col2hex <- function(color) {
  .rgb <- col2rgb(color)
  sapply(seq_along(color), function(i) {
    paste0("#", paste0(sprintf(
      "%02s",
      c(
        as.character(as.hexmode(.rgb[1, i])),
        as.character(as.hexmode(.rgb[2, i])),
        as.character(as.hexmode(.rgb[3, i]))
      )
    ), collapse = ""))
  })
} # rtemis::col2hex


#' Adjust HSV Color
#'
#' Modify alpha, hue, saturation and value (HSV) of a color
#'
#' @param color Input color. Any format that grDevices::col2rgb() recognizes
#' @param alpha Numeric: Scale alpha by this amount. Future: replace with absolute setting
#' @param hue Float: How much hue to add to `color`
#' @param sat Float: How much saturation to add to `color`
#' @param val Float: How much to increase value of `color` by
#' @return Adjusted color
#' @author EDG
#' @export

color_adjust <- function(color,
                         alpha = NULL,
                         hue = 0,
                         sat = 0,
                         val = 0) {
  ac <- color
  # HSV ----
  ac.hsv <- grDevices::rgb2hsv(grDevices::col2rgb(ac))
  ac <- grDevices::hsv(ac.hsv[1] + hue, ac.hsv[2] + sat, ac.hsv[3] + val)
  # alpha ----
  if (!is.null(alpha)) ac <- adjustcolor(color, alpha.f = alpha)
  ac
} # rtemis::color_adjust

#' Create an alternating sequence of graded colors
#'
#' @param color List: List of two or more elements, each containing two colors.
#' A gradient will be created from the first to the second color of each element
#' @param n Integer: Number of steps in each gradient.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' color <- list(
#'   blue = c("#82afd3", "#000f3a"),
#'   gray = c("gray10", "gray85")
#' )
#' previewcolor(desaturate(color_mix(color, 6), .3))
#'
#' color <- list(
#'   blue = c("#82afd3", "#57000a"),
#'   gray = c("gray10", "gray85")
#' )
#' previewcolor(desaturate(color_mix(color, 6), .3))
#'
#' color <- list(
#'   blue = c("#82afd3", "#000f3a"),
#'   purple = c("#23001f", "#c480c1")
#' )
#' previewcolor(desaturate(color_mix(color, 5), .3))
#' }
color_mix <- function(color, n = 4) {
  if (class(color)[1] != "list") stop("Please provide list of color pairs")

  color.grad <- lapply(color, function(i) colorRampPalette(i)(n))

  c(t(as.data.frame(color.grad)))
} # rtemis::color_mix


#' Preview color
#'
#' Preview one or multiple colors using little rhombi with their little labels up top
#'
#' @param x Color, vector: One or more colors that R understands
#' @param main Character: Title. Default = NULL, which results in
#' `deparse(substitute(x))`
#' @param bg Background color.
#' @param main.col Color: Title color
#' @param main.x Float: x coordinate for `main`.
#' @param main.y Float: y coordinate for `main`.
#' @param main.adj Float: `adj` argument to mtext for `main`.
#' @param main.cex Float: character expansion factor for `main`.
#' @param main.font Integer, 1 or 2: Weight of `main` 1: regular, 2: bold.
#' @param width Float: Plot width. Default = NULL, i.e. set automatically
#' @param xlim Vector, length 2: x-axis limits. Default = NULL, i.e. set automatically
#' @param ylim Vector, length 2: y-axis limits.
#' @param asp Float: Plot aspect ratio.
#' @param labels.y Float: y coord for labels. Default = 1.55 (rhombi are fixed and range y .5 - 1.5)
#' @param label.cex Float: Character expansion for labels. Default = NULL, and is
#' calculated automatically based on length of `x`
#' @param mar Numeric vector, length 4: margin size.
#' @param par.reset Logical: If TRUE, reset `par` settings on exit.
#' @param filename Character: Path to save plot as PDF.
#' @param pdf.width Numeric: Width of PDF in inches.
#' @param pdf.height Numeric: Height of PDF in inches.
#'
#' @return Nothing, prints plot.
#' 
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' colors <- colorgradient.x(seq(-5, 5))
#' previewcolor(colors)
#' }
previewcolor <- function(x,
                         main = NULL,
                         bg = "#333333",
                         main.col = "#b3b3b3",
                         main.x = .7,
                         main.y = 0.2,
                         main.adj = 0,
                         main.cex = .9,
                         main.font = 1,
                         width = NULL,
                         xlim = NULL,
                         ylim = c(0, 2.2),
                         asp = 1,
                         labels.y = 1.55,
                         label.cex = NULL,
                         mar = c(0, 0, 0, 1),
                         par.reset = TRUE,
                         filename = NULL,
                         pdf.width = 8,
                         pdf.height = 2.5) {
  if (is.null(main)) main <- deparse(substitute(x))
  x <- unlist(x)
  if (par.reset) {
    .par <- par(no.readonly = TRUE)
    on.exit(par(.par))
  }

  if (is.null(width)) width <- max(3, .3 * length(x))
  if (is.null(xlim)) xlim <- c(0.3, width + .7)
  if (!is.null(filename)) pdf(filename, pdf.width, pdf.height)
  par(bg = bg, xaxs = "i", yaxs = "i", mar = mar, oma = c(0, 0, 0, 0))

  # Plot ----
  plot(NULL, NULL,
    axes = FALSE,
    xlim = xlim, ylim = ylim, xlab = NA, ylab = NA,
    asp = asp
  )

  if (length(x) >= 3) {
    xmid <- seq(1, width, length.out = length(x))
  } else if (length(x) == 2) {
    xmid <- c(.3333 * width, .6666 * width) + .5
  } else {
    xmid <- .5 * width + .5
  }

  for (i in seq(x)) rhombus(xmid[i], 1, col = x[i])

  # '- Labels ----
  # ncolors => label.cex
  # 100, .4
  # 10, 1.2
  # 4, 1.3
  # lm(c(.4, 1.2, 1.3) ~ c(100, 10, 4))

  if (is.null(label.cex)) {
    # label.cex <- max(.1, 1.30 - .02 * length(x))
    label.cex <- 1.30 - .02 * length(x)
    # label.cex <- max(.1, 1.34167 - .01042 * length(x))
    label.cex <- 1.314869 - 0.009163 * length(x)
  }

  if (is.null(names(x))) {
    labels <- as.character(x)
  } else {
    labels <- names(x)
  }
  text(xmid + .1, labels.y, labels,
    col = x, srt = 45, adj = 0,
    offset = 0, cex = label.cex,
    xpd = TRUE
  )

  # '- Title ----
  if (!is.null(main)) {
    text(main.x, main.y, main,
      col = main.col, adj = main.adj,
      font = main.font, cex = main.cex
    )
  }

  if (!is.null(filename)) dev.off()
} # rtemis::previewcolor


rhombus <- function(xmid = 1, ymid = 1, width = 1, height = 1, col = "#80FFFF") {
  # left, top, right, bottom
  hw <- .5 * width
  hh <- .5 * height
  polygon(
    x = c(xmid - hw, xmid, xmid + hw, xmid),
    y = c(ymid, ymid + hh, ymid, ymid - hh),
    col = col, border = NA
  )
}


#' Color gradient for continuous variable
#'
#' @param x Float, vector
#' @param symmetric Logical: If TRUE, make symmetric gradient between
#' `-max(abs(x))` and `max(abs(x))`
#' @param lo.col Low color
#' @param mid.col Middle color
#' @param hi.col High color
#' @param space Character: "rgb" or "Lab".
#' 
#' @author EDG
#' @export
#' 
#' @examples
#' \dontrun{
#' x <- seq(-10, 10, length.out = 51)
#' previewcolor(colorgradient.x(x))
#' x <- sort(rnorm(40))
#' previewcolor(colorgradient.x(x, mid.col = "white"))
#' # Notice how most values are near zero therefore almost white
#' }
colorgradient.x <- function(x,
                            symmetric = FALSE,
                            lo.col = "#0290EE",
                            mid.col = "#1A1A1A",
                            hi.col = "#FFBD4F",
                            space = "Lab") {
  grad <- colorRampPalette(c(lo.col, mid.col, hi.col), space = space)(201)

  if (symmetric) {
    maxabsx <- max(abs(x))
    cuts <- cut(c(-maxabsx, x, maxabsx), 201, labels = FALSE)[-c(1, length(x) + 2)]
  } else {
    cuts <- cut(x, 201, labels = FALSE)
  }

  grad[cuts]
} # rtemis::colorgradient.x


# colorgrad.R
# ::rtemis::
# 2016 EDG rtemis.org

#' Color Gradient
#'
#' Create a gradient of colors and optionally a colorbar
#'
#' It is best to provide an odd number, so that there is always an equal number of colors on either side
#' of the midpoint.
#' For example, if you want a gradient from -1 to 1 or equivalent, an n = 11, will give 5 colors on either
#' side of 0, each representing a 20\% change from the next.
#'
#' `colors` can be defined as a sequence of 3-letter color abbreviations of 2, 3, 4, or 5 colors
#'   which will correspond to values: \{"lo","hi"\}; \{"lo", "mid", "hi"\}; \{"lo", "mid", "midhi", "hi"\}, and
#'   \{"lo", "lomid", "mid", "midhi", "hi"\}, respectively.
#'   For example, try `colorgrad(21, "blugrnblkredyel", colorbar = TRUE)`
#'   3-letter color abbreviations:
#'    wht: white; blk: black; red; grn: green; blu: blue; yel: yellow; rng: orange; prl: purple
#'
#' @param n Integer: How many distinct colors you want. If not odd, converted to `n + 1`
#'   Defaults to 21
#' @param colors Character: Acts as a shortcut to defining `lo`, `mid`, etc for a number of defaults:
#'   "french", "penn", "grnblkred",
#' @param space Character: Which colorspace to use. Option: "rgb", or "Lab".
#'   Recommendation: If `mid` is "white" or "black" (default), use "rgb", otherwise "Lab"
#' @param lo Color for low end
#' @param lomid Color for low-mid
#' @param mid Color for middle of the range or "mean", which will result in `color_op(c(lo, hi), "mean")`.
#' If `mid = NA`, then only `lo` and `hi` are used to create the color gradient.
#' @param midhi Color for middle-high
#' @param hi Color for high end
#' @param preview Logical: Plot the colors horizontally
#' @param colorbar Logical: Create a vertical colorbar
#' @param cb.n Integer: How many steps you would like in the colorbar
#' @param cb.mar Vector, length 4: Colorbar margins. Default: c(1, 1, 1, 1)
#' @param cb.add Logical: If TRUE, colorbar will be added to existing plot
#' @param cb.add.mar Vector: Margins for colorbar (See `par("mar")`)
#' @param cb.axis.pos Float: Position of axis (See `axis("pos")`)
#' @param cb.axis.las Integer {0,1,2,3}: Style of axis labels. 0: Always parallel to the axis,
#' 1: Horizontal, 2: Perpendicular, 3: Vertical.
#' @param cb.axis.hadj Float: Adjustment parallel to the reading direction (See `par("adj")`)
#' @param cb.cex FLoat: Character expansion factor for colorbar (See `par("cex")`)
#' @param bar.min Numeric: Lowest value in colorbar
#' @param bar.mid Numeric: Middle value in colorbar
#' @param bar.max Numeric: Max value in colorbar
#' @param cex Float: Character expansion for axis
#' @param filename String (Optional: Path to file to save colorbar
#' @param pdf.width Float: Width for PDF output.
#' @param pdf.height Float: Height for PDF output.
#' @param theme Character: "light", "dark"
#' @param bg Color: Background color
#' @param col.text Color: Colorbar text color
#' @param plotlycb Logical: Create colorbar using `plotly` (instead of base R graphics)
#' @param plotly.width Float: Width for plotly colorbar.
#' @param plotly.height Float: Height for plotly colorbar.
#' @param rtrn.plotly Logical: If TRUE, return `plotly` object
#' @param margins Vector: Plotly margins.
#' @param pad Float: Padding for `plotly`.
#' @param par.reset Logical: If TRUE (Default), reset `par` settings after running
#' @return Invisible vector of hexadecimal colors / plotly object if `rtrn.plotly = TRUE`
#' @author EDG
#' @export

colorgrad <- function(n = 21,
                      colors = NULL,
                      space = c("rgb", "Lab"),
                      lo = "#18A3AC",
                      lomid = NULL,
                      mid = NULL,
                      midhi = NULL,
                      hi = "#F48024",
                      preview = FALSE,
                      colorbar = FALSE,
                      cb.n = 21,
                      cb.mar = c(1, 1, 1, 1),
                      cb.add = FALSE,
                      cb.add.mar = c(5, 0, 2, 5),
                      cb.axis.pos = 1.1,
                      cb.axis.las = 1,
                      cb.axis.hadj = 0,
                      cb.cex = 6,
                      bar.min = -1,
                      bar.mid = 0,
                      bar.max = 1,
                      cex = 1.2,
                      filename = NULL,
                      pdf.width = 3,
                      pdf.height = 7,
                      theme = getOption("rt.theme", "light"),
                      bg = NULL,
                      col.text = NULL,
                      plotlycb = FALSE,
                      plotly.width = 80,
                      plotly.height = 500,
                      rtrn.plotly = FALSE,
                      margins = c(0, 0, 0, 0),
                      pad = 0,
                      par.reset = TRUE) {
  # Arguments ----
  n <- as.integer(n)
  if (n %% 2 != 1) n <- n + 1
  if (!is.null(filename)) colorbar <- TRUE
  if (rtrn.plotly) plotlycb <- TRUE
  if (is.null(cb.n)) {
    cb.n <- n
    if (cb.n %% 2 != 1) cb.n <- cb.n + 1
  }
  space <- match.arg(space)
  theme <- if (strtrim(theme, 4) == "dark") "dark" else "light"

  # Colors ----
  if (!is.null(colors)) {
    if (colors == "french") {
      lo <- "#01256E"
      lomid <- NULL
      mid <- "white"
      midhi <- NULL
      hi <- "#95001A"
    } else if (colors == "penn") {
      lo <- "#02CFFF"
      lomid <- NULL
      mid <- "#01256E"
      midhi <- "#95001A"
      hi <- "#F2C100"
    } else if (colors == "blues") {
      lo <- "#01256E"
      mid <- NULL
      hi <- "#82AFD3"
    } else if (colors == "greens") {
      lo <- "#005200"
      mid <- NULL
      hi <- "#80DF80"
    } else {
      cols <- colorvec(cols = colors)
      lo <- cols$lo
      lomid <- cols$lomid
      mid <- cols$mid
      midhi <- cols$midhi
      hi <- cols$hi
    }
  }

  # Grad ----
  n <- as.integer(n)
  midpoint <- ceiling(n / 2)
  if (is.null(mid)) mid <- ifelse(theme == "light", "white", "black")
  if (!is.na(mid)) {
    if (mid == "mean") mid <- color_op(c(lo, hi), "mean")
    lo2mid <- colorRampPalette(c(lo, lomid, mid), space = space)
    mid2hi <- colorRampPalette(c(mid, midhi, hi), space = space)
    grad <- c(lo2mid(midpoint), mid2hi(n - midpoint + 1)[-1])
  } else {
    grad <- colorRampPalette(c(lo, hi), space = space)(n)
  }

  if (cb.n != n) {
    cb.n <- as.integer(cb.n)
    cb.midpoint <- ceiling(cb.n / 2)
    # if (is.null(mid)) mid <- color_op(c(lo, hi), "mean")
    # lo2mid <- grDevices::colorRampPalette(c(lo, lomid, mid), space = space)
    # mid2hi <- grDevices::colorRampPalette(c(mid, midhi, hi), space = space)
    if (!is.na(mid)) {
      cb.grad <- c(lo2mid(cb.midpoint), mid2hi(cb.n - cb.midpoint + 1)[-1])
    } else {
      cb.grad <- colorRampPalette(c(lo, hi), space = space)(cb.n)
    }
  } else {
    cb.grad <- grad
    cb.midpoint <- midpoint
  }

  # [ Preview ] ----
  if (preview) {
    plot(rep(1, n),
      col = grad, pch = 19, cex = 6,
      xlim = c(0.5, n + .5), ylim = c(.8, 1.2),
      ann = FALSE, axes = FALSE
    )
    text(x = 0.25, y = 1.05, labels = paste0("Color gradient (n = ", n, ")"), adj = 0, cex = 1.5)
    segments(midpoint, .95, midpoint, 1.05, lwd = 2, lty = 2, col = NA)
  }

  # Colorbar ----
  if (colorbar) {
    if (theme == "light") {
      if (is.null(bg)) bg <- "white"
      if (is.null(col.text)) col.text <- "black"
    } else if (theme == "dark") {
      if (is.null(bg)) bg <- "black"
      if (is.null(col.text)) col.text <- "white"
    }

    par.orig <- par(no.readonly = TRUE)
    if (par.reset && !cb.add) on.exit(suppressWarnings(par(par.orig)))
    if (cb.add) {
      par(new = cb.add, pty = "m", mar = cb.add.mar)
    } else {
      par(bg = bg, mar = cb.mar, pty = "m")
    }

    if (!is.null(filename)) {
      grDevices::pdf(
        filename,
        width = pdf.width,
        height = pdf.height,
        title = "rtemis Graphics"
      )
    }
    plot(rep(1, cb.n), 1:cb.n,
      col = cb.grad, pch = 19, cex = cb.cex,
      xlim = c(.5, 1.5), ylim = c(.5, cb.n + .5),
      ann = FALSE, axes = FALSE
    )
    # box() # to visualize position
    # text(1.5, c(1, midpoint, n), labels = c(bar.min, bar.mid, bar.max), col = col.text)
    axis(
      side = 4,
      at = c(1, cb.midpoint, cb.n),
      labels = c(bar.min, bar.mid, bar.max),
      col = color_adjust("black", 0),
      col.axis = col.text,
      col.ticks = color_adjust("black", 0),
      pos = cb.axis.pos,
      las = cb.axis.las,
      cex.axis = cex,
      hadj = cb.axis.hadj
    )
    if (!is.null(filename)) grDevices::dev.off()
  }

  # Plotly cb ----
  if (plotlycb) {
    requireNamespace("plotly")

    m <- list(
      size = 40,
      color = grad,
      opacity = 1,
      symbol = "circle"
    )

    x.ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0.8, 1.4)
    )

    y.ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    t <- list(
      family = "Open Sans",
      size = 22,
      color = plotly::toRGB("black")
    )

    a <- list()
    for (i in 1:3) {
      a[[i]] <- list(
        x = 1.3,
        y = c(1, midpoint, n)[i],
        text = as.character(c(bar.min, bar.mid, bar.max))[i],
        xref = "x",
        yref = "y",
        showarrow = FALSE
      )
    }

    hovtext <- ddSci(seq(bar.min, bar.max, (bar.max - bar.min) / (n - 1)))

    margin <- list(b = margins[1], l = margins[2], t = margins[3], r = margins[4], pad = pad)

    p <- plotly::plot_ly(
      x = rep(1, n), y = 1:n,
      type = "scatter",
      mode = "markers",
      marker = m,
      hoverinfo = "text",
      text = hovtext
    ) |>
      plotly::layout(
        xaxis = x.ax, yaxis = y.ax,
        width = plotly.width,
        height = plotly.height,
        annotations = a,
        font = t,
        margin = margin
      ) |>
      plotly::config(displayModeBar = FALSE)
    if (plotlycb && !rtrn.plotly) print(p)
  }

  # out ----
  if (rtrn.plotly) {
    return(p)
  }
  invisible(grad)
} # rtemis::colorgrad

# 3-letter Color Name Abbreviations
# wht white
# blk black
# red
# grn green
# blu blue
# yel yellow
# rng orange
# prl purple

colorvec <- function(cols) {
  if (nchar(cols) %% 3 != 0) {
    stop("All colors must be specified by their 3-letter abbreviations")
  }

  cols <- tolower(cols)
  ncols <- nchar(cols) / 3
  cols <- lapply(seq(ncols), function(i) substr(cols, i * 3 - 2, i * 3))

  coldf <- data.frame(
    abbr = c("wht", "red", "grn", "blu", "blk", "yel", "rng", "prl"),
    name = c(
      "white", "red", "green", "blue",
      "black", "yellow", "orange", "purple"
    ),
    stringsAsFactors = FALSE
  )

  cols <- sapply(1:ncols, function(i) coldf[coldf$abbr == cols[i], 2])

  lo <- lomid <- mid <- midhi <- hi <- NULL
  collist <- list(
    twocols = c("lo", "hi"),
    threecols = c("lo", "mid", "hi"),
    fourcols = c("lo", "mid", "midhi", "hi"),
    fivecols = c("lo", "lomid", "mid", "midhi", "hi")
  )

  for (i in seq(ncols)) assign(collist[[ncols - 1]][i], cols[i])
  list(lo = lo, lomid = lomid, mid = mid, midhi = midhi, hi = hi)
}



#' Color gradient for continuous variable
#'
#' @param x Float, vector
#' @param color Color, vector, length 2
#' @param space Character: "rgb" or "Lab".
#' @author EDG
#' @export

colorgrad_x <- function(x, color = c("gray20", "#18A3AC"),
                        space = "Lab") {
  colors <- rep(color[1], length(x))
  bipolar <- min(x) < 0 & 0 < max(x)

  if (bipolar) {
    maxabsx <- max(abs(x))
    grad <- colorRampPalette(c(color[2], color[1], color[2]), space = space)(201)
    cuts <- cut(c(-maxabsx, x, maxabsx), 201, labels = FALSE)[-c(1, length(x) + 2)]
    neg.index <- which(x < 0)
    colors[neg.index] <- grad[cuts[neg.index]]
    colors[-neg.index] <- grad[cuts[-neg.index]]
  } else {
    grad <- colorRampPalette(color, space = space)(101)
    cuts <- cut(x, 101, labels = FALSE)
    colors <- grad[cuts]
  }

  colors
} # rtemis::colorgrad_x


autoalpha <- function(x, gamma = .0008, min = .3) {
  max(min, 1 - x * gamma)
}
