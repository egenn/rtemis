# colorOps.R
# ::rtemis::
# 2016-22 E.D. Gennatas

#' Simple Color Operations
#'
#' Invert a color or calculate the mean of two colors in HSV or RGB space.
#' This may be useful in creating colors for plots
#'
#' The average of two colors in RGB space will often pass through gray,
#' which is likely undesirable. Averaging in HSV space, better for most applications.
#' @param col Input color(s)
#' @param fn Character: "invert", "mean": Function to perform
#' @param space Character: "HSV", "RGB": Colorspace to operate in - for averaging only
#' @return Color
#' @author E.D. Gennatas
#' @export

colorOp <- function(col,
                    fn = c("invert", "mean"),
                    space = c("HSV", "RGB")) {

  # Arguments ====
  fn <- match.arg(fn)
  space <- match.arg(space)

  # Colors ====
  col <- as.list(col)
  col.rgb <- col2rgb(col, alpha = TRUE)

  if (fn == "invert") {
    inverted <- apply(col.rgb, 2, function(i) 255 - i)
    inverted[4, ] <- col.rgb[4, ]
    invertedl <- lapply(1:NCOL(inverted), function(i) rgb(inverted[1, i],
                                                          inverted[2, i],
                                                          inverted[3, i],
                                                          inverted[4, i], maxColorValue = 255))
    if (!is.null(names(col))) names(invertedl) <- paste0(names(col), ".invert")
    return(invertedl)
  } else if (fn == "mean") {
    if (space == "RGB") {
      if (length(col) < 2) stop("Need at least two colors to average")
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

} # rtemis::colorOp

#' Squared Color Distance
#' 
#' Get the squared RGB distance between two colors
#' 
#' @param x Color
#' @param y Color
#' 
#' @author E.D. Gennatas
#' @export 
#' 
#' @examples
#' sqcoldist("red", "green")
#' sqcoldist("#16A0AC", "#FA6E1E")

sqcoldist <- function(x, y) {

    x.rgb <- col2rgb(x)
    y.rgb <- col2rgb(y)

    sum((x.rgb-y.rgb)^2)

} # rtemis::sqcoldist

#' Order colors
#' 
#' Order colors by RGB distance
#' 
#' @param x Vector of colors
#' @param start_with Integer: Which color to output in first position
#' @param order_by Character: "similarity" or "dissimilarity"
#' 
#' @author E.D. Gennatas
#' @export 
#' 
#' @examples
#' 

order_colors <- function(x, start_with = 1, order_by = c("similarity", "dissimilarity")) {
    order_by <- match.arg(order_by)
    if (!is.integer(start_with)) start_with <- which(x == start_with)
    fn <- switch(order_by,
        similarity = which.min,
        dissimilarity = which.max)
    out <- x[start_with]
    x <- x[-start_with]
    while(length(x) > 1) {
        id <- fn(sapply(x, \(i) sqcoldist(rev(out)[1], i)))
        out <- c(out, x[id])
        x <- x[-id]
    }
    c(out, x)
    
}

#' Separate colors
#' 
#' Separate colors by RGB distance
#' 
#' Starting with the first color defined by \code{start_with}, the next color
#' is chosen to be max distance from all preceding colors
#' 
#' @param x Vector of colors
#' @param start_with Integer: Which color to output in first position
#' 
#' @author E.D. Gennatas
#' @export 

separate_colors <- function(x, start_with = 1) {
    if (!is.integer(start_with)) start_with <- which(x == start_with)
    out <- start_with
    dist <- outer(x, x, Vectorize(sqcoldist))
    colnames(dist) <- seq_along(x)
    out <- c(out, as.numeric(colnames(dist)[which.max(dist[out, ])]))
    dist <- dist[, -out, drop = F]
    while(length(out) < length(x)) {
        id <- which.max(colSums(dist[out, , drop = FALSE]))
        out <- c(out, as.numeric(colnames(dist)[id]))
        dist <- dist[, -id, drop = F]
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
#' @export 
#' 
#' @examples
#' col2grayscale("red")
#' col2grayscale("red", "dec")

col2grayscale <- function(x,
                          what = c("color", "decimal")) {
    
    what <- match.arg(what)
    col <- col2rgb(x)
    gs <- (0.299 * col[1, ] + 0.587 * col[2, ] + 0.114 * col[3, ])/255
    if (what == "color") {
        gray(gs)
    } else {
        gs
    }

} # col2grayscale

palettize <- function(x,
                      grayscale_hicut = .8,
                      start_with = "#16A0AC",
                      order_by = c("separation", "dissimilarity", "similarity")) {
        order_by <- match.arg(order_by)
        x <- unlist(x)
        if (!is.integer(start_with)) {
            start_with <- x[which.min(Vectorize(sqcoldist)(x, start_with))]
        }
        if (!is.null(grayscale_hicut)) {
            xgray <- col2grayscale(x, "dec")
            xf <- x[xgray < grayscale_hicut]
        } else {
            xf <- x
        }
        
        switch(order_by,
            separation = separate_colors(xf, start_with),
            dissimilarity = order_colors(xf, start_with, "dissimilarity"),
            similarity = order_colors(xf, start_with, "similarity"))
    } # palettize
