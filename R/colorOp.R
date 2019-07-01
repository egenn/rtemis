# colorOp.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Simple Color Operations
#'
#' Invert a color or calculate the mean of two colors in HSV or RGB space.
#' This may be useful in creating colors for plots
#'
#' The average of two colors in RGB space will often pass through gray,
#' which is likely undesirable. Averaging in HSV space, better for most applications.
#' @param col Input color(s)
#' @param fn String: "invert", "mean": Function to perform
#' @param space String: "HSV", "RGB": Colorspace to operate in - for averaging only
#' @return Color
#' @author Efstathios D. Gennatas
#' @export

colorOp <- function(col,
                    fn = c("invert", "mean"),
                    space = c("HSV", "RGB")) {

  # [ ARGUMENTS ] ====
  fn <- match.arg(fn)
  space <- match.arg(space)

  # [ COLORS ] ====
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
