# seql.R
# ::rtemis::
# 2019 Efstathios D Gennatas egenn.github.io

#' Sequence generation with automatic cycling
#'
#' @param x R object with \code{length}
#' @param target R object with \code{length}
#' @examples 
#' color <- c("red", "blue")
#' target <- 1:5
#' color[seql(color, target)]
#' # "red"  "blue" "red"  "blue" "red"
#' color <- c("red", "green", "blue", "yellow", "orange")
#' target <- 1:3
#' color[seql(color, target)]
#' # "red"   "green" "blue" 
#' @author Efstathios D Gennatas
#' @export

seql <- function(x, target) {
  xlength <- length(x)
  tlength <- length(target)
  if (xlength == tlength) return(seq(tlength))
  if (xlength < tlength) {
    return(rep(seq(xlength), ceiling(tlength/xlength))[seq(tlength)])
  } else {
    return(seq(tlength))
  }
} # rtemis::seql

