# seql.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Sequence generation with automatic cycling
#'
#' @param x R object of some `length`
#' @param target R object of some `length`
#' @examples
#' color <- c("red", "blue")
#' target <- 1:5
#' color[seql(color, target)]
#' # "red"  "blue" "red"  "blue" "red"
#' color <- c("red", "green", "blue", "yellow", "orange")
#' target <- 1:3
#' color[seql(color, target)]
#' # "red"   "green" "blue"
#' @author E.D. Gennatas
#' @export

seql <- function(x, target) {
  xlength <- length(x)
  tlength <- length(target)
  if (xlength == tlength) {
    return(seq(tlength))
  }
  if (xlength < tlength) {
    return(rep(seq(xlength), ceiling(tlength / xlength))[seq(tlength)])
  } else {
    return(seq(tlength))
  }
} # rtemis::seql
