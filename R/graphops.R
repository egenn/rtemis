# graphops.R
# ::rtemis::
# 2020 Efstathios D. Gennatas rtemis.lambdamd.org

#' Get y below current plot area
#'
#' @export

ylo <- function(pct_lower = .08) {

  ylo <- par("usr")[3]
  yhi <- par("usr")[4]
  ylo - pct_lower * (yhi - ylo)

} # rtemis::ylo

ymid <- function() {

  .5 * (par("usr")[3] + par("usr")[4])

} # rtemis::ymid

#' Get midpoint of `cut` label
#'
#' @export

cutmidpoint <- function(x) {

  unlist(lapply(strsplit(x, ","),
                function(i) mean(as.numeric(gsub("[^num[:digit:]]", "", i)))))

} # rtemis::cutmidpoint
