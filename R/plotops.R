# plotops.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.lambdamd.org

# nchar 46 => 19.5
# nchar 20 => 10
# lm(c(10, 19.5) ~ c(20, 46))
textwidth <- function(x) {
  .nchar <- if (is.null(x)) 1 else max(nchar(x), na.rm = TRUE)
  2.6923 + 0.3654 * .nchar
}

#' Get y below current plot area
#'

ylo <- function(pct_lower = .08) {

  ylo <- par("usr")[3]
  yhi <- par("usr")[4]
  ylo - pct_lower * (yhi - ylo)

}

yhi <- function(pct_higher = .08) {

  ylo <- par("usr")[3]
  yhi <- par("usr")[4]
  yhi + pct_lower * (yhi - ylo)

}

xleft <- function(pct_left = .08) {

  xleft <- par("usr")[1]
  xright <- par("usr")[2]
  xleft - pct_left * (xright - xleft)

}

xright <- function(pct_right = .08) {

  xleft <- par("usr")[1]
  xright <- par("usr")[2]
  xright + pct_right * (xright - xleft)

}

ymid <- function() .5 * sum(par("usr")[3:4])

xmid <- function() .5 * (par("usr")[1:2])

#' Get midpoint of `cut` label
#'

cutmidpoint <- function(x) {

  unlist(lapply(strsplit(x, ","),
                function(i) mean(as.numeric(gsub("[^num[:digit:]]", "", i)))))

} # rtemis::cutmidpoint


getlim <- function(x, axs = c("r", "i"), axs.r.pct = .04) {
  axs <- match.arg(axs)

  .x <- na.exclude(x)
  .min <- min(.x)
  .max <- max(.x)

  if (axs == "r") {
    .diff <- .max - .min
    c(.min - axs.r.pct * .diff, .max + axs.r.pct * .diff)
  } else {
    c(.min, .max)
  }
} # rtemis::getlim

