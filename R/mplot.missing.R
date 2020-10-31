# mplot.missing.R
# :: rtemis::
# 2020 Efstathios D. Gennatas egenn.lambdamd.org
# Biostat214: Documentation with roxygen + keeping function definitions tidy + push to GitHub

#' Plot missingness
#'
#' @param x Data matrix or data.frame
#' @param feat.names Character: Feature names. Defaults to \code{colnames(x)}
#' @param case.names Character: Case names. Defaults to \code{rownames(x)}
#' @examples
#' \dontrun{
#' dat <- iris
#' dat[c(1, 5, 17:20, 110, 115, 140), 1] <-
#' dat[c(12, 15, 55, 73, 100:103), 2] <-
#' dat[sample(1:150, 25), 4] <- NA
#' mplot.missing(dat)
#' }
#'
#' @export

mplot.missing <- function(x,
                          feat.names = NULL,
                          case.names = NULL,
                          main = NULL,
                          col.present = "#18A3AC",
                          col.missing = "#F48024",
                          feat.names.y = 0,
                          names.srt = 90,
                          x.tick.labs.adj = 0,
                          case.names.x = .25,
                          case.names.every = NULL,
                          y.srt = 0,
                          mar = c(3, 3.5, 5.5, 1),
                          oma = c(.5, .5, .5, .5),
                          par.reset = TRUE, ...) {

  # Arguments ====
  if (is.null(feat.names)) feat.names <- colnames(x)
  if (is.null(case.names)) case.names <- rownames(x)
  if (is.null(main)) main <- paste("Missing Data in", deparse(substitute(x)))
  if (is.null(case.names.every)) case.names.every <- floor(NROW(x) * .1)

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(par(par.orig))
  par(mar = mar, oma = oma)

  xnai <- matrix(as.integer(is.na(x)), NROW(x))
  NAperColumn <- apply(xnai, 2, sum)
  x <- seq_len(NCOL(xnai))
  y <- seq_len(NROW(xnai))
  z <- t(apply(xnai, 2, rev))

  ylim <- c(.5, max(y) + .5)
  xlim <- c(.5, max(x) + .5)

  image(x, y, data.matrix(z),
        col = c(col.present, col.missing),
        zlim = c(0, 1),
        asp = NULL, ann = FALSE, axes = FALSE,
        mar = c(0, 0, 0, 0),
        oma = c(0, 0, 0, 0))

  # title below image
  mtext(main, side = 1, font = 2, line = 2)

  # column names above image
  text(x,
       ylim[2] + diff(range(y)) * .04,
       labels = feat.names,
       adj = 0,
       offset = 0,
       xpd = TRUE,
       srt = names.srt)

  # row index left of image
  indl <- logical(case.names.every)
  indl[case.names.every] <- TRUE
  text(x = xlim[1] - diff(range(x)) * .02,
       y[rev(indl)],
       labels = rev(case.names)[rev(indl)],
       adj = 1,
       xpd = TRUE)

  # ylab
  mtext("Case (row nr.)", 2, line = 2.5)

  # Column counts below image
  lo.y <- ylim[1] - diff(ylim) * .06
  polygon(c(0.5, 0.5, xlim[2], xlim[2]),
          c(lo.y, 0.5, 0.5, lo.y),
          col = "gray85",
          border = NA,
          xpd = TRUE)
  text(x = x,
       mean(c(lo.y, .5)),
       adj = c(.5, .5),
       labels = NAperColumn,
       col = ifelse(NAperColumn > 0, "red", "black"),
       xpd = TRUE)

} # rtemis:: mplot.missing
