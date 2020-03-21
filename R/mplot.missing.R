# mplot.missing.R
# :: rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io
# Biostat214: Documentation with roxygen + keeping function definitions tidy + push to GitHub

#' Plot missingness
#'
#' @param x Data matrix or data.frame
#' @param feat.names Character: Feature names. Defaults to \code{colnames(x)}
#' @param case.names Character: Case names. Defaults to \code{rownames(x)}
#' @examples
#' dat <- iris
#' dat[c(1, 5, 17:20, 110, 115, 140), 1] <- dat[c(12, 15, 55, 73, 100:103), 2] <- dat[sample(1:150, 25), 4] <- NA
#' mplot.missing(dat)
#' @export

mplot.missing <- function(x,
                          feat.names = NULL,
                          case.names = NULL,
                          main = NULL,
                          col.present = "#18A3AC",
                          col.missing = "#F48024",
                          feat.names.y = 0,
                          x.srt = 65,
                          x.tick.labs.adj = 0,
                          case.names.x = 0,
                          case.names.every = 20,
                          y.srt = 0,
                          mar = c(3, 3.5, 5.5, 1),
                          oma = c(.5, .5, .5, .5),
                          par.reset = TRUE, ...) {


  if (is.null(feat.names)) feat.names <- colnames(x)
  if (is.null(case.names)) case.names <- rownames(x)
  if (is.null(main)) main <- paste("Missing Data in", deparse(substitute(x)))

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(par(par.orig))

  par(mar = mar, oma = oma)

  # NAperColumn <- sapply(x, function(i) sum(is.na(i)))
  xnai <- matrix(as.integer(is.na(x)), NROW(x))
  NAperColumn <- apply(xnai, 2, sum)
  x <- 1:NCOL(xnai)
  y <- 1:NROW(xnai)
  z <- t(apply(xnai, 2, rev))

  image(x, y, data.matrix(z),
        col = c(col.present, col.missing),
        zlim = c(0, 1),
        asp = NULL, ann = FALSE, axes = FALSE)
  mtext(main, side = 1, font = 2, line = 1.7)
  text(x,
       max(y) + diff(range(y)) * .02,
       labels = feat.names,
       adj = 0,
       # pos = 4,
       offset = 0,
       # cex = cex.x,
       # col = xnames.col,
       xpd = TRUE,
       srt = x.srt)
  indl <- logical(case.names.every)
  indl[case.names.every] <- TRUE
  text(x = case.names.x, y[rev(indl)],
       labels = rev(case.names)[rev(indl)],
       adj = 0,
       xpd = TRUE)
  mtext("Case", 2, line = 2.6)
  lo.y <- 0 - diff(range(y)) * .06
  polygon(c(0.5, 0.5, max(x) + .5, max(x) + .5), c(lo.y, 0, 0, lo.y),
          col = "gray85",
          border = NA,
          xpd = TRUE)
  text(x = x, 0 - diff(range(y)) * .03,
       labels = NAperColumn,
       col = ifelse(NAperColumn > 0, "red", "black"),
       xpd = TRUE)

} # rtemis:: mplot.missing
