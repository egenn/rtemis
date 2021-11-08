# oneHot.default.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' @rdname oneHot
#' @export

oneHot.default <- function(x,
                           xname = NULL,
                           verbose = TRUE) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  x <- factor(x)
  .levels <- levels(x)
  ncases <- NROW(x)
  index <- as.integer(x)
  oh <- matrix(0, ncases, length(.levels))
  colnames(oh) <- paste(xname, .levels, sep = "_")
  for (i in seq(ncases)) oh[i, index[i]] <- 1
  oh

} # rtemis::oneHot.default


# included for benchmarking mostly
onehotcm <- function(x, xname = deparse(substitute(x)),
                     return = "data.frame") {
  stopifnot(is.factor(x))
  dt <- data.table(ID = 1:length(x),
                   x = x)
  setnames(dt, "x", xname)
  out <- dcast(melt(dt, id.vars = "ID"), ID ~ variable + value, fun = length)[, -1]
  if (return == "data.frame") setDF(out)
  out
}

# loop is faster than dcast/melt
# x <- iris$Species
# microbenchmark::microbenchmark(loop = oneHot.default(x), dt = onehotcm(x))
