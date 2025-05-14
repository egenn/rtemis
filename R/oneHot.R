# oneHot.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' One hot encoding
#'
#' One hot encode a vector or factors in a data.frame
#'
#' A vector input will be one-hot encoded regardless of type by looking at all unique values. With data.frame input,
#' only column of type factor will be one-hot encoded.
#' This function is used by [preprocess].
#' `oneHot.data.table` operates on a copy of its input.
#' `oneHot_` performs one-hot encoding in-place.
#'
#' @param x Vector or data.frame
#' @param xname Character: Variable name
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @return For vector input, a one-hot-encoded matrix, for data.frame frame
#' input, an expanded data.frame where all factors are one-hot encoded
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' iris_oh <- oneHot(iris)
#' # factor with only one unique value but 2 levels:
#' vf <- factor(rep("alpha", 20), levels = c("alpha", "beta"))
#' vf_onehot <- oneHot(vf)
#' }

oneHot <- function(x, xname = NULL, verbose = FALSE) {
  UseMethod("oneHot", x)
} # rtemis::oneHot


#' @rdname oneHot
#' @export

oneHot.default <- function(x, xname = NULL, verbose = TRUE) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  # ensures if factor without all levels present, gets all columns created
  if (!is.factor(x)) x <- factor(x)
  .levels <- levels(x)
  ncases <- NROW(x)
  index <- as.integer(x)
  oh <- matrix(0, ncases, length(.levels))
  colnames(oh) <- paste(xname, .levels, sep = "_")
  for (i in seq(ncases)) oh[i, index[i]] <- 1
  oh
} # rtemis::oneHot.default


# included for benchmarking mostly
onehotcm <- function(x, xname = deparse(substitute(x)), return = "data.frame") {
  stopifnot(is.factor(x))
  dt <- data.table(ID = seq_along(x), x = x)
  setnames(dt, "x", xname)
  out <- dcast(
    melt(dt, id.vars = "ID"),
    ID ~ variable + value,
    fun.aggregate = length
  )[, -1]
  if (return == "data.frame") setDF(out)
  out
}

# loop is faster than dcast/melt
# x <- iris$Species
# microbenchmark::microbenchmark(loop = oneHot.default(x), dt = onehotcm(x))

#' @rdname oneHot
#' @export
#' @examples
#' oneHot(iris) |> head()

oneHot.data.frame <- function(x, xname = NULL, verbose = TRUE) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  one.hot <- as.list(x)
  if (verbose) .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) msg20("One hot encoding ", .names[i], "...")
    .levels <- levels(x[, i])
    index <- as.numeric(x[, i])
    oh <- matrix(0, ncases, length(.levels))
    colnames(oh) <- paste(xname, .levels, sep = "_")
    for (j in seq(ncases)) oh[j, index[j]] <- 1
    one.hot[[i]] <- oh
  }
  if (verbose) msg2("Done")
  as.data.frame(one.hot)
} # rtemis::oneHot


#' @rdname oneHot
#'
#' @export
#' @examples
#' ir <- data.table::as.data.table(iris)
#' ir_oh <- oneHot(ir)
#' ir_oh

oneHot.data.table <- function(x, xname = NULL, verbose = TRUE) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  x <- copy(x)
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) info(paste0("One hot encoding ", .names[i], "..."))
    .levels <- levels(x[[i]])
    index <- as.numeric(x[[i]])
    oh <- as.data.table(matrix(0, ncases, length(.levels)))
    .colnames <- colnames(oh) <- paste(xname, .levels, sep = "_")
    for (k in seq_along(.levels)) oh[index == k, (.colnames[k]) := 1]
    x[, (paste(.names[i], .levels, sep = "_")) := oh]
  }
  # remove original factor(s)
  x[, paste(.names[factor.index]) := NULL]
  if (verbose) msg2("Done")
  invisible(x)
} # rtemis::oneHot.data.table


#' @rdname oneHot
#'
#' @export
#' @examples
#' ir <- data.table::as.data.table(iris)
#' # dt_set_oneHot operates in-place; therefore no assignment is used:
#' dt_set_oneHot(ir)
#' ir

dt_set_oneHot <- function(x, xname = NULL, verbose = TRUE) {
  if (is.null(xname)) xname <- deparse(substitute(x))
  ncases <- NROW(x)
  factor.index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor.index) {
    if (verbose) info(paste0("One hot encoding ", .names[i], "..."))
    .levels <- levels(x[[i]])
    index <- as.numeric(x[[i]])
    oh <- as.data.table(matrix(0, ncases, length(.levels)))
    .colnames <- colnames(oh) <- paste(xname, .levels, sep = "_")
    for (k in seq_along(.levels)) oh[index == k, (.colnames[k]) := 1]
    x[, (paste(.names[i], .levels, sep = "_")) := oh]
  }
  # remove original factor(s)
  x[, paste(.names[factor.index]) := NULL]
  if (verbose) msg2("Done")
  invisible(x)
} # rtemis::dt_set_oneHot
