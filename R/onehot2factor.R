# onehot2factor
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Convert one-hot encoded matrix to factor
#'
#' @details If input has a single column, it will be converted to factor and
#' returned
#'
#' @param x one-hot encoded matrix or data.frame
#' @param labels Character vector of level names. Default = `colnames(x)`
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(matrix(F, 10, 3))
#' colnames(x) <- c("Dx1", "Dx2", "Dx3")
#' x$Dx1[1:3] <- x$Dx2[4:6] <- x$Dx3[7:10] <- T
#' onehot2factor(x)
#' }
#'
onehot2factor <- function(x, labels = colnames(x)) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  if (any(na.exclude(rowSums(x)) > 1)) stop("Input must be one-hot encoded.")
  out <- factor(rep(NA, NROW(x)), levels = labels)
  for (i in seq_along(labels)) {
    out[x[, i] == 1] <- labels[i]
  }
  out
} # rtemis::onehot2factor


#' Binary matrix times character vector
#'
#' @param x A binary matrix or data.frame
#' @param labels Character vector length equal to `ncol(x)`
#'
#' @returns a character vector
#' @export
# input: mat/df/dt of binary columns
# output: character vector of concatenated values
# repeated vals removed
binmat2vec <- function(x, labels = colnames(x)) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  dt <- as.data.table(x)
  # dt[, which (.SD == 1), by = 1:NROW(dt)]
  fn <- \(r) paste(unique(labels[which(r == 1)]), collapse = ",")
  out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))][[2]]
  out[out == ""] <- NA
  out
} # rtemis::binmat2vec


#' Binary matrix times character vector
#'
#' @param x A binary matrix or data.frame
#' @param labels Character vector length equal to `ncol(x)`
#'
#' @author E.D. Gennatas
#' @returns a character vector
#' @export

`%BC%` <- function(x, labels) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  dt <- as.data.table(x)
  fn <- \(r) paste(unique(labels[which(r == 1)]), collapse = ",")
  out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))][[2]]
  out[out == ""] <- NA
  out
}


binmat2lvec <- function(x, labels = colnames(x), return.list = FALSE) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  dt <- as.data.table(x)
  if (return.list) {
    fn <- \(r) list(labels[which(r == 1)])
    out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))][[2]]
    out[sapply(out, length) == 0] <- NA
  } else {
    fn <- \(r) paste(unique(labels[which(r == 1)]), collapse = ",")
    out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))]
    out[out == ""] <- NA
  }
  out
} # rtemis::binmat2lvec
