# onehot2factor
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Convert one-hot encoded matrix to factor
#'
#' @param x one-hot encoded matrix or data.frame
#' @param labels Character vector of level names. Default = \code{colnames(x)}
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- data.frame(matrix(F, 10, 3))
#' colnames(x) <- c("Dx1", "Dx2", "Dx3")
#' x$Dx1[1:3] <- x$Dx2[4:6] <- x$Dx3[7:10] <- T
#' onehot2factor(x)
#' }

onehot2factor <- function(x, labels = colnames(x)) {
  if (any(rowSums(x) > 1)) stop("Input must be one-hot encoded.")
  out <- factor(NROW(x), levels = labels)
  for (i in seq_along(labels)) {
    out[x[, i] == 1] <- labels[i]
  }
  out
  
} # rtemis::onehot2factor
