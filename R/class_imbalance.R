# class_imbalance.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Class Imbalance
#'
#' Calculate class imbalance as given by:
#' \deqn{I = K\cdot\sum_{i=1}^K (n_i/N - 1/K)^2}{I = K * sum(n_i/N - 1/K)^2}
#' where \eqn{K} is the number of classes, and \eqn{n_i} is the number of
#' instances of class \eqn{i}
#'
#' @param x Vector, factor: Labels of outcome. If `x` has more than 1
#' column, the last one will be used
#' @author E.D. Gennatas
#' @export

class_imbalance <- function(x) {
  if (NCOL(x) > 1) {
    x <- as.data.frame(x)
    x <- x[, NCOL(x)]
  }

  if (!is.factor(x)) stop("Input must be a factor")
  K <- length(levels(x))
  N <- length(x)
  freq <- as.data.frame(table(x))

  K * sum(sapply(seq(K), function(i) (freq$Freq[i] / N - 1 / K)^2))
} # rtemis::class_imbalance
