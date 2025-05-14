# permute.R
# ::rtemis::
# rtemis.org

#' Create permutations
#'
#' Creates all possible permutations
#'
#' n higher than 10 will take a while, or may run out of memory in systems
#' with limited RAM
#'
#' @param n Integer: Length of elements to permute
#'
#' @export
#' @returns Matrix where each row is a different permutation

permute <- function(n) {
  if (n == 1) {
    matrix(1)
  } else {
    mat0 <- permute(n - 1)
    p <- nrow(mat0)
    mat1 <- matrix(nrow = n * p, ncol = n)
    for (i in seq_len(n)) {
      mat1[(i - 1) * p + seq_len(p), ] <- cbind(i, mat0 + (mat0 >= i))
    }
    mat1
  }
} # rtemis::permute
