# oddsratio.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Calculate odds ratio for a 2x2 contingency table
#'
#' @param x 2x2 contingency table (created with `table(x, y)`, where `x` and `y`
#' are factors with the first level being the control / unaffected / negative)
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @export

oddsratio <- function(x, verbose = TRUE) {
  stopifnot(inherits(x, "table"))
  stopifnot(dim(x) == c(2, 2))

  odds1 <- x[2, 1] / sum(x[, 1])
  odds2 <- x[2, 2] / sum(x[, 2])
  out <- data.frame(matrix(c(x), 2))
  colnames(out) <- colnames(x)
  rownames(out) <- rownames(x)
  out <- rbind(out, c(odds1, odds2))
  rownames(out) <- c(rownames(x), "Odds")
  out
  or <- odds2 / odds1
  attr(out, "OddsRatio") <- or

  if (verbose) {
    print(out)
    cat("Odds Ratio:", hilite(ddSci(or)), "\n")
  }

  out
}
