# lotri2edgeList.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Connectivity Matrix to Edge List
#'
#' Turn the lower triangle of a connectivity matrix (e.g. correlation matrix or similar)
#'  to an edge list of the form:
#'       Source, Target, Weight
#'
#' The output can be read, for example, into gephi
#' @param A Square matrix
#' @param filename Character: Path for csv file. Defaults to "conmat2edgelist.csv"
#' @param verbose Logical: If TRUE, print messages to console
#' @author E.D. Gennatas
#' @export

lotri2edgeList <- function(A, filename = NULL, verbose = TRUE) {
  # [ MAIN ] ----
  # Check A is a square matrix
  dim.A <- dim(A)
  if (verbose) msg2("Input dimensions are", dim.A)
  if (dim.A[1] != dim.A[2]) stop("Error: Input matrix is not square.")
  n <- dim.A[1]
  l <- list()
  # low tri has n(n-1)/2
  l$w <- A[lower.tri(A)]
  c <- 1
  l$r <- c()
  l$c <- c()
  for (i in 1:(n - 1)) {
    l$r <- c(l$r, (c + 1):n)
    l$c <- c(l$c, rep(c, n - c))
    c <- c + 1
  }
  out <- data.frame(NodeA = l$c, NodeB = l$r, Weight = l$w)
  gephiout <- data.frame(Source = l$c, Target = l$r, Weight = l$w)
  if (!is.null(filename)) {
    write.table(
      gephiout,
      file = filename,
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      sep = ","
    )
    rtOut("Saved", filename)
  }
  invisible(out)
} # rtemis::lotri2edgelist
