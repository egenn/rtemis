# d_LLE.R
# ::rtemis::
# 2023 E.D. Gennatas rtemis.org

#' Locally Linear Embedding
#'
#' Perform LLE decomposition using `RDRToolbox::lle`
#'
#' Project scaled variables to LLE components
#' Input must be n by p,
#' where n represents number of cases,
#' and p represents number of features.
#'
#' @inheritParams d_SVD
#' @param x Input data
#' @param k Integer: dimensionality of the embedding
#' @param nn Integer: Number of neighbors.
#'
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_LLE <- function(x, k = 2, nn = 6, verbose = TRUE) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "LLE"

  # Dependencies ----
  dependency_check("RDRToolbox")

  # Data ----
  x <- as.data.frame(x)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg2("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg2("    interpreted as", n, "cases with", p, "features.")
  }

  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  xnames <- colnames(x)

  # LLE ----
  if (verbose) msg2("Performing Locally Linear Embedding...")
  decom <- RDRToolbox::LLE(data = as.matrix(x), dim = k, k = nn)

  # Projections ----
  projections.train <- decom

  # Outro ----
  rtdecom <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = NULL,
    parameters = list(
      k = k,
      nn = nn
    )
  )
  outro(start.time, verbose = verbose)
  rtdecom
} # rtemis::d_LLE
