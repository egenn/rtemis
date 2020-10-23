# decomSelect.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' Select \pkg{rtemis} Decomposer
#'
#' Accepts decomposer name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param decom Character: Decomposition name. Case insensitive. e.g. "iso" for isomap
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name of algorithm \code{decom}
#' @return Function or name of function (see param \code{fn}) or full name of algorithm (\code{desc})
#' @author Efstathios D. Gennatas
#' @export

decomSelect <- function(decom, fn = FALSE, desc = FALSE) {

  description <- list(
    "CUR" = "CUR Matrix Approximation",
    "H2OAE" = "H2O Autoencoder",
    "H2OGLRM" = "H2O Generalized Low-Rank Model",
    "ICA" = "Independent Component Analysis",
    "ISOMAP" = "ISOMAP",
    "KPCA" = "Kernel Principal Component Analysis",
    "LLE" = "Locally Linear Embedding",
    "MDS" = "Multidimensional Scaling",
    "NMF" = "Non-negative Matrix Factorization",
    "PCA" = "Principal Component Analysis",
    "SPCA" = "Sparse Principal Component Analysis",
    "SVD" = "Singular Value Decomposition",
    "TSNE" = "t-distributed Stochastic Neighbor Embedding",
    "UMAP" = "Uniform Manifold Approximation and Projection"
  )
  description <- t(as.data.frame(description))
  description <- data.frame(Name = rownames(description), Description = description)

  if (missing(decom)) {
    cat(".:decomSelect\nrtemis supports the following decomposition algorithms:\n\n")
    print(description, quote = F, row.names = FALSE)
    return(invisible(9))
  }

  name.vec <- toupper(c("cur", "h2oae", "h2oglrm", "ica", "isomap", "kpca", "lle", "mds", "nmf",
                        "pca", "spca", "svd", "tsne", "umap"))
  name <- name.vec[pmatch(toupper(decom), name.vec)]
  if (is.na(name)) {
    print(description, quote = FALSE)
    stop("Incorrect decomposer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  if (name == "CUR") decomposer <- if (fn) d.CUR else "d.CUR"
  if (name == "H2OAE") decomposer <- if (fn) d.H2OAE else "d.H2OAE"
  if (name == "H2OGLRM") decomposer <- if (fn) d.H2OGLRM else "d.H2OGLRM"
  if (name == "ICA") decomposer <- if (fn) d.ICA else "d.ICA"
  if (name == "ISOMAP") decomposer <- if (fn) d.ISOMAP else "d.ISOMAP"
  if (name == "KPCA") decomposer <- if (fn) d.KPCA else "d.KPCA"
  if (name == "LLE") decomposer <- if (fn) d.LLE else "d.LLE"
  if (name == "MDS") decomposer <- if (fn) d.MDS else "d.MDS"
  if (name == "NMF") decomposer <- if (fn) d.NMF else "d.NMF"
  if (name == "PCA") decomposer <- if (fn) d.PCA else "d.PCA"
  if (name == "SPCA") decomposer <- if (fn) d.SPCA else "d.SPCA"
  if (name == "SVD") decomposer <- if (fn) d.SVD else "d.SVD"
  if (name == "TSNE") decomposer <- if (fn) d.TSNE else "d.TSNE"
  if (name == "UMAP") decomposer <- if (fn) d.UMAP else "d.UMAP"

  return(decomposer)

} # rtemis::decomSelect
