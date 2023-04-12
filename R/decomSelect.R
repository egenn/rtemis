# decomSelect.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Select \pkg{rtemis} Decomposer
#'
#' Accepts decomposer name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param decom Character: Decomposition name. Case insensitive. e.g. "iso" for isomap
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name of algorithm `decom`
#' @return Function or name of function (see param `fn`) or full name of algorithm (`desc`)
#' @author E.D. Gennatas
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

  if (name == "CUR") decomposer <- if (fn) d_CUR else "d_CUR"
  if (name == "H2OAE") decomposer <- if (fn) d_H2OAE else "d_H2OAE"
  if (name == "H2OGLRM") decomposer <- if (fn) d_H2OGLRM else "d_H2OGLRM"
  if (name == "ICA") decomposer <- if (fn) d_ICA else "d_ICA"
  if (name == "ISOMAP") decomposer <- if (fn) d_ISOMAP else "d_ISOMAP"
  if (name == "KPCA") decomposer <- if (fn) d_KPCA else "d_KPCA"
  if (name == "LLE") decomposer <- if (fn) d_LLE else "d_LLE"
  if (name == "MDS") decomposer <- if (fn) d_MDS else "d_MDS"
  if (name == "NMF") decomposer <- if (fn) d_NMF else "d_NMF"
  if (name == "PCA") decomposer <- if (fn) d_PCA else "d_PCA"
  if (name == "SPCA") decomposer <- if (fn) d_SPCA else "d_SPCA"
  if (name == "SVD") decomposer <- if (fn) d_SVD else "d_SVD"
  if (name == "TSNE") decomposer <- if (fn) d_TSNE else "d_TSNE"
  if (name == "UMAP") decomposer <- if (fn) d_UMAP else "d_UMAP"

  return(decomposer)

} # rtemis::decomSelect
