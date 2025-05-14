# select_decom.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Select \pkg{rtemis} Decomposer
#'
#' Accepts decomposer name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param decom Character: Decomposition name. Case insensitive. e.g. "iso" for isomap
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name of algorithm `decom`
#'
#' @return Function or name of function (see param `fn`) or full name of algorithm (`desc`)
#' @author E.D. Gennatas
#' @export

select_decom <- function(decom, fn = FALSE, desc = FALSE) {
  description <- list(
    # "CUR" = "CUR Matrix Approximation",
    "H2OAE" = "H2O Autoencoder",
    "H2OGLRM" = "H2O Generalized Low-Rank Model",
    "ICA" = "Independent Component Analysis",
    "Isomap" = "Isomap",
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
  description <- data.frame(
    Name = rownames(description),
    Description = description
  )

  if (missing(decom)) {
    cat(
      ".:select_decom\nrtemis supports the following decomposition algorithms:\n\n"
    )
    print(description, quote = FALSE, row.names = FALSE)
    return(invisible(9))
  }

  name <- description[, 1][tolower(decom) == tolower(description[, 1])]
  if (length(name) == 0) {
    stop("Incorrect decomposer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  d_algname <- paste0("d_", name)
  if (fn) getFromNamespace(d_algname, "rtemis") else d_algname
} # rtemis::select_decom
