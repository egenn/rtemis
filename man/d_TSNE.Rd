% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_TSNE.R
\name{d_TSNE}
\alias{d_TSNE}
\title{t-distributed Stochastic Neighbor Embedding}
\usage{
d_TSNE(
  x,
  k = 3,
  initial.dims = 50,
  perplexity = 15,
  theta = 0,
  check.duplicates = TRUE,
  pca = TRUE,
  max.iter = 1000,
  scale = FALSE,
  center = FALSE,
  is.distance = FALSE,
  verbose = TRUE,
  outdir = "./",
  ...
)
}
\arguments{
\item{x}{Input matrix}

\item{k}{Integer. Number of t-SNE components required}

\item{initial.dims}{Integer: Number of dimensions to retain in initial PCA. Default = 50}

\item{perplexity}{Numeric: Perplexity parameter}

\item{theta}{Float: 0.0: exact TSNE. Increase for higher speed, lower accuracy. Default = 0}

\item{check.duplicates}{Logical: If TRUE, Checks whether duplicates are present. Best to set test manually}

\item{pca}{Logical: If TRUE, perform initial PCA step. Default = TRUE}

\item{max.iter}{Integer: Maximum number of iterations. Default = 1000}

\item{scale}{Logical: If TRUE, scale before running t-SNE using \code{base::scale}. Default = FALSE}

\item{center}{Logical: If TRUE, and \code{scale = TRUE}, also center. Default = FALSE}

\item{is.distance}{Logical: If TRUE, \code{x} should be a distance matrix. Default = FALSE}

\item{verbose}{Logical: If TRUE, print messages to output}

\item{outdir}{Path to output directory}

\item{...}{Options for \code{Rtsne::Rtsne}}
}
\value{
\code{rtDecom} object
}
\description{
Perform t-SNE decomposition using \code{Rtsne::Rtsne}
}
\seealso{
Other Decomposition: 
\code{\link{d_H2OAE}()},
\code{\link{d_H2OGLRM}()},
\code{\link{d_ICA}()},
\code{\link{d_Isomap}()},
\code{\link{d_KPCA}()},
\code{\link{d_LLE}()},
\code{\link{d_MDS}()},
\code{\link{d_NMF}()},
\code{\link{d_PCA}()},
\code{\link{d_SPCA}()},
\code{\link{d_SVD}()},
\code{\link{d_UMAP}()}
}
\author{
E.D. Gennatas
}
\concept{Decomposition}
