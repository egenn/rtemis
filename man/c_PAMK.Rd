% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/c_PAMK.R
\name{c_PAMK}
\alias{c_PAMK}
\title{Partitioning Around Medoids with k Estimation}
\usage{
c_PAMK(
  x,
  krange = 2:10,
  criterion = "asw",
  usepam = ifelse(nrow(x) < 2000, TRUE, FALSE),
  scaling = TRUE,
  diss = inherits(data, "dist"),
  metric = "euclidean",
  do.swap = TRUE,
  trace = 0,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{Input matrix / data.frame}

\item{krange}{Integer vector: Range of k values to try}

\item{criterion}{Character: Criterion to use for selecting k: "asw",
"multiasw" or "ch". See \code{fpc::pamk}}

\item{usepam}{Logical: If TRUE, use \code{cluster::pam}, otherwise use
\code{cluster::clara}.}

\item{scaling}{Logical or Numeric vector: If TRUE, scale input. If numeric
vector of length equal to number of features, the features are divided by
the corresponding value.}

\item{diss}{Logical: If TRUE, treat \code{x} as a dissimilarity matrix,
otherwise as a matrix of
cases by features. Default = TRUE, if x inherits from class \code{dist},
FALSE otherwise.}

\item{metric}{Character: Dissimilarity metric to be used. Options:
'euclidean', 'manhattan'}

\item{do.swap}{Logical: If TRUE, perform the swap phase. See \code{fpc::pam}
for more info}

\item{trace}{Integer [0, 3]: Trace level for \code{fpc::pamk}}

\item{verbose}{Logical: If TRUE, print messages to console}

\item{...}{Additional parameters to be passed to \code{fpc::pamk} and/or
\code{cluster::pam}}
}
\value{
\code{rtClust} object
}
\description{
Estimate
\href{https://en.wikipedia.org/wiki/K-medoids#Partitioning_Around_Medoids_(PAM)}{PAM clustering}
solution and optimal k using \code{fpc::pamk}
}
\seealso{
Other Clustering: 
\code{\link{c_CMeans}()},
\code{\link{c_DBSCAN}()},
\code{\link{c_EMC}()},
\code{\link{c_H2OKMeans}()},
\code{\link{c_HARDCL}()},
\code{\link{c_HOPACH}()},
\code{\link{c_KMeans}()},
\code{\link{c_MeanShift}()},
\code{\link{c_NGAS}()},
\code{\link{c_PAM}()},
\code{\link{c_SPEC}()}
}
\author{
E.D. Gennatas
}
\concept{Clustering}
