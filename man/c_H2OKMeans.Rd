% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/c_H2OKMeans.R
\name{c_H2OKMeans}
\alias{c_H2OKMeans}
\title{K-Means Clustering with H2O}
\usage{
c_H2OKMeans(
  x,
  x.test = NULL,
  k = 2,
  estimate.k = FALSE,
  nfolds = 0,
  max.iterations = 10,
  ip = "localhost",
  port = 54321,
  n.cores = rtCores,
  seed = -1,
  init = c("Furthest", "Random", "PlusPlus", "User"),
  categorical.encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary",
    "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{Input matrix / data.frame}

\item{x.test}{Testing set matrix / data.frame}

\item{k}{Integer: Number of clusters to get}

\item{estimate.k}{Logical: if TRUE, estimate k up to a maximum set by the \code{k} argument}

\item{nfolds}{Integer: Number of cross-validation folds}

\item{max.iterations}{Integer: Maximum number of iterations}

\item{ip}{Character: IP address of H2O server. Default = "localhost"}

\item{port}{Integer: Port number of H2O server. Default = 54321}

\item{n.cores}{Integer: Number of cores to use}

\item{seed}{Integer: Seed for H2O's random number generator. Default = -1 (time-based ranodm number)}

\item{init}{Character: Initialization mode: "Furthest", "Random", "PlusPlus", "User".
Default = "Furthest"}

\item{categorical.encoding}{Character: How to encode categorical variables: "AUTO", "Enum", "OneHotInternal",
"OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited".
Default = "AUTO"}

\item{verbose}{Logical: If TRUE, print messages to screen}

\item{...}{Additional arguments to pass to \code{h2p::h2o.kmeans}}
}
\value{
\code{rtMod} object
}
\description{
Perfomr \href{https://en.wikipedia.org/wiki/K-means_clustering}{K-Means clustering} using
\code{h2o::h2o.kmeans}
}
\details{
Check out the H2O Flow at \verb{[ip]:[port]}, Default IP:port is "localhost:54321"
e.g. if running on localhost, point your web browser to \code{localhost:54321}
For additional information, see help on \code{h2o::h2o.kmeans}
}
\seealso{
Other Clustering: 
\code{\link{c_CMeans}()},
\code{\link{c_DBSCAN}()},
\code{\link{c_EMC}()},
\code{\link{c_HARDCL}()},
\code{\link{c_HOPACH}()},
\code{\link{c_KMeans}()},
\code{\link{c_MeanShift}()},
\code{\link{c_NGAS}()},
\code{\link{c_PAM}()},
\code{\link{c_PAMK}()},
\code{\link{c_SPEC}()}
}
\author{
E.D. Gennatas
}
\concept{Clustering}
