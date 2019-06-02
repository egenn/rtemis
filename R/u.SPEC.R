# u.SPEC.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Spectral Clustering
#'
#' Perform Spectral Clustering using \code{kernlab::specc}
#'
#' @inheritParams u.KMEANS
#' @param x Input matrix / data.frame
#' @param k Integer: Number of clusters to get
#' @param kernel String: Kernel to use: "rbfdot", "polydot", "vanilladot", tanhdot", "laplacedot", "besseldot",
#' "anovadot", "splinedot", "stringdot"
#' @param kpar String OR List: "automatic", "local" OR list with: sigma (for "rbfdor", "laplacedot");
#' degree, scale, offset (for "polydot");
#' scale, offset (for "tanhdot");
#' sigma, order, degree (for "besseldot");
#' sigma, degree (for "anovadot");
#' length, lambda, normalized (for "stringdot")
#' @param nystrom.red Logical: if TRUE, use nystrom method to calculate eigenvectors (Default = FALSE)
#' @param nystrom.sample Integer: Number of points to use for estimating the eigenvalues when \code{nystrom.red = TRUE}
#'  Default = \code{dim(x)[1]/6}
#' @param iterations Integer: Number of iterations allowed
#' @param mod.sample Float (0, 1): Proportion of data to use when estimating sigma. Default = .75
#' @param na.action Function: Action to perform on NA (Default = \code{na.omit})
#' @param ... Additional parameters to be passed to \code{flexclust::cclust}
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.SPEC <- function(x,
                   k = 2,
                   kernel = "rbfdot",
                   kpar = "automatic",
                   nystrom.red = FALSE,
                   nystrom.sample = dim(x)[1]/6,
                   iterations = 200,
                   mod.sample = 0.75,
                   na.action = na.omit,
                   verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "SPEC"

  # [ DATA ] ====
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("kernlab", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(u.SPEC))
    stop("x is missing")
  }

  # [ SPECTRAL CLUST ] ====
  if (verbose) msg("Performing Spectral Clustering with k = ", k, "...", sep = "")
  clust <- kernlab::specc(data.matrix(x),
                          centers = k,
                          kernel = kernel,
                          kpar = kpar,
                          nystrom.red = nystrom.red,
                          nystrom.sample = nystrom.sample,
                          iterations = iterations,
                          mod.sample = mod.sample,
                          na.action = na.action, ...)

  # [ CLUSTERS ] ====
  clusters.train <- clust@.Data
  clusters.test <- NULL

  # [ OUTRO ] ====
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = clusters.test,
                    extra = list())
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.SPEC
