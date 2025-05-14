# c_SPEC.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Spectral Clustering
#'
#' Perform [Spectral Clustering](https://en.wikipedia.org/wiki/Spectral_clustering)
#' using `kernlab::specc`
#'
#' @inheritParams c_KMeans
#' @param x Input matrix / data.frame
#' @param k Integer: Number of clusters to get
#' @param kernel Character: Kernel to use: "rbfdot", "polydot", "vanilladot", tanhdot", "laplacedot", "besseldot",
#' "anovadot", "splinedot", "stringdot"
#' @param kpar String OR List: "automatic", "local" OR list with: sigma (for "rbfdor", "laplacedot");
#' degree, scale, offset (for "polydot");
#' scale, offset (for "tanhdot");
#' sigma, order, degree (for "besseldot");
#' sigma, degree (for "anovadot");
#' length, lambda, normalized (for "stringdot")
#' @param nystrom.red Logical: if TRUE, use nystrom method to calculate eigenvectors (Default = FALSE)
#' @param nystrom.sample Integer: Number of points to use for estimating the eigenvalues when `nystrom.red = TRUE`
#'  Default = `dim(x)[1]/6`
#' @param iterations Integer: Number of iterations allowed
#' @param mod.sample Float (0, 1): Proportion of data to use when estimating sigma. Default = .75
#' @param na.action Function: Action to perform on NA (Default = `na.omit`)
#' @param ... Additional parameters to be passed to `flexclust::cclust`
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_SPEC <- function(
  x,
  k = 2,
  kernel = "rbfdot",
  kpar = "automatic",
  nystrom.red = FALSE,
  nystrom.sample = dim(x)[1] / 6,
  iterations = 200,
  mod.sample = 0.75,
  na.action = na.omit,
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "SPEC"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("kernlab")

  # Arguments ----
  if (missing(x)) {
    print(args(c_SPEC))
    stop("x is missing")
  }

  # Spectral clust ----
  if (verbose)
    msg2("Performing Spectral Clustering with k = ", k, "...", sep = "")
  clust <- kernlab::specc(
    data.matrix(x),
    centers = k,
    kernel = kernel,
    kpar = kpar,
    nystrom.red = nystrom.red,
    nystrom.sample = nystrom.sample,
    iterations = iterations,
    mod.sample = mod.sample,
    na.action = na.action,
    ...
  )

  # Clusters ----
  clusters.train <- clust@.Data
  clusters.test <- NULL

  # Outro ----
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = clusters.test,
    parameters = list(
      k = k,
      kernel = kernel,
      kpar = kpar,
      nystrom.red = nystrom.red,
      nystrom.sample = nystrom.sample,
      iterations = iterations,
      mod.sample = mod.sample
    ),
    extra = list()
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_SPEC
