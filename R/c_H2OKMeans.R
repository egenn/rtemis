# c_H2OKMeans.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' K-Means Clustering with H2O
#'
#' Perfomr [K-Means clustering](https://en.wikipedia.org/wiki/K-means_clustering) using
#' `h2o::h2o.kmeans`
#'
#' Check out the H2O Flow at `[ip]:[port]`, Default IP:port is "localhost:54321"
#' e.g. if running on localhost, point your web browser to `localhost:54321`
#' For additional information, see help on `h2o::h2o.kmeans`
#'
#' @inheritParams c_KMeans
#' @param estimate.k Logical: if TRUE, estimate k up to a maximum set by the `k` argument
#' @param nfolds Integer: Number of cross-validation folds
#' @param max.iterations Integer: Maximum number of iterations
#' @param ip Character: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port number of H2O server. Default = 54321
#' @param seed Integer: Seed for H2O's random number generator. Default = -1 (time-based ranodm number)
#' @param init Character: Initialization mode: "Furthest", "Random", "PlusPlus", "User".
#' Default = "Furthest"
#' @param categorical.encoding Character: How to encode categorical variables: "AUTO", "Enum", "OneHotInternal",
#' "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited".
#' Default = "AUTO"
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional arguments to pass to `h2p::h2o.kmeans`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @family Clustering
#' @export

c_H2OKMeans <- function(
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
  categorical.encoding = c(
    "AUTO",
    "Enum",
    "OneHotInternal",
    "OneHotExplicit",
    "Binary",
    "Eigen",
    "LabelEncoder",
    "SortByResponse",
    "EnumLimited"
  ),
  verbose = TRUE,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  clust.name <- "H2OKMeans"

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # Dependencies ----
  dependency_check("h2o")

  # Arguments ----
  init <- match.arg(init)
  categorical.encoding <- match.arg(categorical.encoding)

  # Data ----
  # h2o Frames
  if (verbose) msg2("Connecting to H2O server...")
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg2("Creating H2O frames...")
  df.train <- h2o::as.h2o(data.frame(x), "df_train")
  if (!is.null(x.test)) {
    df.test <- h2o::as.h2o(data.frame(x.test), "df_test")
  } else {
    df.test <- NULL
  }

  # H2OKMEANS ----
  if (verbose)
    msg2(
      "Performing K-Means Clustering using H2O with k = ",
      k,
      "...",
      sep = ""
    )
  clust <- h2o::h2o.kmeans(
    training_frame = df.train,
    model_id = paste0(
      "rtemis.H2OKMEANS.",
      format(Sys.time(), "%b%d.%H:%M:%S.%Y")
    ),
    k = k,
    estimate_k = estimate.k,
    nfolds = nfolds,
    max_iterations = max.iterations,
    seed = seed,
    init = init,
    categorical_encoding = categorical.encoding,
    ...
  )

  # Clusters ----
  clusters.train <- as.data.frame(h2o::h2o.predict(clust, df.train))[, 1] + 1
  if (!is.null(x.test)) {
    clusters.test <- as.data.frame(h2o::h2o.predict(clust, df.test))[, 1] + 1
  } else {
    clusters.test <- NULL
  }

  # Outro ----
  extra <- list(centroids = clust@model$centers)
  cl <- rtClust$new(
    clust.name = clust.name,
    k = k,
    xnames = xnames,
    clust = clust,
    clusters.train = clusters.train,
    clusters.test = clusters.test,
    parameters = list(
      k = k,
      estimate.k = estimate.k,
      nfolds = nfolds,
      max.iterations = max.iterations,
      seed = seed,
      init = init,
      categorical.encoding = categorical.encoding
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  cl
} # rtemis::c_H2OKMeans
