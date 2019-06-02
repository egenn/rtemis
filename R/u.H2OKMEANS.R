# u.H2OKMEANS.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' K-Means Clustering on H2O
#'
#' K-Means clustering using \code{h2o::h2o.kmeans}
#' Check out the H2O Flow at \code{[ip]:[port]}, Default IP:port is "localhost:54321"
#' e.g. if running on localhost, point your web browser to \code{localhost:54321}
#'
#' For additional information, see help on \code{h2o::h2o.kmeans}
#' @inheritParams u.KMEANS
#' @param estimate.k Logical: if TRUE, estimate k up to a maximum set by the \code{k} argument
#' @param nfolds Integer: Number of cross-validation folds
#' @param max.iterations Integer: Maximum number of iterations
#' @param ip String: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port number of H2O server. Default = 54321
#' @param seed Integer: Seed for H2O's random number generator. Default = -1 (time-based ranodm number)
#' @param init String: Initialization mode: "Furthest", "Random", "PlusPlus", "User".
#' Default = "Furthest"
#' @param categorical.encoding String: How to encode categorical variables: "AUTO", "Enum", "OneHotInternal",
#' "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited".
#' Default = "AUTO"
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional arguments to pass to \code{h2p::h2o.kmeans}
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @family Clustering
#' @export

u.H2OKMEANS <- function(x, x.test = NULL,
                        k = 2,
                        estimate.k = FALSE,
                        nfolds = 0,
                        max.iterations = 10,
                        ip = "localhost",
                        port = 54321,
                        # na.action = na.fail,
                        n.cores = rtCores,
                        seed = -1,
                        init = c("Furthest", "Random", "PlusPlus", "User"),
                        categorical.encoding = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary",
                        "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
                        verbose = TRUE, ...) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)
  clust.name <- "H2OKMEANS"

  # [ DATA ] ====
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  x <- as.data.frame(x)
  xnames <- colnames(x)

  # [ DEPENDENCIES ] ====
  if (!depCheck("h2o", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  init <- match.arg(init)
  categorical.encoding <- match.arg(categorical.encoding)

  # [ DATA ] ====
  # h2o Frames
  if (verbose) msg("Connecting to H2O server...")
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg("Creating H2O frames...")
  df.train <- h2o::as.h2o(data.frame(x), "df_train")
  if (!is.null(x.test)) {
    df.test <- h2o::as.h2o(data.frame(x.test), "df_test")
  } else {
    df.test <- NULL
  }

  # [ H2OKMEANS ] ====
  if (verbose) msg("Performing K-Means Clustering using H2O with k = ", k, "...", sep = "")
  clust <- h2o::h2o.kmeans(training_frame = df.train,
                           model_id = paste0("rtemis.H2OKMEANS.", format(Sys.time(), "%b%d.%H:%M:%S.%Y")),
                           k = k,
                           estimate_k = estimate.k,
                           nfolds = nfolds,
                           max_iterations = max.iterations,
                           seed = seed,
                           init = init,
                           categorical_encoding = categorical.encoding, ...)

  # [ CLUSTERS ] ====
  clusters.train <- as.data.frame(h2o::h2o.predict(clust, df.train))[, 1] + 1
  if (!is.null(x.test)) {
    clusters.test <- as.data.frame(h2o::h2o.predict(clust, df.test))[, 1] + 1
  } else {
    clusters.test <- NULL
  }

  # [ OUTRO ] ====
  extra <- list(centroids = clust@model$centers)
  cl <- rtClust$new(clust.name = clust.name,
                    k = k,
                    xnames = xnames,
                    clust = clust,
                    clusters.train = clusters.train,
                    clusters.test = clusters.test,
                    extra = extra)
  outro(start.time, verbose = verbose)
  cl

} # rtemis::u.H2OKMEANS
