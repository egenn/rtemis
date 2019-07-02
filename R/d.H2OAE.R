# d.H2OAE.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Autoencoder using H2O
#'
#' Train an Autoencoder using \code{h2o::h2o.deeplearning}
#' Check out the H2O Flow at \code{[ip]:[port]}, Default IP:port is "localhost:54321"
#' e.g. if running on localhost, point your web browser to \code{localhost:54321}
#'
#' @inheritParams s.H2ODL
#' @param learning.rate Float: Learning rate. Default = .005
#' @param learning.rate.annealing Float: Learning rate annealing. Default = 1e-06
#' @param stopping.rounds Integer: Stop if simple moving average of length \code{stopping.rounds} of the
#' \code{stopping.metric} does not improve. Set to 0 to disable. Default = 50
#' @param stopping.metric String: Stopping metric to use: "AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE",
#' "AUC", "lift_top_group", "misclassification", "mean_per_class_error". Default = "AUTO" ("logloss" for Classification,
#' "deviance" for Regression)
#' @param scale Logical: If TRUE, scale input before training autoencoder. Default = TRUE
#' @param center Logical: If TRUE, center input before training autoencoder. Default = TRUE
#' @param extract.layer Integer: Which layer to extract. For regular autoencoder, this is the middle layer.
#' Default = \code{ceiling(length(n.hidden.nodes)/2)}
#' @param epochs Integer: How many times to iterate through the dataset. Default = 5000
#' @param activation String: Activation function to use: "Tanh" (Default), "TanhWithDropout", "Rectifier", "RectifierWithDropout",
#' "Maxout", "MaxoutWithDropout"
#' @param loss String: "Automatic" (Default), "CrossEntropy", "Quadratic", "Huber", "Absolute"
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional arguments to pass to \code{h2p::h2o.deeplearning}
#' @return \link{rtDecom} object
#' @author Efstathios D. Gennatas
#' @seealso \link{decom}
#' @family Decomposition
#' @family Deep Learning
#' @export

d.H2OAE <- function(x,
                    x.test = NULL,
                    x.valid = NULL,
                    ip = "localhost",
                    port = 54321,
                    n.hidden.nodes = c(ncol(x), 3, ncol(x)),
                    extract.layer = ceiling(length(n.hidden.nodes) / 2),
                    epochs = 5000,
                    activation = "Tanh",
                    loss = "Automatic",
                    input.dropout.ratio = 0,
                    hidden.dropout.ratios = rep(0, length(n.hidden.nodes)),
                    learning.rate = .005,
                    learning.rate.annealing = 1e-06,
                    l1 = 0,
                    l2 = 0,
                    stopping.rounds = 50,
                    stopping.metric = "AUTO",
                    scale = TRUE,
                    center = TRUE,
                    n.cores = rtCores,
                    verbose = TRUE,
                    save.mod = FALSE,
                    outdir = NULL, ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(d.H2OAE))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # [ DEPENDENCIES ] ====
  if (!depCheck("h2o", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ INTRO ] ====
  decom.name <- "H2OAE"

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(d.H2OAE)); stop("x is missing")
  }
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", decom.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ DATA ] ====
  x <- as.data.frame(x)
  if (!is.null(x.test)) x.test <- as.data.frame(x.test)
  n <- NROW(x); p <- NCOL(x)
  if (verbose) cat("||| Input has dimensions ", n, " rows by ", p, " columns,\n", sep = "")
  if (verbose) cat("    interpreted as", n, "cases with", p, "features.\n")
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames
  if (scale) {
    x <- scale(x, center = center)
    if (!is.null(x.test)) x.test <- scale(x.test, center = center)
  }

  # h2o Frames
  if (verbose) msg("Connecting to H2O server...")
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg("Creating H2O frames...")
  df.train <- h2o::as.h2o(x, "df_train")
  if (!is.null(x.test)) {
    df.test <- h2o::as.h2o(x.test, "df_test")
  } else {
    df.test <- NULL
  }

  # [ H2OAE ] ====
  if (verbose) msg("Training H2O Autoencoder...")
  params <- list(x = 1:ncol(x), training_frame = df.train,
                 model_id = paste0("rtemis.H2OAE.", format(Sys.time(), "%b%d.%H:%M:%S.%Y")),
                 validation_frame = df.test,
                 hidden = n.hidden.nodes,
                 epochs = epochs,
                 activation = activation,
                 loss = loss,
                 rate = learning.rate,
                 rate_annealing = learning.rate.annealing,
                 input_dropout_ratio = input.dropout.ratio,
                 stopping_rounds = stopping.rounds,
                 stopping_metric = stopping.metric,
                 # nfolds = nfolds, # not supported for Autoencoder
                 autoencoder = TRUE, ...)
  if (sum(hidden.dropout.ratios) > 0) params$hidden_dropout_ratios <- hidden.dropout.ratios

  mod <- do.call(h2o::h2o.deeplearning, params)

  if (verbose) print(summary(mod))

  # [ PROJECTIONS ] ====
  if (verbose) msg("Extracting Deep Features...")
  projections.train <- as.data.frame(h2o::h2o.deepfeatures(mod, df.train, layer = extract.layer))
  if (!is.null(x.test)) {
    projections.test <- as.data.frame(h2o::h2o.deepfeatures(mod, df.test, layer = extract.layer))
  } else {
    projections.test <- NULL
  }

  # [ OUTRO ] ====
  extra <- list()
  rt <- rtDecom$new(decom.name = decom.name,
                    decom = mod,
                    xnames = xnames,
                    projections.train = projections.train,
                    projections.test = projections.test,
                    parameters = list(n.hidden.nodes = n.hidden.nodes,
                                      extract.layer = extract.layer,
                                      epochs = epochs,
                                      activation = activation,
                                      loss = loss,
                                      input.dropout.ratio = input.dropout.ratio,
                                      hidden.dropout.ratios = hidden.dropout.ratios,
                                      learning.rate = learning.rate,
                                      learning.rate.annealing = learning.rate.annealing,
                                      l1 = l1,
                                      l2 = l2,
                                      stopping.rounds = stopping.rounds,
                                      stopping.metric = stopping.metric,
                                      scale = scale,
                                      center = center),
                    extra = extra)
  if (verbose) msg("Access H2O Flow at ", ip, ":", port, " in your browser", sep = "")
  outro(start.time, verbose = verbose)
  rt

} # rtemis::d.H2OAE
