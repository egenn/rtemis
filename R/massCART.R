# massCART.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org
# TODO: Add resampling

#' Mass-univariate CART prediction and variable importance
#'
#' Predict outcome from each predictor separately and rank by percent Variance explained
#' or Classification Accuracy
#'
#' @inheritParams s_CART
#' @param n.cores Integer: Number of cores to use
#' @author E.D. Gennatas
#' @export

massCART <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     metric = NULL,
                     minsplit = 2,
                     minbucket = round(minsplit/3),
                     cp = 0.01,
                     maxcompete = 0,
                     maxsurrogate = 0,
                     usesurrogate = 2,
                     surrogatestyle = 0,
                     maxdepth = 22,
                     xval = 0,
                     ipw = FALSE,
                     upsample = FALSE,
                     downsample  = FALSE,
                     resample.seed = NULL,
                     n.cores = 1,
                     parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock"),
                     save.mod = FALSE,
                     grid.print.plot = FALSE,
                     verbose = TRUE,
                     grid.verbose = TRUE,
                     print.plot = FALSE, ...) {

  # Intro ====
  start.time <- intro(verbose = verbose)

  # Arguments ====
  parallel.type <- match.arg(parallel.type)

  # Dependencies ====
  dependency_check("plyr", "pbapply")

  # Data ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  # df.train <- data.frame(x, y)
  # colnames(df.train)[ncol(df.train)] <- y.name
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  weights <- dt$weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  nfeatures <- NCOL(x)
  if (type == "Classification") nlevels <- length(levels(y))
  if (is.null(metric)) metric <- if (type == "Classification") "Balanced Accuracy" else "Rsq"
  nclasses <- length(levels(y))

  # CART ====
  if (verbose) msg("Running massCART analysis...")
  if (verbose) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")
  if (n.cores > 1) {
    if (parallel.type == "psock") {
      if (verbose) msg("Starting PSOCK cluster on", n.cores, "cores...")
      cl <- makePSOCKcluster(n.cores)
      on.exit(stopCluster(cl))
      clusterEvalQ(cl, library("rtemis"))
    } else {
      if (verbose) msg("Parallelizing by forking on", n.cores, "cores...")
      cl <- n.cores
    }
  } else {
    cl <- 1
  }

  gridFn <- function(index,
                     x, y,
                     x.test, y.test,
                     weights,
                     minsplit,
                     minbucket,
                     cp,
                     maxcompete,
                     maxsurrogate,
                     usesurrogate,
                     surrogatestyle,
                     maxdepth,
                     xval,
                     print.plot,
                     verbose, ...) {

    x <- x[, index, drop = FALSE]
    x.test <- x.test[, index, drop = FALSE]
    mod <- s_CART(x, y, x.test, y.test,
                  weights = weights,
                  minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp,
                  maxcompete = maxcompete,
                  maxsurrogate = maxsurrogate,
                  usesurrogate = usesurrogate,
                  surrogatestyle = surrogatestyle,
                  maxdepth = maxdepth,
                  xval = xval,
                  verbose = verbose,
                  print.plot = print.plot, ...)

  } # gridFn

  mod <- pbapply::pblapply(1:nfeatures, gridFn,
                           x = x, y = y,
                           x.test = x.test, y.test = y.test,
                           weights = weights,
                           minsplit = minsplit,
                           minbucket = minbucket,
                           cp = cp,
                           maxcompete = maxcompete,
                           maxsurrogate = maxsurrogate,
                           usesurrogate = usesurrogate,
                           surrogatestyle = surrogatestyle,
                           maxdepth = maxdepth,
                           xval = xval,
                           print.plot = grid.print.plot,
                           verbose = grid.verbose,
                           # ...,
                           cl = cl)
  names(mod) <- paste0("CART", 1:length(mod))

  # Error & metrics ====
  error.train <- plyr::llply(mod, function(m) m$error.train)
  # names(error.train) <- paste0("CART", 1:length(mod))
  if (!is.null(x.test)) {
    error.test <- plyr::llply(mod, function(m) m$error.test)
    # names(error.test) <- paste0("CART", 1:length(mod))
  } else {
    error.test <- NULL
  }

  s.out <- list(error.train = error.train,
                error.test = error.test)
  if (save.mod) s.out$mod <- mod

  if (type == "Classification") {
    if (nclasses == 2) {
      metrics.train <- plyr::ldply(mod, function(m) as.data.frame(t(m$error.train$byClass)),
                                   .id = "Feature")
      metrics.train$Feature <- NULL
      rownames(metrics.train) <- xnames
    } else {
      # TODO: maybe create list of lists for each class
      # Get one metric, for each class
      metrics.train <- plyr::ldply(mod, function(m) as.data.frame(t(m$error.train$byClass)[, metric]),
                                  .id = "Feature")
      names(metrics.train)[2:(nlevels + 1)] <- gsub("Class: ", "", names(metrics.train)[2:(nlevels + 1)])
    }
    # TODO: Check whether we can cast to multi-dimensional array for binary and multiclass
    # and is it even practical <- what. no
    # metrics.train.a <- plyr::laply(mod, function(m) as.data.frame(t(m$error.train$byClass)))

    if (!is.null(x.test)) {
      if (nclasses == 2) {
        metrics.test <- plyr::ldply(mod, function(m) as.data.frame(t(m$error.test$byClass)),
                                     .id = "Feature")
        metrics.train$Feature <- NULL
        rownames(metrics.test) <- xnames
      } else {
        metrics.test <- plyr::ldply(mod, function(m) t(as.data.frame(m$error.test$byClass)[, metric]),
                                   .id = "Feature")
        names(metrics.test)[2:(nlevels + 1)] <- gsub("Class: ", "", names(metrics.train)[2:(nlevels + 1)])
      }
    } else {
      metrics.test <- NULL
    }

  } else {
    # Regression
    metrics.train <- plyr::ldply(mod, function(m) m$error.train, .id = "Feature")
    metrics.train$Feature <- xnames
    if (!is.null(x.test)) {
      metrics.test <- plyr::ldply(mod, function(m) m$error.test, .id = "Feature")
      metrics.test$Feature <- xnames
    } else {
      metrics.test <- NULL
    }
  }
  s.out$metrics.train <- metrics.train
  s.out$metrics.test <- metrics.test

  # mplot3_bar ====
  if (print.plot) {
    mplot3_bar(metrics.train[, 2:4], legend.names = metrics.train$Feature,
               group.legend = TRUE, col = pennPalette[1:4],
               main = paste("Training set:", metric))
    if (!is.null(x.test)) {
      mplot3_bar(metrics.test[, 2:4], legend.names = metrics.test$Feature,
                 group.legend = TRUE, col = pennPalette[1:4],
                 main = paste("Testing set:", metric))
    }
  }

  # Outro ====
  outro(start.time, verbose = verbose)
  s.out

} # rtemis::massCART
