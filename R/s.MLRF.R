# s.MLRF.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Spark MLlib Random Forest [C, R]
#'
#' Train an MLlib Random Forest model on Spark
#'
#' The overhead incurred by Spark means this should be used only for really large datasets on
#' a Spark cluster, not on a regular local machine.
#' @inheritParams s.GLM
#' @param x vector, matrix or dataframe of training set features
#' @param y vector of outcomes
#' @param x.test vector, matrix or dataframe of testing set features
#' @param y.test vector of testing set outcomes
#' @param n.trees Integer. Number of trees to train
#' @param max.depth Integer. Max depth of each tree
#' @param max.bins Integer. Max N of bins used for discretizing continuous features and for
#'   choosing how to split on features at each node. More bins give higher granularity.
#' @param type "regression" for continuous outcome; "classification" for categorical outcome.
#'   "auto" will result in regression for numeric \code{y} and classification otherwise
#' @param spark.master Spark cluster URL or "local"
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.MLRF <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   upsample = FALSE,
                   upsample.seed = NULL,
                   n.trees = 500,
                   max.depth = 30L,
                   subsampling.rate = 1,
                   min.instances.per.node = 1,
                   feature.subset.strategy = "auto",
                   max.bins = 32L,
                   x.name = NULL,
                   y.name = NULL,
                   spark.master = "local",
                   print.plot = TRUE,
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plot.theme = getOption("rt.fit.theme", "lightgrid"),
                   question = NULL,
                   verbose = TRUE,
                   trace = 0,
                   outdir = NULL,
                   save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.MLRF))
    invisible(9)
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "MLRF"

  # [ DEPENDENCIES ] ====
  if (!depCheck("sparklyr", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (missing(x)) {
    print(args(s.MLRF))
    stop("x is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  # verbose <- verbose | !is.null(logFile)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  # Training set dataframe
  df <- data.frame(x, y)
  .formula <- "y ~ ."
  # Testing set dataframe
  if (!is.null(x.test)) {
    df.test <- data.frame(x.test)
  }

  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ Spark cluster ] ====
  sc <- sparklyr::spark_connect(master = spark.master, app_name = "rtemis")
  if (is(sc, "spark_connection")) {
    if (verbose) msg("[@] Connected to Spark cluster")
  } else {
    stop("[X] Failed to connect to Spark cluster. Please check cluster is available")
  }

  # Copy dataframe to Spark cluster
  if (verbose) msg("Copying training set to cluster...")
  tbl <- sparklyr::sdf_copy_to(sc, df, overwrite = TRUE)
  if (is(tbl, "tbl_spark")) {
    if (verbose) msg("...Success")
  } else {
    stop("Failed to copy dataframe to Spark cluster. Check cluster")
  }

  # [ MLRF ] ====
  if (verbose) msg("Training MLlib Random Forest", type, "...", newline = TRUE)
  args <- c(list(x = tbl,
                 formula = .formula,
                 type = ifelse(type == "Classification",
                               "classification", "regression"),
                 num_trees = n.trees,
                 subsampling_rate = subsampling.rate,
                 max_depth = max.depth,
                 min_instances_per_node = min.instances.per.node,
                 max_bins = max.bins,
                 feature_subset_strategy = feature.subset.strategy),
            list(...))
  mod <- do.call(sparklyr::ml_random_forest, args)
  if (trace > 0) print(mod)

  # [ FITTED ] ====
  fitted.raw <- as.data.frame(sparklyr::sdf_predict(tbl, mod))
  if (type == "Classification") {
    fitted <- factor(fitted.raw$predicted_label, levels = levels(y))
    fitted.prob <- fitted.raw$probability_0
  } else {
    fitted <- fitted.raw$prediction
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (verbose) msg("Copying testing set to cluster")
    tbl.test <- sparklyr::sdf_copy_to(sc, df.test, overwrite = TRUE)
    if (is(tbl.test, "tbl_spark")) {
      if (verbose) msg("...Success")
    } else {
      stop("Failed to copy testing set dataframe to Spark cluster. Check cluster")
    }
    predicted.raw <- as.data.frame(sparklyr::sdf_predict(tbl.test, mod))
    if (type == "Classification") {
      predicted.prob <- predicted.raw$probability_0
      predicted <- factor(predicted.raw$predicted_label, levels = levels(y))
    } else {
      predicted <- predicted.raw$prediction
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  varimp <- mod$model$feature_importances()
  names(varimp) <- xnames
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
                 error.test = error.test, list,
                 varimp = varimp,
                 question = question)

  rtMod.out(rt,
            print.plot,
            plot.fitted,
            plot.predicted,
            y.test,
            mod.name,
            outdir,
            save.mod,
            verbose,
            plot.theme)

  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s.MLRF
