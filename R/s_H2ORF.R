# s_H2ORF.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Random Forest on H2O (C, R)
#'
#' Trains a Random Forest model using H2O (http://www.h2o.ai)
#'
#' @inheritParams s_GLM
#' @param x Training set features
#' @param y Training set outcome
#' @param x.test Testing set features (Used to evaluate model performance)
#' @param y.test Testing set outcome
#' @param x.valid Validation set features (Used to build model / tune hyperparameters)
#' @param y.valid Validation set outcome
#' @param ip Character: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port to connect to at `ip`
#' @param n.trees Integer: Number of trees to grow
#' @param max.depth Integer: Maximum tree depth
#' @param n.stopping.rounds Integer: Early stopping if simple moving average of this
#' many rounds does not improve. Set to 0 to disable early stopping.
#' @param mtry Integer: Number of variables randomly sampled and considered for
#' splitting at each round. If set to -1, defaults to `sqrt(N_features)` for
#' classification and `N_features/3` for regression.
#' @param nfolds Integer: Number of folds for K-fold CV used by `h2o.randomForest`.
#' Set to 0 to disable (included for experimentation only, use [train_cv] for outer
#' resampling)
#' @param balance.classes Logical: If TRUE, `h2o.randomForest` will over/undersample
#' to balance data. (included for experimentation only)
#' @param h2o.shutdown.at.end Logical: If TRUE, run `h2o.shutdown(prompt = FALSE)` after
#' training is complete.
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional parameters to pass to `h2o::h2o.randomForest`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_H2ORF <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.valid = NULL,
  y.valid = NULL,
  x.name = NULL,
  y.name = NULL,
  ip = "localhost",
  port = 54321,
  n.trees = 500,
  max.depth = 20,
  n.stopping.rounds = 0,
  mtry = -1,
  nfolds = 0,
  weights = NULL,
  balance.classes = TRUE,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  na.action = na.fail,
  h2o.shutdown.at.end = TRUE,
  n.cores = rtCores,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  save.mod = FALSE,
  outdir = NULL,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_H2ORF))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "H2ORF"

  # Dependencies ----
  dependency_check("h2o")

  # Arguments ----
  if (missing(x)) {
    print(args(s_H2ORF))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_H2ORF))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  prefix <- paste0(y.name, "~", x.name)
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  # Heuristic: for 10 or fewer features, set mtry to N of features
  if (mtry == -1) {
    if (NCOL(x) < 11) mtry <- NCOL(x)
  }

  # h2o Frames
  if (verbose) msg2("Connecting to H2O server...", newline.pre = TRUE)
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg2("Creating H2O frames...")
  if (is.null(weights)) weights <- rep(1, NROW(y))
  df.train <- h2o::as.h2o(data.frame(x, y = y, weights = weights), "df_train")
  if (!is.null(x.valid) && !is.null(y.valid)) {
    if (is.null(weights.valid)) weights.valid <- rep(1, NROW(y.valid))
    df.valid <- h2o::as.h2o(
      data.frame(x.valid, y = y.valid, weights = weights.valid),
      "df_valid"
    )
  } else {
    df.valid <- NULL
  }
  if (!is.null(x.test)) {
    df.test <- h2o::as.h2o(data.frame(x.test), "df_test")
  } else {
    df.test <- NULL
  }

  # H2ORF ----
  if (verbose) msg2("Training H2O Random Forest model...", newline.pre = TRUE)
  mod <- h2o::h2o.randomForest(
    y = "y",
    training_frame = df.train,
    validation_frame = df.valid,
    model_id = paste0("rtemis_H2ORF.", format(Sys.time(), "%b%d.%H:%M:%S.%Y")),
    nfolds = nfolds,
    ntrees = n.trees,
    max_depth = max.depth,
    stopping_rounds = n.stopping.rounds,
    mtries = mtry,
    weights_column = "weights",
    balance_classes = balance.classes,
    ...
  )
  if (trace > 0) print(summary(mod))

  # Fitted ----
  if (verbose) msg2("Getting fitted values...")
  fitted <- as.data.frame(predict(mod, df.train))[, 1]
  if (type == "Classification") {
    fitted <- as.factor(fitted)
    levels(fitted) <- levels(y)
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (verbose) msg2("Getting predicted values...")
    predicted <- as.data.frame(predict(mod, df.test))[, 1]
    if (type == "Classification") {
      predicted <- as.factor(predicted)
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list()
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    bag.resample.params = NULL,
    fitted.bag = NULL,
    fitted = fitted,
    se.fit.bag = NULL,
    se.fit = NULL,
    error.train = error.train,
    predicted.bag = NULL,
    predicted = predicted,
    se.predicted.bag = NULL,
    se.prediction = NULL,
    error.test = error.test,
    question = question,
    extra = extra
  )

  rtMod.out(
    rt,
    print.plot,
    plot.fitted,
    plot.predicted,
    y.test,
    mod.name,
    outdir,
    save.mod,
    verbose,
    plot.theme
  )

  if (h2o.shutdown.at.end) h2o::h2o.shutdown(prompt = FALSE)
  if (verbose)
    msg20("Access H2O Flow at http://", ip, ":", port, " in your browser")
  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_H2ORF
