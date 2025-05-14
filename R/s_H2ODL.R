# s_H2ODL.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Deep Learning on H2O (C, R)
#'
#' Trains a Deep Neural Net using H2O (http://www.h2o.ai)
#' Check out the H2O Flow at `[ip]:[port]`, Default IP:port is "localhost:54321"
#' e.g. if running on localhost, point your web browser to `localhost:54321`
#'
#' x & y form the training set.
#' x.test & y.test form the testing set used only to test model generalizability.
#' x.valid & y.valid form the validation set used to monitor training progress
#'
#' @inheritParams s_GLM
#' @param x Vector / Matrix / Data Frame: Training set Predictors
#' @param y Vector: Training set outcome
#' @param x.test Vector / Matrix / Data Frame: Testing set Predictors
#' @param y.test Vector: Testing set outcome
#' @param x.valid Vector / Matrix / Data Frame: Validation set Predictors
#' @param y.valid Vector: Validation set outcome
#' @param ip Character: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port number for server. Default = 54321
#' @param n.hidden.nodes Integer vector of length equal to the number of hidden layers you wish to create
#' @param activation Character: Activation function to use: "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout",
#' "Maxout", "MaxoutWithDropout". Default = "Rectifier"
#' @param input.dropout.ratio Float (0, 1): Dropout ratio for inputs
#' @param hidden.dropout.ratios Vector, Float (0, 2): Dropout ratios for hidden layers
#' @param l1 Float (0, 1): L1 regularization
#' (introduces sparseness; i.e. sets many weights to 0; reduces variance, increases generalizability)
#' @param l2 Float (0, 1): L2 regularization
#' (prevents very large absolute weights; reduces variance, increases generalizability)
#' @param epochs Integer: How many times to iterate through the dataset. Default = 1000
#' @param learning.rate Float: Learning rate to use for training. Default = .005
#' @param adaptive.rate Logical: If TRUE, use adaptive learning rate. Default = TRUE
#' @param rate.annealing Float: Learning rate annealing: rate / (1 + rate_annealing * samples). Default = 1e-6
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional parameters to pass to `h2o::h2o.deeplearning`
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Deep Learning
#' @export

s_H2ODL <- function(
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
  n.hidden.nodes = c(20, 20),
  epochs = 1000,
  activation = "Rectifier",
  mini.batch.size = 1,
  learning.rate = 0.005,
  adaptive.rate = TRUE,
  rho = .99,
  epsilon = 1e-08,
  rate.annealing = 1e-06,
  rate.decay = 1,
  momentum.start = 0,
  momentum.ramp = 1e+06,
  momentum.stable = 0,
  nesterov.accelerated.gradient = TRUE,
  input.dropout.ratio = 0,
  hidden.dropout.ratios = NULL,
  l1 = 0,
  l2 = 0,
  max.w2 = 3.4028235e+38,
  nfolds = 0,
  initial.biases = NULL,
  initial.weights = NULL,
  loss = "Automatic",
  distribution = "AUTO",
  stopping.rounds = 5,
  stopping.metric = "AUTO",
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  na.action = na.fail,
  n.cores = rtCores,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_H2ODL))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
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
  mod.name <- "H2ODL"

  # Dependencies ----
  dependency_check("h2o")

  # Arguments ----
  if (missing(x)) {
    print(args(s_H2ODL))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_H2ODL))
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

  # h2o Frames
  if (verbose) msg2("Connecting to H2O server...")
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg2("Creating H2O frames...")
  df.train <- h2o::as.h2o(data.frame(x, y = y), "df_train")
  if (!is.null(x.test)) {
    df.test <- h2o::as.h2o(data.frame(x.test, y = y.test), "df_test")
  } else {
    df.test <- NULL
  }
  if (!is.null(x.valid)) {
    df.valid <- h2o::as.h2o(data.frame(x.valid, y = y.valid), "df_valid")
  } else {
    df.valid <- NULL
  }

  # H2ODL ----
  net.args <- list(
    y = "y",
    training_frame = df.train,
    model_id = paste0("rtemis_H2ODL.", format(Sys.time(), "%b%d.%H:%M:%S.%Y")),
    validation_frame = df.valid,
    hidden = n.hidden.nodes,
    epochs = epochs,
    activation = activation,
    mini_batch_size = mini.batch.size,
    rate = learning.rate,
    adaptive_rate = adaptive.rate,
    rho = rho,
    epsilon = epsilon,
    rate_annealing = rate.annealing,
    rate_decay = rate.decay,
    momentum_start = momentum.start,
    momentum_ramp = momentum.ramp,
    momentum_stable = momentum.stable,
    nesterov_accelerated_gradient = nesterov.accelerated.gradient,
    input_dropout_ratio = input.dropout.ratio,
    l1 = l1,
    l2 = l2,
    max_w2 = max.w2,
    nfolds = nfolds,
    initial_weights = initial.weights,
    initial_biases = initial.biases,
    loss = loss,
    distribution = distribution,
    stopping_rounds = stopping.rounds,
    stopping_metric = stopping.metric,
    ...
  )
  if (!is.null(hidden.dropout.ratios))
    net.args$hidden_dropout_ratios <- hidden.dropout.ratios
  if (verbose) msg2("Training H2O Deep Net...", newline.pre = TRUE)
  mod <- do.call(h2o::h2o.deeplearning, net.args)
  if (trace > 0) print(summary(mod))

  # Fitted ----
  if (verbose) msg2("Getting fitted values...")
  fitted <- as.data.frame(predict(mod, df.train))[, 1]
  if (type == "Classification") {
    fitted <- factor(fitted, levels = levels(y))
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (verbose) msg2("Getting predicted values...")
    predicted <- as.data.frame(predict(mod, df.test))[, 1]
    if (type == "Classification") {
      predicted <- factor(predicted, levels = levels(y))
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(
    n.hidden.nodes = n.hidden.nodes,
    epochs = epochs
  )
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
    parameters = net.args,
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

  if (verbose)
    msg20("Access H2O Flow by pointing your browser to ", ip, ":", port)
  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_H2ODL
