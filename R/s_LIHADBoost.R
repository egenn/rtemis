# s_LIHADBoost.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org
# boosting learning.rate vs. hytree learning.rate
# ... added to allow "weights = NULL" from gridSearchLearn

#' Boosting of Linear Hard Additive Trees \[R\]
#'
#' Boost a Linear Hard Additive Tree (i.e. LIHAD, i.e. LINAD with hard splits)
#'
#' By default, early stopping works by checking training loss.
#'
# @inheritParams hytboost
#' @inheritParams s_GLM
#' @param learning.rate Float (0, 1] Learning rate for the additive steps
#' @param init Float: Initial value for prediction. Default = mean(y)
#' @param max.iter Integer: Maximum number of iterations (additive steps) to perform. Default = 10
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param base.verbose Logical: `verbose` argument passed to learner
#' @param print.error.plot String or Integer: "final" plots a training and validation (if available) error curve at the
#' end of training. If integer, plot training and validation error curve every this many iterations
#' during training
#' @param print.base.plot Logical: Passed to `print.plot` argument of base learner, i.e. if TRUE, print error plot
#' for each base learner
#' @param ... Additional parameters to be passed to learner
#'
#' @author E.D. Gennatas
#' @export

s_LIHADBoost <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  # x.valid = NULL, y.valid = NULL,
  resid = NULL,
  boost.obj = NULL,
  learning.rate = .5, # overwrite mod.params$learning.rate
  case.p = 1,
  # mod.params = setup.LIHAD(),
  # ++ hytreew params ++
  max.depth = 5,
  gamma = .1,
  alpha = 0,
  lambda = 1,
  lambda.seq = NULL,
  minobsinnode = 2,
  minobsinnode.lin = 10,
  shrinkage = 1,
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  part.minbucket = 5,
  # init = mean(y),
  lin.type = c(
    "glmnet",
    "cv.glmnet",
    "lm.ridge",
    "allSubsets",
    "forwardStepwise",
    "backwardStepwise",
    "glm",
    "sgd",
    "solve",
    "none"
  ),
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = "lambda.min",
  # -- hytreew params --
  # weights = NULL,
  max.iter = 10,
  tune.n.iter = TRUE,
  # cv.n.iter = TRUE, # By default, CV to find best n.iter
  earlystop.params = setup.earlystop(),
  lookback = TRUE,
  init = NULL,
  .gs = FALSE,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = "exhaustive",
  metric = NULL,
  maximize = NULL,
  cxrcoef = FALSE,
  print.progress.every = 5,
  print.error.plot = "final",
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  base.verbose = FALSE,
  verbose = TRUE,
  grid.verbose = FALSE,
  trace = 0,
  prefix = NULL,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  print.plot = FALSE,
  print.base.plot = FALSE,
  print.tune.plot = TRUE,
  plot.type = 'l',
  save.gridrun = FALSE,
  outdir = NULL,
  n.cores = rtCores,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(boost))
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
  mod.name <- "LIHADBoost"
  lin.type <- match.arg(lin.type)

  # Dependencies ----
  dependency_check("rpart", "glmnet")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  # if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  # extra.args <- list(...)
  # mod.params <- c(mod.params, extra.args)
  # mod.params$max.depth <- max.depth
  # mod.params$learning.rate <- learning.rate
  # if (!is.null(force.n.iter)) max.iter <- force.n.iter

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    # x.valid = x.valid, y.valid = y.valid,
    # ifw = ifw, ifw.type = ifw.type,
    # upsample = upsample, resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  # x.valid <- dt$x.valid
  # y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)

  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(init)) init <- mean(y)

  # Grid Search ----
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Regression") {
      metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    }
  }

  if (is.null(maximize)) {
    maximize <- if (type == "Classification") TRUE else FALSE
  }

  # .final <- FALSE
  gc <- gridCheck(
    max.depth,
    learning.rate,
    # hytreenow params
    shrinkage,
    alpha,
    lambda
  )
  if (!.gs && (gc || tune.n.iter)) {
    gs <- gridSearchLearn(
      x = x,
      y = y,
      mod = mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        learning.rate = learning.rate,
        max.depth = max.depth,
        shrinkage = shrinkage,
        alpha = alpha,
        lambda = lambda
      ),
      fixed.params = list(
        max.iter = max.iter,
        earlystop.params = earlystop.params,
        lookback = lookback,
        lambda.seq = lambda.seq,
        minobsinnode = minobsinnode,
        minobsinnode.lin = minobsinnode.lin,
        part.minsplit = part.minsplit,
        part.xval = part.xval,
        part.max.depth = part.max.depth,
        part.cp = part.cp,
        part.minbucket = part.minbucket,
        lin.type = lin.type,
        cv.glmnet.nfolds = cv.glmnet.nfolds,
        which.cv.glmnet.lambda = which.cv.glmnet.lambda,
        .gs = TRUE
      ),
      search.type = gridsearch.type,
      # weights = weights,
      metric = metric,
      maximize = maximize,
      save.mod = save.gridrun,
      verbose = verbose,
      grid.verbose = grid.verbose,
      n.cores = n.cores
    )

    max.depth <- gs$best.tune$max.depth
    learning.rate <- gs$best.tune$learning.rate
    max.iter <- gs$best.tune$n.steps

    # Now ready to train final full model
    # .final <- TRUE
    .gs <- FALSE
  } else {
    gs <- NULL
  }

  # LIHADBoost ----
  if (verbose)
    parameterSummary(init, max.iter, learning.rate, newline.pre = TRUE)
  # mod.params)
  if (trace > 0) msg2("Initial MSE =", mse(y, init))
  if (verbose) msg2("Training LIHADBoost...", newline.pre = TRUE)
  if (.gs) {
    .xval <- x.test # this is the validation set carved out of the training set during gridSearch
    .yval <- y.test
  } else {
    .xval <- .yval <- NULL
    # .xval <- x.valid # these may be null
    # .yval <- y.valid
  }
  mod <- hytboost(
    x = x,
    y = y,
    x.valid = .xval,
    y.valid = .yval,
    resid = resid,
    boost.obj = boost.obj,
    learning.rate = learning.rate,
    case.p = case.p,
    # mod.params = mod.params,
    # ++ hytreew params ++
    max.depth = max.depth,
    gamma = gamma,
    shrinkage = shrinkage,
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    minobsinnode = minobsinnode,
    minobsinnode.lin = minobsinnode.lin,
    part.minsplit = part.minsplit,
    part.xval = part.xval,
    part.max.depth = part.max.depth,
    part.cp = part.cp,
    part.minbucket = part.minbucket,
    lin.type = lin.type,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda,
    # -- hytreew params --
    max.iter = max.iter,
    earlystop.params = earlystop.params,
    init = init,
    cxrcoef = cxrcoef,
    print.error.plot = print.error.plot,
    print.progress.every = print.progress.every,
    base.verbose = base.verbose,
    verbose = verbose,
    trace = trace,
    prefix = prefix,
    print.plot = print.plot,
    plot.type = 'l'
  )

  # if lookback, use best n.iter to get fitted and predicted
  if (.gs && lookback) {
    sni <- selectiter(mod$error.valid, mod$error, plot = print.tune.plot)
    n.iter <- sni$best.nsteps
    if (verbose)
      msg2(
        "Selected",
        n.iter,
        "iterations based on smoothed",
        ifelse(is.null(mod$error.valid), "training", "validation"),
        "loss curve"
      )
    mod$selected.n.steps <- sni$best.nsteps
  } else {
    n.iter <- NULL # will use all iterations, will not be max.iter if earlystopping on training
  }

  # Fitted ----
  if (is.null(n.iter)) {
    fitted <- mod$fitted
  } else {
    fitted <- predict(mod, x, n.iter = n.iter)
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train)

  # VALID ----
  # error.valid <- if (!is.null(y.valid)) mod$error.valid else NULL

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (verbose) cat("\n")
    msg2("Getting predicted values...")
    predicted <- predict(mod, x.test, n.iter = n.iter)
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # Outro ----
  parameters <- list(
    init = init,
    max.iter = max.iter,
    earlystop.params = earlystop.params,
    learning.rate = learning.rate
    # mod.params = mod.params
  )
  extra <- list(gs = gs)
  # error.valid = error.valid)
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    parameters = parameters,
    call = NULL,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    varimp = NULL,
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_LIHADBoost
