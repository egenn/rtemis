# s.DN.R
# ::rtemis::
# 2018 Efstathios D Gennatas egenn.github.io

#' Artificial Neural Network [C, R]
#'
#' Train an DN for Regression or Classification using \pkg{deepnet}
#'
#' @inheritParams s.GLM
#' @param n.hidden.nodes Integer, vector: Length indicated number of hidden layers, value of each element determines
#' number of nodes for given layer
#' @author Efstathios D. Gennatas
#' @export

s.DN <- function(x, y = NULL,
                 x.test = NULL, y.test = NULL,
                 weights = NULL,
                 ipw = TRUE,
                 ipw.type = 2,
                 upsample = FALSE,
                 downsample =  FALSE,
                 resample.seed = NULL,
                 initW = NULL,
                 initB = NULL,
                 n.hidden.nodes = 10,
                 activation = NULL,
                 learning.rate = 0.8,
                 momentum = 0.5,
                 learningrate_scale = 1,
                 output = NULL,
                 numepochs = 200,
                 batchsize = NULL,
                 hidden_dropout = 0,
                 visible_dropout = 0,
                 metric = NULL,
                 maximize = NULL,
                 grid.resample.rtset = rtset.grid.resample(),
                 .preprocess = NULL,
                 verbose = TRUE,
                 verbose.predict = FALSE,
                 trace = 0,
                 n.cores = rtCores,
                 x.name = NULL,
                 y.name = NULL,
                 question = NULL,
                 outdir = NULL,
                 print.plot = TRUE,
                 plot.fitted = NULL,
                 plot.predicted = NULL,
                 plot.theme = getOption("rt.fit.theme", "lightgrid"),
                 save.mod = FALSE) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.DN))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "DN"

  # [ DEPENDENCIES ] ====
  if (!depCheck("deepnet", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample,
                    downsample =  downsample,
                    resample.seed = resample.seed,
                    .preprocess = .preprocess,
                    verbose = verbose)
  x <- data.matrix(dt$x)
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(batchsize)) batchsize <- round(.25 * length(y))

  if (is.null(activation)) {
    activation <- if (type == "Classification") "tanh" else "sigm"
  }

  if (is.null(output)) {
    output <- if (type == "Classification") "softmax" else "linear"
  }

  if (type == "Classification") {
    # The following sets 1 as the positive case, i.e. first level of factor
    y <- 2 - as.numeric(y)
    if (!is.null(x.test)) y.test <- 2 - as.numeric(y.test)
  }

  if (verbose) parameterSummary(n.hidden.nodes,
                                activation,
                                output,
                                batchsize,
                                numepochs,
                                learning.rate,
                                momentum,
                                hidden_dropout,
                                visible_dropout)

  # [ GRID SEARCH ] ====
  if (gridCheck(batchsize,
                numepochs,
                learning.rate,
                momentum)) {
    gs <- gridSearchLearn(x, y,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(batchsize, numepochs),
                          weights = weights,
                          maximize = maximize,
                          verbose = verbose,
                          n.cores = n.cores)
    batchsize <- gs$best.tune$batchsize
    numepochs <- gs$best.tune$numepochs
    learning.rate <- gs$best.tune$learning.rate
    momentum <- gs$best.tune$momentum
  } else {
    gs <- NULL
  }

  # [ DN ] ====
  if (verbose) msg0("Training Artificial Neural Network for ", type, "...",
                    newline.pre = TRUE)
  mod <- deepnet::nn.train(x, y,
                           initW = .weights,
                           initB = initB,
                           hidden = n.hidden.nodes,
                           activationfun = activation,
                           learningrate = learning.rate,
                           momentum = momentum,
                           learningrate_scale = learningrate_scale,
                           output = output,
                           numepochs = numepochs,
                           batchsize = batchsize,
                           hidden_dropout = hidden_dropout,
                           visible_dropout = visible_dropout)

  # [ FITTED ] ====
  fitted <- c(deepnet::nn.predict(mod, x))
  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- factor(levels(dt$y)[as.numeric(fitted.prob < .5) + 1],
                     levels = levels(dt$y))
  } else {
    fitted.prob <- NULL
  }
  error.train <- modError(dt$y, fitted)
  if (verbose) errorSummary(error.train)

  # [ PREDICTED ] ====
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- c(deepnet::nn.predict(mod, x.test))
    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- factor(levels(dt$y)[as.numeric(predicted.prob < .5) + 1],
                          levels = levels(dt$y))
    }
    if (!is.null(y.test)) {
      error.test <- modError(dt$y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # [ OUTRO ] ====
  extra <- list(gridSearch = gs)
  rt <- rtModSet(mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = list(initW = .weights,
                                    initB = initB,
                                    hidden = n.hidden.nodes,
                                    activation = activation,
                                    learningrate = learning.rate,
                                    momentum = momentum,
                                    learningrate_scale = learningrate_scale,
                                    output = output,
                                    numepochs = numepochs,
                                    batchsize = batchsize,
                                    hidden_dropout = hidden_dropout,
                                    visible_dropout = visible_dropout),
                 y.train = dt$y,
                 y.test = dt$y.test,
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
                 error.test = error.test,
                 varimp = NULL,
                 question = question,
                 extra = extra)

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

} # rtemis:: s.DN
