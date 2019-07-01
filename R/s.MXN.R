# s.MXN.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io
# TODO: Add weights / ipw?
# TODO: Consider order of L2Norm and BatchNorm

#' Neural Network with \code{mxnet} [C, R]
#'
#' Train a Neural Network using \code{mxnet} with optional early stopping
#'
#' Early stopping is considered after training has taken place for \code{min.epochs} epochs.
#' After that point, early stopping is controlled by three criteria:
#' an absolute threshold (\code{early.stop.absolute.threshold}),
#' a relative threshold (\code{early.stop.relative.threshold}),
#' or a relative variance across a set number of steps (\code{early.stop.realtiveVariance.threshold} along
#' \code{early.stop.n.steps}).
#' Early stopping by default (if you change none of the \code{early.stop} arguments), will look at training error
#' and stop when the relative variance of the loss over the last 24 steps (classification) or 12 steps (regression)
#' is lower than 5e-06 (classification) or lower than 5e-03 (regression). To set early stopping OFF, set all
#' early stopping criteria to NA.
#' It is important to tune learning rate and adjust max.epochs accordingly depending on the learning type
#' (Classification vs. Regression) and the specific dataset. Defaults can not be expected to work on all problems.
#'
#' @inheritParams s.GLM
#' @param n.hidden.nodes Integer vector: Length must be equal to the number of hidden layers you wish to create
#' @param activation String vector: Activation types to use: 'relu', 'sigmoid', 'softrelu', 'tanh'.
#'  If length < n of hidden layers, elements are recycled. See \code{mxnet::mx.symbol.Activation}
#' @param output String: "Logistic" for binary classification, "Softmax" for classification of 2 or more classes,
#' "Linear" for Regression. Defaults to "Logistic" for binary outcome, "Softmax" for 3+ classes, "LinearReg" for
#' regression.
#' @param net MXNET Symbol: provide a previously defined network. logger will not work in this case at the moment,
#' so early stopping cannot be applied
#' @param ctx MXNET context: \code{mxnet::mx.cpu()} to use CPU(s). Define N of cores using \code{n.cores} argument.
#' \code{mxnet::mx.gpu()} to use GPU. For multiple GPUs, provide list like such:
#' \code{ctx = list(mxnet::mx.gpu(0), mxnet::mx.gpu(1)} to use two GPUs.
#' @param l2.normalization Logical: If TRUE, apply L2 normalization after fully connected step. Default = FALSE
#' @param batch.normalization Logical: If TRUE, batch normalize before activation. Default = TRUE
#' @param max.epochs Integer: Number of iterations for training.
#' @param learning.rate Float: learning rate
#' @param dropout Float (0, 1): Probability of dropping nodes
#' @param dropout.before Integer: Index of hidden layer before which dropout should be applied
#' @param dropout.after Integer: Index of hidden layer after which dropout should be applied
#' @param eval.metric String: Metrix used for evaluation during train. Default: "rmse"
#' @param plot.graphviz Logical: if TRUE, plot the network structure using \code{graphviz}
#' @param n.cores Integer: Number of cores to use. Caution: Only set to >1 if you're sure MXNET is not using already
#'   using multiple cores
#' @param ... Additional parameters to be passed to \code{mxnet::mx.model.FeedForward.create}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Deep Learning
#' @export

s.MXN <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.valid = NULL, y.valid = NULL,
                  upsample = FALSE,
                  upsample.seed = NULL,
                  net = NULL,
                  n.hidden.nodes = NULL,
                  output = NULL,
                  ctx = mxnet::mx.cpu(),
                  initializer = mxnet::mx.init.Xavier(),
                  batch.normalization = TRUE,
                  l2.normalization = FALSE,
                  activation = 'relu',
                  optimizer = "adadelta",
                  batch.size = NULL,
                  momentum = .9,
                  max.epochs = 1000,
                  min.epochs = 25,
                  early.stop = c("train", "valid"),
                  early.stop.absolute.threshold = NA,
                  early.stop.relative.threshold = NA,
                  early.stop.relativeVariance.threshold = NULL,
                  early.stop.n.steps = NULL,
                  learning.rate = NULL,
                  dropout = 0,
                  dropout.before = 1,
                  dropout.after = 0,
                  eval.metric = NULL,
                  minimize = NULL,
                  arg.params = NULL,
                  mx.seed = NULL,
                  x.name = NULL,
                  y.name = NULL,
                  # type = "auto",
                  plot.graphviz = FALSE,
                  print.plot = TRUE,
                  print.error.plot = NULL,
                  rtlayout.mat = c(2, 1),
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  question = NULL,
                  verbose = TRUE,
                  verbose.mxnet = TRUE,
                  verbose.checkpoint = FALSE,
                  outdir = NULL,
                  n.cores = rtCores,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.MXN))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "MXN"
  Sys.setenv(MXNET_CPU_WORKER_NTHREADS = n.cores)

  # [ DEPENDENCIES ] ====
  if (!depCheck("mxnet", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.MXN))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (n.cores > 1) verbose.mxnet <- FALSE
  if (max.epochs < min.epochs) max.epochs <- min.epochs
  if (is.null(print.error.plot)) print.error.plot <- print.plot
  early.stop <- match.arg(early.stop)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    x.valid, y.valid,
                    # ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  x.valid <- dt$x.valid
  y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  x.dm <- data.matrix(x)
  if (type == "Classification") {
    y0 <- y
    y <- as.numeric(y) - 1
    n.outputs <- length(levels(y0))
  }
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  # if (is.null(eval.metric)) {
  #   if (type == "Classification") {
  #     eval.metric <- mxnet::mx.metric.accuracy
  #   } else {
  #     eval.metric <- mxnet::mx.metric.rmse
  #   }
  # }

  # Default n.hidden.nodes
  if (is.null(n.hidden.nodes)) n.hidden.nodes <- NCOL(x)
  # if (n.hidden.nodes == 0) n.hidden.nodes <- NULL

  # Default batch.size
  if (is.null(batch.size)) {
    if (is.null(y.valid)) {
      batch.size <- length(y)
    } else {
      # MXNET limitation still as of 11.11.2018:
      # batch size limited by size of validation set
      batch.size <- min(length(y), length(y.valid))
    }
  }

  # Default learning.rate for Classification vs. Regression
  if (is.null(learning.rate)) learning.rate <- ifelse(type == "Classification", .05, .005)

  # [ NETWORK ] ====
  if (is.null(net)) {
    if (n.hidden.nodes[1] == 0) {
      n.hnodes <- n.hlayers <- 0
    } else {
      n.hnodes <- n.hidden.nodes
      n.hlayers <- length(n.hidden.nodes)
    }

    if (length(activation) < n.hlayers) activation <- rep(activation, n.hlayers/length(activation))

    ### '- INPUT ====
    net <- mxnet::mx.symbol.Variable("data")

    ### '- HIDDEN LAYERS ====
    if (n.hlayers > 0) {
      for (i in seq(n.hlayers)) {
        if (dropout > 0) {
          if (i == dropout.before) net <- mxnet::mx.symbol.Dropout(net,
                                                                   p = dropout,
                                                                   name = paste0("Dropout.", i))
        }
        net <- mxnet::mx.symbol.FullyConnected(data = net,
                                               name = paste0("FC.", i),
                                               num_hidden = n.hnodes[i])
        if (l2.normalization) net <- mxnet::mx.symbol.L2Normalization(net,
                                                                      name = paste0("L2Norm.", i))
        if (batch.normalization) net <- mxnet::mx.symbol.BatchNorm(net,
                                                                   name = paste0("BatchNorm.", i))
        net <- mxnet::mx.symbol.Activation(data = net,
                                           name = paste0(activation[i], ".", i),
                                           act.type = activation[i])
        if (dropout > 0) {
          if (i == dropout.after) net <- mxnet::mx.symbol.Dropout(net,
                                                                  p = dropout,
                                                                  name = paste0("Dropout.", i))
        }
      }
    } # /if (n.hlayers > 0)

    # if (dropout > 0) {
    #   net <- mxnet::mx.symbol.Dropout(net, p = dropout, name = "Dropout")
    # }

    ### '- OUTPUT ====
    # n.outputs <- if (type == "Regression") 1 else length(levels(y0))
    if (is.null(output)) {
      if (type == "Classification") {
        if (n.outputs == 2) {
          output <- "Logistic"
        } else {
          output <- "Softmax"
        }
      } else {
        output <- "Linear"
      }
    }

    if (!is.null(eval.metric)) eval.metric.name <- sub('.*\\.', '', deparse(substitute(eval.metric)))

    if (output == "Logistic") {
      net <- mxnet::mx.symbol.FullyConnected(data = net, name = "FC.final", num_hidden = 1)
      net <- mxnet::mx.symbol.LogisticRegressionOutput(data = net, name = "Logistic")
      if (is.null(eval.metric)) {
        eval.metric <- mxnet::mx.metric.logloss
        eval.metric.name <- "logloss"
      }
      if (is.null(minimize)) minimize <- TRUE
    } else if (output == "Softmax") {
      net <- mxnet::mx.symbol.FullyConnected(data = net, name = "FC.final", num_hidden = n.outputs)
      net <- mxnet::mx.symbol.SoftmaxOutput(data = net, name = "Softmax")
      if (is.null(eval.metric)) {
        eval.metric <- mxnet::mx.metric.accuracy
        eval.metric.name <- "accuracy"
      }
      if (is.null(minimize)) minimize <- FALSE
    } else {
      net <- mxnet::mx.symbol.FullyConnected(data = net, name = "FC.final", num_hidden = 1)
      net <- mxnet::mx.symbol.LinearRegressionOutput(data = net, name = "LinearReg")
      if (is.null(eval.metric)) {
        eval.metric <- mxnet::mx.metric.mse
        eval.metric.name <- "mse"
      }
      if (is.null(minimize)) minimize <- TRUE
    }

  } else {# / if (is.null(net))
    # This is likely inaccurate for more complex networks
    # net <- mxnet::mx.apply(net)
    n.hlayers <- (length(mxnet::arguments(net)) - 2) / 2
  }

  # [ GRAPHVIZ ] ====
  if (plot.graphviz) mxnet::graph.viz(net)

  # [ MXN ] ====
  logger <- mxnet::mx.metric.logger$new()
  if (verbose) msg0("Training Neural Network ", type, " with ",
                    n.hlayers, " hidden ", ifelse(n.hlayers == 1, "layer", "layers"),
                    "...\n", newline = TRUE)

  if (!is.null(x.valid) & !is.null(y.valid)) {
    eval.data <- list(data = data.matrix(x.valid),
                      label = y.valid)
  } else {
    eval.data <- NULL
  }

  # Early stopping for Classification vs. Regression
  if (early.stop != 'none') {
    if (type == "Classification") {
      if (is.null(early.stop.n.steps)) early.stop.n.steps <- 24
      if (is.null(early.stop.relativeVariance.threshold)) early.stop.relativeVariance.threshold <- 5e-6
    } else {
      if (is.null(early.stop.n.steps)) early.stop.n.steps <- 12
      if (is.null(early.stop.relativeVariance.threshold)) early.stop.relativeVariance.threshold <- 1e-5
    }
  }

  # Set MXNET seed
  if (!is.null(mx.seed)) mxnet::mx.set.seed(mx.seed)
  # Train network
  extra.args <- list(...)
  net.args <- c(list(symbol = net,
                     X = x.dm,
                     y = y,
                     ctx = ctx,
                     num.round = max.epochs,
                     optimizer = optimizer,
                     initializer = initializer,
                     array.batch.size = batch.size,
                     learning.rate = learning.rate,
                     eval.data = eval.data,
                     eval.metric = eval.metric,
                     epoch.end.callback = rt.mx.callback.earlyStop(period = 1,
                                                                   logger = logger,
                                                                   which.error = early.stop,
                                                                   n.steps = early.stop.n.steps,
                                                                   min.epochs = min.epochs,
                                                                   early.stop.absolute.threshold = early.stop.absolute.threshold,
                                                                   early.stop.relative.threshold = early.stop.relative.threshold,
                                                                   minimize = minimize,
                                                                   early.stop.relativeVariance.threshold = early.stop.relativeVariance.threshold,
                                                                   early.stop.n.steps = early.stop.n.steps,
                                                                   verbose = verbose,
                                                                   verbose.checkpoint = verbose.checkpoint),
                     array.layout = "rowmajor",
                     arg.params = arg.params,
                     verbose = verbose.mxnet),
                extra.args)
  if (optimizer == "sgd") net.args$momentum <- momentum
  if (optimizer == "adadelta") net.args$learning.rate <- NULL
  mod <- do.call(mxnet::mx.model.FeedForward.create, net.args)

  # If Classification, cannot plot mplot3.conf which uses layout itself
  # if (print.error.plot & print.plot & is.null(outdir) & !is.null(rtlayout.mat))
  #   rtlayout(rtlayout.mat[1], rtlayout.mat[2])

  if (print.error.plot) {
    if (is.null(y.valid)) {
      Metric <- logger$train
    } else {
      Metric <- list(Training = logger$train, Validation = logger$eval)
    }
    if (!is.null(outdir)) {
      filename.metric <- paste0(outdir, "s.", mod.name, "_TrainingMetric.pdf")
    } else {
      filename.metric <- NULL
    }

    if (length(Metric) > 0) {
      mplot3.xy(seq(logger$train), Metric,
                # list(Training = logger$train, Validation = logger$eval),
                xlab = "Epochs", ylab = eval.metric.name,
                type = 'l',
                lwd = 1.5,
                line.col = c(ucsfCol$teal, ucsfCol$orange),
                group.legend = !is.null(y.valid),
                group.adj = .95,
                zero.lines = FALSE,
                # mar = rep(1.5, 4),
                theme = plot.theme,
                filename = filename.metric)
    }
  } # / print.error.plot

  # return(list(mod = mod, x.dm = x.dm))
  # [ FITTED ] ====
  if (type == "Regression") {
    fitted <- c(predict(mod, x.dm, array.layout = "rowmajor"))
    error.train <- modError(y, fitted, type = type)
  } else {
    fitted.prob <- predict(mod, x.dm, array.layout = "rowmajor")
    if (min(dim(fitted.prob)) == 1) {
      fitted.prob <- c(1 - fitted.prob)
      fitted <- factor(ifelse(fitted.prob >= .5, 1, 0), levels = c(1, 0))
    } else {
      fitted.prob <- predict(mod, x.dm, array.layout = "rowmajor")
      fitted <- factor(apply(fitted.prob, 2, function(i) which.max(i)))
    }
    levels(fitted) <- levels(y0)
    error.train <- modError(y0, fitted, fitted.prob)
  }

  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression") {
      predicted <- c(predict(mod, data.matrix(x.test), array.layout = "rowmajor"))
    } else {
      predicted.prob <- predict(mod, data.matrix(x.test), array.layout = "rowmajor")
      if (min(dim(predicted.prob)) == 1) {
        # Classification with Logistic output
        predicted.prob <- 1 - predicted.prob
        predicted <- factor(ifelse(predicted.prob >= .5, 1, 0), levels = c(1, 0))
      } else {
        # Classification with Softmax output
        predicted <- factor(apply(predicted.prob, 2, function(i) which.max(i)))
      }
      levels(predicted) <- levels(y0)
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(logger = logger)
  parameters <- list(n.hidden.nodes = n.hidden.nodes,
                     max.epochs = max.epochs,
                     optimizer = optimizer,
                     batch.size = batch.size,
                     learning.rate = learning.rate,
                     early.stop = early.stop,
                     early.stop.n.steps = early.stop.n.steps,
                     early.stop.relativeVariance.threshold = early.stop.relativeVariance.threshold,
                     momentum = momentum,
                     eval.metric = eval.metric)
  rt <- rtModSet(mod.name = mod.name,
                 type = type,
                 y.train = if (type == "Classification") y0 else y,
                 y.test = y.test,
                 x.name = x.name,
                 xnames = xnames,
                 mod = mod,
                 fitted = fitted,
                 fitted.prob = fitted.prob,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 predicted.prob = predicted.prob,
                 se.prediction = NULL,
                 parameters = parameters,
                 error.test = error.test,
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

  # if (print.error.plot & print.plot & is.null(outdir) & !is.null(rtlayout.mat)) rtlayout()

  if (save.mod) mxnet::mx.model.save(mod, prefix = paste0(outdir, "rt_mxnet"), length(logger$train))
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s.MXN


rt.mx.callback.earlyStop <- function(period,
                                     logger,
                                     which.error,
                                     n.steps,
                                     min.epochs,
                                     early.stop.absolute.threshold,
                                     early.stop.relative.threshold,
                                     minimize,
                                     early.stop.relativeVariance.threshold,
                                     early.stop.n.steps,
                                     verbose,
                                     verbose.checkpoint) {
  function(iteration, nbatch, env,
           verbose = TRUE) {
    verbose = T
    if (nbatch %% period == 0 && !is.null(env$metric)) {
      result <- env$metric$get(env$train.metric)
      if (nbatch != 0 && verbose)
        message("Batch [", nbatch, "] Train-", result$name,
                "=", result$value)
      if (!is.null(logger)) {
        if (class(logger) != "mx.metric.logger") {
          stop("Invalid mx.metric.logger.")
        }
        logger$train <- c(logger$train, result$value)
        if (!is.null(env$eval.metric)) {
          result <- env$metric$get(env$eval.metric)
          if (nbatch != 0 && verbose)
            message("Batch [", nbatch, "] Validation-",
                    result$name, "=", result$value)
          logger$eval <- c(logger$eval, result$value)
        }
      }
    }

    early.stop.metric <- if (which.error == "train") logger$train else logger$eval
    early.stop.check <- checkpoint.earlyStopping(x = early.stop.metric,
                                                 absolute.threshold = early.stop.absolute.threshold,
                                                 relative.threshold = early.stop.relative.threshold,
                                                 minimize = minimize,
                                                 relativeVariance.threshold = early.stop.relativeVariance.threshold,
                                                 n.steps = early.stop.n.steps,
                                                 min.steps = min.epochs,
                                                 verbose = verbose.checkpoint)

    if (early.stop.check$stop) {
      # msg0("Early stopping threshold reached (nbatch = ", nbatch, ", period = ", period, ")",
      #      as.message = TRUE)
      if (verbose) msg0("Early stopping threshold reached.", as.message = TRUE)
      if (verbose) printls(early.stop.check)
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
} # rtemis::rt.mx.callback.earlyStop
