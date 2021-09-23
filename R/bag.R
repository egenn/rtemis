# bag.R
# ::rtemis::
# 2018-9 E.D. Gennatas lambdamd.org
# check fitted.prob and predicted.prob; varimp

#' Bag an \pkg{rtemis} learner for regression or classification [C, R]
#'
#' Train a bagged ensemble using any learner
#'
#' @inheritParams s.GLM
#' @param mod Character: Algorithm to bag, for options, see \link{modSelect}
#' @param k Integer: Number of base learners to train
#' @param mod.params Named list of arguments for \code{mod}
#' @param mtry Integer: Number of features to randomly sample for each base learner.
#' @param .resample List: Resample settings to use. There is no need to edit this, unless you want to change the type of
#' resampling. It will use stratified bootstrap by default. Use \link{rtset.resample} for convenience.
#' Default = \code{rtset.resample(resampler = "strat.boot", n.resamples = k)}
#' @param aggr.fn Function: used to average base learners' predictions. Default = mean for Classification, median for
#' Regression
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param base.verbose Logical: \code{verbose} argument passed to learner
#' @param print.base.plot Logical: Passed to \code{print.plot} argument of base learner, i.e. if TRUE, print error plot
#' for each base learner
#' @param n.cores Integer: Number of cores to use
#' @param parallel.type Character: "fork" or "psock". Type of parallelization. Default = "fork" for macOS and Linux, "psock" for Windows
#' @param outdir: Character: Path to output directory to save model. Default = NULL
#' @param ... Additional parameters to be passed to learner
#' @author E.D. Gennatas
#' @export

bag <- function(x, y = NULL,
                x.test = NULL, y.test = NULL,
                weights = NULL,
                mod = 'cart',
                k = 10,
                mtry = NULL,
                mod.params = list(),
                ipw = TRUE,
                ipw.type = 2,
                upsample = FALSE,
                downsample = FALSE,
                resample.seed = NULL,
                .resample = rtset.resample(resampler = "strat.boot",
                                           n.resamples = k),
                aggr.fn = NULL,
                x.name = NULL,
                y.name = NULL,
                question = NULL,
                base.verbose = FALSE,
                verbose = TRUE,
                trace = 0,
                print.plot = TRUE,
                plot.fitted = NULL,
                plot.predicted = NULL,
                plot.theme = getOption("rt.fit.theme", "lightgrid"),
                print.base.plot = FALSE,
                n.cores = rtCores,
                parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock"),
                outdir = NULL, ...) {

  # [ Intro ] ====
  if (missing(x)) {
    print(args(bag))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)

  # [ Arguments ] ====
  n.cores <- as.numeric(n.cores)[1]
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  extra.args <- list(...)
  mod.params <- c(mod.params, extra.args)

  # [ Data ] ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  # x.valid <- dt$x.valid
  # y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  # .weights <- if (is.null(weights) & ipw) dt$weights else weights
  # TODO: x0, y0
  # x0 <- if (upsample|downsample) dt$x0 else x
  # y0 <- if (upsample|downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(aggr.fn)) aggr.fn <- if (type == "Classification") mean else median
  n.features <- NCOL(x)
  if (is.null(mtry)) {
    if (n.features <= 20) mtry <- n.features
    mtry <- if (type == "Classification") floor(sqrt(n.features)) else max(floor(n.features/3), 1)
  }

  # [ BAG ] ====
  mod.name <- paste0("Bagged", toupper(mod))
  mod.desc <- modSelect(mod, desc = TRUE)

  if (verbose) parameterSummary(mod, mod.params)

  # [ resLearn ] ====
  if (verbose) msg0("Bagging ", .resample$n.resamples, " ", mod.desc, "...")
  rl <- resLearn(x = x, y = y,
                 mod = mod,
                 resample.rtset = .resample,
                 weights = weights,
                 params = mod.params,
                 verbose = verbose,
                 res.verbose = base.verbose,
                 save.mods = TRUE,
                 outdir = NULL,
                 n.cores = n.cores,
                 parallel.type = parallel.type)

  # [ Fitted ] ====
  if (!verbose) pbapply::pboptions(type = "none")

  if (type == "Classification") {
    fitted.bag <- pbapply::pbsapply(rl$mods, function(k) as.numeric(predict(k$mod1, x)),
                                    cl = n.cores)
    fitted <- factor(round(apply(fitted.bag, 1, aggr.fn)))
    levels(fitted) <- levels(y)
  } else if (type == "Regression") {
    fitted.bag <- pbapply::pbsapply(rl$mods, function(k) predict(k$mod1, x),
                                    cl = n.cores)
    fitted <- apply(fitted.bag, 1, aggr.fn)
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train)

  # [ Predicted ] ====
  predicted.bag <- predicted <- error.test <- NULL

  if (!is.null(x.test)) {
    # as.numeric is to convert factors to numeric for type = Classification
    predicted.bag <- pbapply::pbsapply(rl$mods, function(k) as.numeric(predict(k$mod1, x.test)),
                                       cl = n.cores)
    if (type == "Classification") {
      predicted <- factor(round(apply(predicted.bag, 1, aggr.fn)))
      levels(predicted) <- levels(y)
    } else {
      predicted <- apply(predicted.bag, 1, aggr.fn)
    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # [ VARIMP ] ====
  if (length(rl$mods[[1]]$mod1$varimp) > 0) {
    varimp.res <- sapply(rl$mods, function(j) j$mod1$varimp)
    varimp.res[is.na(varimp.res)] <- 0
    varimp <- apply(varimp.res, 1, aggr.fn)
  } else {
    varimp <- NULL
  }


  # [ Outro ] ====
  parameters <- list(mod = mod.name,
                     mod.params = mod.params,
                     k = k)
  rt <- rtModBag$new(mod.name = mod.name,
                     y.train = y,
                     y.test = y.test,
                     x.name = x.name,
                     y.name = y.name,
                     xnames = xnames,
                     bag.resample.rtset = .resample,
                     mod = rl,
                     type = type,
                     fitted.bag = fitted.bag,
                     fitted = fitted,
                     se.fit.bag = NULL,
                     se.fit = NULL,
                     error.train = error.train,
                     predicted.bag = predicted.bag,
                     predicted = predicted,
                     se.prediction.bag = NULL,
                     se.prediction = NULL,
                     aggr.fn = aggr.fn,
                     error.test = error.test,
                     varimp = varimp,
                     parameters = parameters,
                     question = question,
                     extra = NULL)

  if (print.plot & !is.null(outdir)) {
    filename.train <- paste0(outdir, "s.", mod.name, "_Fitted.vs.True.pdf")
    if (!is.null(y.test)) {
      filename.test <- paste0(outdir, "s.", mod.name, "_Predicted.vs.True.pdf")
    }
  } else {
    filename.train <- filename.test <- NULL
  }

  if (print.plot) {
    if (plot.fitted | !is.null(outdir)) plot(rt, estimate = "fitted", theme = plot.theme,
                                             print.plot = plot.fitted, filename = filename.train)
                                             # main = paste0(mod.name, "Bagging (k = ", k, ") Training"))
    if (plot.predicted | !is.null(outdir)) plot(rt, estimate = "predicted", theme = plot.theme,
                                                print.plot = plot.predicted, filename = filename.test)
                                                # main = paste0(mod.name, "Bagging (k = ", k, ") Testing"))
  }
  if (!is.null(outdir)) rtSave(rt, outdir, verbose = verbose)
  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::bag
