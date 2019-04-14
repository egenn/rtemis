# s.CART.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Classification and Regression Trees [C, R, S]
#'
#' Train a CART for regression or classification using \code{rpart}
#'
#' [gS] indicates grid search will be performed automatically if more than one value is passed
#' @inheritParams s.GLM
#' @param method String: "auto", "anova", "poisson", "class" or "exp". Default = "auto"
#' @param cp [gS] Float: Complexity threshold for allowing a split. Default = .01
#' @param maxdepth [gS] Integer: Maximum depth of tree. Default = 20
#' @param minsplit [gS] Integer: Minimum number of cases that must belong in a node before considering a split.
#' Default = 2
#' @param minbucket [gS] Integer: Minimum number of cases allowed in a child node. Default = round(minsplit/3)
#' @param prune.cp [gS] Float: Complexity for cost-complexity pruning after tree is built
#' @param use.prune.rpart.rt [Testing only, do not change]
#' @param return.unpruned Logical: If TRUE and \code{prune.cp} is set, return unpruned tree under \code{extra}
#' in \link{rtMod}
#' @param grid.resample.rtset List: Output of \link{rtset.resample} defining \link{gridSearchLearn} parameters.
#' Default = \code{rtset.resample("kfold", 5)}
#' @param grid.search.type String: Type of grid search to perform: "exhaustive" or "randomized". Default = "exhaustive"
#' @param grid.randomized.p Float (0, 1): If \code{grid.search.type = "randomized"}, randomly run this proportion of
#' combinations. Default = .1
#' @param metric String: Metric to minimize, or maximize if \code{maximize = TRUE} during grid search.
#' Default = NULL, which results in "Balanced Accuracy" for Classification,
#' "MSE" for Regression, and "Coherence" for Survival Analysis.
#' @param maximize Logical: If TRUE, \code{metric} will be maximized if grid search is run. Default = FALSE
#' @param parms List of additional parameters for the splitting function.
#' See \code{rpart::rpart("parms")}
#' @param cost Vector, Float (> 0): One for each variable in the model.
#' See \code{rpart::rpart("cost")}
#' @param model Logical: If TRUE, keep a copy of the model. Default = TRUE
#' @param grid.verbose Logical: Passed to \link{gridSearchLearn}
#' @param n.cores Integer: Number of cores to use. Defaults to available cores reported by
#' \code{future::availableCores()}, unles option \code{rt.cores} is set at the time the library is loaded
#' @return Object of class \link{rtMod}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s.CART <- function(x, y = NULL,
                   x.test = NULL, y.test = NULL,
                   x.name = NULL, y.name = NULL,
                   weights = NULL,
                   ipw = TRUE,
                   ipw.type = 2,
                   upsample = FALSE,
                   upsample.seed = NULL,
                   method = "auto",
                   parms = NULL,
                   minsplit = 2,
                   minbucket = round(minsplit/3),
                   cp = 0.01,
                   maxdepth = 20,
                   maxcompete = 0,
                   maxsurrogate = 0,
                   usesurrogate = 2,
                   surrogatestyle = 0,
                   xval = 0,
                   cost = NULL,
                   model = TRUE,
                   prune.cp = NULL,
                   use.prune.rpart.rt = TRUE,
                   return.unpruned = FALSE,
                   grid.resample.rtset = rtset.resample("kfold", 5),
                   grid.search.type = c("exhaustive", "randomized"),
                   grid.randomized.p = .1,
                   metric = NULL,
                   maximize = NULL,
                   na.action = na.exclude,
                   n.cores = rtCores,
                   print.plot = TRUE,
                   plot.fitted = NULL,
                   plot.predicted = NULL,
                   plot.theme = getOption("rt.fit.theme", "lightgrid"),
                   question = NULL,
                   verbose = TRUE,
                   grid.verbose = TRUE,
                   outdir = NULL,
                   save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
                   rtModLog = NULL) {

  tree.depth <- getFromNamespace("tree.depth", "rpart")

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.CART))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "CART"

  # [ DEPENDENCIES ] ====
  if (!depCheck("rpart", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.CART))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = F), "/")
  control <- list(minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp,
                  maxcompete = maxcompete,
                  maxsurrogate = maxsurrogate,
                  usesurrogate = usesurrogate,
                  surrogatestyle = surrogatestyle,
                  maxdepth = maxdepth,
                  xval = xval)
  grid.search.type <- match.arg(grid.search.type)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  x0 <- if (upsample) dt$x0 else x # x0, y0 are passed to gridSearchLearn
  y0 <- if (upsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  df.train <- data.frame(y = y, x)
  if (method == "auto") {
    if (type == "Regression") {
      method <- "anova"
      if (is.null(metric)) metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    } else if (type == "Classification") {
      method <- "class"
      if (is.null(metric)) metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Survival") {
      method <- "exp"
      if (is.null(metric)) metric <- "Concordance"
      if (is.null(maximize)) maximize <- TRUE
    } else {
      stop("Method of type", method, "is not supported")
    }
  }
  if (is.null(cost)) cost <- rep(1, NCOL(x))
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
    } else if (type == "Regression") {
      metric <- "MSE"
    } else {
      metric <- "Concordance"
    }
  }
  if (type == "Classification") n.classes <- length(levels(y))

  # [ FORMULA ] ====
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste0(y.name, " ~ ", features))

  # [ GRID SEARCH for prune.cp ] ====
  if (gridCheck(maxdepth, minsplit, minbucket, cp, prune.cp)) {
    gs <- gridSearchLearn(x0, y0,
                          mod = mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(maxdepth = maxdepth,
                                             minsplit = minsplit,
                                             minbucket = minbucket,
                                             cp = cp,
                                             prune.cp = prune.cp),
                          fixed.params = list(method = method,
                                              model = model,
                                              maxcompete = maxcompete,
                                              maxsurrogate = maxsurrogate,
                                              usesurrogate = usesurrogate,
                                              surrogatestyle = surrogatestyle,
                                              xval = xval,
                                              cost = cost,
                                              na.action = na.action,
                                              ipw = ipw,
                                              ipw.type = ipw.type,
                                              upsample = upsample,
                                              upsample.seed = upsample.seed),
                          search.type = grid.search.type,
                          randomized.p = grid.randomized.p,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          verbose = grid.verbose,
                          n.cores = n.cores)
    maxdepth <- gs$best.tune$maxdepth
    minsplit <- gs$best.tune$minsplit
    minbucket <- gs$best.tune$minbucket
    cp <- gs$best.tune$cp
    prune.cp <- gs$best.tune$prune.cp
  } else {
    gs <- NULL
  }
  parameters <- list(prune.cp = prune.cp,
                     method = method,
                     model = model,
                     minsplit = minsplit,
                     minbucket = minbucket,
                     cp = cp,
                     maxcompete = maxcompete,
                     maxsurrogate = maxsurrogate,
                     usesurrogate = usesurrogate,
                     surrogatestyle = surrogatestyle,
                     maxdepth = maxdepth,
                     xval = xval,
                     cost = cost,
                     na.action = na.action,
                     weights = .weights)

  # [ RPART ] ====
  if (verbose) msg("Training CART...", newline = TRUE)
  mod <- rpart::rpart(formula = .formula,
                      data = df.train,
                      weights = .weights,
                      method = method,
                      model = model,
                      control = control,
                      cost = cost,
                      na.action = na.action)

  # [ COST-COMPLEXITY PRUNING ] ====
  if (!is.null(prune.cp)) {
    if (return.unpruned) mod.unpruned <- mod
    if (use.prune.rpart.rt) {
      mod <- prune.rpart.rt(mod, cp = prune.cp)
    } else {
      mod <- rpart::prune(mod, cp = prune.cp)
    }
  }

  # [ FITTED ] ====
  fitted.prob <- NULL
  if (type == "Regression" | type == "Survival") {
    fitted <- predict(mod, x, type = "vector")
  } else if (type == "Classification") {
    if (n.classes == 2) {
      fitted.prob <- predict(mod, x, type = "prob")[, 1]
    } else {
      fitted.prob <- predict(mod, x, type = "prob")
    }
    fitted <- predict(mod, x, type = "class")
  }


  attr(fitted, "names") <- NULL
  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression" | type == "Survival") {
      predicted <- predict(mod, x.test, type = "vector")
      predicted.prob <- NULL
    } else if (type == "Classification") {
      predicted.prob <- predict(mod, x.test, type = "prob")[, 1]
      predicted <- predict(mod, x.test, type = "class")
    }

    attr(predicted, "names") <- NULL
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- predicted.prob <- error.test <- NULL
  }

  # [ OUTRO ] ====
  varimp <- rep(NA, NCOL(x))
  varimp.cart <- if (!is.null(mod$variable.importance)) as.matrix(mod$variable.importance) else NULL
  varimp.index <- match(rownames(varimp.cart), colnames(x))
  varimp[varimp.index] <- varimp.cart
  varimp[is.na(varimp)] <- 0
  names(varimp) <- colnames(x)
  extra <- list(gridSearch = gs,
                imetrics = list(n.nodes = NROW(mod$frame),
                                depth = max(tree.depth(as.numeric(rownames(mod$frame))))))
  if (!is.null(prune.cp) & return.unpruned) extra$mod.unpruned <- mod.unpruned
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = parameters,
                 call = call,
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
                 error.test = error.test,
                 varimp = varimp,
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

} # rtemis::s.CART
