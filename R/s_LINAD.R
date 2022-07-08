# s_LINAD.R
# ::rtemis::
# 2019-20 E.D. Gennatas www.lambdamd.org
# Allow early stopping
# varimp: N cases-weighted mean of absolute coefficients

#' Linear Additive Tree [C, R]
#'
#' Train a Linear Additive Tree for Regression or Binary Classification
#'
#' The Linear Additive Tree trains a tree using a sequence of regularized linear models and splits.
#' We specify an upper threshold of leaves using \code{max.leaves} instead of directly defining a number,
#' because depending on the other parameters and the datasets, splitting may stop early.
#'
#' @inheritParams s_GLM
#' @param max.leaves Integer: Maximum number of terminal nodes to grow
#' @param nvmax [gS] Integer: Number of max features to use for lin.type "allSubsets", "forwardStepwise", or
#' "backwardStepwise". If values greater than n of features in \code{x} are provided, they will be excluded
#' @param lookback Logical: If TRUE, check validation error to decide best number of leaves to use.
#' Default = TRUE
#' @param lin.type Character: See \link{lincoef} for options
#' @param single.lin.type Character same options as \code{lin.type}, linear model to fit when
#' \code{max.leaves = 1}
#' @param init Initial value. Default = \code{mean(y)}
#' @param gamma Numeric: Soft weighting parameter. Weights of cases that do not
#' belong to node get multiplied by this amount 
#' @param lambda Float: lambda parameter for \code{MASS::lm.ridge} Default = .01
#' @param part.max.depth Integer: Max depth for each tree model within the additive tree
#' @param .gs internal use only
#' @param plot.tuning Logical: If TRUE, plot validation error during gridsearch
#' 
#' @author E.D. Gennatas
#' @export

s_LINAD <- function(x, y = NULL,
                    x.test = NULL, y.test = NULL,
                    weights = NULL,
                    ipw = TRUE,
                    ipw.type = 1,
                    upsample = FALSE,
                    downsample = FALSE,
                    resample.seed = NULL,
                    max.leaves = 8,
                    leaf.model = c("line", "spline"),
                    gamlearner = "gamsel",
                    gam.params = list(),  # use force.lambda to force gamsel over cv.gamsel
                    nvmax = 3,
                    force.max.leaves = NULL,
                    lookback = TRUE, # requires cross-validation with gridSearchLearn
                    gamma = .5,
                    gamma.on.lin = FALSE,
                    lin.type = c("glmnet", "forwardStepwise", "cv.glmnet", "lm.ridge",
                                 "allSubsets", "backwardStepwise", "glm", "solve", "none"),
                    single.lin.type = "glmnet",
                    cv.glmnet.nfolds = 5,
                    which.cv.glmnet.lambda = "lambda.min",
                    alpha = 1,
                    lambda = .05,
                    lambda.seq = NULL,
                    minobsinnode.lin = 10,
                    learning.rate =.5,
                    # rpart
                    part.minsplit = 2,
                    part.xval = 0,
                    part.max.depth = 1,
                    part.cp = 0,
                    part.minbucket = 1,
                    .rho = TRUE,
                    rho.max = 1000,
                    init = NULL,
                    metric = "auto",
                    maximize = NULL,
                    grid.resample.rtset = rtset.resample("kfold", 5),
                    grid.search.type = "exhaustive",
                    save.gridrun = FALSE,
                    grid.verbose = verbose,
                    select.leaves.smooth = TRUE,
                    cluster = FALSE,
                    keep.x = FALSE,
                    simplify = TRUE,
                    cxrcoef = FALSE,
                    n.cores = rtCores,
                    .preprocess = NULL,
                    verbose = TRUE,
                    plot.tuning = TRUE,
                    verbose.predict = FALSE,
                    trace = 1,
                    x.name = NULL,
                    y.name = NULL,
                    question = NULL,
                    outdir = NULL,
                    print.plot = TRUE,
                    plot.fitted = NULL,
                    plot.predicted = NULL,
                    plot.theme = rtTheme,
                    save.mod = FALSE,
                    .gs = FALSE) {

  # Intro  ----
  if (missing(x)) {
    print(args(s_LINAD))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "LINAD"
  leaf.model <- match.arg(leaf.model)

  # Dependencies  ----
  # ENH: deps for lincoef
  dependency_check("glmnet", "rpart")

  # Arguments  ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  lin.type <- match.arg(lin.type)
  # if (.gs && nvmax == 0) lin.type = "none"

  # Data  ----
  dt <- dataPrepare(x, y, x.test, y.test,
                    ipw = ipw, ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    .preprocess = .preprocess,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  x0 <- if (upsample) dt$x0 else x
  y0 <- if (upsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) parameterSummary(max.leaves,
                                learning.rate,
                                gamma,
                                lin.type,
                                nvmax,
                                alpha,
                                lambda,
                                minobsinnode.lin,
                                part.minsplit,
                                part.minbucket,
                                part.cp,
                                newline.pre = TRUE)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(init)) {
    init <- if (type == "Classification") 0 else mean(y)
  }
  init <- if (type == "Classification") 0 else mean(y)
  if (type == "Classification" && length(levels(y)) != 2)
    stop("s_LINAD currently supports only binary classification")

  if (!is.null(force.max.leaves)) lookback <- FALSE

  # Remove nvmax values that are greater than N features
  if (lin.type %in% c("allSubsets", "forwardStepwise", "backwardStepwise")) {
    nvmax <- Filter(function(z) z <= NCOL(x), nvmax)
  }

  # Grid Search  ----
  if (metric == "auto") {
    if (type == "Classification") {
      metric <- "Balanced Accuracy"
      if (is.null(maximize)) maximize <- TRUE
    } else if (type == "Regression") {
      metric <- "MSE"
      if (is.null(maximize)) maximize <- FALSE
    } else {
      metric <- "Concordance"
      maximize <- TRUE
    }
  }

  if (is.null(maximize)) {
    maximize <- if (type == "Classification") TRUE else FALSE
  }

  if (is.null(force.max.leaves)) {
    if (lookback) {
      gc <- gridCheck(learning.rate, gamma, minobsinnode.lin, lambda, nvmax,
                      part.minsplit, part.minbucket, part.cp)
    } else {
      # not recommended to cv max.leaves - use lookback instead; option likely will be removed
      gc <- gridCheck(learning.rate, gamma, minobsinnode.lin, lambda, nvmax,
                      part.minsplit, part.minbucket, part.cp, max.leaves)
    }
  } else {
    gc <- FALSE
  }

  # Must turn off plotting during parallel grid search or else likely to hang
  if (!.gs & n.cores > 1) plot.tuning <- FALSE

  if ((!.gs && gc) | (!.gs && lookback && max.leaves > 1)) {
    grid.params <- if (lookback) list() else list(max.leaves = max.leaves)
    grid.params <- c(grid.params, list(learning.rate = learning.rate,
                                       gamma = gamma,
                                       minobsinnode.lin = minobsinnode.lin,
                                       lambda = lambda,
                                       nvmax = nvmax,
                                       part.minsplit = part.minsplit,
                                       part.minbucket = part.minbucket,
                                       part.cp = part.cp))
    fixed.params <- if (lookback) list(max.leaves = max.leaves) else list()
    fixed.params <- c(fixed.params, list(init = init,
                                         lin.type = lin.type,
                                         gamma.on.lin = gamma.on.lin,
                                         cv.glmnet.nfolds = cv.glmnet.nfolds,
                                         which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                                         metric = metric,
                                         ipw = ipw,
                                         ipw.type = ipw.type,
                                         upsample = upsample,
                                         resample.seed = resample.seed,
                                         plot.tuning = plot.tuning,
                                         .gs = TRUE))

    gs <- gridSearchLearn(x = x0, y = y0,
                          mod = mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = grid.params,
                          fixed.params = fixed.params,
                          search.type = grid.search.type,
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          save.mod = save.gridrun,
                          verbose = verbose,
                          grid.verbose = grid.verbose,
                          n.cores = n.cores)

    gamma <- gs$best.tune$gamma
    lambda <- gs$best.tune$lambda
    minobsinnode.lin <- gs$best.tune$minobsinnode.lin
    learning.rate <- gs$best.tune$learning.rate
    part.minsplit <- gs$best.tune$part.minsplit
    part.minbucket <- gs$best.tune$part.minbucket
    part.cp <- gs$best.tune$part.cp
    nvmax <- gs$best.tune$nvmax
    # Return special from gridSearchLearn
    max.leaves <- gs$best.tune$n.leaves

    # if tuning fails
    if (length(lambda) == 0) lambda = .05

    # Now ready to train final full model
    .gs <- FALSE
    lookback <- FALSE
  } else {
    gs <- NULL
  }
  if (!is.null(force.max.leaves)) max.leaves <- force.max.leaves

  # shytreegamleaves  ----
  if (.gs) {
    if (lookback) {
      x.valid <- x.test
      y.valid <- y.test
    } else {
      x.valid <- y.valid <- NULL
    }
  } else {
    x.valid <- y.valid <- NULL
    msg("Training LINAD on full training set...", newline = TRUE)
  }

  if (length(nvmax) == 1 && nvmax == 0) lin.type <- "none"

  if (length(max.leaves) == 0) max.leaves <- 1
  mod <- shytreegamleaves(x, y,
                          x.valid = x.valid, y.valid = y.valid,
                          type = type,
                          lookback = lookback,
                          max.leaves = max.leaves,
                          gamleaves = leaf.model == "spline",
                          gamlearner = gamlearner,
                          gam.params = gam.params,
                          nvmax = nvmax,
                          gamma = gamma,
                          gamma.on.lin = gamma.on.lin,
                          alpha = alpha,
                          lambda = lambda,
                          lambda.seq = lambda.seq,
                          minobsinnode.lin = minobsinnode.lin,
                          learning.rate = learning.rate,
                          part.minsplit = part.minsplit,
                          part.xval = part.xval,
                          part.max.depth = part.max.depth,
                          part.cp = part.cp,
                          part.minbucket = part.minbucket,
                          .rho = .rho,
                          rho.max = rho.max,
                          weights = .weights,
                          lin.type = lin.type,
                          single.lin.type = single.lin.type,
                          cv.glmnet.nfolds = cv.glmnet.nfolds,
                          which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                          select.leaves.smooth = select.leaves.smooth,
                          verbose = verbose,
                          plot.tuning = plot.tuning,
                          trace = trace)

  # Get categorical index and levels
  cat_index <- which(sapply(x, is.factor))
  cat_levels <- lapply(x[, cat_index], function(i) levels(i))
  conditions_fmtd <- mod$all.step.leaves$rules$rule
  conditions_fmtd <- gsub(".*&", "", conditions_fmtd)
  # conditions_fmtd <- gsub(" ", "",
  #                         gsub(".*&", "", conditions_fmtd))
  conditions_fmtd[1] <- "All cases"
  negate_index <- grep("!", conditions_fmtd)
  conditions_fmtd <- gsub("!", "", conditions_fmtd)
  for (i in negate_index) {
    var <- gsub("%in%.*| ", "", conditions_fmtd[i])
    # levels <- cat_levels[[var]]
    levels <- paste0("'", cat_levels[[var]], "'")
    excl <- strsplit(gsub(".*\\(|\\)", "", conditions_fmtd[i]), ", ")[[1]]
    incl <- setdiff(levels, excl)
    conditions_fmtd[i] <- gsub("%in%.*", paste("%in%", paste(incl, collapse = ", ")),
                               conditions_fmtd[i])
  }
  conditions_fmtd <- gsub("%in%", "is", conditions_fmtd)
  conditions_fmtd <- gsub("^ |c\\(|\\)|'", "", conditions_fmtd)

  # ddSci numeric thresholds
  cond_num_index <- grep("<|>=", conditions_fmtd)
  for (i in cond_num_index) {
    threepart <- strsplit(conditions_fmtd[i], " ")[[1]]
    threepart[3] <- ddSci(threepart[3])
    conditions_fmtd[i] <- paste(threepart, collapse = " ")
  }

  mod$all.step.leaves$rules$condition <- conditions_fmtd

  parameters <- list(max.leaves = max.leaves,
                     learning.rate = learning.rate,
                     gamma = gamma,
                     n.leaves = mod$n.leaves,
                     lin.type = lin.type,
                     nvmax = nvmax,
                     alpha = alpha,
                     lambda = lambda,
                     lambda.seq = lambda.seq,
                     minobsinnode.lin = minobsinnode.lin,
                     part.minsplit = part.minsplit,
                     part.xval = part.xval,
                     part.max.depth = part.max.depth,
                     part.cp = part.cp,
                     weights = .weights,
                     init = init,
                     cv.glmnet.nfolds = cv.glmnet.nfolds,
                     which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                     metric = metric,
                     ipw = ipw,
                     ipw.type = ipw.type,
                     upsample = upsample,
                     resample.seed = resample.seed,
                     plot.tuning = plot.tuning)

  # Fitted  ----
  if (trace > 1) msg("Getting fitted values...")
  if (type == "Classification") {
    .fitted <- predict(mod, x, type = "all")
    fitted <- .fitted$estimate
    fitted.prob <- .fitted$probability
  } else {
    fitted <- predict(mod, x)
    fitted.prob <- NULL
  }
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train)

  # Predicted  ----
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    if (trace > 1) msg("Getting predicted values...")
    if (type == "Classification") {
      .predicted <- predict(mod, x.test,
                            type = "all",
                            learning.rate = learning.rate,
                            trace = trace,
                            verbose = verbose.predict)
      predicted <- .predicted$estimate
      predicted.prob <- .predicted$probability
    } else {
      predicted <- predict(mod, x.test,
                           learning.rate = learning.rate,
                           trace = trace,
                           verbose = verbose.predict)
      predicted.prob <- NULL
    }

    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # Outro  ----
  varimp <- if (lin.type == "none") {
    numeric()
  } else {
    # In general, look at each leaf's coefficients
    # This is probably a vague measure of overall variable importance.
    apply(mod$leaves$coefs[, -1, drop = FALSE], 2, function(i) mean(abs(i)))
  }
  rt <- rtModSet(mod = mod,
                 mod.name = mod.name,
                 type = type,
                 gridsearch = gs,
                 parameters = parameters,
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
                 question = question)

  if (cluster) {
    if (verbose) msg("Getting clusters...")
    mod$extra$clusters.train <- indexCasesByRules(x, mod$leaves$rules$rule)
    if (!is.null(x.test)) {
      mod$extra$clusters.test <- indexCasesByRules(x.test, mod$leaves$rules$rule)
    }
  }

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

} # rtemis:: s_LINAD
