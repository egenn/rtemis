# s_MARS.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org
# method = "cv" fails to find nk and penalty

#' Multivariate adaptive regression splines (MARS) (C, R)
#'
#' Trains a MARS model using `earth::earth`.
#' \[gS\] in Arguments description indicates that hyperparameter will be tuned if more than one value are provided
#' For more info on algorithm hyperparameters, see `?earth::earth`
#'
#' @inheritParams s_GLM
#' @inheritParams s_CART
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as `x`
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param glm List of parameters to pass to [glm]
#' @param degree \[gS\] Integer: Maximum degree of interaction. Default = 2
#' @param penalty \[gS\] Float: GCV penalty per knot. 0 penalizes only terms, not knots.
#' -1 means no penalty. Default = 3
#' @param pmethod \[gS\] Character: Pruning method: "backward", "none", "exhaustive", "forward",
#' "seqrep", "cv". Default = "forward"
#' @param nprune \[gS\] Integer: Max N of terms (incl. intercept) in the pruned model
#' @param nk \[gS\] Integer: Maximum number of terms created by the forward pass.
#' See `earth::earth`
#' @param thresh \[gS\] Numeric: Forward stepping threshold. Forward pass terminates if RSq
#' reduction is less than this.
#' @param minspan Numeric: Minimum span of the basis functions. Default = 0
#' @param ... Additional parameters to pass to `earth::earth`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export

s_MARS <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  grid.resample.params = setup.grid.resample(),
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  glm = NULL,
  degree = 2,
  penalty = 3, # if (degree > 1) 3 else 2
  nk = NULL,
  thresh = 0,
  minspan = 0,
  endspan = 0,
  newvar.penalty = 0,
  fast.k = 2,
  fast.beta = 1,
  linpreds = FALSE,
  pmethod = "forward",
  nprune = NULL,
  nfold = 4,
  ncross = 1,
  stratify = TRUE,
  wp = NULL,
  na.action = na.fail,
  metric = NULL,
  maximize = NULL,
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
    print(args(s_MARS))
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
  mod.name <- "MARS"

  # Dependencies ----
  dependency_check("earth")

  # Arguments ----
  if (missing(x)) {
    print(args(s_MARS))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_MARS))
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
    ifw = ifw,
    ifw.type = ifw.type,
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
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  x0 <- if (upsample || downsample) dt$x0 else x
  y0 <- if (upsample || downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (type == "Classification" && is.null(glm)) {
    glm <- list(family = binomial)
  }
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(nk)) nk <- min(200, max(20, 2 * NCOL(x))) + 1

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

  gs <- NULL
  if (gridCheck(pmethod, degree, nprune, penalty, nk, thresh)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        pmethod = pmethod,
        degree = degree,
        nprune = nprune,
        penalty = penalty,
        nk = nk
      ),
      fixed.params = list(glm = glm),
      weights = weights,
      metric = "MSE",
      maximize = FALSE,
      verbose = verbose,
      n.cores = n.cores
    )
    pmethod <- as.character(gs$best.tune$pmethod)
    degree <- gs$best.tune$degree
    nprune <- gs$best.tune$nprune
    penalty <- gs$best.tune$penalty
    nk <- gs$best.tune$nk
    thresh <- gs$best.tune$thresh
  }

  # earth::earth ----
  if (verbose) msg2("Training MARS model...", newline.pre = TRUE)
  if (verbose) {
    parameterSummary(
      pmethod,
      degree,
      nprune,
      ncross,
      nfold,
      penalty,
      nk,
      newline.pre = TRUE
    )
  }
  # We do not pass penalty or nk if pmethod is "cv", because they are not handled correctly by
  # update.earth or related function and error out.
  args <- c(
    list(
      x = x,
      y = y,
      weights = .weights,
      wp = wp,
      na.action = na.action,
      trace = trace,
      glm = glm,
      degree = degree,
      penalty = penalty,
      nk = nk,
      thresh = thresh,
      minspan = minspan,
      endspan = endspan,
      newvar.penalty = newvar.penalty,
      fast.k = fast.k,
      fast.beta = fast.beta,
      linpreds = linpreds,
      pmethod = pmethod,
      nprune = nprune,
      nfold = nfold,
      ncross = ncross,
      stratify = stratify
    ),
    list(...)
  )
  if (pmethod == "cv") args$penalty <- args$nk <- NULL
  mod <- do.call(earth::earth, args)
  if (trace > 0) print(summary(mod))
  params <- args
  params$x <- params$y <- NULL

  # Fitted ----
  fitted <- predict(mod)
  if (type == "Classification") {
    fitted.prob <- fitted
    fitted <- ifelse(fitted.prob >= .5, 1, 0)
    fitted <- factor(levels(y)[fitted + 1])
    levels(fitted) <- levels(y)
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (type == "Classification") {
      predicted.prob <- predicted
      predicted <- ifelse(predicted.prob >= .5, 1, 0)
      predicted <- factor(levels(y)[predicted + 1])
      levels(predicted) <- levels(y)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Variable importance ----
  .evimp <- as.matrix(earth::evimp(mod))
  .evimp <- earth::evimp(mod)
  varimp <- rep(0, NCOL(x))
  names(varimp) <- xnames
  .evimpnames <- rownames(.evimp)
  for (i in seq_len(NROW(.evimp))) {
    varimp[which(.evimpnames[i] == xnames)] <- .evimp[i, 4]
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    parameters = params,
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
    question = question
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
} # rtemis::s_MARS
