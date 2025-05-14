# s_SDA.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Sparse Linear Discriminant Analysis
#'
#' Train an SDA Classifier using `sparseLDA::sda`
#'
#' @inheritParams s_CART
#'
#' @param lambda L2-norm weight for elastic net regression
#' @param stop If STOP is negative, its absolute value corresponds to the desired number of variables. If STOP is positive, it corresponds to an upper bound on the L1-norm of the b coefficients. There is a one to one correspondence between stop and t. The default is -p (-the number of variables).
#' @param maxIte Integer: Maximum number of iterations
#' @param Q Integer: Number of components
#' @param tol Numeric: Tolerance for change in RSS, which is the stopping
#' criterion
#' @param .preprocess List of preprocessing parameters. Scaling and centering
#' is enabled by default, because it is crucial for algorithm to learn.
#' @param trace Integer: passed to `sparseLDA::sda`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @export
#' @examples
#' \dontrun{
#' datc2 <- iris[51:150, ]
#' datc2$Species <- factor(datc2$Species)
#' resc2 <- resample(datc2)
#' datc2_train <- datc2[resc2$Subsample_1, ]
#' datc2_test <- datc2[-resc2$Subsample_1, ]
#' # Without scaling or centering, fails to learn
#' mod_c2 <- s_SDA(datc2_train, datc2_test, .preprocess = NULL)
#' # Learns fine with default settings (scaling & centering)
#' mod_c2 <- s_SDA(datc2_train, datc2_test)
#' }
s_SDA <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  lambda = 1e-6,
  stop = NULL,
  maxIte = 100,
  Q = NULL,
  tol = 1e-6,
  .preprocess = setup.preprocess(scale = TRUE, center = TRUE),
  upsample = TRUE,
  downsample = FALSE,
  resample.seed = NULL,
  x.name = NULL,
  y.name = NULL,
  grid.resample.params = setup.resample("kfold", 5),
  gridsearch.type = c("exhaustive", "randomized"),
  gridsearch.randomized.p = .1,
  metric = NULL,
  maximize = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  grid.verbose = verbose,
  trace = 0,
  outdir = NULL,
  n.cores = rtCores,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE)
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_SDA))
    invisible(9)
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
  mod.name <- "SDA"

  # Dependencies ----
  dependency_check("sparseLDA")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
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
    .preprocess = .preprocess,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x0 <- if (upsample || downsample) dt$x0 else x # x0, y0 are passed to gridSearchLearn
  y0 <- if (upsample || downsample) dt$y0 else y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Classification", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)

  if (is.null(stop)) stop <- -NCOL(x)
  if (is.null(Q)) Q <- length(levels(y)) - 1

  if (type == "Regression") {
    if (is.null(metric)) metric <- "MSE"
    if (is.null(maximize)) maximize <- FALSE
  } else if (type == "Classification") {
    if (is.null(metric)) metric <- "Balanced Accuracy"
    if (is.null(maximize)) maximize <- TRUE
  }

  # Grid Search ----
  if (gridCheck(lambda, stop, Q)) {
    gs <- gridSearchLearn(
      x0,
      y0,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        lambda = lambda,
        stop = stop,
        Q = Q
      ),
      fixed.params = list(
        maxIte = maxIte,
        tol = tol,
        upsample = upsample,
        resample.seed = resample.seed
      ),
      search.type = gridsearch.type,
      randomized.p = gridsearch.randomized.p,
      weights = weights,
      metric = metric,
      maximize = maximize,
      verbose = grid.verbose,
      n.cores = n.cores
    )
    lambda <- gs$best.tune$lambda
    stop <- gs$best.tune$stop
    Q <- gs$best.tune$Q
  } else {
    gs <- NULL
  }

  # sparseLDA::sda ----
  params <- list(
    x = x,
    y = y,
    lambda = lambda,
    stop = stop,
    maxIte = maxIte,
    Q = Q,
    trace = trace,
    tol = tol
  )
  if (verbose)
    msg2("Running Sparse Linear Discriminant Analysis...", newline.pre = TRUE)
  mod <- do.call(sparseLDA::sda, args = params)

  # Fitted ----
  fitted.raw <- predict(mod, x)
  fitted <- fitted.raw$class
  fitted.prob <- fitted.raw$posterior
  train.projections <- fitted.raw$x
  error.train <- mod_error(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.raw <- predicted <- predicted.prob <- test.projections <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.raw <- predict(mod, x.test)
    predicted <- predicted.raw$class
    predicted.prob <- predicted.raw$posterior
    test.projections <- predicted.raw$x
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(
    fitted.prob = fitted.prob,
    predicted.prob = predicted.prob,
    train.projections = train.projections,
    test.projections = test.projections,
    params = params
  )
  rt <- rtMod$new(
    mod.name = mod.name,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    xnames = xnames,
    mod = mod,
    type = type,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    varimp = coef(mod)[, 1],
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
} # rtemis::s_SDA
