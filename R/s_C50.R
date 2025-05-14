# s_C50.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' C5.0 Decision Trees and Rule-Based Models [C]
#'
#' Train a C5.0 decision tree using `C50::C5.0`
#'
#' @inheritParams s_GLM
#' @param trials Integer \[1, 100\]: Number of boosting iterations
#' @param rules Logical: If `TRUE`, decompose the tree to a rule-based model
#' @param control List: output of `C50::C5.0Control()`
#' @param costs Matrix: Cost matrix. See `C50::C5.0`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#'
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s_C50 <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  trials = 10,
  rules = FALSE,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  control = C50::C5.0Control(),
  costs = NULL,
  x.name = NULL,
  y.name = NULL,
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
    print(args(s_C50))
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
  mod.name <- "C50"

  # Dependencies ----
  dependency_check("C50")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
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
  checkType(type, "Classification", mod.name)
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  if (type != "Classification") {
    stop("C5.0 is for classification; please provide factor outcome")
  }
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  parameters <- list(control = control, costs = costs, weights = .weights)

  # C5.0 ----
  if (verbose) msg2("Training C5.0 decision tree...", newline.pre = TRUE)
  mod <- C50::C5.0(
    x,
    y,
    trials = trials,
    rules = rules,
    weights = .weights,
    control = control,
    costs = costs,
    ...
  )
  if (trace > 0) print(summary(mod))

  # Fitted ----
  fitted <- predict(mod, x)
  error.train <- mod_error(y, fitted, type = "Classification")
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtMod$new(
    mod.name = mod.name,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    xnames = xnames,
    mod = mod,
    type = "Classification",
    parameters = parameters,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
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
} # rtemis::s_C50
