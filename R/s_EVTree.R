# s_EVTree.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Evolutionary Learning of Globally Optimal Trees (C, R)
#'
#' Train a EVTree for regression or classification using `evtree`
#'
#' @inheritParams s_GLM
#' @param control Passed to `evtree::evtree`
#' @param ... Additional arguments to be passed to `evtree::evtree`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_EVTree <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  control = evtree::evtree.control(),
  na.action = na.exclude,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_EVTree))
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
  mod.name <- "EVTree"

  # Dependencies ----
  dependency_check("evtree")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_EVTree))
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
  if (is.null(weights) && ifw) weights <- dt$weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  df.train <- data.frame(y = y, x)

  # Formula ----
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste0(y.name, " ~ ", features))

  # evtree::evtree ----
  if (verbose) msg2("Training EVTree...", newline.pre = TRUE)
  mod <- evtree::evtree(
    formula = .formula,
    data = df.train,
    weights = weights,
    control = control,
    na.action = na.action,
    ...
  )

  # Fitted ----
  if (type == "Regression" || type == "Survival") {
    fitted <- predict(mod, x, type = "response")
    fitted.prob <- NULL
  } else if (type == "Classification") {
    fitted.prob <- predict(mod, x, type = "prob")
    fitted <- predict(mod, x, type = "response")
  }

  attr(fitted, "names") <- NULL
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  if (!is.null(x.test)) {
    if (type == "Regression" || type == "Survival") {
      predicted <- predict(mod, x.test, type = "response")
      predicted.prob <- NULL
    } else if (type == "Classification") {
      predicted.prob <- predict(mod, x.test, type = "prob")
      predicted <- predict(mod, x.test, type = "response")
    }
    attr(predicted, "names") <- NULL
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- predicted.prob <- error.test <- NULL
  }

  # Outro ----
  extra <- list(fitted.prob = fitted.prob, prdicted.prob = predicted.prob)
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
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
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
} # rtemis::s_EVTree
