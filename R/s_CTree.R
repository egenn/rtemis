# s_CTree.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Conditional Inference Trees \[C, R, S\]
#'
#' Train a conditional inference tree using {partykit::ctree}
#'
#' @inheritParams s_GLM
#' @param control List of parameters for the CTree algorithms. Set using
#' `partykit::ctree_control`
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @seealso [train_cv]
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_CTree <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  weights = NULL,
  control = partykit::ctree_control(),
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  x.name = NULL,
  y.name = NULL,
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
    print(args(s_CTree))
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
  mod.name <- "CTree"

  # Dependencies ----
  dependency_check("partykit")

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

  # Formula ----
  df.train <- data.frame(y = y, x)
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste(y.name, "~", features))

  # CTree ----
  if (verbose)
    msg2("Training Conditional Inference Tree...", newline.pre = TRUE)
  # Instead of loading the whole package
  # because partykit::ctree does this:
  # mf[[1L]] <- quote(extree_data)
  # d <- eval(mf, parent.frame())
  extree_data <- partykit::extree_data
  mod <- partykit::ctree(
    formula = .formula,
    data = df.train,
    weights = weights,
    control = control,
    ...
  )

  # Fitted ----
  if (type == "Classification") {
    fitted.prob <- predict(mod, x, type = "prob")
  }
  fitted <- predict(mod, x, type = "response")
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted.prob <- predict(mod, x.test, type = "prob")
    predicted <- predict(mod, x.test, type = "response")
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(formula = .formula, weights = weights)
  if (type == "Classification") {
    extra$fitted.prob <- fitted.prob
    extra$predicted.prob <- predicted.prob
  }
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
} # rtemis::s_CTree
