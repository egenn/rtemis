# s_BART.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Bayesian Additive Regression Trees (C, R)
#'
#' Trains a Bayesian Additive Regression Tree (BART) model using package `bartMachine`
#'
#' Be warned this can take a very long time to train.
#' If you are having trouble with rJava in Rstudio on macOS, see:
#' https://support.rstudio.com/hc/en-us/community/posts/203663956/comments/249073727
#' `bartMachine` does not support case weights
#' @inheritParams s_GLM
#' @param save.mod Logical: if TRUE, sets `bartMachine`'s `serialize` to TRUE and saves model to `outdir`
#' @param ... Additional arguments to be passed to `bartMachine::bartMachine`
#'
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_BART <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  n.trees = c(100, 200),
  k_cvs = c(2, 3),
  nu_q_cvs = list(c(3, 0.9), c(10, 0.75)),
  k_folds = 5,
  n.burnin = 250,
  n.iter = 1000,
  n.cores = rtCores,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  trace = 0,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  java.mem.size = 12,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_BART))
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
  mod.name <- "BART"

  # Dependencies ----
  dependency_check("bartMachine")

  # Arguments ----
  if (missing(x)) {
    print(args(s_BART))
    stop("x is missing")
  }
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_BART))
    stop("y is missing")
  }
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
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  # For multinomial classification, must provide integer
  if (type == "Classification") {
    if (length(levels(y)) > 2) {
      y.train <- as.integer(y)
    } else {
      # bartMachine considers first level negative, second positive
      y.train <- factor(y, levels = rev(levels(y)))
      nu_q_cvs <- NULL
    }
  } else {
    y.train <- y
  }

  # BART ----
  java.mem <- paste0("-Xmx", java.mem.size, "g")
  options(java.parameters = java.mem)
  bartMachine::set_bart_machine_num_cores(n.cores)
  if (verbose) {
    msg2("Training Bayesian Additive Regression Trees...", newline.pre = TRUE)
  }
  mod <- bartMachine::bartMachineCV(
    x,
    y.train,
    num_tree_cvs = n.trees,
    k_cvs = k_cvs,
    nu_q_cvs = nu_q_cvs,
    k_folds = k_folds,
    num_burn_in = n.burnin,
    num_iterations_after_burn_in = n.iter,
    serialize = save.mod,
    verbose = trace > 0,
    ...
  )
  if (trace > 0) summary(mod)

  # Fitted ----
  if (type == "Classification") {
    fitted.prob <- predict(mod, x, type = "prob")
    fitted <- factor(levels(y)[round(fitted.prob) + 1], levels = levels(y))
  } else {
    fitted.prob <- NULL
    fitted <- as.numeric(predict(mod, x))
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  if (!is.null(x.test) && !is.null(y.test)) {
    if (type == "Classification") {
      predicted.prob <- predict(mod, x.test, type = "prob")
      predicted <- factor(
        levels(y)[round(predicted.prob) + 1],
        levels = levels(y)
      )
    } else {
      predicted.prob <- NULL
      predicted <- as.numeric(predict(mod, x.test))
    }
    error.test <- mod_error(y.test, predicted)
    if (verbose) errorSummary(error.test, mod.name)
  } else {
    predicted <- error.test <- NULL
  }

  # Outro ----
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
    fitted.prob = fitted.prob,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = predicted.prob,
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
} # rtemis::s_BART
