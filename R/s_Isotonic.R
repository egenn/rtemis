# s_Isotonic.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Classification and Regression Trees \[C, R, S\]
#'
#' Train an isotonic regression model for regression
#'
#'
#' @inheritParams s_CART
#' @param ... Not used
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation [calibrate_cv] which uses this function
#' @family Supervised Learning
#' @export

s_Isotonic <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  binclasspos = NULL,
  verbose = TRUE,
  question = NULL,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  if (is.null(binclasspos)) binclasspos <- rtenv$binclasspos

  # Intro ----
  if (missing(x)) {
    print(args(s_Isotonic))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "Isotonic"

  # Arguments ----
  check_supervised_inputs(x, y)
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test, verbose = verbose)
  x <- dt$x
  # isoreg: x must be a single vector
  if (NCOL(x) > 1) {
    print(args(s_Isotonic))
    stop("x must be a single vector")
  }
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  .class <- dt$type == "Classification"

  # Model ----
  if (verbose) msg2("Training Isotonic regression...", newline.pre = TRUE)
  if (.class) {
    # Positive class => 1, negative class => 0
    yf <- y
    y <- if (rtenv$binclasspos == 1) {
      2 - as.numeric(y)
    } else {
      as.numeric(y) - 1
    }
  }
  ir <- isoreg(cbind(x, y))
  mod <- as.stepfun(ir)

  # Fitted ----
  fitted <- mod(x[[1]])
  if (.class) {
    fitted.prob <- fitted
    fitted <- prob2categorical(
      fitted.prob,
      levels(yf),
      binclasspos = rtenv$binclasspos
    )
  } else {
    fitted.prob <- NULL
  }

  error.train <- mod_error(if (.class) yf else y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- predicted.prob <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- mod(x.test[[1]])
    if (.class) {
      predicted.prob <- predicted
      predicted <- prob2categorical(
        predicted.prob,
        levels(yf),
        binclasspos = rtenv$binclasspos
      )
    } else {
      predicted.prob <- NULL
    }
    error.test <- mod_error(
      if (.class) y.test else y.test,
      predicted,
      predicted.prob
    )
    if (verbose) errorSummary(error.test, mod.name)
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = dt$type,
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
    rt = rt,
    print.plot = FALSE,
    plot.fitted = FALSE,
    plot.predicted = FALSE,
    y.test = y.test,
    mod.name = mod.name,
    outdir = outdir,
    save.mod = save.mod,
    verbose = verbose
  )

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_Isotonic


#' @rdname s_Isotonic
#' @export
#'
#' @param object Object of class `rtMod` that has been trained with [s_Isotonic]
#' @param newdata Data frame of new data to predict
#' @param ... Not used
#'
#' @return Predicted values
predict.Isotonic <- function(object, newdata, ...) {
  object$mod(newdata)
}
