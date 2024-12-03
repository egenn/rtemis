# s_Isotonic.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Classification and Regression Trees \[C, R, S\]
#'
#' Train an isotonic regression model for regression
#'
#'
#' @inheritParams s_CART
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation [calibrate_cv] which uses this function
#' @family Supervised Learning
#' @export

s_Isotonic <- function(x, y = NULL,
                       x.test = NULL, y.test = NULL,
                       x.name = NULL, y.name = NULL,
                       weights = NULL,
                       verbose = TRUE,
                       question = NULL,
                       outdir = NULL,
                       save.mod = ifelse(!is.null(outdir), TRUE, FALSE)) {
  # .call <- match.call()
  tree.depth <- getFromNamespace("tree.depth", "rpart")

  # Intro ----
  if (missing(x)) {
    print(args(s_Isotonic))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "Isotonic"

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_Isotonic))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test,
    verbose = verbose
  )
  x <- dt$x
  # x must be a single vector
  if (NCOL(x) > 1) {
    print(args(s_Isotonic))
    stop("x must be a single vector")
  }
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  if (type != "Regression") stop("Isotonic regression only supports continuous outcomes")

  # Model ----
  if (verbose) msg2("Training Isotonic regression...", newline.pre = TRUE)
  ir <- isoreg(cbind(x, y))
  mod <- as.stepfun(ir)


  # Fitted ----
  fitted <- mod(x[[1]])
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- mod(x.test[[1]])
    error.test <- mod_error(y.test, predicted)
    if (verbose) errorSummary(error.test, mod.name)
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
    fitted.prob = NULL,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = NULL,
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

  outro(start.time,
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
