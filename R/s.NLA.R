# s.NLA.R
# ::rtemis::
# 2018 E.D. Gennatas lambdamd.org

#' NonLinear Activation unit Regression (NLA) [R]
#'
#' Train an equivalent of a 1 hidden unit neural network with a defined nonlinear activation function
#' using \code{optim}
#'
#' Since we are using \code{optim}, results will be sensitive to the combination of
#' optimizer method (See \code{optim::method} for details),
#' initialization values, and activation function.
#' @inheritParams s.GLM
#' @param activation Function: Activation function to use. Default = \link{softplus}
#' @param b_o Float, vector (length y): Output bias. Defaults to \code{mean(y)}
#' @param W_o Float: Output weight. Defaults to 1
#' @param b_h Float: Hidden layer bias. Defaults to 0
#' @param W_h Float, vector (length \code{NCOL(x)}): Hidden layer weights. Defaults to 0
#' @param optim.method Character: Optimization method to use: "Nelder-Mead", "BFGS", "CG", "L-BFGS-B",
#' "SANN", "Brent". See \code{stats::optim} for more details. Default = \code{"BFGS"}
#' @param ... Additional arguments to be passed to \code{sigreg}
#' @return Object of class \pkg{rtemis}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.NLA <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  activation = softplus,
                  b_o = mean(y),
                  W_o = 1,
                  b_h = 0,
                  W_h = .01,
                  optim.method = "BFGS",
                  control = list(),
                  lower = -Inf,
                  upper = Inf,
                  x.name = NULL, y.name = NULL,
                  save.func = TRUE,
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.theme"),
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  trace = 0,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # Intro ] ====
  if (missing(x)) {
    print(args(s.NLA))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "NLA"

  # Arguments ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  if (is.character(activation)) {
    .activation <- activation
  } else {
    .activation <- deparse(substitute(activation))
  }

  # Data ====
  dt <- dataPrepare(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # NLA ====
  if (verbose) msg("Training NLA model with", .activation, "activation function using",
                   optim.method, "optimization...", newline.pre = TRUE)
  mod <- nlareg(x, y,
                activation = .activation,
                b_o = b_o,
                W_o = W_o,
                b_h = b_h,
                W_h = W_h,
                optim.method = optim.method,
                control = control,
                lower = lower,
                upper = upper, ...)
  if (trace > 0) print(summary(mod))

  # Fitted ====
  fitted <- predict(mod, x)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ] ====
  rt <- rtModSet(rtclass = "rtMod",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 varimp = coef(mod),
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 question = question,
                 extra = NULL)

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

} # rtemis::s.NLA
