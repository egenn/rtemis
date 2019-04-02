# s.PPTREE.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Projection Pursuit Tree Classification [C]
#'
#' Train a PPTREE Classifier using \code{PPtree::PP.Tree}
#'
#' Note: \code{PP.Tree} does not support case weights
#' @inheritParams s.GLM
#' @param PPmethod String: "LDA": LDA index, "Lp": Lp index, "PDA": PDA index. Default = "LDA"
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s.PPTREE <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     PPmethod = "LDA",
                     weight = TRUE,
                     r = NULL,
                     lambda = NULL,
                     cooling = .999,
                     temp = 1,
                     energy = .01,
                     upsample = FALSE,
                     upsample.seed = NULL,
                     x.name = NULL,
                     y.name = NULL,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = getOption("rt.fit.theme", "lightgrid"),
                     question = NULL,
                     rtclass = NULL,
                     verbose = TRUE,
                     trace = 0,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.PPTREE))
    invisible(9)
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "PPTREE"

  # [ DEPENDENCIES ] ====
  if (!depCheck("PPtree", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = F), "/")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    upsample = upsample, upsample.seed = upsample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Classification", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ PPTREE ] ====
  if (verbose) msg("Training Projection Pursuit Tree...", newline = TRUE)
  mod <- PPtree::PP.Tree(PPmethod = PPmethod,
                         i.class = y,
                         i.data = x,
                         weight = weight,
                         r = r,
                         lambda = lambda,
                         cooling = cooling,
                         temp = temp,
                         energy = energy, ...)
  if (trace > 0) summary(mod)

  # [ FITTED ] ====
  fitted <- PPtree::PP.classify(test.data = x, true.class = y, Tree.result = mod)$predict.class
  fitted <- factor(fitted)
  levels(fitted) <- levels(y)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test) & !is.null(y.test)) {
    predicted <- PPtree::PP.classify(test.data = x.test, true.class = y.test, Tree.result = mod)$predict.class
    predicted <- factor(predicted)
    levels(predicted) <- levels(y)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, type = "Classification")
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list()
  rt <- rtMod$new(mod.name = mod.name,
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
                  extra = extra)

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

} # rtemis::s.PPTREE
