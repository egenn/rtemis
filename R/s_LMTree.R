# s_LMTree.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Linear Model Tree \[R\]
#'
#' Train a LMTree for regression or classification using `partykit::lmtree`
#'
#' @inheritParams s_CART
#' @param offset Numeric vector of a priori known offsets
#' @param na.action Character: How to handle missing values. See `?model.frame`
#' @param ... Additional arguments passed to `partykit::mob_control`
#'
#' @return Object of class `rtMod`
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s_LMTree <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.name = NULL,
  y.name = NULL,
  weights = NULL,
  offset = NULL,
  #  alpha = 0.05,
  #  bonferroni = TRUE,
  #  minsize = NULL,
  #  maxdepth = Inf,
  #  mtry = Inf,
  #  trim = 0.1,
  #  breakties = FALSE,
  #  parm = NULL,
  #  dfsplit = TRUE,
  #  prune = NULL,
  #  restart = TRUE,
  #  verbose = FALSE,
  #  caseweights = TRUE,
  #  ytype = "vector",
  #  xtype = "matrix",
  #  terminal = "object",
  #  inner = terminal,
  #  model = TRUE,
  #  numsplit = "left",
  #  catsplit = "binary",
  #  vcov = "opg",
  #  ordinal = "chisq",
  #  nrep = 10000,
  #  minsplit = minsize,
  #  minbucket = minsize,
  #  applyfun = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
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
    print(args(s_LMTree))
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
  mod.name <- "LMTree"

  # Dependencies ----
  dependency_check("partykit")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_LMTree))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
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
  .weights <- if (is.null(weights) && ifw) dt$weights else weights
  # x0 <- if (upsample | downsample) dt$x0 else x # x0, y0 are passed to gridSearchLearn
  # y0 <- if (upsample | downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (type != "Survival") df.train <- data.frame(y = y, x)

  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Formula ----
  features <- paste(xnames, collapse = " + ")
  .formula <- as.formula(paste0(y.name, " ~ ", features))

  # lmtree ----
  if (verbose) msg2("Training LMTree...", newline.pre = TRUE)
  mod <- partykit::lmtree(
    formula = .formula,
    data = df.train,
    na.action = na.action,
    weights = .weights,
    offset = offset,
    ...
  )

  # Fitted ----
  fitted.prob <- NULL
  fitted <- predict(mod, x, type = "response")

  # attr(fitted, "names") <- NULL
  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test, type = "response")
    predicted.prob <- NULL
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = mod,
    mod.name = mod.name,
    type = type,
    # call = .call,
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
    varimp = NULL,
    question = question,
    extra = NULL
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
} # rtemis::s_LMTree
