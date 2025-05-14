# meta_mod.R
# ::rtemis::
# 2016-9 E.D. Gennatas rtemis.org

#' Meta Models for Regression (Model Stacking)
#'
#' Train a meta model from the output of base learners trained using different learners (algorithms)
#'
#' This is included mainly for educational purposes.
#' \itemize{
#'   \item Train a set of base learners on resamples of the training set x
#'   \item Train a meta learner to map bases' validation set predictions to outcomes
#'   \item Train base learners on full training set x
#'   \item Use the meta learner to predict test set outcome y.test from testing set (x.test)
#' }
#'
#' @param x Numeric vector or matrix of features, i.e. independent variables
#' @param y Numeric vector of outcome, i.e. dependent variable
#' @param x.test (Optional) Numeric vector or matrix of validation set features
#'   must have set of columns as `x`
#' @param y.test (Optional) Numeric vector of validation set outcomes
#' @param x.name Character: Name for predictor set. (What kind of data is it?)
#' @param y.name Character: Name for outcome
#' @param base.mods Character vector: Two or more base learners. Options: [select_learn]
#' @param base.params List of length equal to N of `base.mods`. Each element should be a list of arguments to pass
#'   to the corresponding base mod
#' @param meta.mod String. Meta learner. Options: [select_learn]
#' @param resampler String. Resampling method to use. Options: "bootstrap", "kfold", "strat.boot", "strat.sub"
#' @param se.lty How to plot standard errors. If a number, it corresponds to par("lty") line types and is
#'   plotted with lines(). If "solid", a transparent polygon is plotted using polygon()
#'
#' @author E.D. Gennatas
#' @export

meta_mod <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  base.mods = c("mars", "ranger"),
  base.params = vector("list", length(base.mods)),
  base.resample.params = setup.resample(
    resampler = "kfold",
    n.resamples = 4
  ),
  meta.mod = "gam",
  # meta.input = c("retrain", "bag"),
  meta.params = list(),
  x.name = NULL,
  y.name = NULL,
  save.base.res = TRUE,
  save.base.full = FALSE,
  col = NULL,
  se.lty = 3,
  print.base.plot = FALSE,
  print.plot = TRUE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose.base.res.mods = FALSE,
  verbose.base.mods = FALSE,
  verbose = TRUE,
  trace = 0,
  base.n.cores = 1,
  n.cores = rtCores,
  save.mod = FALSE,
  outdir = NULL,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(meta_mod))
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
  base.mod.names <- toupper(base.mods)
  meta.mod.name <- toupper(meta.mod)
  mod.name <- paste0("META.", paste(base.mod.names, collapse = "."))

  # Dependencies ----
  dependency_check("plyr")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  names(base.params) <- base.mod.names
  # meta.input <- match.arg(meta.input)
  meta.input <- "retrain"

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test, verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Resamples ----
  res.part <- resample(
    y,
    rtset = base.resample.params,
    verbosity = as.integer(verbose)
  )

  # {Grid} Function ----
  waffle1 <- function(
    index,
    grid,
    x.int,
    y.int,
    res.part,
    base.params,
    verbose,
    ...
  ) {
    mod.name <- grid$mod[index]
    res.id <- grid$resample.id[index]
    x.int.train <- x.int[res.part[[res.id]], ]
    x.int.test <- x.int[-res.part[[res.id]], ]
    y.int.train <- y.int[res.part[[res.id]]]
    y.int.test <- y.int[-res.part[[res.id]]]
    params <- base.params[[mod.name]]
    args <- list(
      x = x.int.train,
      y = y.int.train,
      x.test = x.int.test,
      y.test = y.int.test,
      x.name = "X",
      y.name = "y",
      print.plot = FALSE,
      verbose = verbose
    )
    args <- c(args, params)
    base.mod1 <- do.call(select_learn(mod.name), args = args)
    base.mod1
  }

  # Base res mods ----
  # For each resample, for each base
  grid <- expand.grid(mod = base.mod.names, resample.id = seq(res.part))
  nbases <- length(base.mod.names)
  if (verbose) {
    cat(
      "\n  I will train ",
      nbases,
      " base learners: ",
      hilite(paste(base.mod.names, collapse = ", ")),
      sep = ""
    )
  }
  if (verbose) {
    cat(
      "  using ",
      base.resample.params$n.resamples,
      " internal resamples (",
      base.resample.params$resampler,
      "),",
      sep = ""
    )
  }
  if (verbose) cat("  and build a", hilite(toupper(meta.mod)), "meta model")
  if (verbose) {
    cat(
      "  Training ",
      nbases,
      " base learners",
      " on ",
      length(res.part),
      " training set resamples (",
      nrow(grid),
      " models total)...\n",
      sep = ""
    )
  }
  if (verbose) {
    cat("\n")
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  base.res <- pbapply::pblapply(
    seq_len(NROW(grid)),
    waffle1,
    grid = grid,
    x.int = x,
    y.int = y,
    res.part = res.part,
    base.params = base.params,
    verbose = verbose.base.res.mods,
    cl = base.n.cores
  )
  names(base.res) <- paste0(grid$mod, "_", grid$resample.id)

  # Collect all grid lines' y.test and predicted
  base.res.y.test <- unlist(lapply(
    seq(res.part),
    function(res) c(y[-res.part[[res]]])
  ))
  base.res.predicted <- as.data.frame(plyr::llply(
    seq(base.mod.names),
    function(mod) {
      cbind(plyr::ldply(seq(res.part), function(res) {
        grid.index <- seq(mod, nrow(grid), nbases)[res]
        if (trace > 0)
          cat(
            "mod.name is",
            base.mod.names[mod],
            "and grid.index is ",
            grid.index,
            "\n"
          )
        cbind(base.res[[grid.index]]$predicted)
      }))
    }
  ))
  colnames(base.res.predicted) <- base.mod.names

  # Base res Performance  ----
  # Get error accross resamples
  base.res.error <- lapply(
    seq(base.mod.names),
    function(mod) {
      mod_error(
        base.res.y.test,
        base.res.predicted[, mod]
      )
    }
  )
  names(base.res.error) <- base.mod.names

  # Meta Learner ----
  if (verbose) msg2("Training", toupper(meta.mod), "meta learner...")
  meta.mod <- do.call(
    select_learn(meta.mod.name),
    c(
      list(
        x = base.res.predicted,
        y = base.res.y.test,
        print.plot = FALSE
      ),
      meta.params
    )
  )

  # Full Training Base mods ----
  if (meta.input == "bag") {
    if (verbose) msg2("Bagging base resamples to get final base outputs...")
    base.mods.fitted <- sapply()
  } else {
    if (verbose)
      msg2("Training", nbases, "base learners on full training set...")
    base.mods <- pbapply::pblapply(
      seq(base.mod.names),
      function(mod) {
        do.call(
          select_learn(base.mod.names[mod]),
          list(
            x = x,
            y = y,
            x.test = x.test,
            y.test = y.test,
            print.plot = print.base.plot,
            verbose = verbose.base.mods
          )
        )
      },
      cl = base.n.cores
    )
    names(base.mods) <- base.mod.names
  }

  # Fitted ----
  base.mods.fitted <- as.data.frame(sapply(
    base.mods,
    function(mod) c(mod$fitted)
  ))
  base.mods.error.train <- as.data.frame(sapply(
    base.mods,
    function(mod) c(mod$error.train)
  ))
  fitted <- as.numeric(predict(meta.mod, base.mods.fitted))
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  base.mods.error.test <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    base.mods.predicted <-
      as.data.frame(sapply(
        base.mods,
        function(mod) c(mod$predicted)
      ))
    base.mods.error.test <- as.data.frame(sapply(
      base.mods,
      function(mod) c(mod$error.test)
    ))
    if (meta.mod.name == "GBM") {
      meta.predicted <- predict(
        meta.mod$mod,
        newdata = base.mods.predicted,
        n.trees = meta.mod$best.tune$n.trees
      )
    } else {
      meta.predicted <- predict(meta.mod, newdata = base.mods.predicted)
    }

    predicted <- as.numeric(meta.predicted)
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, meta.predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtMeta$new(
    mod.name = mod.name,
    type = type,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    grid = grid,
    base.mod.names = base.mod.names,
    base.res.y.test = base.res.y.test,
    base.res.predicted = base.res.predicted,
    base.resample.params = base.resample.params,
    base.params = base.params,
    base.mods = base.mods,
    base.mods.error.train = base.mods.error.train,
    base.mods.error.test = base.mods.error.test,
    meta.mod.name = meta.mod.name,
    meta.mod = meta.mod,
    meta.params = meta.params,
    fitted = fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
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
} # rtemis::meta_mod
