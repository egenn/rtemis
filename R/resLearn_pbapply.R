# resLearn_pbapply.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' \pkg{rtemis} internal: Resample Learn
#'
#' Train an \pkg{rtemis} learner on a set of resamples
#'
#' Input: features (x) and outcome (y)
#' Procedure: \link{resample}, train learners
#' Output: trained learners
#' This is used internally by \link{elevate} and for bagging, when the \code{bag.resampler}
#' argument is set in a learner.
#' @param x features - training set
#' @param y outcome - training set
#' @param mod Character: \pkg{rtemis} model. See \code{modSelect} gives available models
#' @param resample.rtset List: output of \link{rtset} (or a list of same structure)
#' @param params List of named elements, each is a single value
#' @param verbose Logical: If TRUE, print messages to screen
#' @param res.verbose Logical: Will be passed to each \code{mod}'s \code{verbose} argument
#' @param save.mods Logical: If TRUE, save all models, otherwise discard after training.
#' Use with \link{elevate} when training a large number of resamples. Default = TRUE
#' @param outdir Character: Path to save output. Default = NULL
#' @author E.D. Gennatas
#' @export

resLearn_pbapply <- function(x, y, mod,
                     resample.rtset = rtset.cv.resample(),
                     weights = NULL,
                     params = list(),
                     mtry = NULL,
                     .preprocess = NULL,
                     verbose = TRUE,
                     res.verbose = FALSE,
                     trace = 0,
                     save.mods = TRUE,
                     outdir = NULL,
                     n.workers = rtCores,
                     parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock")) {

  # Intro ----
  start.time <- intro(verbose = trace > 0,
                      message = "Starting resLearn...",
                      newline.pre = TRUE)

  # Arguments ----
  n.workers <- as.numeric(n.workers)[1]
  if (missing(x) | missing(y)) {
    print(args(resLearn))
    stop("Input missing")
  }
  mod.name <- toupper(mod)
  if (!is.null(outdir)) {
    outdir <- normalizePath(outdir, mustWork = FALSE)
    if (!dir.exists(outdir)) dir.create(outdir, showWarnings = TRUE,
                                        recursive = TRUE, mode = "0777")
  }

  # Resamples ----
  learner <- modSelect(mod)
  res <- resample(y, rtset = resample.rtset, verbose = trace > 0)
  resampler <- attr(res, "type") # for res.group and res.index

  if (n.workers > resample.rtset$n.resamples) n.workers <- resample.rtset$n.resamples

  # {Grid} function ----
  learner1 <- function(index, learner,
                       x, y,
                       weights = NULL,
                       mtry,
                       res,
                       params,
                       .preprocess,
                       verbose,
                       outdir,
                       save.mods) {
    if (verbose) msg("Running resample #", index, sep = "")
    res1 <- res[[index]]
    if (is.null(mtry)) {
      feat.index <- seq(NCOL(x))
    } else {
      feat.index <- sample(seq(NCOL), mtry, replace = FALSE)
    }
    x.train1 <- x[res1, feat.index, drop = FALSE]
    y.train1 <- y[res1]
    x.test1 <- x[-res1, feat.index, drop = FALSE]
    y.test1 <- y[-res1]
    if (!is.null(.preprocess)) {
      # This allows imputing training and testing sets separately
      preproc.params <- c(list(x = x.train1), .preprocess, verbose = verbose)
      x.train1 <- do.call(preprocess, preproc.params)
      preproc.params <- c(list(x = x.test1), .preprocess, verbose = verbose)
      x.test1 <- do.call(preprocess, preproc.params)
    }
    if (!is.null(weights)) weights <- weights[res1]
    if (!is.null(outdir) & parallel.type != "psock") {
      outdir1 <- paste0(outdir, "/", mod.name, ".resLearn.", index)
    } else {
      outdir1 <- NULL
    }

    if (!is.null(weights)) params$weights <- weights
    args <- list(x = x.train1, y = y.train1,
                 x.test = x.test1, y.test = y.test1,
                 print.plot = FALSE,
                 verbose = res.verbose,
                 outdir = outdir1)
    args <- c(args, params)
    mod1 <- do.call(learner, args = args)
    if (!save.mods) mod1$mod <- NA
    out1 <- list(mod1 = mod1,
                 res.id = index,
                 params = params)
  }

  # Res run ----
  desc <- switch(resampler,
                 kfold = "independent folds",
                 strat.sub = "stratified subsamples",
                 strat.boot = "stratified bootstraps",
                 bootstrap = "bootstrap resamples",
                 loocv = "independent folds (LOOCV)",
                 "custom resamples")

  if (verbose) msg0("Training ", modSelect(mod, desc = TRUE), " on ",
                    length(res), " ", desc, "...", newline.pre = TRUE)
  pbapply.type <- if (verbose) "timer" else "none"
  pbapply::pboptions(type = pbapply.type)
  if (n.workers > 1) {
    if (parallel.type == "psock") {
      if (verbose) msg("Starting PSOCK cluster on", n.workers, "cores...")
      cl <- makePSOCKcluster(n.workers)
      on.exit(stopCluster(cl))
      clusterEvalQ(cl, library("rtemis"))
    } else {
      if (verbose) msg("Parallelizing by forking on", n.workers, "cores...")
      cl <- n.workers
    }
  } else {
    cl <- 1
  }

  res.run <- pbapply::pblapply(seq_along(res), learner1,
                               learner,
                               x, y,
                               weights,
                               mtry,
                               res,
                               params,
                               .preprocess,
                               verbose = res.verbose,
                               outdir = outdir,
                               save.mods = save.mods,
                               cl = cl)
  names(res.run) <- paste0(toupper(mod), seq(res))
  if (res.verbose) cat("\n")

  # Outro ----
  outro(start.time, verbose = trace > 0)
  list(res = res, mods = res.run)

} # rtemis::resLearn_pbapply
