# bagLearn.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' \pkg{rtemis} internal: Bootstrap Aggregating
#'
#' Train an \pkg{rtemis} model on a set of resamples.
#'
#' This is meant to be called using \link{bag} instead of directly
#'
#' @inheritParams s.GLM
#' @param mod String: \pkg{rtemis} model. See \link{modSelect} gives available models
#' @param resample.rtset List: output of \link{rtset} (or a list of same structure)
#' @param params List of named elements, each is a single value
#' @param verbose Logical: If TRUE, print messages to screen
#' @param res.verbose Logical: Will be passed to each \code{mod}'s \code{verbose} argument
#' @param n.cores Integer: Number of cores to use
#' @author Efstathios D. Gennatas
#' @export

bagLearn <- function(x, y, mod,
                     resample.rtset = rtset.bag.resample(),
                     weights = NULL,
                     params = list(),
                     verbose = TRUE,
                     res.verbose = FALSE,
                     n.cores = NULL) {

  # [ INTRO ] ====
  start.time <- intro(verbose = verbose)

  # [ ARGUMENTS ] ====
  if (missing(x) | missing(y)) {
    print(args(bagLearn))
    stop("Input missing")
  }

  # [ RESAMPLES ] ====
  learner <- modSelect(mod)
  res <- resample(y, rtset = resample.rtset, verbose = verbose)

  # [ {GRID} FUNCTION ] ====
  learner1 <- function(index, learner, x, y, res, params, weights, verbose) {
    call <- match.call()
    res1 <- res[[index]]
    x.train1 <- x[res1, ]
    y.train1 <- y[res1]
    weights.train1 <- weights[res1]
    x.test1 <- x[-res1, ]
    y.test1 <- y[-res1]
    args <- list(x = x.train1, y = y.train1,
                 x.test = x.test1, y.test = y.test1,
                 weights = weights.train1,
                 print.plot = FALSE,
                 verbose = res.verbose,
                 bag.resample.rtset = NULL)
    args <- c(args, params)
    mod1 <- do.call(learner, args)
    out1 <- list(mod1 = mod1,
                 res.id = index,
                 error.train = mod1$error.train,
                 error.test = mod1$error.test,
                 type = mod1$type,
                 params = params,
                 call = call)
  }

  # [ RES RUN ] ====
  if (verbose) msg("Bagging ", modSelect(mod, desc = TRUE), "...", sep = "")
  if (verbose) msg(resample.rtset$n.resamples, " resamples; running on ",
                   n.cores, " cores", " (", Sys.getenv("R_PLATFORM"), ")\n", sep = "")
  if (verbose) {
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  res.run <- pbapply::pblapply(1:resample.rtset$n.resamples, learner1,
                               learner = learner,
                               x = x,
                               y = y,
                               res = res,
                               params = params,
                               weights = weights,
                               verbose = res.verbose,
                               cl = n.cores)
  if (length(res.run) != resample.rtset$n.resamples) {
    warning("Error: Check output. If you were using multiple cores, try running with n.cores = 1")
  }
  names(res.run) <- paste0(toupper(mod), 1:length(res.run))
  if (verbose) cat("\n")

  # [ OUTRO ] ====
  outro(start.time, verbose = verbose)
  class(res.run) <- c("bagLearn", "list")
  res.run

} # rtemis::bagLearn
