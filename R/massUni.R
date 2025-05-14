# massUni.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Mass-univariate Analysis
#'
#' Run a mass-univariate analysis: same features (predictors)
#' on multiple outcomes
#'
#' @param x Matrix / data frame of features
#' @param y Matrix / data frame of outcomes
#' @param mod \pkg{rtemis} algorithm to use. Options: run `select_learn()`
#' @param save.mods Logical: If TRUE, save fitted models
#' @param verbose Logical: If TRUE, print messages during run
#' @param n.cores Integer: Number of cores to use
#' @param ... Arguments to be passed to `mod`
#' @author E.D. Gennatas
#' @export

massUni <- function(
  x,
  y,
  mod = "gam",
  save.mods = FALSE,
  verbose = TRUE,
  n.cores = rtCores,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)

  # Arguments ----
  learner <- select_learn(mod)
  args <- list(...)

  # Data ----
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  ynames <- colnames(y)

  # mod1 ----
  mod1 <- function(index, x, y, learner, args) {
    mod.1 <- R.utils::doCall(
      learner,
      x = x,
      y = y[, index],
      print.plot = FALSE,
      args = args
    )
    return(mod.1)
  }

  # Mods ----
  if (verbose) msg2("Training mass-univariate models")
  if (verbose) {
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  mods <- pbapply::pblapply(
    seq_len(NCOL(y)),
    mod1,
    x = x,
    y = y,
    learner = learner,
    args = args,
    cl = n.cores
  )

  # Errors ----
  if (verbose) msg2("Collecting model errors")
  errors <- t(sapply(mods, function(m) as.data.frame(m$error.train)))
  rownames(errors) <- ynames
  # errors <- plyr::ldply(mods, function(m) as.data.frame(m$error.train), .progress = "text")
  # errors <- t(pbapply::pbsapply(mods, function(m) as.data.frame(m$error.train)))

  # Outro ----
  outro(start.time, verbose = verbose)
  if (!save.mods) mods <- NULL
  list(
    mods = mods,
    errors = errors
  )
} # rtemis::massUni
