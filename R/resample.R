# resample.R
# ::rtemis::
# 2015- EDG rtemis.org

#' Resample data
#'
#' Create resamples of your data, e.g. for model building or validation.
#' "Bootstrap" gives the standard bootstrap, i.e. random sampling with replacement, using
#' [bootstrap], "StratSub" creates stratified subsamples using [StratSub],
#' while "StratBoot" uses [StratBoot] which runs [StratSub] and then
#' randomly duplicates some of the training cases to reach original length of input
#' (default) or length defined by `target_length`.
#'
#' `resample` is used by multiple \pkg{rtemis} learners, `gridSearchLearn`, and
#' [train_cv]. Note that option 'KFold', which uses [kfold] results in resamples
#' of slightly different length for y of small length, so avoid all operations which rely
#' on equal-length vectors. For example, you can't place resamples in a data.frame, but
#' must use a list instead.
#'
#' @param x Vector or data.frame: Usually the outcome; `NROW(y)` defines sample size
#' @param parameters Resampler object created by [setup_Resampler].
#' @param verbosity Logical: If TRUE, print messages to console.
#'
#' @author EDG
#' @export
#' @examples
#' y <- rnorm(200)
#' # 10-fold (stratified)
#' res <- resample(y, 10, "KFold")
#' # 25 stratified subsamples
#' res <- resample(y, 25, "StratSub")
#' # 100 stratified bootstraps
#' res <- resample(y, 100, "StratBoot")
resample <- function(x,
                     parameters = setup_Resampler(),
                     #  index = NULL,
                     #  group = NULL,
                     verbosity = 1L) {
  check_is_S7(parameters, ResamplerParameters)
  # Input ----
  type <- parameters@type
  if (NCOL(x) > 1) {
    if (survival::is.Surv(x)) {
      if (verbosity > 0L) msg2("Survival object will be stratified on time.")
      y <- x[, 1]
    } else {
      if (verbosity > 0L) msg2("Input contains more than one column; will stratify on last.")
      y <- x[[NCOL(x)]]
    }
  }

  # Stratify on case IDs ----
  id_strat <- parameters@id_strat
  if (!is.null(id_strat)) {
    # Only keep unique IDs
    idl <- !duplicated(id_strat)
    x <- x[idl]
  }

  if (type == "StratBoot") {
    target_length <- if (is.null(parameters@target_length)) NROW(y) else parameters@target_length
  }

  # resample ----
  .stratify_var <- if (is.null(parameters@stratify_var)) y else parameters@stratify_var
  # stratify_var is for printing with parameter_summary
  # stratify_var <- if (is.null(parameters@stratify_var)) {
  #   getName(x, "x")
  # } else {
  #   deparse(substitute(stratify_var))
  # }

  n_resamples <- if (type == "LOOCV") length(y) else parameters@n

  # Print parameters ----
  if (verbosity > 1L) {
    print(parameters)
  }

  # Make resamples ----
  if (type == "StratSub") {
    ## StratSub ----
    res.part <- strat.sub(
      x = y,
      n_resamples = n_resamples,
      train_p = parameters@train_p,
      stratify_var = .stratify_var,
      strat_n_bins = parameters@strat_n_bins,
      seed = parameters@seed,
      verbosity = verbosity
    )
  } else if (type == "Bootstrap") {
    ## Bootstrap ----
    res.part <- bootstrap(
      x = y,
      n_resamples = n_resamples,
      seed = parameters@seed
    )
  } else if (type == "KFold") {
    ## KFold ----
    res.part <- kfold(
      x = y,
      k = n_resamples,
      stratify_var = .stratify_var,
      strat_n_bins = parameters@strat_n_bins,
      seed = parameters@seed,
      verbosity = verbosity
    )
  } else if (type == "loocv") {
    ## LOOCV ----
    res.part <- loocv(x = y)
  } else if (type == "StratBoot") {
    ## StratBoot ----
    res.part <- strat.boot(
      x = y,
      n_resamples = n_resamples,
      train_p = parameters@train_p,
      stratify_var = .stratify_var,
      strat_n_bins = parameters@strat_n_bins,
      target_length = target_length,
      seed = parameters@seed,
      verbosity = verbosity
    )
  }

  # Update strat_n_bins ----
  if (type == "StratSub" || type == "StratBoot") {
    actual_n_bins <- attr(res.part, "strat_n_bins")
    if (actual_n_bins != parameters@strat_n_bins) {
      if (verbosity > 0L) {
        msg20(
          "Updated strat_n_bins from ", parameters@strat_n_bins, " to ", actual_n_bins,
          " in ResamplerParameters object."
        )
      }
      parameters@strat_n_bins <- actual_n_bins
    }
  }

  if (!is.null(id_strat)) {
    ### Get ID by resample ----
    id_by_res <- lapply(res.part, \(x) id_strat[idl][x])
    ### Get resamples on original data with replicates ----
    res.part <- lapply(id_by_res, \(x) which(id_strat %in% x))
  }

  # Output ----
  Resampler(type, res.part, parameters)
} # rtemis::resample


#' Bootstrap Resampling
#'
#' @param x Input vector
#' @param n_resamples Integer: Number of resamples to make. Default = 10
#' @param seed Integer: If provided, set seed for reproducibility. Default = NULL
#' @author EDG
#' @export

bootstrap <- function(x, n_resamples = 10,
                      seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  ids <- seq_along(x)
  .length <- length(x)
  if (!is.null(seed)) set.seed(seed)

  res <- lapply(seq(n_resamples), function(i) sort(sample(ids, .length, replace = TRUE)))
  names(res) <- paste0("Bootsrap_", seq(n_resamples))
  res
} # rtemis::bootstrap


#' K-fold Resampling
#'
#' @inheritParams resample
#' @param x Input Vector
#' @param k Integer: Number of folds. Default = 10
#' @author EDG
#' @export

kfold <- function(x, k = 10,
                  stratify_var = NULL,
                  strat_n_bins = 4,
                  seed = NULL,
                  verbosity = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(stratify_var)) stratify_var <- x
  stratify_var <- as.numeric(stratify_var)
  # ->> update
  max.bins <- length(unique(stratify_var))
  if (max.bins < strat_n_bins) {
    if (max.bins == 1) stop("Only one unique value present in stratify_var.")
    if (verbosity > 0L) msg20("Using max n bins possible = ", max.bins, ".")
    strat_n_bins <- max.bins
  }

  ids <- seq_along(x)
  # cuts
  cuts <- cut(stratify_var, breaks = strat_n_bins, labels = FALSE)
  cut.bins <- sort(unique(cuts))

  # ids by cut
  idl <- lapply(seq_along(cut.bins), function(i) ids[cuts == cut.bins[i]])
  # length of each cut
  # idl.length <- sapply(idl, length)
  idl.length <- as.numeric(table(cuts))

  # split each idl into k folds after randomizing them
  idl.k <- vector("list", length(cut.bins))
  for (i in seq_along(cut.bins)) {
    cut1 <- cut(sample(idl.length[i]), breaks = k, labels = FALSE)
    idl.k[[i]] <- lapply(seq(k), function(j) idl[[i]][cut1 == j])
  }

  res <- lapply(seq(k), \(i) seq(ids)[-sort(unlist(lapply(seq_along(cut.bins), \(j) idl.k[[j]][[i]])))])

  names(res) <- paste0("Fold_", seq(k))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # rtemis::kfold


#' Resample using Stratified Subsamples
#'
#' @inheritParams resample
#' @param x Input vector
#' @author EDG
#' @export

strat.sub <- function(x,
                      n_resamples = 10,
                      train_p = .75,
                      stratify_var = NULL,
                      strat_n_bins = 4,
                      seed = NULL,
                      verbosity = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(stratify_var)) stratify_var <- x
  stratify_var <- as.numeric(stratify_var)
  max.bins <- length(unique(stratify_var))
  if (max.bins < strat_n_bins) {
    if (verbosity > 0L) msg2("Using max n bins possible =", max.bins)
    strat_n_bins <- max.bins
  }
  ids <- seq_along(x)
  cuts <- cut(stratify_var, breaks = strat_n_bins, labels = FALSE)
  cut.bins <- sort(unique(cuts))
  idl <- lapply(seq_along(cut.bins), function(i) ids[cuts == cut.bins[i]])
  idl.length <- as.numeric(table(cuts))
  res <- lapply(seq(n_resamples), function(i) {
    sort(unlist(sapply(seq_along(cut.bins), function(j) {
      sample(idl[[j]], train_p * idl.length[j])
    })))
  })
  names(res) <- paste0("Subsample_", seq(n_resamples))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # rtemis::strat.sub


#' Stratified Bootstrap Resampling
#'
#' @inheritParams resample
#' @param x Input vector
#' @author EDG
#' @export

strat.boot <- function(x, n_resamples = 10,
                       train_p = .75,
                       stratify_var = NULL,
                       strat_n_bins = 4,
                       target_length = NULL,
                       seed = NULL,
                       verbosity = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  res.part1 <- strat.sub(
    x = x, n_resamples = n_resamples,
    train_p = train_p,
    stratify_var = stratify_var,
    strat_n_bins = strat_n_bins,
    verbosity = verbosity
  )

  # Make sure target_length was not too short by accident
  res.length <- length(res.part1[[1]])
  if (is.null(target_length)) target_length <- length(x)
  if (target_length < res.length) target_length <- length(x)

  # Add back this many cases
  add.length <- target_length - res.length
  doreplace <- ifelse(add.length > res.length, 1, 0)
  res.part2 <- lapply(res.part1, function(i) sample(i, add.length, replace = doreplace))
  res <- mapply(c, res.part1, res.part2, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  names(res) <- paste0("StratBoot_", seq(n_resamples))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # rtemis::strat.boot


#' Leave-one-out Resampling
#'
#' @param x Input vector
#' @author EDG
#' @export

loocv <- function(x) {
  res <- lapply(seq(x), function(i) (seq(x))[-i])
  names(res) <- paste0("Fold_", seq(res))
  res
} # rtemis::loocv
