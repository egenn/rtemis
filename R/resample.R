# resample.R
# ::rtemis::
# 2015-9 E.D. Gennatas lambdamd.org

#' Resampling methods
#'
#' Create resamples of your data, e.g. for model building or validation.
#' "bootstrap" gives the standard bootstrap, i.e. random sampling with replacement, using \link{bootstrap},
#' "strat.sub" creates stratified subsamples using \link{strat.sub}, while "strat.boot"
#' uses \link{strat.boot} which runs \link{strat.sub} and then randomly duplicates some of the training cases to reach original
#' length of input (default) or length defined by \code{target.length}.
#'
#' \code{resample} is used by multiple \pkg{rtemis} learners, \link{gridSearchLearn}, and
#' \link{elevate}. Note that option 'kfold', which uses \link{kfold} results in resamples of slightly
#'   different length for y of small length, so avoid all operations which rely on equal-length vectors.
#'   For example, you can't place resamples in a data.frame, but must use a list instead.
#'
#' @param y Numeric vector. Usually the outcome; \code{length(y)} defines sample size
#' @param n.resamples Integer: Number of training/testing sets required
#' @param resampler Character: Type of resampling to perform: "bootstrap", "kfold", "strat.boot", "strat.sub".
#'   Default = "strat.boot" for \code{length(y) < 200}, otherwise "strat.sub"
#' @param index List where each element is a vector of training set indices. Use this for manual or precalculated
#' train/test splits
#' @param group Integer, vector, length = \code{length(y)}: Integer vector, where numbers define fold membership.
#' e.g. for 10-fold on a dataset with 1000 cases, you could use \code{group = rep(1:10, each = 100)}
#' @param stratify.var Numeric vector (optional): Variable used for stratification. Defaults to \code{y}
#' @param train.p Float (0, 1): Fraction of cases to assign to traininig set for \code{resampler = "strat.sub"}
#' @param strat.n.bins Integer: Number of groups to use for stratification for
#'   \code{resampler = "strat.sub" / "strat.boot"}
#' @param target.length Integer: Number of cases for training set for \code{resampler = "strat.boot"}.
#'   Default = \code{length(y)}
#' @param rtset List: Output of an \link{rtset.resample} (or named list with same structure).
#' NOTE: Overrides all other arguments. Default = NULL
#' @param seed Integer: (Optional) Set seed for random number generator, in order to make output reproducible.
#'   See \code{?base::set.seed}
#' @param verbose Logical: If TRUE, print messages to screen
#' @author E.D. Gennatas
#' @seealso \link{elevate}
#' @export
#' @examples
#' y <- rnorm(200)
#' # 10-fold (stratified)
#' res <- resample(y, 10, "kfold")
#' # 25 stratified subsamples
#' res <- resample(y, 25, "strat.sub")
#' # 100 stratified bootstraps
#' res <- resample(y, 100, "strat.boot")

resample <- function(y,
                     n.resamples = 10,
                     resampler = c("strat.sub", "strat.boot", "kfold", "bootstrap", "loocv"),
                     index = NULL,
                     group = NULL,
                     stratify.var = y,
                     train.p = .75,
                     strat.n.bins = 4,
                     target.length = NROW(y),
                     rtset = NULL,
                     seed = NULL,
                     verbose = TRUE) {

  # If rtset is provided, it takes precedence over all other arguments,
  # excluding the verbose arg
  if (!is.null(rtset)) {
    resampler <- rtset$resampler
    n.resamples <- rtset$n.resamples
    .stratify.var <- rtset$stratify.var
    train.p <- rtset$train.p
    strat.n.bins <- rtset$strat.n.bins
    target.length <- rtset$target.length
    seed <- rtset$seed
    index <- rtset$index
    group <- rtset$group
  }

  type <- if (!is.null(index)) ".index" else if (!is.null(group)) ".group" else ".res"

  if (type == ".res") {
    # type = "res" ====
    resampler <- match.arg(resampler)

    # [ INPUT ] ====
    if (NCOL(y) > 1) {
      if (verbose) msg("Input contains more than one columns; will stratify on last")
      y <- y[, NCOL(y)]
    }
    if (is.null(target.length)) target.length <- NROW(y) # TODO: move
    if (resampler == "strat.sub" | resampler == "strat.boot") {
      if (train.p <= 0 | train.p >= 1) stop("train.p must be greater than 0 and less than 1")
    }

    # [ RESAMPLE ] ====
    .stratify.var <- if (is.null(stratify.var)) y else stratify.var
    # stratify.var is for printing with parameterSummary
    stratify.var <- if (is.null(stratify.var)) getName(y, "y") else deparse(substitute(stratify.var))

    n.resamples <- as.integer(n.resamples)
    if (resampler == "loocv") n.resamples <- length(y)

    # [ Print parameters ] ====
    if (verbose) {
      if (resampler == "strat.sub") {
        parameterSummary(n.resamples, resampler, stratify.var, train.p, strat.n.bins,
                         title = "Resampling Parameters")
      } else if (resampler == "strat.boot") {
        parameterSummary(n.resamples, resampler, stratify.var, train.p, strat.n.bins, target.length,
                         title = "Resampling Parameters")
      } else if (resampler == "kfold") {
        parameterSummary(n.resamples, resampler, stratify.var, strat.n.bins,
                         title = "Resampling Parameters")
      } else {
        parameterSummary(n.resamples, resampler,
                         title = "Resampling Parameters")
      }
    }

    # [ Make resamples ] ====
    if (resampler == "bootstrap") {
      # '- Bootstrap ====
      res.part <- bootstrap(x = y,
                            n.resamples = n.resamples,
                            seed = seed)
    } else if (resampler == "kfold") {
      # '- kfold ====
      res.part <- kfold(x = y,
                        k = n.resamples,
                        stratify.var = .stratify.var,
                        strat.n.bins = strat.n.bins,
                        seed = seed,
                        verbose = verbose)
    } else if (resampler == "loocv") {
      # '- LOOCV ====
      res.part <- loocv(x = y)
    } else if (resampler == "strat.boot") {
      # '- strat.boot ====
      res.part <- strat.boot(x = y,
                             n.resamples = n.resamples,
                             train.p = train.p,
                             stratify.var = .stratify.var,
                             strat.n.bins = strat.n.bins,
                             target.length = target.length)
    } else {
      # '- strat.sub ====
      res.part <- strat.sub(x = y,
                            n.resamples = n.resamples,
                            train.p = train.p,
                            stratify.var = .stratify.var,
                            strat.n.bins = strat.n.bins,
                            seed = seed,
                            verbose = verbose)
    }

  } else if (type == ".group") {

    # type = "group" ====
    n.resamples <- length(unique(group))
    res.part <- vector("list", n.resamples)
    res.part <- plyr::llply(unique(group), function(i) which(group == i))
    names(res.part) <- unique(group)
    resampler <- "custom.group"

  } else {

    # type = "index" ====
    if (!is.list(index)) stop("index must be list of training set indices")
    n.resamples <- length(index)
    res.part <- index
    resampler <- "custom"
    if (is.null(names(res.part))) names(res.part) <- paste0("Resample", seq(n.resamples))

  }

  desc <- switch(resampler,
                 kfold = "independent folds",
                 strat.sub = "stratified subsamples",
                 strat.boot = "stratified bootstraps",
                 bootstrap = "bootstrap resamples",
                 loocv = "independent folds (LOOCV)",
                 "custom resamples")

  if (verbose) msg("Created", n.resamples, desc, newline.pre = TRUE)

  # Attributes ====
  class(res.part) <- c("resample", "list")
  attr(res.part, "N") <- n.resamples
  attr(res.part, "type") <- resampler
  attr(res.part, "seed") <- seed
  if (resampler %in% c("strat.sub", "strat.boot")) {
    attr(res.part, "train.p") <- train.p
  }
  if (resampler %in% c("strat.sub", "strat.boot", "kfold")) {
    strat.n.bins <- attr(res.part, "strat.n.bins")
    attr(res.part, "strat.n.bins") <- NULL
    attr(res.part, "strat.n.bins") <- strat.n.bins
  }
  if (resampler == "strat.boot") attr(res.part, "target.length") <- target.length
  res.part

} # rtemis::resample


#' \code{plot} method for \code{resample} object
#'
#' @method plot resample
#' @param x Vector; numeric or factor: Outcome used for resampling
#' @param res \link{resample} object
#' @param col Vector, color
#' @author E.D. Gennatas
#' @export

plot.resample <- function(x, col = NULL, ...) {

  mplot3.res(x, col = col, ...)

} # rtemis::plot.resample


#' \code{print} method for \link{resample} object
#'
#' Print resample information
#'
#' @method print resample
#' @param x \link{resample} object
#' @author E.D. Gennatas
#' @export

print.resample <- function(x, ...) {

  objcat("resample object")
  .attributes <- attributes(x)
  .attributes$names <- .attributes$class <- NULL
  # .attributes[[1]] <- .attributes[[2]] <- NULL
  printls(.attributes)

}


#' Bootstrap Resampling
#'
#' @param x Input vector
#' @param n.resamples Integer: Number of resamples to make. Default = 10
#' @param seed Integer: If provided, set seed for reproducibility. Default = NULL
#' @author E.D. Gennatas
#' @export

bootstrap <- function(x, n.resamples = 10,
                      seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  ids <- seq(length(x))
  .length <- length(x)
  if (!is.null(seed)) set.seed(seed)

  res <- lapply(seq(n.resamples), function(i) sort(sample(ids, .length, replace = TRUE)))
  names(res) <- paste0("Bootsrap_", seq(n.resamples))
  res

} # rtemis::bootstrap


#' K-fold Resampling
#'
#' @inheritParams resample
#' @param x Input Vector
#' @param k Integer: Number of folds. Default = 10
#' @author E.D. Gennatas
#' @export

kfold <- function(x, k = 10,
                  stratify.var = NULL,
                  strat.n.bins = 4,
                  seed = NULL,
                  verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (is.null(stratify.var)) stratify.var <- x
  stratify.var <- as.numeric(stratify.var)
  # ->> update
  max.bins <- length(unique(stratify.var))
  if (max.bins < strat.n.bins) {
    if (verbose) msg("Using max n bins possible =", max.bins)
    strat.n.bins <- max.bins
  }

  ids <- seq_along(x)
  # cuts
  cuts <- cut(stratify.var, breaks = strat.n.bins, labels = FALSE)
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

  res <- lapply(seq(k), function(i) seq(ids)[-sort(unlist(lapply(seq_along(cut.bins), function(j) idl.k[[j]][[i]])))])

  names(res) <- paste0("Fold_", seq(k))
  attr(res, "strat.n.bins") <- strat.n.bins
  res

} # rtemis::kfold


#' Resample using Stratified Subsamples
#'
#' @inheritParams resample
#' @param x Input vector
#' @author E.D. Gennatas
#' @export

strat.sub <- function(x,
                      n.resamples = 10,
                      train.p = .75,
                      stratify.var = NULL,
                      strat.n.bins = 4,
                      seed = NULL,
                      verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (is.null(stratify.var)) stratify.var <- x
  stratify.var <- as.numeric(stratify.var)
  max.bins <- length(unique(stratify.var))
  if (max.bins < strat.n.bins) {
    if (verbose) msg("Using max n bins possible =", max.bins)
    strat.n.bins <- max.bins
  }
  ids <- seq_along(x)
  cuts <- cut(stratify.var, breaks = strat.n.bins, labels = FALSE)
  cut.bins <- sort(unique(cuts))
  idl <- lapply(seq_along(cut.bins), function(i) ids[cuts == cut.bins[i]])
  # idl.length <- sapply(idl, length)
  idl.length <- as.numeric(table(cuts))
  res <- lapply(seq(n.resamples), function(i)
    sort(unlist(sapply(seq_along(cut.bins), function(j)
      sample(idl[[j]], train.p * idl.length[j])))))
  names(res) <- paste0("Subsample_", seq(n.resamples))
  attr(res, "strat.n.bins") <- strat.n.bins
  res

} # rtemis::strat.sub


#' Stratified Bootstrap Resampling
#'
#' @inheritParams resample
#' @param x Input vector
#' @author E.D. Gennatas
#' @export

strat.boot <- function(x, n.resamples = 10,
                       train.p = .75,
                       stratify.var = NULL,
                       strat.n.bins = 4,
                       target.length = NULL,
                       seed = NULL,
                       verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  res.part1 <- strat.sub(x = x, n.resamples = n.resamples,
                         train.p = train.p,
                         stratify.var = stratify.var,
                         strat.n.bins = strat.n.bins,
                         verbose = verbose)

  # Make sure target.length was not too short by accident
  res.length <- length(res.part1[[1]])
  if (is.null(target.length)) target.length <- length(x)
  if (target.length < res.length) target.length <- length(x)

  # Add back this many cases
  add.length <- target.length - res.length
  doreplace <- ifelse(add.length > res.length, 1, 0)
  res.part2 <- lapply(res.part1, function(i) sample(i, add.length, replace = doreplace))
  res <- mapply(c, res.part1, res.part2, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  names(res) <- paste0("StratBoot_", seq(n.resamples))
  attr(res, "strat.n.bins") <- strat.n.bins
  res

} # rtemis::strat.boot


#' Leave-one-out Resampling
#'
#' @param x Input vector
#' @author E.D. Gennatas
#' @export

loocv <- function(x) {

  res <- lapply(seq(x), function(i) (seq(x))[-i])
  names(res) <- paste0("Fold_", seq(res))
  res

} # rtemis::loocv
