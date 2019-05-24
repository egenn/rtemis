# resample.R
# ::rtemis::
# 2015-8 Efstathios D. Gennatas egenn.github.io

#' Resampling methods
#'
#' Create resamples of your data, e.g. for model building or validation.
#' "bootstrap" gives the standard bootstrap, i.e. random sampling with replacement, using \code{caret::createResample},
#' "strat.sub" creates stratified subsamples using \code{caret::createDataPartition}, while "strat.boot"
#' runs \code{caret::createDataPartition} and then randomly duplicates some of the training cases to reach original
#' length of input (default) or length defined by \code{target.length}.
#'
#' \code{resample} is used by multiple \pkg{rtemis} learners and functions. Note that option 'kfold', which uses \code{caret::createFolds} results in resamples of slightly
#'   different length for y of small length, so avoid all operations which rely on equal-length vectors.
#'   For example, you can't place resamples in a data.frame, but must use a list instead.
#'
#' @param y Numeric vector. Usually the outcome; \code{length(y)} defines sample size
#' @param n.resamples Integer: Number of training/testing sets required
#' @param resampler String: Type of resampling to perform: "bootstrap", "kfold", "strat.boot", "strat.sub".
#'   Default = "strat.boot" for \code{length(y) < 200}, otherwise "strat.sub"
#' @param index List where each element is a vector of training set indices. Use this for manual or precalculated
#' train/test splits
#' @param group Integer, vector, length = \code{length(y)}: Integer vector, where numbers define fold membership.
#' e.g. for 10-fold on a dataset with 1000 cases, you could use \code{group = rep(1:10, each = 100)}
#' @param stratify.var Numeric vector (optional): Variable used for stratification. Defaults to \code{y}
#' @param cv.p Float (0, 1): Fraction of cases to assign to traininig set for \code{resampler = "strat.sub"}
#' @param cv.groups Integer: Number of groups to use for stratification for
#'   \code{resampler = "strat.sub" / "strat.boot"}
#' @param target.length Integer: Number of cases for training set for \code{resampler = "strat.boot"}.
#'   Default = \code{length(y)}
#' @param rtset List: Output of an \link{rtset.resample} (or named list with same structure).
#' NOTE: Overrides all other arguments. Default = NULL
#' @param seed Integer: (Optional) Set seed for random number generator, in order to make output reproducible.
#'   See \code{?base::set.seed}
#' @param verbose Logical: If TRUE, print messages to screen
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate}
#' @export

resample <- function(y = NULL, n.resamples = 10,
                     resampler = c("strat.sub", "strat.boot", "kfold", "bootstrap", "loocv"),
                     index = NULL,
                     group = NULL,
                     stratify.var = y,
                     cv.p = .75,
                     cv.groups = 4,
                     target.length = NROW(y),
                     rtset = NULL,
                     seed = NULL,
                     verbose = FALSE) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("caret", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  if (!is.null(rtset)) {
    # return(do.call(resample, args = c(list(y = y), rtset))) # do.call always causes msg() to not print fn name
    resampler <- rtset$resampler
    n.resamples <- rtset$n.resamples
    .stratify.var <- rtset$stratify.var
    cv.p <- rtset$cv.p
    cv.groups <- rtset$cv.groups
    target.length <- rtset$target.length
    seed <- rtset$seed
    index <- rtset$index
    group <- rtset$group
    # verbose <- rtset$verbose # Rather use the verbose arg here
  }

  type <- if (!is.null(index)) ".index" else if (!is.null(group)) ".group" else ".res"

  if (type == ".res") {
    # type = "res" ====
    resampler <- match.arg(resampler)

    # [ INPUT ] ====
    if (is.null(y)) stop("Please provide y")
    if (NCOL(y) > 1) {
      if (verbose) msg("Input contains more than one columns; will stratify on last")
      y <- y[, NCOL(y)]
    }
    if (is.null(target.length)) target.length <- NROW(y)
    if (resampler == "strat.sub" | resampler == "strat.boot") {
      if (cv.p <= 0 | cv.p >= 1) stop("cv.p must be greater than 0 and less than 1")
    }
    # Check there are enough cases for stratification in given k
    if (is.factor(y)) {
      if (resampler %in% c("strat.sub", "strat.boot", "kfold")) {
        freq <- table(y)
        if (min(freq) < n.resamples) {
          warning("Insufficient number of cases for ", n.resamples,
                  " stratified resamples: will use ", min(freq)," instead")
          n.resamples <- min(freq)
        }
      }
    }

    # [ RESAMPLE ] ====
    .stratify.var <- if (is.null(stratify.var)) y else stratify.var
    # stratify.var is for printing with parameterSummary
    stratify.var <- if (is.null(stratify.var)) getName(y, "y") else deparse(substitute(stratify.var))
    # Override all settings with rtset
    # if (!is.null(rtset)) for (i in 1:length(rtset)) assign(names(rtset[i]), rtset[[i]])

    # delta
    # if (!is.null(rtset)) {
    #   # return(do.call(resample, args = c(list(y = y), rtset))) # do.call always causes msg() to not print fn name
    #   resampler <- rtset$resampler
    #   n.resamples <- rtset$n.resamples
    #   .stratify.var <- rtset$stratify.var
    #   cv.p <- rtset$cv.p
    #   cv.groups <- rtset$cv.groups
    #   target.length <- rtset$target.length
    #   seed <- rtset$seed
    #   # verbose <- rtset$verbose # Rather use the verbose arg here
    # }
    # /delta

    .stratify.var <- if (is.null(.stratify.var)) y else .stratify.var # TODO: lol

    n.resamples <- as.integer(n.resamples)
    if (length(resampler) > 1) {
      resampler <- ifelse(length(y) < 200, "strat.boot", "strat.sub")
    }
    if (resampler == "loocv") n.resamples <- length(y)

    # [ Print parameters ] ====
    if (resampler == "strat.sub") {
      if (verbose) parameterSummary(n.resamples, resampler, stratify.var, cv.p, cv.groups,
                                    title = "Resampling Parameters")
    } else if (resampler == "strat.boot") {
      if (verbose) parameterSummary(n.resamples, resampler, stratify.var, cv.p, cv.groups, target.length,
                                    title = "Resampling Parameters")
    } else {
      if (verbose) parameterSummary(n.resamples, resampler,
                                    title = "Resampling Parameters")
    }

    # Set seed
    if (!is.null(seed)) set.seed(seed)

    # [ Make resamples ] ====
    if (resampler == "bootstrap") {
      # Bootstrap
      res.part <- caret::createResample(y = y, times = n.resamples)
    } else if (resampler == "kfold") {
      res.part <- caret::createFolds(y, k = n.resamples, returnTrain = T)
    } else if (resampler == "loocv") {
      res.part <- lapply(1:NROW(y), function(i) (1:NROW(y))[-i])
      names(res.part) <- paste0("Fold", seq(res.part))
    } else if (resampler == "strat.boot") {
      # "Stratified Bootstrap"
      # Create stratified resamples
      res.part1 <- caret::createDataPartition(y = .stratify.var, times = n.resamples, p = cv.p, groups = cv.groups)
      # Make sure target.length was not too short by accident
      if (is.null(target.length)) target.length <- NROW(y)
      if (target.length < length(res.part1[[1]])) target.length <- NROW(y)
      # Add back this many cases
      add.length <- target.length - length(res.part1[[1]])
      doreplace <- ifelse(add.length > length(res.part1[[1]]), 1, 0)
      res.part2 <- lapply(res.part1, function(x) sample(x, add.length, replace = doreplace))
      res.part <- mapply(c, res.part1, res.part2, SIMPLIFY = FALSE)
      res.part <- lapply(res.part, sort)
    } else {
      # Stratified Cross-Validation
      res.part <- caret::createDataPartition(y = .stratify.var, times = n.resamples, p = cv.p, groups = cv.groups)
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

  if (verbose) msg("Created", n.resamples, desc)
  class(res.part) <- c("resample", "list")
  attr(res.part, "N") <- n.resamples
  attr(res.part, "type") <- resampler
  attr(res.part, "seed") <- seed
  if (resampler %in% c("strat.sub", "strat.boot")) {
    attr(res.part, "cv.p") <- cv.p
    attr(res.part, "cv.groups") <- cv.groups
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
#' @author Efstathios D. Gennatas
#' @export

plot.resample <- function(x, res, col = NULL, ...) {

  mplot3.res(x, res, col = col, ...)

} # rtemis::plot.resample


#' \code{print} method for \link{resample} object
#'
#' Print resample information
#'
#' @method print resample
#' @param x \link{resample} object
#' @param ... Unused
#' @author Efstathios D. Gennatas
#' @export

print.resample <- function(x, ...) {

  boxcat(".:rtemis resample object", newline.pre = FALSE)
  .attributes <- attributes(x)
  .attributes[[1]] <- .attributes[[2]] <- NULL
  printls(.attributes)

} # rtemis::print.resample
