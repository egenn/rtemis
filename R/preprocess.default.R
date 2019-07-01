# preprocess.data.frame.R
# ::rtemis::
# 2017-8 Efstathios D. Gennatas egenn.github.io

#' \code{preprocess.default} Data preprocessing for \code{data.frame}
#'
#' @rdname preprocess
#' @export

preprocess.default <- function(x, y = NULL,
                               completeCases = FALSE,
                               removeCases.thres = NULL,
                               removeFeatures.thres = NULL,
                               impute = FALSE,
                               impute.type = c("missForest", "rfImpute", "meanMode"),
                               impute.niter = 10,
                               impute.ntree = 500,
                               missForest.parallelize = c('no', 'variables', 'forests'),
                               impute.discrete = getMode,
                               impute.numeric = mean,
                               integer2factor = FALSE,
                               integer2numeric = FALSE,
                               logical2factor = FALSE,
                               logical2numeric = FALSE,
                               numeric2factor = FALSE,
                               numeric2factor.levels = NULL,
                               character2factor = FALSE,
                               nonzeroFactors = FALSE,
                               scale = FALSE,
                               center = FALSE,
                               removeConstant = TRUE,
                               oneHot = FALSE,
                               exclude = NULL,
                               verbose = TRUE) {

  # Arguments ====
  impute.type <- match.arg(impute.type)

  x <- as.data.frame(x)

  # [ Complete cases ] ====
  if (completeCases) {
    if (verbose) msg("Filtering complete cases...")
    x <- x[complete.cases(x), ]
  }

  # [ Set aside excluded ] ====
  if (!is.null(exclude)) {
    excluded <- x[, exclude, drop = FALSE]
    excluded.names <- colnames(x)[exclude]
    x <- x[, -exclude, drop = FALSE]
  }

  # [ Remove Cases by missing feature threshold ] ====
  if (!is.null(removeCases.thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      # na.fraction.bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na.fraction.bycase <- data.table::transpose(xt[, -1])[, lapply(.SD, function(i) sum(is.na(i))/length(i))]
      removeCases.thres.index <- which(na.fraction.bycase >= removeCases.thres)
      if (length(removeCases.thres.index) > 0) {
        if (verbose) msg("Removing", length(removeCases.thres.index), "cases with >=",
                         removeCases.thres, "missing data...")
        xt <- xt[-removeCases.thres.index, ]
      }
      x <- as.data.frame(xt)
    }
  }

  # [ Remove Features by missing feature threshold ] ====
  if (!is.null(removeFeatures.thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      na.fraction.byfeat <- xt[, lapply(.SD, function(i) sum(is.na(i))/length(i))]
      removeFeat.thres.index <- which(na.fraction.byfeat >= removeFeatures.thres)
      if (length(removeFeat.thres.index) > 0) {
        if (verbose) msg("Removing", length(removeFeat.thres.index), "features with >=",
                         removeFeatures.thres, "missing data...")
        x <- x[, -removeFeat.thres.index]
      }
    }
  }

  # [ Integer to factor ] ====
  if (integer2factor) {
    index.integer <- which(sapply(x, is.integer))
    if (verbose) msg("Converting integers to factors...")
    for (i in index.integer) x[, i] <- as.factor(x[, i])
  }

  # [ Integer to numeric ] ====
  if (integer2numeric) {
    index.integer <- which(sapply(x, is.integer))
    if (verbose) msg("Converting integers to numeric")
    for (i in index.integer) x[, i] <- as.numeric(x[, i])
  }

  # [ Logical to factor ] ====
  if (logical2factor) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) msg("Converting logicals to factor")
    for (i in index.logical) x[, i] <- as.factor(x[, i])
  }

  # [ Logical to numeric ] ====
  if (logical2numeric) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) msg("Converting logicals to factor")
    for (i in index.logical) x[, i] <- as.numeric(x[, i])
  }

  # [ Numeric to factor ] ====
  if (numeric2factor) {
    index.numeric <- which(sapply(x, is.numeric))
    if (verbose) msg("Converting numeric to factor")
    if (is.null(numeric2factor.levels)) {
      for (i in index.numeric) x[, i] <- as.factor(x[, i])
    } else {
      for (i in index.numeric) x[, i] <- factor(x[, i], levels = numeric2factor.levels)
    }
  }

  # [ Character to factor ] ====
  if (character2factor) {
    index.char <- which(sapply(x, is.character))
    if (verbose) msg("Converting characters to factors...")
    for (i in index.char) x[, i] <- as.factor(x[, i])
  }

  # [ Nonzero factors ] ====
  if (nonzeroFactors) {
    if (verbose) msg("Shifting factor levels to exclude 0")
    if (any(sapply(x, is.factor))) {
      for (i in seq(NCOL(x))) {
        if (is.factor(x[, i])) {
          while (any(x[, i] == 0)) {
            x[, i] <- factor(as.numeric(as.character(x[, i])) + 1)
          }
        }
      }
    }
  }

  # [ Impute ] ====
  if (impute) {
    if (impute.type == "missForest") {
      # '- missFores ----
      if (verbose) msg("Imputing missing values using missForest...")
      x <- missForest::missForest(x, maxiter = impute.niter, ntree = impute.ntree,
                                  parallelize = missForest.parallelize)$ximp

    } else if (impute.type == "rfImpute") {
      # '- rfImpute ----
      if (is.null(y)) stop("Please provide outcome 'y' for imputation using proximity from randomForest or use missForest instead")
      x <- randomForest::rfImpute(x, y, iter = impute.niter, ntree = impute.ntree)

    } else {
      # '- mean/mode ----
      if (verbose) msg0("Imputing missing values using ", deparse(substitute(impute.numeric)),
                        " and ", deparse(substitute(impute.discrete)), "...")

      discrete.index <- which(sapply(x, function(i) is.discrete(i) && anyNA(i)))
      if (length(discrete.index) > 0) {
        for (i in discrete.index) {
          index <- which(is.na(x[, i]))
          imputed <- impute.discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      integer.index <- which(sapply(x, function(i) is.integer(i) && anyNA(i)))
      if (length(integer.index) > 0) {
        for (i in integer.index) {
          index <- which(is.na(x[, i]))
          imputed <- impute.discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      numeric.index <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
      if (length(numeric.index) > 0) {
        for (i in numeric.index) {
          index <- which(is.na(x[, i]))
          imputed <- impute.numeric(x[, i], na.rm = TRUE)
          x[index, i] <- imputed
        }
      }
    }
    }

  # [ Scale +/- center ] ====
  if (scale | center) {
    sc <- if (scale) "Scaling" else NULL
    ce <- if (center) "Centering" else NULL
    if (verbose) msg(paste(c(sc, ce), collapse = " and "), "dataset...")
    x <- as.data.frame(scale(x, scale = scale, center = center))
  }

  # [ Remove constants ] ====
  if (removeConstant) {
    constant <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
    if (length(constant) > 0) {
      if (verbose) msg("Removing constant features...")
      x <- x[, -constant]
    }
  }

  # [ One Hot Encoding ] ====
  if (oneHot) x <- oneHot(x, verbose = verbose)

  # [ Add back excluded ] ====
  if (!is.null(exclude)) {
    if (!is.null(removeCases.thres) && length(removeCases.thres.index) > 0) {
      n.feat.inc <- NCOL(x)
      x <- cbind(x, excluded[-removeCases.thres.index, ])
      colnames(x)[-c(seq(n.feat.inc))] <- excluded.names
    } else {
      x <- cbind(x, excluded)
    }
  }

  if (verbose) msg("Done")
  x

} # rtemis::preprocess
