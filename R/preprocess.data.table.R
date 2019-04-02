# preprocess.data.table.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' \code{preprocess.data.table} Data preprocessing optimized for \code{data.table}
#'
#' @rdname preprocess
#' @export

preprocess.data.table <- function(x,
                                  completeCases = FALSE,
                                  removeCases.thres = NULL,
                                  removeFeatures.thres = NULL,
                                  impute = FALSE,
                                  impute.discrete = getMode,
                                  impute.numeric = mean,
                                  integer2factor = FALSE,
                                  integer2numeric = FALSE,
                                  logical2factor = FALSE,
                                  logical2numeric = FALSE,
                                  nonzeroFactors = FALSE,
                                  scale = FALSE,
                                  center = FALSE,
                                  removeConstant = TRUE,
                                  verbose = TRUE, ...) {
  
  # [ Complete cases ] ====
  if (completeCases) {
    if (verbose) msg("Filtering complete cases...")
    x <- x[complete.cases(x), ]
  }
  
  # [ Remove Cases by missing feature threshold ] ====
  if (!is.null(removeCases.thres)) {
    if (anyNA(x)) {
      # na.fraction.bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na.fraction.bycase <- data.table::transpose(x[, -1])[, lapply(.SD, function(i) sum(is.na(i))/length(i))]
      removeCases.thres.index <- which(na.fraction.bycase >= removeCases.thres)
      if (length(removeCases.thres.index) > 0) {
        if (verbose) msg("Removing", length(removeCases.thres.index), "cases with >=",
                         removeCases.thres, "missing data...")
        x <- x[-removeCases.thres.index, ]
      }
    }
  }
  
  # [ Remove Features by missing feature threshold ] ====
  if (!is.null(removeFeatures.thres)) {
    if (anyNA(x)) {
      na.fraction.byfeat <- x[, lapply(.SD, function(i) sum(is.na(i))/length(i))]
      removeFeat.thres.index <- which(na.fraction.byfeat >= removeFeatures.thres)
      if (length(removeFeat.thres.index) > 0) {
        if (verbose) msg("Removing", length(removeFeat.thres.index), "features with >=", 
                         removeFeatures.thres, "missing data...")
        x <- x[, -removeFeat.thres.index, with = FALSE]
      }
    }
  }
  
  # [ Remove constants ] ====
  if (removeConstant) {
    constant <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
    if (length(constant) > 0) {
      if (verbose) msg("Removing constant features...")
      x[, (constant):= NULL]
    }
  }
  
  # [ Integer to factor ] ====
  if (integer2factor) {
    index.integer <- which(sapply(x, is.integer))
    if (verbose) msg("Converting integers to factors...")
    for (i in index.integer) x[[i]] <- as.factor(x[[i]])
  }
  
  # [ Integer to factor ] ====
  if (integer2numeric) {
    index.integer <- which(sapply(x, is.integer))
    if (verbose) msg("Converting integers to numeric")
    for (i in index.integer) x[[i]] <- as.numeric(x[[i]])
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
  
  # [ Impute ] ====
  if (impute) {
    if (verbose) msg("Imputing missing values...")
    
    factor.index <- which(sapply(x, function(i) is.factor(i) && anyNA(i)))
    if (length(factor.index) > 0) {
      for (j in factor.index) {
        index <- which(is.na(x[, j]))
        imputed <- impute.discrete(x[[j]])
        for (i in index) data.table::set(x, i, j, imputed)
      }
    }
    
    integer.index <- which(sapply(x, function(i) is.integer(i) && anyNA(i)))
    if (length(integer.index) > 0) {
      for (j in integer.index) {
        imputed <- impute.discrete(x[[j]])
        set(x, which(is.na(x[[j]])), j, value = imputed)
      }
    }
    
    numeric.index <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
    if (length(numeric.index) > 0) {
      for (j in numeric.index) {
        imputed <- impute.numeric(x[[j]], na.rm = TRUE)
        set(x, which(is.na(x[[j]])), j, value = imputed)
      }
    }
    
  } # /impute
  
  # [ Scale +/- center ] ====
  if (scale | center) {
    sc <- if (scale) "Scaling" else NULL
    ce <- if (center) "Centering" else NULL
    if (verbose) msg(paste(c(sc, ce), collapse = " and "), "dataset...")
    # x <- as.data.table(scale(x, scale = scale, center = center))
    numeric.index <- which(sapply(x, function(i) is.numeric(i) && !is.integer(i)))
    for (col in names(x)[numeric.index]) set(x, j = col, value = scale(x[[col]],
                                                                       scale = scale,
                                                                       center = center))
  }
  
  if (verbose) msg("Done")
  x
  
} # rtemis::preprocess
