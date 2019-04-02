# dataPrepare.R
# ::rtemis::
# Efstathios D. Gennatas egenn.github.io

#' \code{rtemis-internals}: \code{dataPrepare}
#'
#' Prepare data for \pkg{rtemis} modeling
#'
#' @inheritParams s.GLM
#' @param x.valid Matrix / Data frame: Validation set features
#' @param y.valid Vector: Validation outcome
#' @param ipw Logical: If TRUE, return class weights for inverse probability weighting
#' (for Classification)
#' @param ipw.type {1, 2}: 1:
#' @param removeDots Logical: If TRUE, replace dots in variable names with underscores.
#' Some algorithms do not work with variable names containing dots (SparkML)
#' @param .preprocess List: Preprocessing settings. Set with \link{rtset.preprocess}
#' @param verbose Logical: If TRUE, print messages to console
#' @export

dataPrepare <- function(x, y, x.test = NULL, y.test = NULL,
                        x.valid = NULL, y.valid = NULL,
                        ipw = FALSE,
                        ipw.type = 2,
                        upsample = FALSE,
                        upsample.seed = NULL,
                        removeDots = FALSE,
                        .preprocess = NULL,
                        verbose = FALSE) {
  
  if (!is.list(x)) x <- as.data.frame(x)
  ncol.x <- NCOL(x)
  
  # x/y, train/test ====
  
  # (x.train, x.test) scenario
  if (!is.null(y) && NCOL(y) > 1 && !inherits(y, "Surv") && NCOL(y) == ncol.x && is.null(x.test) && is.null(y.test)) {
    y.test <- y[, ncol.x]
    x.test <- y[, seq(ncol.x - 1)]
    y <- x[, ncol.x]
    x <- x[, seq(ncol.x - 1)]
  }
  
  # If no y is specified, assume it is the last column of x
  if (is.null(y) & ncol.x > 1) {
    y <- x[, ncol.x]
    x <- x[, seq(ncol.x - 1), drop = FALSE]
  }
  
  if (!is.null(x.test) & is.null(y.test) & NCOL(x.test) == ncol.x) {
    y.test <- x.test[, ncol.x]
    x.test <- x.test[, seq(ncol.x - 1)]
  }
  
  if (!is.null(x.valid) & is.null(y.valid) & NCOL(x.valid) == ncol.x) {
    y.valid <- x.valid[, ncol.x]
    x.valid <- x.valid[, seq(ncol.x - 1)]
  }
  
  # [ preprocess ] ====
  if (!is.null(.preprocess)) {
    .preprocess$x <- x
    x <- do.call(preprocess, .preprocess)
    if (!is.null(x.test)) {
      .preprocess$x <- x.test
      x.test <- do.call(preprocess, .preprocess)
    }
  }
  
  # If outcome vector is character, convert to factor
  if (class(y) == "character") {
    y <- as.factor(y)
    if (!is.null(y.test)) y.test <- as.factor(y.test)
  }
  
  # If x is vector, convert to matrix - later to data.frame
  if (NCOL(x) == 1 & class(x)[1] != "list") {
    x <- as.matrix(x) # if you use as.data.frame, it will be given colnames
    if (!is.null(x.test)) if (NCOL(x.test) == 1) x.test <- as.matrix(x.test)
  }
  
  # [ xnames and Dimensions check ] ====
  if (class(x)[1] == "list") {
    # for meta models
    # Test list lengths match
    if (!is.null(x.test)) {
      if (length(x) != length(x.test)) stop("Training and testing feature sets are not of same length")
    }
    # Extract names from first list element / feature set
    # if (is.null(colnames(x[[1]]))) {
    #   xnames <- paste0("Predictor", 1:NCOL(x[[1]]))
    #   if (!is.null(x.test)) lapply(x.test, function(i) colnames(i) <- paste0("Predictor", 1:NCOL(x[[i]])))
    # }
    xnames <- c(sapply(x, colnames))
    
    # Remove dots from names if unsupported by algorithm (SparkML)
    if (removeDots) xnames <- gsub("\\.", "_", colnames(x))
    
    x <- lapply(x, as.data.frame)
    if (!is.null(x.test)) {
      x.test <- lapply(x.test, as.data.frame)
    }
    # Test that dimensions match
    for (i in 1:length(x)) {
      if (NROW(x[[i]]) != NROW(y)) stop("Training set features and outcome do not contain same number of cases")
    }
    if (!is.null(x.test)) {
      for (i in 1:length(x)) {
        if (NCOL(x[[i]]) != NCOL(x.test[[i]]))
          stop("Feature set #", i, ": Training and testing set features do not contain same number of variables")
      }
    }
    if (!is.null(y.test)) {
      for (i in 1:length(x)) {
        if (NROW(x.test[[i]]) != NROW(y.test))
          stop("Feature set #", i, ": Testing set features and outcome do not contain same number of cases")
      }
    }
    # [ end for meta.list ]
  } else {
    # '- Test dimensions match ====
    if (NROW(x) != NROW(y)) stop("Training set features and outcome do not contain same number of cases")
    if (!is.null(x.test)) if (NCOL(x) != NCOL(x.test))
      stop("Training and testing set do not contain same number of features")
    if (!is.null(y.test)) if (NROW(x.test) != NROW(y.test))
      stop("Testing set features and outcome do not contain same number of cases")
    
    # '- Column names ====
    if (is.null(colnames(x))) {
      xnames <- paste0("Predictor", 1:NCOL(x))
      if (!is.null(x.test)) colnames(x.test) <- paste0("Predictor", 1:NCOL(x))
    } else {
      xnames <- if (removeDots) xnames <- gsub("\\.", "_", colnames(x)) else colnames(x)
    }
    colnames(x) <- xnames
    x <- as.data.frame(x)
    if (!is.null(x.test)) {
      x.test <- as.data.frame(x.test)
      colnames(x.test) <- xnames
    }
  } # //xnames and dimensions check
  
  # [ TYPE ] ====
  if (!is.null(dim(y))) y <- as.vector(y)
  type <- switch(class(y),
                 factor = "Classification",
                 Surv = "Survival",
                 "Regression")
  
  # [ UPSAMPLE: balance outcome class ] ====
  if (type == "Classification" & upsample) {
    if (!is.null(upsample.seed)) set.seed(upsample.seed)
    freq <- as.data.frame(table(y))
    maxfreq.i <- which.max(freq$Freq)
    if (verbose) {
      msg("Upsampling to create balanced set...", newline = TRUE)
      msg(levels(y)[maxfreq.i], "is majority outcome with length =", max(freq$Freq))
    }
    y.classIndex.list <- lapply(levels(y), function(x) which(y == x))
    # all <- 1:length(levels(y))
    to.upsample <- setdiff(1:length(levels(y)), maxfreq.i)
    y.upsampled.classIndex.list <- y.classIndex.list
    target.length <- length(y.classIndex.list[[maxfreq.i]])
    for (i in to.upsample) {
      do.replace <- length(y.classIndex.list[[i]]) * 2 < length(y.classIndex.list[[maxfreq.i]])
      y.upsampled.classIndex.list[[i]] <- c(y.classIndex.list[[i]],
                                            sample(y.classIndex.list[[i]],
                                                   target.length - length(y.classIndex.list[[i]]),
                                                   do.replace))
    }
    upsample.index <- unlist(y.upsampled.classIndex.list)
    x0 <- x
    y0 <- y
    x <- x[upsample.index, , drop = FALSE]
    y <- y[upsample.index]
  } else {
    x0 <- y0 <- NULL
  }
  
  # [ IPW: Inverse Probability Weighting for Classification ] ====
  class.weights <- weights <- NULL
  if (type == "Classification" & ipw) {
    freq <- as.data.frame(table(y))[, 2]
    class.weights <- rev(freq/sum(freq)) # add to 1
    names(class.weights) <- levels(y)
    # if (sum(diff(freq[, 2])) == 0) {
    if (sum(diff(freq)) == 0) {
      # class.weights <- rep(1, length(levels(y)))/length(levels(y))
      weights <- rep(1, NROW(y))
    } else {
      # class.weights <- 1 / (freq[, 2]/sum(freq[, 2]))
      if (ipw.type == 1) {
        weights <- class.weights / max(class.weights)
      } else if (ipw.type == 2) {
        weights <- class.weights / min(class.weights)
      }
      if (verbose) msg("Imbalanced classes: using Inverse Probability Weighting", newline = TRUE)
      weights <- weights[as.integer(y)]
    }
  }
  # maybe return weights <- rep(1, NROW(y))
  
  # [ SURVIVAL ] ====
  if (survival::is.Surv(y)) {
    type <- "Survival"
  }
  
  # [ OUTRO ] ====
  list(x = x, y = y,
       x.test = x.test, y.test = y.test,
       x.valid = x.valid, y.valid = y.valid,
       x0 = x0, y0 = y0,
       xnames = xnames, type = type,
       class.weights = class.weights, weights = weights)
  
} # rtemis::dataPrepare
