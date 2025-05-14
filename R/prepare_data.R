# prepare_data.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Prepare data for \pkg{rtemis} supervised learning
#'
#' @param x Either training set features or combined training features and
#' outcome in last column
#' @param y Either training set outcome or combined testing set features and
#' outcome in last column.
#' @param x.test Testing set features
#' @param y.test Testing set outcome
#' @param x.valid Matrix / Data frame: Validation set features
#' @param y.valid Vector: Validation outcome
#' @param filter.y.na Logical: If TRUE, filter out cases with missing values
#' in `y`
#' @param ifw Logical: If TRUE, return class weights for inverse frequency
#' weighting for Classification
#' @param ifw.type {1, 2}: 1:
#' @param upsample Logical: If TRUE, downsample majority class to match size of minority class
#' @param downsample Logical: If TRUE, downsample majority class to match size of minority class
#' @param resample.seed Integer: If set, use `set.seed` for reproducibility. Default = NULL
#' @param removeDots Logical: If TRUE, replace dots in variable names with underscores.
#' Some algorithms do not work with variable names containing dots (SparkML)
#' @param .preprocess List: Preprocessing parameters to be passed to [preprocess]. Set with [setup.preprocess]
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @keywords internal
#' @noRd
#' @author E.D. Gennatas

prepare_data <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  x.valid = NULL,
  y.valid = NULL,
  filter.y.na = FALSE,
  ifw = FALSE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  removeDots = FALSE,
  .preprocess = NULL,
  verbose = FALSE
) {
  if (upsample && downsample)
    stop("Only one of upsample and downsample can be TRUE")

  if (class(x)[1] != "list") {
    x <- as.data.frame(x)
  } else {
    x <- lapply(x, as.data.frame)
  }
  ncol.x <- NCOL(x)

  if (!is.null(x.test)) {
    if (class(x.test)[1] != "list") {
      x.test <- as.data.frame(x.test)
    } else {
      x.test <- lapply(x.test, as.data.frame)
    }
  }

  # x/y, train/test ----

  # '- (x = x.train_y, y = x.test_y) ----
  if (
    !is.null(y) &&
      NCOL(y) > 1 &&
      !inherits(y, "Surv") &&
      NCOL(y) == ncol.x &&
      is.null(x.test) &&
      is.null(y.test)
  ) {
    y <- as.data.frame(y)
    y.test <- y[, ncol.x]
    x.test <- y[, -ncol.x]
    y <- x[, ncol.x]
    x <- x[, -ncol.x, drop = FALSE]
  }

  # '- (x = x.train_y) or (x = x.train_y, x.test = x.test_y) ----
  # If no y is specified, assume it is the last column of x
  if (is.null(y) && ncol.x > 1) {
    y <- x[, ncol.x]
    x <- x[, -ncol.x, drop = FALSE]
  }

  # '- (x = x.train_y, x.test = x.test_y) ----
  if (!is.null(x.test) && is.null(y.test) && NCOL(x.test) == ncol.x) {
    y.test <- x.test[, ncol.x]
    x.test <- x.test[, seq(ncol.x - 1)]
  }

  if (!is.null(x.valid) && is.null(y.valid) && NCOL(x.valid) == ncol.x) {
    y.valid <- x.valid[, ncol.x]
    x.valid <- x.valid[, seq(ncol.x - 1)]
  }

  # preprocess ----
  if (!is.null(.preprocess)) {
    .preprocess$x <- x
    x <- do.call(preprocess, .preprocess)
    if (!is.null(x.test)) {
      .preprocess$x <- x.test
      x.test <- do.call(preprocess, .preprocess)
    }
  }

  if (!(is.numeric(y) || is.factor(y))) {
    stop(
      "Outcome is class ",
      class(y),
      " but must be either numeric (for regression) or a factor (for classification)."
    )
  }

  # xnames and dimensions check ----
  if (class(x)[1] == "list") {
    # for meta models
    # Test list lengths match
    if (!is.null(x.test)) {
      if (length(x) != length(x.test)) {
        stop("Training and testing feature sets are not of same length")
      }
    }
    xnames <- c(sapply(x, colnames))

    # Remove dots from names if unsupported by algorithm (SparkML)
    # Must do same before predict
    if (removeDots) xnames <- gsub("\\.", "_", colnames(x))

    x <- lapply(x, as.data.frame)
    if (!is.null(x.test)) {
      x.test <- lapply(x.test, as.data.frame)
    }
    # Test that dimensions match
    for (i in seq_along(x)) {
      if (NROW(x[[i]]) != NROW(y)) {
        stop(
          "Training set features and outcome do not contain same number of cases"
        )
      }
    }
    if (!is.null(x.test)) {
      for (i in seq_along(x)) {
        if (NCOL(x[[i]]) != NCOL(x.test[[i]])) {
          stop(
            "Feature set #",
            i,
            ": Training and testing sets do not contain same number of features"
          )
        }
      }
    }
    if (!is.null(y.test)) {
      for (i in seq_along(x)) {
        if (NROW(x.test[[i]]) != NROW(y.test)) {
          stop(
            "Feature set #",
            i,
            ": Testing set features and outcome do not contain same number of cases"
          )
        }
      }
    }
    # [ end for meta.list ]
  } else {
    # x is not list
    # '- Test dimensions match ----
    if (NROW(x) != NROW(y)) {
      stop(
        "Training set features and outcome do not contain same number of cases"
      )
    }
    if (!is.null(x.test)) {
      if (NCOL(x) != NCOL(x.test)) {
        stop("Training and testing set do not contain same number of features")
      }
    }
    if (!is.null(y.test)) {
      if (NROW(x.test) != NROW(y.test)) {
        stop(
          "Testing set features and outcome do not contain same number of cases"
        )
      }
    }

    # '- Column names ----
    xnames <- colnames(x)
    if (!is.null(x.test)) {
      x.test <- as.data.frame(x.test)
      colnames(x.test) <- xnames
    }
  } # //xnames and dimensions check

  # Filter y NAs ----
  if (filter.y.na) {
    if (anyNA(y)) {
      if (verbose) {
        msg2("Filtering out cases with missing values in training outcome")
      }
      exclude <- is.na(y)
      x <- x[!exclude, , drop = FALSE]
      y <- y[!exclude]
    }
  }

  # Type ----
  if (!is.null(dim(y)) && !inherits(y, "Surv")) y <- as.vector(y)
  type <- switch(
    class(y)[1],
    factor = "Classification",
    Surv = "Survival",
    "Regression"
  )

  # Upsample: balance outcome class ----
  if (type == "Classification" && upsample) {
    if (!is.null(resample.seed)) set.seed(resample.seed)
    freq <- as.data.frame(table(y))
    maxfreq.i <- which.max(freq$Freq)
    if (verbose) {
      msg2("Upsampling to create balanced set...", newline.pre = TRUE)
      msg2(
        levels(y)[maxfreq.i],
        "is majority outcome with length =",
        max(freq$Freq)
      )
    }
    y.classIndex.list <- lapply(levels(y), function(x) which(y == x))
    to.upsample <- setdiff(seq_along(levels(y)), maxfreq.i)
    y.upsampled.classIndex.list <- y.classIndex.list
    target.length <- length(y.classIndex.list[[maxfreq.i]])
    for (i in to.upsample) {
      do.replace <- length(y.classIndex.list[[i]]) * 2 <
        length(y.classIndex.list[[maxfreq.i]])
      y.upsampled.classIndex.list[[i]] <- c(
        y.classIndex.list[[i]],
        sample(
          y.classIndex.list[[i]],
          target.length - length(y.classIndex.list[[i]]),
          do.replace
        )
      )
    }
    upsample.index <- unlist(y.upsampled.classIndex.list)
    x0 <- x
    y0 <- y
    x <- x[upsample.index, , drop = FALSE]
    y <- y[upsample.index]
  } else {
    x0 <- y0 <- NULL
  }

  # Downsample: balance outcome class ----
  if (type == "Classification" && downsample) {
    if (!is.null(resample.seed)) set.seed(resample.seed)
    freq <- as.data.frame(table(y))
    minfreq.i <- which.min(freq$Freq)
    if (verbose) {
      msg2("Downsampling to balance outcome classes...", newline.pre = TRUE)
      msg2(
        levels(y)[minfreq.i],
        "is the minority outcome with",
        min(freq$Freq),
        "cases"
      )
    }
    y.classIndex.list <- lapply(levels(y), function(x) which(y == x))
    to.downsample <- setdiff(seq_along(levels(y)), minfreq.i)
    y.downsampled.classIndex.list <- y.classIndex.list
    target.length <- length(y.classIndex.list[[minfreq.i]])
    for (i in to.downsample) {
      y.downsampled.classIndex.list[[i]] <- sample(
        y.classIndex.list[[i]],
        target.length
      )
    }
    downsample.index <- unlist(y.downsampled.classIndex.list)
    x0 <- x
    y0 <- y
    x <- x[downsample.index, , drop = FALSE]
    y <- y[downsample.index]
  } else {
    x0 <- y0 <- NULL
  }

  # IFW: Inverse Frequency Weighting for Classification ----
  class.weights <- weights <- NULL
  if (type == "Classification" && ifw) {
    freq <- as.data.frame(table(y))[, 2]
    class.weights <- 1 / freq
    names(class.weights) <- levels(y)
    if (sum(diff(freq)) == 0) {
      weights <- rep(1, NROW(y))
    } else {
      if (ifw.type == 1) {
        weights <- class.weights / min(class.weights)
      } else if (ifw.type == 2) {
        weights <- class.weights / max(class.weights)
      }
      if (verbose) {
        msg2(
          "Imbalanced classes: using Inverse Frequency Weighting",
          newline.pre = TRUE
        )
      }
      weights <- weights[as.integer(y)]
    }
  }

  # Survival ----
  if (survival::is.Surv(y)) {
    type <- "Survival"
  }

  # Outro ----
  list(
    x = x,
    y = y,
    x.test = x.test,
    y.test = y.test,
    x.valid = x.valid,
    y.valid = y.valid,
    x0 = x0,
    y0 = y0,
    xnames = xnames,
    type = type,
    class.weights = class.weights,
    weights = weights
  )
} # rtemis::prepare_data
