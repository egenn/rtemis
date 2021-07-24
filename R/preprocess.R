# preprocess.R
# ::rtemis::
# 2017-9 E.D. Gennatas lambdamd.org

#' Data preprocessing
#'
#' Prepare data for analysis and visualization
#'
#' By default, removes constant features and duplicated cases
#' (removeConstants = TRUE, removeDuplicates = TRUE), everything else must be specified.
#'
#' Order of operations (reflected by order of arguments in usage):
#'
#'   * keep complete cases only
#'   * remove duplicates
#'   * remove cases by missingness threshold
#'   * remove features by missingness threshold
#'   * integer to factor
#'   * integer to numeric
#'   * logical to factor
#'   * logical to numeric
#'   * numeric to factor
#'   * numeric with less than N unique values to factor
#'   * character to factor
#'   * factor NA to named level
#'   * add missingness column
#'   * impute
#'   * scale and/or center
#'   * remove constants
#'   * one-hot encoding
#'
#' @md
#' @param x Input data frame
#' @param completeCases Logical: If TRUE, only retain complete cases (no missing data).
#' Default = FALSE
#' @param removeCases.thres Float (0, 1): Remove cases with >= to this fraction of missing features.
#' Default = NULL
#' @param removeFeatures.thres Float (0, 1): Remove features with missing values in >= to this fraction of
#' cases. Default = NULL
#' @param missingness Logical: If TRUE, generate new boolean columns for each feature with missing values, indicating
#' which cases were missing data. Default = FALSE
#' @param impute Logical: If TRUE, impute missing cases. See \code{impute.discrete} and
#' \code{impute.numeric} for how
#' @param impute.type Character: How to impute data: "missRanger" and "missForest" use the packages of the same name to
#' impute by iterative random forest regression. "rfImpute" uses \code{randomForest::rfImpute} (see its documentation),
#' "meanMode" will use mean and mode by default or any custom function defined in \code{impute.discrete} and
#' \code{impute.numeric}. Default = "missRanger" (which is much faster than "missForest").
#' "missForest" is included for compatibility with older pipelines.
#' @param impute.missRanger.params Named list with elements "pmm.k" and "maxiter", which are passed to
#' \code{missRanger::missRanger}. \code{pmm.k} greater than 0 results in predictive mean matching.
#' Default \code{pmm.k = 3} \code{maxiter = 10} \code{num.trees = 500}. Reduce \code{num.trees} for
#' faster imputation especially in large datasets. Set \code{pmm.k = 0} to disable predictive mean
#' matching
#  @param impute.missForest.params Named list with elements "maxiter", "ntree", and "parallelize",  which are passed
#' to \code{missForest::missForest}
# @param impute.rfImpute.params Names list with elements "niter", "ntree" for \code{randomForest::rfImpute}
#' @param impute.discrete Function that returns single value: How to impute discrete variables for
#' \code{impute.type = "meanMode"}. Default = \link{getMode}
#' @param impute.numeric Function that returns single value: How to impute continuous variables for
#' \code{impute.type = "meanMode"}.
#' Default = \code{mean}
#' @param integer2factor Logical: If TRUE, convert all integers to factors
#' @param integer2numeric Logical: If TRUE, convert all integers to numeric (will only work
#' if \code{integer2factor = FALSE})
#' @param logical2factor Logical: If TRUE, convert all logical variables to factors
#' @param logical2numeric Logical: If TRUE, convert all logical variables to numeric
#' @param numeric2factor Logical: If TRUE, convert all numeric variables to factors
#' @param ltn2factor Integer (>2): Convert all numeric variables with less than this number of unique
#' values to factors. Default = NULL. For example, if binary variables are encoded with 1, 2,
#' you could use `ltn2factor = 3` to convert them to factors.
#' @param character2factor Logical: If TRUE, convert all character variables to factors
#' @param factorNA2missing Logical: If TRUE, make NA values in factors be of level
#' \code{factorNA2missing.level}. In many cases this is the preferred way to handle missing data in
#' categorical variables. Note that since this step is performed before imputation, you can use this
#' option to handle missing data in categorical variables and impute numeric variables in the same
#' \code{preprocess} call.
#' @param factorNA2missing.level Character: Name of level if \code{factorNA2missing = TRUE}.
#' Default = "missing"
#' @param nonzeroFactors Logical: Shift factor values to exclude zeros. Default = FALSE
#' @param scale Logical: If TRUE, scale columns of \code{x}
#' @param center Logical: If TRUE, center columns of \code{x}
#' @param removeConstants Logical: If TRUE, remove constant columns. Default = TRUE
#' @param removeDuplicates Logical: If TRUE, remove duplicated cases. Default = FALSE
#' @param oneHot Logical: If TRUE, convert all factors using one-hot encoding
#' @param exclude Integer, vector: Exclude these columns from all preprocessing. Default = NULL
#' @param verbose Logical: If TRUE, write messages to console. Default = TRUE
#' @author E.D. Gennatas
#' @export

preprocess <- function(x, y = NULL,
                       completeCases = FALSE,
                       removeCases.thres = NULL,
                       removeFeatures.thres = NULL,
                       missingness = FALSE,
                       impute = FALSE,
                       impute.type = c("missRanger",
                                       "micePMM",
                                       "meanMode"),
                       impute.missRanger.params = list(pmm.k = 3,
                                                       maxiter = 10,
                                                       num.trees = 500),
                       impute.discrete = getMode,
                       impute.numeric = mean,
                       integer2factor = FALSE,
                       integer2numeric = FALSE,
                       logical2factor = FALSE,
                       logical2numeric = FALSE,
                       numeric2factor = FALSE,
                       numeric2factor.levels = NULL,
                       ltn2factor = 0,
                       character2factor = FALSE,
                       factorNA2missing = FALSE,
                       factorNA2missing.level = "missing",
                       nonzeroFactors = FALSE,
                       scale = FALSE,
                       center = FALSE,
                       removeConstants = TRUE,
                       removeDuplicates = FALSE,
                       factorizeThresh = 0,
                       oneHot = FALSE,
                       exclude = NULL,
                       verbose = TRUE,
                       parallel.type = ifelse(.Platform$OS.type == "unix", "fork", "psock")) {

  # Arguments ====
  impute.type <- match.arg(impute.type)

  isdatatable <- data.table::is.data.table(x)
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

  # [ Remove duplicates ] ====
  if (removeDuplicates) {
    Ndups <- sum(duplicated(x))
    if (Ndups > 0) {
      if (verbose) msg0("Removing ", singorplu(Ndups, "duplicated case"), "...")
      x <- unique(x)
    }
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
    if (verbose) msg("Converting integers to numeric...")
    for (i in index.integer) x[, i] <- as.numeric(x[, i])
  }

  # [ Logical to factor ] ====
  if (logical2factor) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) msg("Converting logicals to factors...")
    for (i in index.logical) x[, i] <- as.factor(x[, i])
  }

  # [ Logical to numeric ] ====
  if (logical2numeric) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) msg("Converting logicals to numeric...")
    for (i in index.logical) x[, i] <- as.numeric(x[, i])
  }

  # [ Numeric to factor ] ====
  if (numeric2factor) {
    index.numeric <- which(sapply(x, is.numeric))
    if (verbose) msg("Converting numeric to factors...")
    if (is.null(numeric2factor.levels)) {
      for (i in index.numeric) x[, i] <- as.factor(x[, i])
    } else {
      for (i in index.numeric) x[, i] <- factor(x[, i], levels = numeric2factor.levels)
    }
  }

  # [ ltn2factor ] ====
  if (ltn2factor > 2) {
    index.numeric <- which(sapply(x, is.numeric))
    index.numeric.ltn <- which(sapply(x[, index.numeric, drop = FALSE], function(i) length(unique(na.exclude(i))) < ltn2factor))
    for (i in index.numeric.ltn) x[, index.numeric][, i] <- factor(x[, index.numeric][, i])
  }

  # [ Character to factor ] ====
  if (character2factor) {
    index.char <- which(sapply(x, is.character))
    if (verbose) msg("Converting characters to factors...")
    for (i in index.char) x[, i] <- as.factor(x[, i])
  }

  # [ factor NA to level ] ====
  if (factorNA2missing) {
    index.factor <- which(sapply(x, is.factor))
    if (verbose) msg0('Converting NA in factors to level "', factorNA2missing.level, '"...')
    for (i in index.factor) x[, i] <- factor_NA2missing(x[, i], factorNA2missing.level)
  }

  # [ Nonzero factors ] ====
  if (nonzeroFactors) {
    if (verbose) msg("Shifting factor levels to exclude 0...")
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

  # [ Missingness ] ====
  if (missingness) {
    cols.with.na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    for (i in cols.with.na) {
      x[, paste0(.colnames[i], "_missing")] <- as.numeric(is.na(x[, i]))
      if (verbose) msg0("Created missingness indicator for ", .colnames[i], "...")
    }
  }

  # [ Impute ] ====
  if (impute) {
    if (impute.type == "missRanger") {
      # '- missRanger ====
      if (!depCheck("missRanger", verbose = FALSE)) {
        cat("\n"); stop("Please install dependencies and try again")
      }
      if (verbose) {
        if (impute.missRanger.params$pmm.k > 0) {
          msg("Imputing missing values using predictive mean matching with missRanger...")
        } else {
          msg("Imputing missing values using missRanger...")
        }
      }
      x <- missRanger::missRanger(x, pmm.k = impute.missRanger.params$pmm.k,
                                  verbose = ifelse(verbose, 1, 0))
    } else if (impute.type == "micePMM") {
      if (!depCheck("mice", verbose = FALSE)) {
        cat("\n"); stop("Please install dependencies and try again")
      }
      if (verbose) msg("Imputing missing values by predictive mean matching using mice...")
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
    # } else if (impute.type == "missForest") {
    #   # '- missForest ----
    #   if (!depCheck("missForest", verbose = FALSE)) {
    #     cat("\n"); stop("Please install dependencies and try again")
    #   }
    #   if (verbose) msg("Imputing missing values using missForest...")
    #   x <- missForest::missForest(x,
    #                               maxiter = impute.missForest.params$maxiter,
    #                               ntree = impute.missForest.params$ntree,
    #                               parallelize = impute.missForest.params$parallelize)$ximp
    #
    # } else if (impute.type == "rfImpute") {
    #   # '- rfImpute ----
    #   if (!depCheck("randomForest", verbose = FALSE)) {
    #     cat("\n"); stop("Please install dependencies and try again")
    #   }
    #   if (is.null(y)) stop("Please provide outcome 'y' for imputation using proximity from randomForest or use
    #                        missForest instead")
    #   x <- randomForest::rfImpute(x, y,
    #                               iter = impute.rfImpute.params$niter,
    #                               ntree = impute.rfImpute.params$ntree)
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
    # Get index of numeric features
    numeric_index <- which(sapply(x, is.numeric))
    sc <- if (scale) "Scaling" else NULL
    ce <- if (center) "Centering" else NULL
    if (length(numeric_index) > 0) {
      if (verbose) msg(paste(c(sc, ce), collapse = " and "),
                       length(numeric_index), "numeric features...")
      x_num_scaled <- as.data.frame(scale(x[, numeric_index], scale = scale, center = center))
      # insert into original dataset
      j <- 0
      for (i in numeric_index) {
        j <- j + 1
        x[, i] <- x_num_scaled[, j]
      }
    } else {
      msg(paste(c(sc, ce), collapse = " and "),
          "was requested \n                                but no numeric features were found: Please check data.")
    }

  }

  # [ Remove constants ] ====
  if (removeConstants) {
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

  if (isdatatable) data.table::setDT(x)
  if (verbose) msg("Done")
  x

} # rtemis::preprocess
