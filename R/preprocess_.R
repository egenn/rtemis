# preprocess.R
# ::rtemis::
# 2017-21 E.D. Gennatas lambdamd.org

#' Data preprocessing
#'
#' Prepare data for analysis and visualization **in-place**
#'
#' This function (ending in "_") performs operations **in-place** and returns the
#' preprocessed data.table silenty (e.g. for piping)
#' By default, removes constant features and duplicated cases
#' (removeConstants = TRUE, removeDuplicates = TRUE), everything else must be specified.
#'
#' Order of operations (same as order of arguments in usage):
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
#  @param nonzeroFactors Logical: Shift factor values to exclude zeros. Default = FALSE
#' @param scale Logical: If TRUE, scale columns of \code{x}
#' @param center Logical: If TRUE, center columns of \code{x}
#' @param removeConstants Logical: If TRUE, remove constant columns. Default = TRUE
#' @param removeDuplicates Logical: If TRUE, remove duplicated cases. Default = FALSE
#' @param oneHot Logical: If TRUE, convert all factors using one-hot encoding
#' @param exclude Integer, vector: Exclude these columns from all preprocessing. Default = NULL
#' @param verbose Logical: If TRUE, write messages to console. Default = TRUE
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- data.table(a = sample(c(1:3), 30, T),
#' b = rnorm(30, 12),
#' c = rnorm(30, 200),
#' d = sample(c(21:22), 30, T),
#' e = rnorm(30, -100),
#' f = rnorm(30, 950),
#' g = rnorm(30),
#' h = rnorm(30))
#' ## add duplicates
#' x <- rbind(x, x[c(1, 3), ])
#' ## add constant
#' x[, z := 99]
#' preprocess_(x)
#' }

preprocess_ <- function(x, y = NULL,
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
                       # nonzeroFactors = FALSE,
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
  if (!inherits(x, "data.table")) setDT(x)

  # [ Complete cases ] ====
  if (completeCases) {
    if (anyNA(x)) {

      if (verbose) {
        nrows_start <- NROW(x)
        info("Filtering complete cases...")
      }
      x <- x[complete.cases(x), ]
      if (verbose) {
        nrows <- NROW(x)
        removed <- paste0(nrows_start - nrows, "/", nrows_start)
        msg("Removed", rtOrange$bold(removed), "rows.")
      }
    } else {
      if (verbose) msg(rtOrange$bold("No missing values"), "in dataset")
    }
  }

  # [ Set aside excluded ] ====
  if (!is.null(exclude) && length(exclude) > 0) {
    excluded <- x[, ..exclude, drop = FALSE]
    excluded.names <- colnames(x)[exclude]
    x <- x[, (exclude) := NULL]
  }

  # [ Remove duplicates ] ====
  if (removeDuplicates) {
    Ndups <- sum(duplicated(x))
    if (Ndups > 0) {
      if (verbose) msg0("Removing ", rtOrange$bold(singorplu(Ndups, "duplicated case")), "...")
      x <- unique(x)
    }
  }

  # [ Remove Cases by missing feature threshold ] ====
  if (!is.null(removeCases.thres)) {
    if (anyNA(x)) {
      # na.fraction.bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na.fraction.bycase <- data.table::transpose(x)[, lapply(.SD, function(i) sum(is.na(i))/length(i))]
      ncols <- ncol(x)
      na.fraction.bycase <- x[, sum(is.na(.SD))/ncols, by = 1:NROW(x)]
      removeCases.thres.index <- which(na.fraction.bycase[, 2] >= removeCases.thres)
      if (length(removeCases.thres.index) > 0) {
        if (verbose) msg("Removing", rtOrange$bold(length(removeCases.thres.index)), "cases with >=",
                         removeCases.thres, "missing data...")
        x <- x[-removeCases.thres.index, ]

      }
    }
  }

  # [ Remove Features by missing feature threshold ] ====
  if (!is.null(removeFeatures.thres)) {
    if (anyNA(x)) {
      nrows <- NROW(x)
      na.fraction.byfeat <- x[, lapply(.SD, function(i) sum(is.na(i))/nrows)]
      removeFeat.index <- which(na.fraction.byfeat >= removeFeatures.thres)
      if (length(removeFeat.index) > 0) {
        if (verbose) msg("Removing", rtOrange$bold(length(removeFeat.index)), "features with >=",
                         removeFeatures.thres, "missing data...")
        x[, (removeFeat.index) := NULL]
      }
    }
  }

  # [ Integer to factor ] ====
  if (integer2factor) {
    if (verbose) info("Converting integers to factors...")
    index.integer <- as.integer(which(sapply(x, is.integer)))
    if (length(index.integer) > 0) {
      for (j in index.integer) set(x, i = NULL, j, factor(x[[j]]))
      if (verbose) {
        nintegers <- length(index.integer)
        msg("Converted", rtOrange$bold(singorplu(nintegers, "integer"),
                                       "to", ngettext(nintegers, "a factor", "factors")))
      }
    } else {
      if (verbose) msg("No integers found")
    }
  }

  # [ Integer to numeric ] ====
  if (integer2numeric) {
    if (verbose) info("Converting integers to numeric")
    index.integer <- as.integer(which(sapply(x, is.integer)))
    if (length(index.integer) > 0) {
      for (j in index.integer) set(x, i = NULL, j, as.numeric(x[[j]]))
      if (verbose) {
        nintegers <- length(index.integer)
        msg("Converted", rtOrange$bold(singorplu(nintegers, "integer"),
                                       "to numeric"))
      }
    } else {
      if (verbose) msg("No integers found")
    }
  }

  # [ Logical to factor ] ====
  if (logical2factor) {
    if (verbose) info("Converting logical to factor")
    index.logical <- as.integer(which(sapply(x, is.logical)))
    if (length(index.logical) > 0) {
      for (j in index.logical) set(x, i = NULL, j, factor(x[[j]]))
      if (verbose) {
        nlogical <- length(index.logical)
        msg("Converted", rtOrange$bold(nlogical, ngettext(nlogical, "column", "columns"),
                                       "to", ngettext(nlogical, "a factor", "factors")))
      }
    } else {
      if (verbose) msg("No logical columns found")
    }
  }

  # [ Logical to numeric ] ====
  if (logical2numeric) {
    if (verbose) info("Converting logical to numeric")
    index.logical <- as.integer(which(sapply(x, is.logical)))
    if (length(index.logical) > 0) {
      for (j in index.logical) set(x, i = NULL, j, as.numeric(x[[j]]))
      if (verbose) {
        nlogical <- length(index.logical)
        msg("Converted", rtOrange$bold(nlogical, ngettext(nlogical, "column", "columns"),
                                       "to numeric"))
      }
    } else {
      if (verbose) msg("No logical columns found")
    }
  }

  # [ Numeric to factor ] ====
  if (numeric2factor) {
    if (verbose) info("Converting numeric to factor")
    index.numeric <- as.integer(which(sapply(x, is.numeric)))
    if (length(index.numeric) > 0) {
      for (j in index.numeric) set(x, i = NULL, j, factor(x[[j]]))
      if (verbose) {
        nnumeric <- length(index.numeric)
        msg("Converted", rtOrange$bold(nnumeric, ngettext(nnumeric, "column", "columns"),
                                       "to", ngettext(nnumeric, "a factor", "factors")))
      }
    } else {
      if (verbose) msg("No numeric columns found")
    }
  }

  # [ ltn2factor ] ====
  if (ltn2factor > 2) {
    index.numeric <- which(sapply(x, is.numeric))
    if (length(index.numeric) > 0) {
      index.numeric.ltn <- as.integer(which(x[, sapply(.SD, function(i) is.numeric(i) && length(unique(i)) < ltn2factor)]))
      for (j in index.numeric.ltn) set(x, i = NULL, j, as.factor(x[[j]]))
      if (verbose) {
        nnumltn <- length(index.numeric.ltn)
        msg("Converted", rtOrange$bold(nnumltn, ngettext(nnumltn, "column", "columns"),
                                       "to", ngettext(nnumltn, "a factor", "factors")))
      }
    }
  }

  # [ Character to factor ] ====
  if (character2factor) {
    if (verbose) info("Converting character to factor")
    index.char <- as.integer(which(sapply(x, is.character)))
    if (length(index.char) > 0) {
      for (j in index.char) set(x, i = NULL, j, factor(x[[j]]))
      if (verbose) {
        nchar <- length(index.char)
        msg("Converted", rtOrange$bold(nchar, ngettext(nchar, "column", "columns"),
                                       "to", ngettext(nchar, "a factor", "factors")))
      }
    } else {
      if (verbose) msg("No character columns found")
    }
  }

  # [ factor NA to level ] ====
  if (factorNA2missing) {
    index.factor <- which(sapply(x, is.factor))
    index.factorna <- as.integer(which(x[, sapply(.SD, function(i) is.factor(i) && anyNA(i))]))
    # for (i in index.factor) x[, i] <- factor_NA2missing(x[, i], factorNA2missing.level)
    nfactorna <- length(index.factorna)
    if (nfactorna > 0) {
      if (verbose) info(paste0('Converting NA in factors to level "', factorNA2missing.level, '"...'))
      for (j in index.factorna) set(x, i = NULL, j, factor_NA2missing(x[[j]], factorNA2missing.level))
      if (verbose) {
        msg("Converted", rtOrange$bold(nfactorna, ngettext(nfactorna, "factor's", "factors'"),
                                       "NA level to", paste0('"', factorNA2missing.level, '"')))
      }
    } else {
      if (verbose) msg("No factors with NA values found")
    }
  }

  # [ Nonzero factors ] ====
  # if (nonzeroFactors) {
  #   if (verbose) msg("Shifting factor levels to exclude 0...")
  #   if (any(sapply(x, is.factor))) {
  #     for (i in seq(NCOL(x))) {
  #       if (is.factor(x[, i])) {
  #         while (any(x[, i] == 0)) {
  #           x[, i] <- factor(as.numeric(as.character(x[, i])) + 1)
  #         }
  #       }
  #     }
  #   }
  # }

  # [ Missingness ] ====
  if (missingness) {
    cols.with.na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    ncolsna <- length(cols.with.na)
    if (ncolsna > 0) {
      for (i in cols.with.na) {
        x[, (paste0(.colnames[i], "_missing")) := factor(as.numeric(is.na(x[[i]])))]
        if (verbose) msg("Created missingness indicator for",
                         rtOrange$bold(ncolsna), ngettext(ncolsna, "column", "columns"))
      }
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
      setDT(x)
    } else {
      # '- mean/mode ----
      if (verbose) info(paste0("Imputing missing values using ", deparse(substitute(impute.numeric)),
                        " and ", deparse(substitute(impute.discrete)), "..."))

      discrete.index <- which(sapply(x, function(i) is.discrete(i) && anyNA(i)))
      if (length(discrete.index) > 0) {
        for (j in discrete.index) {
          x[is.na(x[[j]]), (names(x)[j]) := impute.discrete(x[[j]])]
        }
      }

      numeric.index <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
      if (length(numeric.index) > 0) {
        for (j in numeric.index) {
          x[is.na(x[[j]]), (names(x)[j]) := impute.numeric(x[[j]], na.rm = TRUE)]
        }
      }
    }
  }

  # [ Scale +/- center ] ====
  if (scale | center) {
    # Get index of numeric features
    numeric_index <- which(sapply(x, is.numeric))
    if (length(numeric_index) > 0) {
      if (verbose) {
        sc <- if (scale) "Scaling" else NULL
        ce <- if (center) "Centering" else NULL
        info(paste(c(sc, ce), collapse = " and "),
                        length(numeric_index), "numeric features...")
      }
      for (j in numeric_index) set(x, i = NULL, j, scale(x[[j]], scale = scale, center = center))
    } else {
      if (verbose) msg("No numeric columns present")
    }
  }

  # [ Remove constants ] ====
  if (removeConstants) {
    constant <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
    if (length(constant) > 0) {
      if (verbose) info("Removing constant features...")
      x[, (names(x)[constant]) := NULL]
    }
  }

  # [ One Hot Encoding ] ====
  if (oneHot) oneHot(x, verbose = verbose)

  # [ Add back excluded ] ====
  if (!is.null(exclude) && length(exclude) > 0) {
    if (!is.null(removeCases.thres) && length(removeCases.thres.index) > 0) {
      excluded <- excluded[-removeCases.thres.index, ]
    }
    x[, (excluded.names) := excluded]
  }

  if (verbose) msg("Done")
  invisible(x)

} # rtemis::preprocess
