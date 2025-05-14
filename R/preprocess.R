# preprocess.R
# ::rtemis::
# 2017-23 E.D. Gennatas rtemis.org
# todo: merge with preprocess_ as single data.table-based preprocess

#' Data preprocessing
#'
#' Prepare data for analysis and visualization
#'
#' Order of operations (reflected by order of arguments in usage):
#'
#'   * keep complete cases only
#'   * remove constants
#'   * remove duplicates
#'   * remove cases by missingness threshold
#'   * remove features by missingness threshold
#'   * integer to factor
#'   * integer to numeric
#'   * logical to factor
#'   * logical to numeric
#'   * numeric to factor
#'   * cut numeric to n bins
#'   * cut numeric to n quantiles
#'   * numeric with less than N unique values to factor
#'   * character to factor
#'   * factor NA to named level
#'   * add missingness column
#'   * impute
#'   * scale and/or center
#'   * one-hot encoding
#'
#' @md
#' @param x data.frame to be preprocessed
#' @param completeCases Logical: If TRUE, only retain complete cases (no missing data).
#' Default = FALSE
#' @param removeCases.thres Float (0, 1): Remove cases with >= to this fraction
#' of missing features.
#' @param removeFeatures.thres Float (0, 1): Remove features with missing
#' values in >= to this fraction of cases.
#' @param missingness Logical: If TRUE, generate new boolean columns for each
#' feature with missing values, indicating which cases were missing data.
#' @param impute Logical: If TRUE, impute missing cases. See `impute.discrete` and
#' `impute.numeric` for how
#' @param impute.type Character: How to impute data: "missRanger" and
#' "missForest" use the packages of the same name to impute by iterative random
#' forest regression. "rfImpute" uses `randomForest::rfImpute` (see its
#' documentation), "meanMode" will use mean and mode by default or any custom
#' function defined in `impute.discrete` and `impute.numeric`.
#' Default = "missRanger" (which is much faster than "missForest").
#' "missForest" is included for compatibility with older pipelines.
#' @param impute.missRanger.params Named list with elements "pmm.k" and
#' "maxiter", which are passed to `missRanger::missRanger`. `pmm.k`
#' greater than 0 results in predictive mean matching. Default `pmm.k = 3`
#' `maxiter = 10` `num.trees = 500`. Reduce `num.trees` for
#' faster imputation especially in large datasets. Set `pmm.k = 0` to
#' disable predictive mean matching
#  @param impute.missForest.params Named list with elements "maxiter", "ntree", and "parallelize",  which are passed
#' to `missForest::missForest`
# @param impute.rfImpute.params Names list with elements "niter", "ntree" for \code{randomForest::rfImpute}
#' @param impute.discrete Function that returns single value: How to impute
#' discrete variables for `impute.type = "meanMode"`.
#' Default = [get_mode]
#' @param impute.numeric Function that returns single value: How to impute
#' continuous variables for `impute.type = "meanMode"`.
#' Default = `mean`
#' @param integer2factor Logical: If TRUE, convert all integers to factors. This includes
#' `bit64::integer64` columns
#' @param integer2numeric Logical: If TRUE, convert all integers to numeric
#' (will only work if `integer2factor = FALSE`) This includes
#' `bit64::integer64` columns
#' @param logical2factor Logical: If TRUE, convert all logical variables to
#' factors
#' @param logical2numeric Logical: If TRUE, convert all logical variables to
#' numeric
#' @param numeric2factor Logical: If TRUE, convert all numeric variables to
#' factors
#' @param numeric2factor.levels Character vector: Optional - will be passed to
#' `levels` arg of `factor()` if `numeric2factor = TRUE` (For advanced/
#' specific use cases; need to know unique values of numeric vector(s) and given all
#' numeric vars have same unique values)
#' @param numeric.cut.n Integer: If > 0, convert all numeric variables to factors by
#' binning using `base::cut` with `breaks` equal to this number
#' @param numeric.cut.labels Logical: The `labels` argument of [base::cut]
#' @param numeric.quant.n Integer: If > 0, convert all numeric variables to factors by
#' binning using `base::cut` with `breaks` equal to this number of quantiles
#' produced using `stats::quantile`
#' @param numeric.quant.NAonly Logical: If TRUE, only bin numeric variables with
#' missing values
#' @param len2factor Integer (>=2): Convert all variables with less
#' than or equal to this number of unique values to factors. Default = NULL.
#' For example, if binary variables are encoded with 1, 2, you could use
#' `len2factor = 2` to convert them to factors.
#' @param character2factor Logical: If TRUE, convert all character variables to
#' factors
#' @param factorNA2missing Logical: If TRUE, make NA values in factors be of
#' level `factorNA2missing.level`. In many cases this is the preferred way
#' to handle missing data in categorical variables. Note that since this step
#' is performed before imputation, you can use this option to handle missing
#' data in categorical variables and impute numeric variables in the same
#' `preprocess` call.
#' @param factorNA2missing.level Character: Name of level if
#' `factorNA2missing = TRUE`. Default = "missing"
#' @param factor2integer Logical: If TRUE, convert all factors to integers
#' @param factor2integer_startat0 Logical: If TRUE, start integer coding at 0
#' @param scale Logical: If TRUE, scale columns of `x`
#' @param center Logical: If TRUE, center columns of `x`. Note that by
#' default it is the same as `scale`
#' @param removeConstants Logical: If TRUE, remove constant columns.
#' @param removeConstants.skipMissing Logical: If TRUE, skip missing values, before
#' checking if feature is constant
#' @param removeDuplicates Logical: If TRUE, remove duplicate cases.
#' @param oneHot Logical: If TRUE, convert all factors using one-hot encoding.
#' @param add_date_features Logical: If TRUE, extract date features from date columns.
#' @param date_features Character vector: Features to extract from dates.
#' @param add_holidays Logical: If TRUE, extract holidays from date columns.
#' @param exclude Integer, vector: Exclude these columns from preprocessing.
#' @param xname Character: Name of `x` for messages.
#' @param verbose Logical: If TRUE, write messages to console.
#'
#' @author E.D. Gennatas
#' @export

preprocess <- function(
  x,
  completeCases = FALSE,
  removeCases.thres = NULL,
  removeFeatures.thres = NULL,
  missingness = FALSE,
  impute = FALSE,
  impute.type = c(
    "missRanger",
    "micePMM",
    "meanMode"
  ),
  impute.missRanger.params = list(
    pmm.k = 3,
    maxiter = 10,
    num.trees = 500
  ),
  impute.discrete = get_mode,
  impute.numeric = mean,
  integer2factor = FALSE,
  integer2numeric = FALSE,
  logical2factor = FALSE,
  logical2numeric = FALSE,
  numeric2factor = FALSE,
  numeric2factor.levels = NULL,
  numeric.cut.n = 0,
  numeric.cut.labels = FALSE,
  numeric.quant.n = 0,
  numeric.quant.NAonly = FALSE,
  len2factor = 0,
  character2factor = FALSE,
  factorNA2missing = FALSE,
  factorNA2missing.level = "missing",
  #    nonzeroFactors = FALSE,
  factor2integer = FALSE,
  factor2integer_startat0 = TRUE,
  scale = FALSE,
  center = scale,
  removeConstants = FALSE,
  removeConstants.skipMissing = TRUE,
  removeDuplicates = FALSE,
  oneHot = FALSE,
  #    cleanfactorlevels = FALSE,
  add_date_features = FALSE,
  date_features = c("weekday", "month", "year"),
  add_holidays = FALSE,
  exclude = NULL,
  xname = NULL,
  verbose = TRUE
) {
  # Intro ----
  xname <- deparse(substitute(x))
  start_time <- intro(
    # paste0("Preprocessing ", hilite(xname), "..."),
    verbose = verbose
  )

  # Arguments ----
  impute.type <- match.arg(impute.type)

  isdatatable <- data.table::is.data.table(x)
  x <- as.data.frame(x)

  # Complete cases ----
  if (completeCases) {
    if (verbose) msg2("Filtering complete cases...")
    x <- x[complete.cases(x), ]
  }

  # Set aside excluded ----
  if (!is.null(exclude) && length(exclude) > 0) {
    excluded <- x[, exclude, drop = FALSE]
    excluded.names <- colnames(x)[exclude]
    x <- x[, -exclude, drop = FALSE]
  }

  # Remove constants ----
  # Must be ahead of numeric quantile at least
  if (removeConstants) {
    constant <- which(sapply(
      x,
      is_constant,
      skip_missing = removeConstants.skipMissing
    ))
    if (length(constant) > 0) {
      if (verbose) {
        msg20(
          "Removing ",
          singorplu(length(constant), "constant feature"),
          "..."
        )
      }
      x <- x[, -constant]
    }
  }

  # Remove duplicates ----
  if (removeDuplicates) {
    # Ndups <- sum(duplicated(x))
    duplicate.index <- which(duplicated(x))
    Ndups <- length(duplicate.index)
    if (Ndups > 0) {
      if (verbose) msg20("Removing ", singorplu(Ndups, "duplicate case"), "...")
      x <- unique(x)
    }
  } else {
    duplicate.index <- NULL
  }

  # Remove Cases by missing feature threshold ----
  if (!is.null(removeCases.thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      # na.fraction.bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na.fraction.bycase <- data.table::transpose(xt)[, lapply(
        .SD,
        function(i) {
          sum(is.na(i)) / length(i)
        }
      )]
      removeCases.thres.index <- which(na.fraction.bycase >= removeCases.thres)
      if (length(removeCases.thres.index) > 0) {
        if (verbose) {
          msg2(
            "Removing",
            length(removeCases.thres.index),
            "cases with >=",
            removeCases.thres,
            "missing data..."
          )
        }
        xt <- xt[-removeCases.thres.index, ]
      }
      x <- as.data.frame(xt)
    }
  }

  # Remove Features by missing feature threshold ----
  if (!is.null(removeFeatures.thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      na.fraction.byfeat <- xt[, lapply(.SD, function(i) {
        sum(is.na(i)) / length(i)
      })]
      removeFeat.thres.index <- which(
        na.fraction.byfeat >= removeFeatures.thres
      )
      if (length(removeFeat.thres.index) > 0) {
        if (verbose) {
          msg2(
            "Removing",
            length(removeFeat.thres.index),
            "features with >=",
            removeFeatures.thres,
            "missing data..."
          )
        }
        x <- x[, -removeFeat.thres.index]
      }
    }
  }

  # Integer to factor ----
  index.integer <- NULL
  if (integer2factor) {
    index.integer <- c(
      which(sapply(x, is.integer)),
      which(sapply(x, bit64::is.integer64))
    )
    if (verbose) {
      if (length(index.integer) > 0) {
        msg2(
          "Converting",
          singorplu(length(index.integer), "integer"),
          "to factor..."
        )
      } else {
        msg2("No integers to convert to factor...")
      }
    }
    for (i in index.integer) x[, i] <- as.factor(x[, i])
  }

  # Logical to factor ----
  if (logical2factor) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) {
      if (length(index.logical) > 0) {
        msg20(
          "Converting ",
          singorplu(length(index.logical), "logical feature"),
          " to ",
          ngettext(length(index.logical), "factor", "factors"),
          "..."
        )
      } else {
        msg2("No logicals to convert to factor...")
      }
    }
    for (i in index.logical) x[, i] <- as.factor(x[, i])
  }

  # Numeric to factor ----
  if (numeric2factor) {
    index_numeric <- which(sapply(x, is.numeric))
    if (verbose) msg2("Converting numeric to factors...")
    if (is.null(numeric2factor.levels)) {
      for (i in index_numeric) x[, i] <- as.factor(x[, i])
    } else {
      for (i in index_numeric)
        x[, i] <- factor(x[, i], levels = numeric2factor.levels)
    }
  }

  # Character to factor ----
  if (character2factor) {
    index.char <- which(sapply(x, is.character))
    if (verbose) {
      if (length(index.char) > 0) {
        msg20(
          "Converting ",
          singorplu(length(index.char), "character feature"),
          " to ",
          ngettext(length(index.char), "a factor", "factors"),
          "..."
        )
      } else {
        msg2("No character features to convert to factors found.")
      }
    }
    for (i in index.char) x[, i] <- as.factor(x[, i])
  }

  # len2factor ----
  if (len2factor > 1) {
    index.len <- which(sapply(x, \(i) length(unique(i)) <= len2factor))
    # Exclude factors
    index.factor <- which(sapply(x, is.factor))
    index.len <- setdiff(index.len, index.factor)
    if (verbose) {
      if (length(index.len) > 0) {
        msg2(
          "Converting",
          singorplu(length(index.len), "feature"),
          "with <=",
          len2factor,
          "unique values to factors..."
        )
      } else {
        msg2("No features with <=", len2factor, "unique values found.")
      }
    }
    for (i in index.len) x[, i] <- factor(x[, i])
  }

  # Integer to numeric ----
  if (integer2numeric) {
    if (is.null(index.integer)) {
      index.integer <- c(
        which(sapply(x, is.integer)),
        which(sapply(x, bit64::is.integer64))
      )
    }
    if (verbose) {
      if (length(index.integer) > 0) {
        msg2(
          "Converting",
          singorplu(length(index.integer), "integer"),
          "to numeric..."
        )
      } else {
        msg2("No integers to convert to numeric...")
      }
    }
    for (i in index.integer) x[, i] <- as.numeric(x[, i])
  }

  # Logical to numeric ----
  if (logical2numeric) {
    index.logical <- which(sapply(x, is.logical))
    if (verbose) msg2("Converting logicals to numeric...")
    for (i in index.logical) x[, i] <- as.numeric(x[, i])
  }

  # Numeric cut ----
  if (numeric.cut.n > 0) {
    index_numeric <- which(sapply(x, is.numeric))
    if (length(index_numeric) > 0) {
      if (verbose) msg2("Cutting numeric features in", numeric.cut.n, "bins...")
      for (i in index_numeric) {
        x[, i] <- factor(
          cut(x[, i], breaks = numeric.cut.n, labels = numeric.cut.labels)
        )
      }
    }
  }

  # Numeric quantile ----
  if (numeric.quant.n > 0) {
    index_numeric2q <- if (numeric.quant.NAonly) {
      index_numeric2q <- which(sapply(x, is.numeric) & sapply(x, anyNA))
    } else {
      which(sapply(x, is.numeric))
    }
    if (length(index_numeric2q) > 0) {
      if (verbose)
        msg2("Cutting numeric features in", numeric.quant.n, "quantiles...")
      for (i in index_numeric2q) {
        rng <- abs(diff(range(x[, i], na.rm = TRUE)))
        quantiles <- quantile(
          x[, i],
          probs = seq(0, 1, length.out = numeric.quant.n),
          na.rm = TRUE
        )
        quantiles[1] <- quantiles[1] - .02 * rng
        quantiles[numeric.quant.n] <- quantiles[numeric.quant.n] + .02 * rng
        quantiles <- unique(quantiles)
        x[, i] <- factor(
          cut(
            x[, i],
            breaks = quantiles
          )
        )
      }
    }
  }

  # factor NA to level ----
  if (factorNA2missing) {
    index.factor <- which(sapply(x, is.factor))
    if (verbose) {
      if (length(index.factor) > 0) {
        msg20(
          "Converting ",
          length(index.factor),
          ngettext(length(index.factor), " factor's", " factors'"),
          " NA values to level '",
          factorNA2missing.level,
          "'..."
        )
      } else {
        msg2("No factors found.")
      }
    }
    for (i in index.factor)
      x[, i] <- factor_NA2missing(x[, i], factorNA2missing.level)
  }

  # Factor to integer ----
  # e.g. for algorithms that do not support factors directly, but can handle integers
  # as categorical (e.g. LightGBM)
  if (factor2integer) {
    index.factor <- which(sapply(x, is.factor))
    if (verbose) {
      if (length(index.factor) > 0) {
        msg2(
          "Converting",
          singorplu(length(index.factor), "factor"),
          "to integer..."
        )
      } else {
        msg2("No factors found to convert to integer...")
      }
    }
    if (factor2integer_startat0) {
      for (i in index.factor) x[, i] <- as.integer(x[, i]) - 1
    } else {
      for (i in index.factor) x[, i] <- as.integer(x[, i])
    }
  }

  # Missingness ----
  if (missingness) {
    cols.with.na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    for (i in cols.with.na) {
      x[, paste0(.colnames[i], "_missing")] <- factor(as.numeric(is.na(x[, i])))
      if (verbose)
        msg20("Created missingness indicator for ", .colnames[i], "...")
    }
  }

  # Impute ----
  if (impute) {
    if (impute.type == "missRanger") {
      # '- missRanger ----
      dependency_check("missRanger")
      if (verbose) {
        if (impute.missRanger.params$pmm.k > 0) {
          msg2(
            "Imputing missing values using predictive mean matching with missRanger..."
          )
        } else {
          msg2("Imputing missing values using missRanger...")
        }
      }
      x <- missRanger::missRanger(
        x,
        pmm.k = impute.missRanger.params$pmm.k,
        verbose = ifelse(verbose, 1, 0)
      )
    } else if (impute.type == "micePMM") {
      dependency_check("mice")
      if (verbose)
        msg2(
          "Imputing missing values by predictive mean matching using mice..."
        )
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
    } else {
      # '- mean/mode ----
      if (verbose) {
        msg20(
          "Imputing missing values using ",
          deparse(substitute(impute.numeric)),
          " and ",
          deparse(substitute(impute.discrete)),
          "..."
        )
      }

      discrete.index <- which(sapply(x, function(i) is_discrete(i) && anyNA(i)))
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

  # Scale +/- center ----
  if (scale || center) {
    # Get index of numeric features
    numeric_index <- which(sapply(x, is.numeric))
    sc <- if (scale) "Scaling" else NULL
    ce <- if (center) "Centering" else NULL
    if (length(numeric_index) > 0) {
      if (verbose) {
        msg2(
          paste(c(sc, ce), collapse = " and "),
          length(numeric_index),
          "numeric features..."
        )
      }
      x_num_scaled <- as.data.frame(scale(
        x[, numeric_index],
        scale = scale,
        center = center
      ))
      # insert into original dataset
      j <- 0
      for (i in numeric_index) {
        j <- j + 1
        x[, i] <- x_num_scaled[, j]
      }
    } else {
      msg2(
        paste(c(sc, ce), collapse = " and "),
        "was requested \n                                but no numeric features were found: Please check data."
      )
    }
  }

  # # Remove constants ----
  # if (removeConstants) {
  #     # constant <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
  #     constant <- which(sapply(x, is_constant, skip_missing = removeConstants.skipMissing))
  #     if (length(constant) > 0) {
  #         if (verbose) msg2("Removing constant features...")
  #         x <- x[, -constant]
  #     }
  # }

  # One Hot Encoding ----
  if (oneHot) x <- oneHot(x, verbose = verbose)

  # Add date features ----
  if (add_date_features) {
    if (verbose) msg2("Extracting date features...")
    # Find date columns
    date_cols <- which(sapply(x, function(col) inherits(col, "Date")))
    # For each date column, extract features
    for (i in date_cols) {
      .date_features <- dates2features(x[[i]], features = date_features)
      names(.date_features) <- paste0(names(x)[i], "_", names(.date_features))
      x <- cbind(x, .date_features)
    }
  }

  # Add holidays ----
  if (add_holidays) {
    if (verbose) msg2("Extracting holidays...")
    # Find date columns
    date_cols <- which(sapply(x, \(col) inherits(col, "Date")))
    # For each date column, extract holidays
    for (i in date_cols) {
      .holidays <- get_holidays(x[, i])
      x[[paste0(names(x)[i], "_holidays")]] <- .holidays
    }
  }

  # Add back excluded ----
  if (!is.null(exclude) && length(exclude) > 0) {
    # remove any duplicates
    if (!is.null(duplicate.index)) {
      excluded <- excluded[-duplicate.index, , drop = FALSE]
    }

    # remove by case thres
    if (!is.null(removeCases.thres) && length(removeCases.thres.index) > 0) {
      n.feat.inc <- NCOL(x)
      x <- cbind(x, excluded[-removeCases.thres.index, ])
      colnames(x)[-c(seq(n.feat.inc))] <- excluded.names
    } else {
      x <- cbind(x, excluded)
    }
  } # /add back excluded

  if (isdatatable) data.table::setDT(x)
  outro(start_time, verbose = verbose)
  x
} # rtemis::preprocess
