# preprocess.R
# ::rtemis::
# 2017- EDG rtemis.org
# => merge with preprocess_ as single data.table-based preprocess function
# => output Preprocessor object with data and parameters needed for application to new data
# (scale, center, etc.)

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

#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @export

preprocess <- function(x,
                       parameters = setup_Preprocessor(),
                       verbosity = 1) {
  # Intro ----
  start_time <- intro(
    verbosity = verbosity
  )

  # Data
  isdatatable <- data.table::is.data.table(x)
  x <- as.data.frame(x)

  # Complete cases ----
  if (parameters@complete_cases) {
    if (verbosity > 0) msg2("Filtering complete cases...")
    x <- x[complete.cases(x), ]
  }

  # Set aside excluded ----
  if (!is.null(parameters@exclude) && length(parameters@exclude) > 0) {
    excluded <- x[, parameters@exclude, drop = FALSE]
    excluded.names <- colnames(x)[parameters@exclude]
    x <- x[, -parameters@exclude, drop = FALSE]
  }

  # Remove constants ----
  # Must be ahead of numeric quantile at least
  if (parameters@remove_constants) {
    constant <- which(sapply(x, is_constant, skip_missing = parameters@remove_constants.skipMissing))
    if (length(constant) > 0) {
      if (verbosity > 0) {
        msg20("Removing ", singorplu(length(constant), "constant feature"), "...")
      }
      x <- x[, -constant]
    }
  }

  # Remove duplicates ----
  if (parameters@remove_duplicates) {
    # Ndups <- sum(duplicated(x))
    duplicate.index <- which(duplicated(x))
    Ndups <- length(duplicate.index)
    if (Ndups > 0) {
      if (verbosity > 0) msg20("Removing ", singorplu(Ndups, "duplicate case"), "...")
      x <- unique(x)
    }
  } else {
    duplicate.index <- NULL
  }

  # Remove Cases by missing feature threshold ----
  if (!is.null(parameters@remove_cases_thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      # na.fraction.bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na.fraction.bycase <- data.table::transpose(xt)[, lapply(.SD, function(i) {
        sum(is.na(i)) / length(i)
      })]
      remove_cases_thres.index <- which(na.fraction.bycase >= parameters@remove_cases_thres)
      if (length(remove_cases_thres.index) > 0) {
        if (verbosity > 0) {
          msg2(
            "Removing", length(remove_cases_thres.index),
            "cases with >=",
            parameters@remove_cases_thres, "missing data..."
          )
        }
        xt <- xt[-remove_cases_thres.index, ]
      }
      x <- as.data.frame(xt)
    }
  }

  # Remove Features by missing feature threshold ----
  if (!is.null(parameters@remove_features_thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      na.fraction.byfeat <- xt[, lapply(.SD, function(i) {
        sum(is.na(i)) / length(i)
      })]
      removeFeat.thres.index <- which(na.fraction.byfeat >= parameters@remove_features_thres)
      if (length(removeFeat.thres.index) > 0) {
        if (verbosity > 0) {
          msg2(
            "Removing", length(removeFeat.thres.index),
            "features with >=",
            parameters@remove_features_thres, "missing data..."
          )
        }
        x <- x[, -removeFeat.thres.index]
      }
    }
  }

  # Integer to factor ----
  index.integer <- NULL
  if (parameters@integer2factor) {
    index.integer <- c(
      which(sapply(x, is.integer)),
      which(sapply(x, bit64::is.integer64))
    )
    if (verbosity > 0) {
      if (length(index.integer) > 0) {
        msg2(
          "Converting", singorplu(length(index.integer), "integer"),
          "to factor..."
        )
      } else {
        msg2("No integers to convert to factor...")
      }
    }
    for (i in index.integer) x[, i] <- as.factor(x[, i])
  }

  # Logical to factor ----
  if (parameters@logical2factor) {
    index.logical <- which(sapply(x, is.logical))
    if (verbosity > 0) {
      if (length(index.logical) > 0) {
        msg20(
          "Converting ", singorplu(length(index.logical), "logical feature"), " to ",
          ngettext(length(index.logical), "factor", "factors"), "..."
        )
      } else {
        msg2("No logicals to convert to factor...")
      }
    }
    for (i in index.logical) x[, i] <- as.factor(x[, i])
  }

  # Numeric to factor ----
  if (parameters@numeric2factor) {
    index_numeric <- which(sapply(x, is.numeric))
    if (verbosity > 0) msg2("Converting numeric to factors...")
    if (is.null(parameters@numeric2factor.levels)) {
      for (i in index_numeric) x[, i] <- as.factor(x[, i])
    } else {
      for (i in index_numeric) x[, i] <- factor(x[, i], levels = parameters@numeric2factor.levels)
    }
  }

  # Character to factor ----
  if (parameters@character2factor) {
    index.char <- which(sapply(x, is.character))
    if (verbosity > 0) {
      if (length(index.char) > 0) {
        msg20(
          "Converting ", singorplu(length(index.char), "character feature"),
          " to ", ngettext(length(index.char), "a factor", "factors"), "..."
        )
      } else {
        msg2("No character features to convert to factors found.")
      }
    }
    for (i in index.char) x[, i] <- as.factor(x[, i])
  }

  # len2factor ----
  if (parameters@len2factor > 1) {
    index.len <- which(sapply(x, \(i) length(unique(i)) <= parameters@len2factor))
    # Exclude factors
    index.factor <- which(sapply(x, is.factor))
    index.len <- setdiff(index.len, index.factor)
    if (verbosity > 0) {
      if (length(index.len) > 0) {
        msg2("Converting", singorplu(length(index.len), "feature"), "with <=", parameters@len2factor, "unique values to factors...")
      } else {
        msg2("No features with <=", parameters@len2factor, "unique values found.")
      }
    }
    for (i in index.len) x[, i] <- factor(x[, i])
  }

  # Integer to numeric ----
  if (parameters@integer2numeric) {
    if (is.null(index.integer)) {
      index.integer <- c(
        which(sapply(x, is.integer)),
        which(sapply(x, bit64::is.integer64))
      )
    }
    if (verbosity > 0) {
      if (length(index.integer) > 0) {
        msg2("Converting", singorplu(length(index.integer), "integer"), "to numeric...")
      } else {
        msg2("No integers to convert to numeric...")
      }
    }
    for (i in index.integer) x[, i] <- as.numeric(x[, i])
  }

  # Logical to numeric ----
  if (parameters@logical2numeric) {
    index.logical <- which(sapply(x, is.logical))
    if (verbosity > 0) msg2("Converting logicals to numeric...")
    for (i in index.logical) x[, i] <- as.numeric(x[, i])
  }

  # Numeric cut ----
  if (parameters@numeric.cut.n > 0) {
    index_numeric <- which(sapply(x, is.numeric))
    if (length(index_numeric) > 0) {
      if (verbosity > 0) msg2("Cutting numeric features in", parameters@numeric.cut.n, "bins...")
      for (i in index_numeric) {
        x[, i] <- factor(
          cut(x[, i], breaks = parameters@numeric.cut.n, labels = parameters@numeric.cut.labels)
        )
      }
    }
  }

  # Numeric quantile ----
  if (parameters@numeric.quant.n > 0) {
    index_numeric2q <- if (parameters@numeric.quant.NAonly) {
      index_numeric2q <- which(sapply(x, is.numeric) & sapply(x, anyNA))
    } else {
      which(sapply(x, is.numeric))
    }
    if (length(index_numeric2q) > 0) {
      if (verbosity > 0) msg2("Cutting numeric features in", parameters@numeric.quant.n, "quantiles...")
      for (i in index_numeric2q) {
        rng <- abs(diff(range(x[, i], na.rm = TRUE)))
        quantiles <- quantile(
          x[, i],
          probs = seq(0, 1, length.out = parameters@numeric.quant.n),
          na.rm = TRUE
        )
        quantiles[1] <- quantiles[1] - .02 * rng
        quantiles[parameters@numeric.quant.n] <- quantiles[parameters@numeric.quant.n] + .02 * rng
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
  if (parameters@factorNA2missing) {
    index.factor <- which(sapply(x, is.factor))
    if (verbosity > 0) {
      if (length(index.factor) > 0) {
        msg20(
          "Converting ", length(index.factor),
          ngettext(length(index.factor), " factor's", " factors'"),
          " NA values to level '", parameters@factorNA2missing.level, "'..."
        )
      } else {
        msg2("No factors found.")
      }
    }
    for (i in index.factor) x[, i] <- factor_NA2missing(x[, i], parameters@factorNA2missing.level)
  }

  # Factor to integer ----
  # e.g. for algorithms that do not support factors directly, but can handle integers
  # as categorical (e.g. LightGBM)
  if (parameters@factor2integer) {
    index.factor <- which(sapply(x, is.factor))
    if (verbosity > 0) {
      if (length(index.factor) > 0) {
        msg2(
          "Converting", singorplu(length(index.factor), "factor"),
          "to integer..."
        )
      } else {
        msg2("No factors found to convert to integer...")
      }
    }
    if (parameters@factor2integer_startat0) {
      for (i in index.factor) x[, i] <- as.integer(x[, i]) - 1
    } else {
      for (i in index.factor) x[, i] <- as.integer(x[, i])
    }
  }

  # Missingness ----
  if (parameters@missingness) {
    cols.with.na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    for (i in cols.with.na) {
      x[, paste0(.colnames[i], "_missing")] <- factor(as.numeric(is.na(x[, i])))
      if (verbosity > 0) msg20("Created missingness indicator for ", .colnames[i], "...")
    }
  }

  # Impute ----
  if (parameters@impute) {
    if (parameters@impute.type == "missRanger") {
      # '- missRanger ----
      check_dependencies("missRanger")
      if (verbosity > 0) {
        if (parameters@impute.missRanger.params$pmm.k > 0) {
          msg2("Imputing missing values using predictive mean matching with missRanger...")
        } else {
          msg2("Imputing missing values using missRanger...")
        }
      }
      x <- missRanger::missRanger(x,
        pmm.k = parameters@impute.missRanger.params$pmm.k,
        verbose = verbosity
      )
    } else if (parameters@impute.type == "micePMM") {
      check_dependencies("mice")
      if (verbosity > 0) msg2("Imputing missing values by predictive mean matching using mice...")
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
    } else {
      # '- mean/mode ----
      if (verbosity > 0) {
        msg20(
          "Imputing missing values using ", deparse(substitute(parameters@impute.numeric)),
          " and ", deparse(substitute(parameters@impute.discrete)), "..."
        )
      }

      discrete.index <- which(sapply(x, function(i) is_discrete(i) && anyNA(i)))
      if (length(discrete.index) > 0) {
        for (i in discrete.index) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute.discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      integer.index <- which(sapply(x, function(i) is.integer(i) && anyNA(i)))
      if (length(integer.index) > 0) {
        for (i in integer.index) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute.discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      numeric.index <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
      if (length(numeric.index) > 0) {
        for (i in numeric.index) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute.numeric(x[, i], na.rm = TRUE)
          x[index, i] <- imputed
        }
      }
    }
  }

  # Scale +/- center ----
  if (parameters@scale || parameters@center) {
    # Get index of numeric features
    numeric_index <- which(sapply(x, is.numeric))
    sc <- if (parameters@scale) "Scaling" else NULL
    ce <- if (parameters@center) "Centering" else NULL
    if (length(numeric_index) > 0) {
      if (verbosity > 0) {
        msg2(
          paste(c(sc, ce), collapse = " and "),
          length(numeric_index), "numeric features..."
        )
      }
      x_num_scaled <- as.data.frame(scale(x[, numeric_index], scale = parameters@scale, center = parameters@center))
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
  # if (remove_constants) {
  #     # constant <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
  #     constant <- which(sapply(x, is_constant, skip_missing = remove_constants.skipMissing))
  #     if (length(constant) > 0) {
  #         if (verbosity > 0) msg2("Removing constant features...")
  #         x <- x[, -constant]
  #     }
  # }

  # One Hot Encoding ----
  if (parameters@oneHot) x <- oneHot(x, verbosity = verbosity)

  # Add date features ----
  if (parameters@add_date_features) {
    if (verbosity > 0) msg2("Extracting date features...")
    # Find date columns
    date_cols <- which(sapply(x, function(col) inherits(col, "Date")))
    # For each date column, extract features
    for (i in date_cols) {
      .date_features <- dates2features(x[[i]], features = parameters@date_features)
      names(.date_features) <- paste0(names(x)[i], "_", names(.date_features))
      x <- cbind(x, .date_features)
    }
  }

  # Add holidays ----
  if (parameters@add_holidays) {
    if (verbosity > 0) msg2("Extracting holidays...")
    # Find date columns
    date_cols <- which(sapply(x, \(col) inherits(col, "Date")))
    # For each date column, extract holidays
    for (i in date_cols) {
      .holidays <- get_holidays(x[, i])
      x[[paste0(names(x)[i], "_holidays")]] <- .holidays
    }
  }

  # Add back excluded ----
  if (!is.null(parameters@exclude) && length(parameters@exclude) > 0) {
    # remove any duplicates
    if (!is.null(duplicate.index)) {
      excluded <- excluded[-duplicate.index, , drop = FALSE]
    }

    # remove by case thres
    if (!is.null(parameters@remove_cases_thres) && length(remove_cases_thres.index) > 0) {
      n.feat.inc <- NCOL(x)
      x <- cbind(x, excluded[-remove_cases_thres.index, ])
      colnames(x)[-c(seq(n.feat.inc))] <- excluded.names
    } else {
      x <- cbind(x, excluded)
    }
  } # /add back excluded

  if (isdatatable) data.table::setDT(x)
  outro(start_time, verbosity = verbosity)
  x
} # rtemis::preprocess
