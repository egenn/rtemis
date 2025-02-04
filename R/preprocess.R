# preprocess.R
# ::rtemis::
# 2017- EDG rtemis.org

#' @title
#' Data preprocessing
#'
#' @description
#' Prepare data for analysis and visualization
#'
#' @details
#' Order of operations:
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
#' @param x data.frame or similar: Data to be preprocessed.
#' @param parameters PreprocessorParameters: Preprocessing parameters. Setup using [setup_Preprocessor].
#' @param dat_validation data.frame or similar: Optional validation set.
#' @param dat_testing data.frame or similar: Optional testing set.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Preprocessor object.
#' @author EDG
#' @rdname preprocess
#' @export
preprocess <- new_generic("preprocess", c("x", "parameters"))
# preprocess(x, PreprocessorParameters, ...) ----
method(preprocess, list(class_data.frame, PreprocessorParameters)) <- function(
    x, parameters, dat_validation = NULL, dat_testing = NULL, verbosity = 1L) { # -> Preprocessor
  # Intro ----
  start_time <- intro(verbosity = verbosity - 1L)
  # parameters <- x
  # x <- dat
  # Init values list for Preprocessor output.
  values <- list(
    scale_centers = NULL,
    scale_coefficients = NULL,
    one_hot_levels = NULL,
    remove_features = NULL
  )

  # Data
  isdatatable <- data.table::is.data.table(x)
  x <- as.data.frame(x)

  # Complete cases ----
  if (parameters@complete_cases) {
    if (verbosity > 0L) msg2("Filtering complete cases...")
    x <- x[complete.cases(x), ]
  }

  # Set aside excluded ----
  if (!is.null(parameters@exclude) && length(parameters@exclude) > 0) {
    excluded <- x[, parameters@exclude, drop = FALSE]
    excluded_names <- colnames(x)[parameters@exclude]
    x <- x[, -parameters@exclude, drop = FALSE]
  }
  
  # Remove named features ----
  if (!is.null(parameters@remove_features)) {
    if (verbosity > 0L) {
      msg2("Removing", length(parameters@remove_features), "features...")
    }
    values$remove_features <- parameters@remove_features
    x <- x[, !names(x) %in% parameters@remove_features, drop = FALSE]
  }

  # Remove constants ----
  # Must be ahead of numeric quantile at least
  if (parameters@remove_constants) {
    constant <- which(sapply(x, is_constant, skip_missing = parameters@remove_constants_skipMissing))
    if (length(constant) > 0) {
      if (verbosity > 0L) {
        msg20("Removing ", singorplu(length(constant), "constant feature"), "...")
      }
      x <- x[, -constant]
    }
  }

  # Remove duplicates ----
  if (parameters@remove_duplicates) {
    # Ndups <- sum(duplicated(x))
    duplicate_index <- which(duplicated(x))
    Ndups <- length(duplicate_index)
    if (Ndups > 0) {
      if (verbosity > 0L) msg20("Removing ", singorplu(Ndups, "duplicate case"), "...")
      x <- unique(x)
    }
  } else {
    duplicate_index <- NULL
  }

  # Remove Cases by missing feature threshold ----
  if (!is.null(parameters@remove_cases_thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      # na_fraction_bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na_fraction_bycase <- data.table::transpose(xt)[, lapply(.SD, function(i) {
        sum(is.na(i)) / length(i)
      })]
      index_remove_cases_thres <- which(na_fraction_bycase >= parameters@remove_cases_thres)
      if (length(index_remove_cases_thres) > 0) {
        if (verbosity > 0L) {
          msg2(
            "Removing", length(index_remove_cases_thres),
            "cases with >=",
            parameters@remove_cases_thres, "missing data..."
          )
        }
        xt <- xt[-index_remove_cases_thres, ]
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
      removeFeat_thres_index <- which(na.fraction.byfeat >= parameters@remove_features_thres)
      if (length(removeFeat_thres_index) > 0) {
        if (verbosity > 0L) {
          msg2(
            "Removing", length(removeFeat_thres_index),
            "features with >=",
            parameters@remove_features_thres, "missing data..."
          )
        }
        x <- x[, -removeFeat_thres_index]
      }
    }
  }

  # Integer to factor ----
  index_integer <- NULL
  if (parameters@integer2factor) {
    index_integer <- c(
      which(sapply(x, is.integer)),
      which(sapply(x, bit64::is.integer64))
    )
    if (verbosity > 0L) {
      if (length(index_integer) > 0) {
        msg2(
          "Converting", singorplu(length(index_integer), "integer"),
          "to factor..."
        )
      } else {
        msg2("No integers to convert to factor...")
      }
    }
    for (i in index_integer) x[, i] <- as.factor(x[, i])
  }

  # Logical to factor ----
  if (parameters@logical2factor) {
    index_logical <- which(sapply(x, is.logical))
    if (verbosity > 0L) {
      if (length(index_logical) > 0) {
        msg20(
          "Converting ", singorplu(length(index_logical), "logical feature"), " to ",
          ngettext(length(index_logical), "factor", "factors"), "..."
        )
      } else {
        msg2("No logicals to convert to factor...")
      }
    }
    for (i in index_logical) x[, i] <- as.factor(x[, i])
  }

  # Numeric to factor ----
  if (parameters@numeric2factor) {
    index_numeric <- which(sapply(x, is.numeric))
    if (verbosity > 0L) msg2("Converting numeric to factors...")
    if (is.null(parameters@numeric2factor_levels)) {
      for (i in index_numeric) x[, i] <- as.factor(x[, i])
    } else {
      for (i in index_numeric) x[, i] <- factor(x[, i], levels = parameters@numeric2factor_levels)
    }
  }

  # Character to factor ----
  if (parameters@character2factor) {
    index_char <- which(sapply(x, is.character))
    if (verbosity > 0L) {
      if (length(index_char) > 0) {
        msg20(
          "Converting ", singorplu(length(index_char), "character feature"),
          " to ", ngettext(length(index_char), "a factor", "factors"), "..."
        )
      } else {
        msg2("No character features to convert to factors found.")
      }
    }
    for (i in index_char) x[, i] <- as.factor(x[, i])
  }

  # len2factor ----
  if (parameters@len2factor > 1) {
    index_len <- which(sapply(x, \(i) length(unique(i)) <= parameters@len2factor))
    # Exclude factors
    index_factor <- which(sapply(x, is.factor))
    index_len <- setdiff(index_len, index_factor)
    if (verbosity > 0L) {
      if (length(index_len) > 0) {
        msg2("Converting", singorplu(length(index_len), "feature"), "with <=", parameters@len2factor, "unique values to factors...")
      } else {
        msg2("No features with <=", parameters@len2factor, "unique values found.")
      }
    }
    for (i in index_len) x[, i] <- factor(x[, i])
  }

  # Integer to numeric ----
  if (parameters@integer2numeric) {
    if (is.null(index_integer)) {
      index_integer <- c(
        which(sapply(x, is.integer)),
        which(sapply(x, bit64::is.integer64))
      )
    }
    if (verbosity > 0L) {
      if (length(index_integer) > 0) {
        msg2("Converting", singorplu(length(index_integer), "integer"), "to numeric...")
      } else {
        msg2("No integers to convert to numeric...")
      }
    }
    for (i in index_integer) x[, i] <- as.numeric(x[, i])
  }

  # Logical to numeric ----
  if (parameters@logical2numeric) {
    index_logical <- which(sapply(x, is.logical))
    if (verbosity > 0L) msg2("Converting logicals to numeric...")
    for (i in index_logical) x[, i] <- as.numeric(x[, i])
  }

  # Numeric cut ----
  if (parameters@numeric_cut_n > 0) {
    index_numeric <- which(sapply(x, is.numeric))
    if (length(index_numeric) > 0) {
      if (verbosity > 0L) msg2("Cutting numeric features in", parameters@numeric_cut_n, "bins...")
      for (i in index_numeric) {
        x[, i] <- factor(
          cut(x[, i], breaks = parameters@numeric_cut_n, labels = parameters@numeric_cut_labels)
        )
      }
    }
  }

  # Numeric quantile ----
  if (parameters@numeric_quant_n > 0) {
    index_numeric2q <- if (parameters@numeric_quant_nAonly) {
      index_numeric2q <- which(sapply(x, is.numeric) & sapply(x, anyNA))
    } else {
      which(sapply(x, is.numeric))
    }
    if (length(index_numeric2q) > 0) {
      if (verbosity > 0L) msg2("Cutting numeric features in", parameters@numeric_quant_n, "quantiles...")
      for (i in index_numeric2q) {
        rng <- abs(diff(range(x[, i], na.rm = TRUE)))
        quantiles <- quantile(
          x[, i],
          probs = seq(0, 1, length.out = parameters@numeric_quant_n),
          na.rm = TRUE
        )
        quantiles[1] <- quantiles[1] - .02 * rng
        quantiles[parameters@numeric_quant_n] <- quantiles[parameters@numeric_quant_n] + .02 * rng
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
    index_factor <- which(sapply(x, is.factor))
    if (verbosity > 0L) {
      if (length(index_factor) > 0) {
        msg20(
          "Converting ", length(index_factor),
          ngettext(length(index_factor), " factor's", " factors'"),
          " NA values to level '", parameters@factorNA2missing_level, "'..."
        )
      } else {
        msg2("No factors found.")
      }
    }
    for (i in index_factor) x[, i] <- factor_NA2missing(x[, i], parameters@factorNA2missing_level)
  }

  # Factor to integer ----
  # e.g. for algorithms that do not support factors directly, but can handle integers
  # as categorical (e.g. LightGBM)
  if (parameters@factor2integer) {
    index_factor <- which(sapply(x, is.factor))
    if (verbosity > 0L) {
      if (length(index_factor) > 0) {
        msg2(
          "Converting", singorplu(length(index_factor), "factor"),
          "to integer..."
        )
      } else {
        msg2("No factors found to convert to integer...")
      }
    }
    if (parameters@factor2integer_startat0) {
      for (i in index_factor) x[, i] <- as.integer(x[, i]) - 1
    } else {
      for (i in index_factor) x[, i] <- as.integer(x[, i])
    }
  }

  # Missingness ----
  if (parameters@missingness) {
    cols_with_na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    for (i in cols_with_na) {
      x[, paste0(.colnames[i], "_missing")] <- factor(as.numeric(is.na(x[, i])))
      if (verbosity > 0L) msg20("Created missingness indicator for ", .colnames[i], "...")
    }
  }

  # Impute ----
  if (parameters@impute) {
    if (parameters@impute_type == "missRanger") {
      # '- missRanger ----
      check_dependencies("missRanger")
      if (verbosity > 0L) {
        if (parameters@impute_missRanger_params$pmm.k > 0) {
          msg2("Imputing missing values using predictive mean matching with missRanger...")
        } else {
          msg2("Imputing missing values using missRanger...")
        }
      }
      x <- missRanger::missRanger(x,
        pmm.k = parameters@impute_missRanger_params$pmm.k,
        verbose = verbosity
      )
    } else if (parameters@impute_type == "micePMM") {
      check_dependencies("mice")
      if (verbosity > 0L) msg2("Imputing missing values by predictive mean matching using mice...")
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
    } else {
      # '- mean/mode ----
      if (verbosity > 0L) {
        msg2(
          "Imputing missing values using", parameters@impute_numeric,
          "(numeric) and", parameters@impute_discrete, "(discrete)..."
        )
      }

      index_discrete <- which(sapply(x, function(i) is_discrete(i) && anyNA(i)))
      if (length(index_discrete) > 0) {
        for (i in index_discrete) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute_discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      index_integer <- which(sapply(x, function(i) is.integer(i) && anyNA(i)))
      if (length(index_integer) > 0) {
        for (i in index_integer) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute_discrete(x[, i])
          x[index, i] <- imputed
        }
      }

      index_numeric <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
      if (length(index_numeric) > 0) {
        for (i in index_numeric) {
          index <- which(is.na(x[, i]))
          imputed <- parameters@impute_numeric(x[, i], na.rm = TRUE)
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
      if (verbosity > 0L) {
        msg2(
          paste(c(sc, ce), collapse = " and "),
          length(numeric_index), "numeric features..."
        )
      }
      # Info: scale outputs a matrix.
      scale_ <- if (!is.null(parameters@scale_coefficients)) {
        # Check names match
        stopifnot(identical(names(parameters@scale_coefficients), names(x[, numeric_index])))
        parameters@scale_coefficients
      } else {
        parameters@scale
      }
      center_ <- if (!is.null(parameters@scale_centers)) {
        # Check names match
        stopifnot(identical(names(parameters@scale_centers), names(x[, numeric_index])))
        parameters@scale_centers
      } else {
        parameters@center
      }
      x_num_scaled <- scale(
        x[, numeric_index, drop = FALSE],
        scale = scale_,
        center = center_
      )
    
      # Collect scale and center values
      values$scale_centers <- attr(x_num_scaled, "scaled:center")
      values$scale_coefficients <- attr(x_num_scaled, "scaled:scale")

      x_num_scaled <- as.data.frame(x_num_scaled)

      # Insert into original dataset
      x[, numeric_index] <- x_num_scaled
      # j <- 0
      # for (i in numeric_index) {
      #   j <- j + 1
      #   x[, i] <- x_num_scaled[, j]
      # }
    } else {
      msg2(
        paste(c(sc, ce), collapse = " and "),
        "was requested \n                                but no numeric features were found: Please check data."
      )
    }
  }

  # One Hot Encoding ----
  if (parameters@one_hot) {
    x <- one_hot(x, verbosity = verbosity, factor_levels = parameters@one_hot_levels)
  }

  # Add date features ----
  if (parameters@add_date_features) {
    if (verbosity > 0L) msg2("Extracting date features...")
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
    if (verbosity > 0L) msg2("Extracting holidays...")
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
    if (!is.null(duplicate_index)) {
      excluded <- excluded[-duplicate_index, , drop = FALSE]
    }

    # remove by case thres
    if (!is.null(parameters@remove_cases_thres) && length(index_remove_cases_thres) > 0) {
      n_feat_inc <- NCOL(x)
      x <- cbind(x, excluded[-index_remove_cases_thres, ])
      colnames(x)[-c(seq(n_feat_inc))] <- excluded_names
    } else {
      x <- cbind(x, excluded)
    }
  } # /add back excluded

  if (isdatatable) data.table::setDT(x)
  if (verbosity > 0L) {
    msg2("Preprocessing completed.")
  }
  
  preprocessed <- list(training = x)
  
  if (!is.null(dat_validation)) {
    if (verbosity > 0L) msg2("Applying preprocessing to validation data...")
    prp_validation <- preprocess(
      x = dat_validation,
      parameters = Preprocessor(
        parameters = parameters,
        preprocessed = list(),
        scale_centers = values$scale_centers,
        scale_coefficients = values$scale_coefficients,
        one_hot_levels = values$one_hot_levels,
        remove_features = values$remove_features
      ),
      verbosity = verbosity
    )
    preprocessed$validation <- prp_validation@preprocessed
  }
  if (!is.null(dat_testing)) {
    if (verbosity > 0L) msg2("Applying preprocessing to testing data...")
    prp_testing <- preprocess(
      x = dat_testing,
      parameters = Preprocessor(
        parameters = parameters,
        preprocessed = list(),
        scale_centers = values$scale_centers,
        scale_coefficients = values$scale_coefficients,
        one_hot_levels = values$one_hot_levels,
        remove_features = values$remove_features
      ),
      verbosity = verbosity
    )
    preprocessed$testing <- prp_testing@preprocessed
  }
  outro(start_time, verbosity = verbosity - 1L)
  Preprocessor(
    parameters = parameters,
    preprocessed = if (length(preprocessed) == 1) preprocessed[[1]] else preprocessed,
    scale_centers = values$scale_centers,
    scale_coefficients = values$scale_coefficients,
    one_hot_levels = values$one_hot_levels,
    remove_features = values$remove_features
  )
} # /rtemis::preprocess(PreprocessorParameters, ...)

# preprocess(x, Preprocessor, ...) ----
method(preprocess, list(class_data.frame, Preprocessor)) <- function(x, parameters, verbosity = 1L) { # -> Preprocessor
  params <- parameters@parameters
  # Overwrite scale_centers, scale_coefficients, one_hot_levels, and remove_features
  params@scale_centers <- parameters@values$scale_centers
  params@scale_coefficients <- parameters@values$scale_coefficients
  params@one_hot_levels <- parameters@values$one_hot_levels
  params@remove_features <- parameters@values$remove_features

  preprocess(x, params, verbosity = verbosity)
} # /rtemis::preprocess(Preprocessor, ...)
