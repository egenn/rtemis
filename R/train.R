# train.R
# ::rtemis::
# 2025 EDG rtemis.org

train <- new_generic("train", "x")

#' @name train
#' 
#' @aliases train
#' 
#' @title
#' Tune, Train, and Test an \pkg{rtemis} Learner
#'
#' @description
#' `train` is a high-level function to preprocess, tune, train, and test an
#' \pkg{rtemis} model using nested crossvalidation.
#' 
#' @usage 
#' ## S7 method for signature 'data.frame'
#' train(x, dat_validation = NULL, dat_testing = NULL, algorithm = NULL, 
#'      preprocessor_parameters = NULL, hyperparameters = NULL, tuner_parameters = setup_GridSearch(),
#'      crossvalidation_parameters = NULL, weights = NULL, question = NULL, 
#'      outdir = NULL, verbosity = 1L)
#'
#' @details
#' For binary classification, the outcome should be a factor where the 2nd level corresponds to the
#' positive class.
#'
#' - Note on resampling: You should never use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and testing sets of the inner resamples, leading to underestimated
#' testing error.
#'
#' @param x data.frame or similar: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_testing data.frame or similar: Testing set data.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor_parameters PreprocessorParameters object or NULL: Setup using [setup_Preprocessor].
#' @param hyperparameters Hyperparameters object: Setup using one of `setup_*` functions.
#' @param tuner_parameters TunerParameters object: Setup using [setup_GridSearch].
#' @param crossvalidation_parameters ResamplerParameters object or NULL: Setup using [setup_Resampler].
#' @param weights Optional vector of case weights.
#' @param question Optional character string defining the question that the model is trying to
#' answer.
#' @param outdir Character, optional: String defining the output directory.
#' @param config Character, optional: Path to configuration file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `Regression(Supervised)`, `RegressionCV(SupervisedCV)`,
#' `Classification(Supervised)`, or `ClassificationCV(SupervisedCV)`.
#' 
#' @author EDG
#' @export
method(train, class_data.frame) <- function(x,
                                            dat_validation = NULL,
                                            dat_testing = NULL,
                                            algorithm = NULL,
                                            preprocessor_parameters = NULL, # PreprocessorParameters
                                            hyperparameters = NULL, # Hyperparameters
                                            tuner_parameters = setup_GridSearch(), # TunerParameters
                                            crossvalidation_parameters = NULL, # ResamplerParameters
                                            weights = NULL,
                                            question = NULL,
                                            outdir = NULL,
                                            verbosity = 1L) {
  # Dependencies ----
  check_dependencies(c("future.apply", "progressr"))

  # Checks ----
  if (!is.null(preprocessor_parameters)) {
    check_is_S7(preprocessor_parameters, PreprocessorParameters)
  }
  if (is.null(hyperparameters)) {
    hyperparameters <- get_default_hyperparameters(algorithm)
  }
  if (!is.null(tuner_parameters)) {
    check_is_S7(tuner_parameters, TunerParameters)
  }
  # Arguments ----
  if (!is.null(crossvalidation_parameters)) {
    check_is_S7(crossvalidation_parameters, ResamplerParameters)
    if (!is.null(crossvalidation_parameters$id_strat)) {
      stopifnot(length(crossvalidation_parameters$id_strat) == NROW(x))
    }
  }
  
  ## Algorithm ----
  if (!is.null(algorithm) && !is.null(hyperparameters) && tolower(algorithm) != tolower(hyperparameters@algorithm)) {
    stop(
      "You defined algorithm to be '", algorithm, "', but defined hyperparameters for ",
      hyperparameters@algorithm, "."
    )
  }
  if (is.null(algorithm)) {
    algorithm <- if (is.null(hyperparameters)) {
      "LightRF"
    } else {
      hyperparameters@algorithm
    }
  }
  algorithm <- get_alg_name(algorithm)
  if (!is.null(outdir)) {
    outdir <- make_path(outdir, algorithm)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (verbosity > 0L) {
      cat("Output directory set to", outdir, "\n")
    }
  }

  log_file <- if (!is.null(outdir)) {
    paste0(
      outdir, "/",
      "train_",
      format(Sys.time(), "%Y%m%d.%H%M%S"), ".log"
    )
  } else {
    NULL
  }
  start_time <- intro(verbosity = verbosity, log_file = log_file)

  # Init ----
  tuner <- NULL

  # Data ----
  ncols <- ncol(x)
  type <- supervised_type(x)
  if (type == "Classification") {
    classes <- levels(x[, ncols])
    n_classes <- length(classes)
  }

  ## Print data summary ----
  if (verbosity > 0L) {
    summarize_supervised_data(
      x = x,
      dat_validation = dat_validation,
      dat_testing = dat_testing
    )
  }

  # CV ----
  # if crossvallidation is set, this function calls itself
  # on multiple outer resamples (training-testing sets), each of which may call itself
  # on multiple inner resamples (training-validation set) for hyperparameter tuning.
  if (!is.null(crossvalidation_parameters)) {
    if (verbosity > 0L) {
      msg2("Training", hilite(algorithm, type), "by cross-validation...")
    }
    crossvalidation_resampler <- resample(x, parameters = crossvalidation_parameters, verbosity = verbosity)
    pcv <- progressr::progressor(crossvalidation_resampler@parameters@n)
    mods <- future.apply::future_lapply(
      seq_len(crossvalidation_resampler@parameters@n),
      function(i) {
        pcv(message = sprintf("Crossvalidation %i/%i", i, crossvalidation_resampler@parameters@n))
        train(
          x = x[crossvalidation_resampler[[i]], ],
          dat_testing = x[-crossvalidation_resampler[[i]], ],
          algorithm = algorithm,
          preprocessor_parameters = preprocessor_parameters,
          hyperparameters = hyperparameters,
          tuner_parameters = tuner_parameters,
          crossvalidation_parameters = NULL,
          weights = weights,
          question = question,
          outdir = outdir,
          verbosity = verbosity - 1L
        )
      },
      future.seed = TRUE,
      future.label = paste0("CV_train_", algorithm, "_%d")
    )
    hyperparameters@crossvalidated <- 1L
    msg2("Crossvalidation done.")
  } # /Crossvalidation

  if (hyperparameters@crossvalidated == 0L) {
    # Tune ----
    if (needs_tuning(hyperparameters)) {
      tuner <- tune(
        x,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        weights = weights,
        verbosity = verbosity
      )
      # Update hyperparameters
      hyperparameters <- update(hyperparameters, tuner@best_hyperparameters, tuned = 1L)
    } # /Tune
    if (verbosity > 0L) cat("\n")

    # Preprocess ----
    if (!is.null(preprocessor_parameters)) {
      preprocessor <- preprocess(
        x = x,
        parameters = preprocessor_parameters,
        dat_validation = dat_validation,
        dat_testing = dat_testing
      )
      x <- if (is.null(dat_validation) && is.null(dat_testing)) {
        preprocessor@preprocessed
      } else {
        preprocessor@preprocessed$training
      }
      if (!is.null(dat_validation)) dat_validation <- preprocessor@preprocessed$validation
      if (!is.null(dat_testing))  dat_testing <- preprocessor@preprocessed$testing
    } else {
      preprocessor <- NULL
    } # /Preprocess

    # IFW ----
    # Must follow preprocessing since N cases may change
    if (hyperparameters$ifw) {
      if (!is.null(weights)) {
        stop("Custom weights are defined, but IFW is set to TRUE.")
      } else {
        weights <- ifw(x[[ncols]], type = "case_weights", verbosity = verbosity)
      }
    } # /IFW

    # Train ALG ----
    if (verbosity > 0L) {
      if (hyperparameters@tuned == 1L) {
        msg2("Training", hilite(algorithm, type), "with tuned hyperparameters...")
      } else {
        msg20("Training ", hilite(algorithm, type), "...")
      }
      cat("\n")
    }
    # Only algorithms with early stopping can use dat_validation.
    # All traiining, validation, and testing metrics are calculated by Supervised or SupervisedCV.
    args <- list(
      x = x,
      weights = weights,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      verbosity = verbosity
    )
    # Validation data is only passed to learners using early stopping.
    # Otherwise, tuning functions collect validation metrics.
    if (algorithm %in% early_stopping_algs) {
      args$dat_validation <- dat_validation
    }
    mod <- do_call(get_train_fn(algorithm), args)
    # each train_* function checks output is the correct model class.

    # Predicted Values ----
    predict_fn <- get_predict_fn(algorithm)
    varimp_fn <- get_varimp_fn(algorithm)
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_testing <- NULL
    predicted_training <- do.call(
      predict_fn,
      list(mod, newdata = x[, -ncols, drop = FALSE], type = type)
    )
    if (type == "Classification") {
      predicted_prob_training <- predicted_training
      predicted_training <- prob2categorical(
        predicted_prob_training,
        levels = classes
      )
    }
    predicted_validation <- predicted_testing <- NULL
    if (!is.null(dat_validation)) {
      predicted_validation <- do.call(
        predict_fn, list(mod,
          newdata = dat_validation[, -ncols, drop = FALSE],
          type = type
        )
      )
      if (type == "Classification") {
        predicted_prob_validation <- predicted_validation
        predicted_validation <- prob2categorical(
          predicted_prob_validation,
          levels = classes
        )
      }
    }
    if (!is.null(dat_testing)) {
      predicted_testing <- do.call(
        predict_fn, list(mod,
          newdata = dat_testing[, -ncols, drop = FALSE],
          type = type
        )
      )
      if (type == "Classification") {
        predicted_prob_testing <- predicted_testing
        predicted_testing <- prob2categorical(
          predicted_prob_testing,
          levels = classes
        )
      }
    }

    # Standard Errors ----
    se_training <- se_validation <- se_testing <- NULL
    if (type == "Regression" && algorithm %in% se_compat_algorithms) {
      se_fn <- get_se_fn(algorithm)
      se_training <- do_call(se_fn, list(mod, newdata = x[, -ncols, drop = FALSE]))
      if (!is.null(dat_validation)) {
        se_validation <- do_call(se_fn, list(mod, newdata = dat_validation[, -ncols, drop = FALSE]))
      }
      if (!is.null(dat_testing)) {
        se_testing <- do_call(se_fn, list(mod, newdata = dat_testing[, -ncols, drop = FALSE]))
      }
    }
    # Make Supervised/CV ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = mod,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = x[[ncols]],
      y_validation = if (!is.null(dat_validation)) dat_validation[[ncols]],
      y_testing = if (!is.null(dat_testing)) dat_testing[[ncols]],
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_testing = predicted_prob_testing,
      se_training = se_training,
      se_validation = se_validation,
      se_testing = se_testing,
      xnames = names(x)[-ncols],
      varimp = do_call(varimp_fn, list(mod)),
      question = question
    )
  } else {
    y_training <- lapply(mods, function(mod) mod@y_training)
    y_testing <- lapply(mods, function(mod) mod@y_testing)
    predicted_training <- lapply(mods, function(mod) mod@predicted_training)
    predicted_testing <- lapply(mods, function(mod) mod@predicted_testing)
    if (type == "Classification") {
      predicted_prob_training <- lapply(mods, function(mod) mod@predicted_prob_training)
      predicted_prob_testing <- lapply(mods, function(mod) mod@predicted_prob_testing)
    } else {
      predicted_prob_training <- predicted_prob_testing <- NULL
    }
    mod <- make_SupervisedCV(
      algorithm = algorithm,
      type = type,
      models = mods,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      crossvalidation_resampler = crossvalidation_resampler,
      y_training = y_training,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_testing = predicted_prob_testing,
      xnames = names(x)[-ncols],
      varimp = lapply(mods, \(mod) mod@varimp),
      question = question
    )
  }

  # Outro ----
  if (verbosity > 0L) {
    print(mod)
    cat("\n")
  }
  outro(start_time,
    verbosity = verbosity,
    sink_off = ifelse(is.null(log_file), FALSE, TRUE)
  )
  mod
} # /rtemis::train
