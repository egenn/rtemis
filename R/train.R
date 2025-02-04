# train.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Tune, Train, and Test an \pkg{rtemis} Learner
#'
#' `train` is a high-level function to preprocess, tune, train, and test an
#' \pkg{rtemis} model using nested crossvalidation.
#'
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
#' @param tuner_parameters TunerParameters object: Setup using [setup_Tuner].
#' @param crossvalidation_parameters ResamplerParameters object or NULL: Setup using [setup_Resampler].
#' @param weights Optional vector of case weights.
#' @param question Optional character string defining the question that the model is trying to
#' answer.
#' @param outdir Character, optional: String defining the output directory.
#' @param config Character, optional: Path to configuration file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Object of class `Regression(Supervised)` `RegressionCV(SupervisedCV)`,
#' `Classification(Supervised)`, or `ClassificationCV(SupervisedCV)`
#' @author EDG
#' @export
train <- new_generic("train", "x")
method(train, class_data.frame) <- function(x,
                                            dat_validation = NULL,
                                            dat_testing = NULL,
                                            algorithm = "cart", # can eliminate
                                            preprocessor_parameters = NULL, # PreprocessorParameters
                                            hyperparameters = NULL, # Hyperparameters
                                            tuner_parameters = setup_GridSearch(), # TunerParameters
                                            crossvalidation_parameters = NULL, # ResamplerParameters
                                            weights = NULL,
                                            question = NULL,
                                            outdir = NULL,
                                            # outdir_save_model = TRUE,
                                            config = NULL,
                                            verbosity = 1L) {
  # Checks ----
  if (!is.null(preprocessor_parameters)) {
    check_is_S7(preprocessor_parameters, PreprocessorParameters)
  }
  if (is.null(hyperparameters)) {
    hyperparameters <- get_default_hyperparameters(algorithm)
  }
  check_is_S7(hyperparameters, Hyperparameters)
  if (!is.null(tuner_parameters)) {
    check_is_S7(tuner_parameters, TunerParameters)
  }
  if (!is.null(crossvalidation_parameters)) {
    check_is_S7(crossvalidation_parameters, ResamplerParameters)
  }

  # Dependencies ----
  check_dependencies(c("future.apply", "progressr"))

  # Arguments ----
  if (!is.null(crossvalidation_parameters$id_strat)) {
    stopifnot(length(crossvalidation_parameters$id_strat) == NROW(x))
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
      # sys.calls()[[1]][[1]],
      "train_",
      format(Sys.time(), "%Y%m%d.%H%M%S"), ".log"
    )
  } else {
    NULL
  }
  start_time <- intro(verbosity = verbosity, log_file = log_file)

  # Data ----
  check_supervised_data(
    x = x,
    dat_validation = dat_validation,
    dat_testing = dat_testing
  )
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
    cvres <- resample(x, parameters = crossvalidation_parameters, verbosity = verbosity)
    pcv <- progressr::progressor(cvres@parameters@n)
    mods <- future.apply::future_lapply(
      seq_len(cvres@parameters@n),
      function(i) {
        pcv(message = sprintf("Crossvalidation %i/%i", i, cvres@parameters@n))
        train(
          x = x[cvres[[i]], ],
          dat_testing = x[-cvres[[i]], ],
          algorithm = algorithm,
          preprocessor_parameters = preprocessor_parameters,
          hyperparameters = hyperparameters,
          tuner_parameters = tuner_parameters,
          crossvalidation_parameters = NULL,
          weights = weights,
          question = question,
          outdir = outdir,
          config = NULL,
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
    tuning <- NULL
    if (needs_tuning(hyperparameters)) {
      tuning <- tune(
        x,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        weights = weights,
        verbosity = verbosity
      )
      # Update hyperparameters
      hyperparameters <- update(hyperparameters, tuning@best_hyperparameters, tuned = 1L)
    }
    if (verbosity > 0L) cat("\n")

    # Preprocess ----
    if (!is.null(preprocessor_parameters)) {
      dat_prp <- preprocess(
        x = x,
        parameters = preprocessor_parameters,
        dat_validation = dat_validation,
        dat_testing = dat_testing
      )
      x <- if (is.null(dat_validation) && is.null(dat_testing)) {
        dat_prp@preprocessed
      } else {
        dat_prp@preprocessed$training
      }
      if (!is.null(dat_validation)) dat_validation <- dat_prp@preprocessed$validation
      if (!is.null(dat_testing))  dat_testing <- dat_prp@preprocessed$testing
    }

    # Weights ----
    # must follow preprocessing since N cases may change
    # => IFW

    # Train ALG ----
    if (verbosity > 0L) {
      if (hyperparameters@tuned == 1L) {
        msg2("Training", hilite(algorithm, type), "with tuned hyperparameters...")
      } else {
        msg20("Training ", hilite(algorithm, type), "...")
      }
      cat("\n")
    }
    mod <- do.call(
      get_train_fn(algorithm),
      list(
        x = x,
        dat_validation = dat_validation,
        dat_testing = dat_testing,
        weights = weights,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        verbosity = verbosity
      )
    )
    # each train_ function checks output is the correct class.

    # Supervised ----
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
    # Make Supervised/CV ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = mod,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      y_training = x[[ncols]],
      y_validation = if (!is.null(dat_validation)) dat_validation[[ncols]],
      y_testing = if (!is.null(dat_testing)) dat_testing[[ncols]],
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_testing = predicted_prob_testing,
      xnames = names(x)[-ncols],
      varimp = do.call(varimp_fn, list(mod)),
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
    varimp_fn <- get_varimp_fn(algorithm)
    mod <- make_SupervisedCV(
      algorithm = algorithm,
      type = type,
      models = mods,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      crossvalidation_parameters = crossvalidation_parameters,
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
