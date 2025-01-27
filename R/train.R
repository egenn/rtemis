# train.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Tune, Train, and Test an \pkg{rtemis} Learner
#'
#' `train` is a high-level function to tune, train, and test an
#' \pkg{rtemis} model by nested resampling, with optional preprocessing and
#' decomposition of input features
#'
#' - Note on resampling: You should never use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and testing sets of the inner resamples, leading to underestimated
#' testing error.
#'
#' - If there is an error while running either the outer or inner resamples in
#' parallel, the error message returned by R will likely be unhelpful. Repeat
#' the command after setting both inner and outer resample run to use a single
#' core, which should provide an informative message.
#'
#' The `train` command is replacing `elevate`.
#' Note: specifying id_strat for the inner resampling is not yet supported.
#'
#' @param dat_training data.frame or similar: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_testing data.frame or similar: Testing set data.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor Preprocessor object or NULL. Setup using [setup_Preprocessor].
#' @param hyperparameters Hyperparameter object. Setup using one of `setup_*` functions.
#' @param tuner Tuner object. Setup using [setup_Tuner].
#' @param crossvalidation Crossvalidation object or NULL. Setup using [setup_Resampler].
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

train <- function(dat_training,
                  dat_validation = NULL,
                  dat_testing = NULL,
                  algorithm = "cart", # can eliminate
                  preprocessor = NULL,
                  hyperparameters = setup_CART(),
                  tuner = setup_GridSearch(),
                  crossvalidation = NULL,
                  weights = NULL,
                  binclasspos = 1L, # => ? move to hyperparameters?
                  question = NULL,
                  outdir = NULL,
                  # outdir_save_model = TRUE,
                  config = NULL,
                  verbosity = 1) {
  # Intro ----
  .call <- match.call()
  # => Make sure data is not substituted by list with entire raw data
  .call[2] <- list(str2lang("dat_training"))
  .call[3] <- list(str2lang("dat_validation"))
  .call[4] <- list(str2lang("dat_testing"))
  # mode <- switch(hyperparameters@crossvalidated,
  #                `0` = "inner",
  #                `1` = "outer")

  # Config ----
  # Use config file if provided.
  if (!is.null(config)) {
    config_path <- path.expand(config)
    if (!file.exists(config_path)) {
      stop("Configuration file not found at: ", config_path)
    }
    config <- read_config(config_path)
    dat_training <- read(config$data_path, character2factor = TRUE)
    if (length(config$target) > 0) {
      dat_training <- set_outcome(dat_training, config$target)
    }
    if (is.null(algorithm)) {
      if (!is.null(config$algorithm)) {
        algorithm <- config$algorithm
      } else {
        algorithm <- hyperparameters@algorithm
      }
    }
    hyperparameters <- config$hyperparameters
    tuner <- config$tuner
    weights <- config$weights
    crossvalidation <- config$crossvalidation
    outdir <- dirname(config$outdir)
  }

  # Arguments ----
  if (!is.null(crossvalidation$id_strat)) {
    stopifnot(length(crossvalidation$id_strat) == NROW(x))
  }
  algorithm <- get_alg_name(algorithm)
  if (!is.null(outdir)) {
    outdir <- make_path(outdir, algorithm)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (verbosity > 0) {
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

  # Dependencies ----
  check_dependencies(c("future.apply", "progressr"))

  # Data ----
  check_supervised_data(
    dat_training = dat_training,
    dat_validation = dat_validation,
    dat_testing = dat_testing
  )
  ncols <- ncol(dat_training)
  type <- supervised_type(dat_training)
  if (type == "Classification") {
    classes <- levels(dat_training[, ncols])
    n_classes <- length(classes)
  }

  ## Print data summary ----
  if (verbosity > 0) {
    summarize_supervised_data(
      dat_training = dat_training,
      dat_validation = dat_validation,
      dat_testing = dat_testing
    )
  }
  if (type == "Classification") {
    nclasses <- length(levels(dat_training[, ncol(dat_training)]))
  }

  # Train ----
  train_fn <- get_train_fn(algorithm)
  # Default hyperparameters
  if (is.null(hyperparameters)) {
    hyperparameters <- get_default_hyperparameters(algorithm)
  }
  check_is_S7(hyperparameters, Hyperparameters)

  ## CV ----
  # if crossvallidation is set, this function runs itself
  # on multiple outer resamples (training-testing), each of which may run itself on
  # on multiple inner resamples (training-validation) for tuning.
  if (!is.null(crossvalidation)) {
    if (verbosity > 0) {
      msg2("Training", hilite(algorithm, type), "by cross-validation...")
    }
    cvres <- resample(dat_training, parameters = crossvalidation, verbosity = verbosity)
    mods <- future.apply::future_lapply(
      seq_len(cvres@parameters@n),
      function(i) {
        train(
          dat_training = dat_training[cvres[[i]], ],
          dat_testing = dat_training[-cvres[[i]], ],
          algorithm = algorithm,
          preprocessor = preprocessor,
          hyperparameters = hyperparameters,
          tuner = tuner,
          crossvalidation = NULL,
          weights = weights,
          question = question,
          outdir = outdir,
          config = NULL,
          verbosity = verbosity - 1L
        )
      }
    )
    hyperparameters@crossvalidated <- 1L
    msg2("Crossvalidation done.")
  } # /crossvalidation

  if (hyperparameters@crossvalidated == 0L) {
    # Tune ----
    tuning <- NULL
    if (needs_tuning(hyperparameters)) {
      tuning <- tune(
        dat_training = dat_training,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner,
        weights = weights,
        verbosity = verbosity
      )
      # Update hyperparameters
      hyperparameters <- update(hyperparameters, tuning@best_hyperparameters, tuned = 1L)
    }
    if (verbosity > 0) cat("\n")

    # Train ----
    if (verbosity > 0) {
      msg20("Training ", hilite(algorithm, type), "...")
      cat("\n")
    }
    mod <- do.call(
      train_fn,
      list(
        dat_training = dat_training,
        dat_validation = dat_validation,
        dat_testing = dat_testing,
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner = tuner,
        verbosity = verbosity
      )
    )
    # train_* checks output is the right class

    # Supervised ----
    predict_fn <- get_predict_fn(algorithm)
    # if (type == "Classification") predict_prob_fn <- get_predict_prob_fn(algorithm)
    varimp_fn <- get_varimp_fn(algorithm)
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_testing <- NULL
    predicted_training <- do.call(predict_fn, list(mod, newdata = dat_training))
    if (type == "Classification") {
      predicted_prob_training <- predicted_training
      predicted_training <- prob2categorical(
        predicted_prob_training,
        levels = classes, binclasspos = binclasspos
      )
    }
    predicted_validation <- predicted_testing <- NULL
    if (!is.null(dat_validation)) {
      predicted_validation <- do.call(predict_fn, list(mod, newdata = dat_validation))
      if (type == "Classification") {
        predicted_prob_validation <- predicted_validation
        predicted_validation <- prob2categorical(
          predicted_prob_validation,
          levels = classes, binclasspos = binclasspos
        )
      }
    }
    if (!is.null(dat_testing)) {
      predicted_testing <- do.call(predict_fn, list(mod, newdata = dat_testing))
      if (type == "Classification") {
        predicted_prob_testing <- predicted_testing
        predicted_testing <- prob2categorical(
          predicted_prob_testing,
          levels = classes, binclasspos = binclasspos
        )
      }
    }
    # Make Supervised/CV ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = mod,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = dat_training[[ncols]],
      y_validation = if (!is.null(dat_validation)) dat_validation[[ncols]],
      y_testing = if (!is.null(dat_testing)) dat_testing[[ncols]],
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_testing = predicted_prob_testing,
      xnames = names(dat_training)[-ncols],
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
      models = mods,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = y_training,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_testing = predicted_prob_testing,
      # se_training = se_training,
      # se_testing = se_testing,
      xnames = names(dat_training)[-ncols],
      varimp = do.call(varimp_fn, list(mods)),
      question = question
    )
  }

  # Outro ----
  if (verbosity > 0) {
    print(mod)
    cat("\n")
  }
  outro(start_time,
    verbosity = verbosity,
    sink_off = ifelse(is.null(log_file), FALSE, TRUE)
  )
  mod
} # rtemis::train
