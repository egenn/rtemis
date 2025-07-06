# train.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train Supervised Learning Models
#'
#' Preprocess, tune, train, and test supervised learning models with a single function
#' using nested resampling
#'
#' @details
#' Important: For binary classification, the outcome should be a factor where the 2nd level corresponds to the
#' positive class.
#'
#' Note on resampling: You should never use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and test sets of the inner resamples, leading to underestimated
#' test error.
#'
#' @param x data.frame or similar: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_test data.frame or similar: Test set data.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor_parameters PreprocessorParameters object or NULL: Setup using [setup_Preprocessor].
#' @param hyperparameters Hyperparameters object: Setup using one of `setup_*` functions.
#' @param tuner_parameters TunerParameters object: Setup using [setup_GridSearch].
#' @param outer_resampling ResamplerParameters object or NULL: Setup using [setup_Resampler]. This
#' defines the outer resampling method, i.e. the splitting into training and test sets for the
#' purpose of assessing model performance. If NULL, no outer resampling is performed, in which case
#' you might want to use a `dat_test` dataset to assess model performance on a single test set.
#' @param weights Optional vector of case weights.
#' @param question Optional character string defining the question that the model is trying to
#' answer.
#' @param outdir Character, optional: String defining the output directory.
#' @param parallel_type Character: "none", or "future".
#' @param verbosity Integer: Verbosity level.
# @param ... Additional arguments to pass to the hyperparameters setup function. Only used if
#' `hyperparameters` is not defined. Avoid relying on this, instead use the appropriate `setup_*`
#' function with the `hyperparameters` argument.
#'
#' @return Object of class `Regression(Supervised)`, `RegressionRes(SupervisedRes)`,
#' `Classification(Supervised)`, or `ClassificationRes(SupervisedRes)`.
#'
#' @author EDG
#' @export
train <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL,
  algorithm = NULL,
  preprocessor_parameters = NULL, # PreprocessorParameters
  hyperparameters = NULL, # Hyperparameters
  tuner_parameters = NULL, # TunerParameters
  outer_resampling = NULL, # ResamplerParameters
  weights = NULL,
  question = NULL,
  outdir = NULL,
  parallel_type = "future",
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies(c("future.apply", "progressr"))
  type <- supervised_type(x)
  ncols <- ncol(x)

  # Checks ----
  # Pass ... to hyperparameters setup_* fn
  # hpr_args <- list(...)
  # if (!is.null(hyperparameters) && length(hpr_args) > 0) {
  #   cli::cli_abort("You can either define `hyperparameters` or pass them as additional arguments.")
  # }
  if (is.null(hyperparameters) && is.null(algorithm)) {
    cli::cli_abort(
      "You must define either `hyperparameters` or `algorithm`."
    )
  }

  if (is.null(algorithm) && !is.null(hyperparameters)) {
    algorithm <- hyperparameters@algorithm
  }

  if (is.null(hyperparameters) && !is.null(algorithm)) {
    # without extra args
    hyperparameters <- get_default_hyperparameters(
      algorithm,
      type = type,
      ncols = ncols
    )
    # with extra args
    # setup_fn <- get_alg_setup(algorithm)
    # hyperparameters <- do_call(setup_fn, hpr_args)
  }

  if (
    !is.null(algorithm) &&
      tolower(algorithm) != tolower(hyperparameters@algorithm)
  ) {
    cli::cli_abort(
      "You defined algorithm to be '",
      algorithm,
      "', but defined hyperparameters for ",
      hyperparameters@algorithm,
      "."
    )
  }

  check_is_S7(hyperparameters, Hyperparameters)
  if (!is.null(tuner_parameters)) {
    check_is_S7(tuner_parameters, TunerParameters)
  }

  if (!is.null(preprocessor_parameters)) {
    check_is_S7(preprocessor_parameters, PreprocessorParameters)
  }

  # If outer_resampling is set, dat_validation and dat_test must be NULL
  if (!is.null(outer_resampling)) {
    if (!is.null(dat_validation) || !is.null(dat_test)) {
      cli::cli_abort(
        "If outer_resampling is set, dat_validation and dat_test must be NULL."
      )
    }
  }

  ## Arguments ----
  if (!is.null(outer_resampling)) {
    check_is_S7(outer_resampling, ResamplerParameters)
    if (!is.null(outer_resampling[["id_strat"]])) {
      stopifnot(length(outer_resampling[["id_strat"]]) == NROW(x))
    }
  }

  # Default to LightRF if no algorithm is set.
  if (is.null(algorithm)) {
    algorithm <- if (is.null(hyperparameters)) {
      "LightRF"
    } else {
      hyperparameters@algorithm
    }
  }
  algorithm <- get_alg_name(algorithm)
  if (!is.null(outdir)) {
    outdir <- make_path(outdir)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (verbosity > 0L) {
      message("Output directory set to ", outdir, ".")
    }
  }

  log_file <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      "train_",
      algorithm,
      "_",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start_time <- intro(verbosity = verbosity, log_file = log_file)

  # Parallelization ----
  # 3 potential points of parallelization from innermost to outermost: algorithm, tuning (inner resampling), outer resampling.

  # Init ----
  tuner <- NULL

  # Data ----
  if (type == "Classification") {
    classes <- levels(outcome(x))
    n_classes <- length(classes)
  }

  ## Print data summary ----
  if (verbosity > 0L) {
    summarize_supervised_data(
      x = x,
      dat_validation = dat_validation,
      dat_test = dat_test
    )
  }

  # Outer Resampling ----
  # if outer_resampling is set, this function calls itself
  # on multiple outer resamples (training-test sets), each of which may call itself
  # on multiple inner resamples (training-validation sets) for hyperparameter tuning.
  if (!is.null(outer_resampling)) {
    if (verbosity > 0L) {
      msg20(
        "Training ",
        hilite(algorithm, type),
        " using ",
        desc(outer_resampling),
        "..."
      )
    }
    outer_resampler <- resample(
      x,
      parameters = outer_resampling,
      verbosity = verbosity
    )
    pcv <- progressr::progressor(outer_resampler@parameters@n)
    models <- lapply(
      seq_len(outer_resampler@parameters@n),
      function(i) {
        pcv(
          message = sprintf(
            "Outer resample %i/%i",
            i,
            outer_resampler@parameters@n
          )
        )
        train(
          x = x[outer_resampler[[i]], ],
          dat_test = x[-outer_resampler[[i]], ],
          algorithm = algorithm,
          preprocessor_parameters = preprocessor_parameters,
          hyperparameters = hyperparameters,
          tuner_parameters = tuner_parameters,
          outer_resampling = NULL,
          weights = weights,
          question = question,
          verbosity = verbosity - 1L
        )
      }
    )
    names(models) <- names(outer_resampler@resamples)
    hyperparameters@resampled <- 1L
    msg2("Outer resampling done.")
  } # /Outer Resampling

  if (hyperparameters@resampled == 0L) {
    # Tune ----
    if (needs_tuning(hyperparameters)) {
      if (is.null(tuner_parameters)) {
        tuner_parameters <- setup_GridSearch()
      }
      if (parallel_type == "future") {
        if (algorithm %in% live[["parallelized_learners"]]) {
          future::plan(strategy = "sequential")
          if (verbosity > 0L) {
            info(
              bold(algorithm),
              "is parallelized. Disabling all other parallelization."
            )
          }
        } else {
          future::plan(strategy = rtemis_plan, workers = rtemis_workers)
          if (verbosity > 0L) {
            info(
              "Tuning parallelization: plan set to",
              bold(rtemis_plan),
              "with",
              bold(rtemis_workers),
              "workers."
            )
          }
        }
      }
      tuner <- tune(
        x = x,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        weights = weights,
        parallel_type = parallel_type,
        verbosity = verbosity
      )
      # Update hyperparameters
      hyperparameters <- update(
        hyperparameters,
        tuner@best_hyperparameters,
        tuned = 1L
      )
    } # /Tune
    # if (verbosity > 0L) {
    #   message()
    # }

    # Preprocess ----
    if (!is.null(preprocessor_parameters)) {
      preprocessor <- preprocess(
        x = x,
        parameters = preprocessor_parameters,
        dat_validation = dat_validation,
        dat_test = dat_test
      )
      x <- if (is.null(dat_validation) && is.null(dat_test)) {
        preprocessor@preprocessed
      } else {
        preprocessor@preprocessed[["training"]]
      }
      if (!is.null(dat_validation)) {
        dat_validation <- preprocessor@preprocessed[["validation"]]
      }
      if (!is.null(dat_test)) dat_test <- preprocessor@preprocessed[["test"]]
    } else {
      preprocessor <- NULL
    } # /Preprocess

    # IFW ----
    # Must follow preprocessing since N cases may change
    if (type == "Classification" && hyperparameters[["ifw"]]) {
      if (!is.null(weights)) {
        cli::cli_abort("Custom weights are defined, but IFW is set to TRUE.")
      } else {
        weights <- ifw(x[[ncols]], type = "case_weights", verbosity = verbosity)
      }
    } # /IFW

    # Train ALG ----
    if (verbosity > 0L) {
      if (is_tuned(hyperparameters)) {
        msg2(
          "Training",
          hilite(algorithm, type),
          "with tuned hyperparameters..."
        )
      } else {
        msg20("Training ", hilite(algorithm, type), "...")
      }
    } # /Print training message
    # Only algorithms with early stopping can use dat_validation.
    # Note: All training, validation, and test metrics are calculated by Supervised or SupervisedRes.
    # => Introduce supports_weights() if any algorithms do NOT support case weights.
    args <- list(
      x = x,
      weights = weights,
      hyperparameters = hyperparameters,
      verbosity = verbosity
    )
    # Validation data is only passed to learners using early stopping.
    # Otherwise, tuning functions collect validation metrics.
    if (algorithm %in% early_stopping_algs) {
      args[["dat_validation"]] <- dat_validation
    }

    model <- do_call(get_train_fn(algorithm), args)
    # each train_* function checks output is the correct model class.

    # Predicted Values ----
    predict_fn <- get_predict_fn(algorithm)
    varimp_fn <- get_varimp_fn(algorithm)
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_test <- NULL
    predicted_training <- do_call(
      predict_fn,
      list(model, newdata = features(x), type = type)
    )
    if (type == "Classification") {
      predicted_prob_training <- predicted_training
      predicted_training <- prob2categorical(
        predicted_prob_training,
        levels = classes
      )
    }
    predicted_validation <- predicted_test <- NULL
    if (!is.null(dat_validation)) {
      predicted_validation <- do_call(
        predict_fn,
        list(model, newdata = features(dat_validation), type = type)
      )
      if (type == "Classification") {
        predicted_prob_validation <- predicted_validation
        predicted_validation <- prob2categorical(
          predicted_prob_validation,
          levels = classes
        )
      }
    }
    if (!is.null(dat_test)) {
      predicted_test <- do_call(
        predict_fn,
        list(model, newdata = features(dat_test), type = type)
      )
      if (type == "Classification") {
        predicted_prob_test <- predicted_test
        predicted_test <- prob2categorical(
          predicted_prob_test,
          levels = classes
        )
      }
    }

    # Standard Errors ----
    se_training <- se_validation <- se_test <- NULL
    if (type == "Regression" && algorithm %in% se_compat_algorithms) {
      se_fn <- get_se_fn(algorithm)
      se_training <- do_call(se_fn, list(model, newdata = features(x)))
      if (!is.null(dat_validation)) {
        se_validation <- do_call(
          se_fn,
          list(model, newdata = features(dat_validation))
        )
      }
      if (!is.null(dat_test)) {
        se_test <- do_call(se_fn, list(model, newdata = features(dat_test)))
      }
    }

    # Make Supervised/Res ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = x[[ncols]],
      y_validation = if (!is.null(dat_validation)) dat_validation[[ncols]],
      y_test = if (!is.null(dat_test)) dat_test[[ncols]],
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test,
      xnames = names(x)[-ncols],
      varimp = do_call(varimp_fn, list(model)),
      question = question
    )
  } else {
    y_training <- lapply(models, function(mod) mod@y_training)
    y_test <- lapply(models, function(mod) mod@y_test)
    predicted_training <- lapply(models, function(mod) mod@predicted_training)
    predicted_test <- lapply(models, function(mod) mod@predicted_test)
    if (type == "Classification") {
      predicted_prob_training <- lapply(
        models,
        function(mod) mod@predicted_prob_training
      )
      predicted_prob_test <- lapply(
        models,
        function(mod) mod@predicted_prob_test
      )
    } else {
      predicted_prob_training <- predicted_prob_test <- NULL
    }
    mod <- make_SupervisedRes(
      algorithm = algorithm,
      type = type,
      models = models,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      outer_resampler = outer_resampler,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_test = predicted_prob_test,
      xnames = names(x)[-ncols],
      varimp = lapply(models, \(mod) mod@varimp),
      question = question
    )
  }

  # Outro ----
  if (verbosity > 0L) {
    message()
    print(mod)
    message()
  }
  if (!is.null(outdir)) {
    rt_save(mod, outdir = outdir, file_prefix = paste0("train_", algorithm))
  }
  outro(
    start_time,
    verbosity = verbosity,
    sink_off = ifelse(is.null(log_file), FALSE, TRUE)
  )
  mod
} # /rtemis::train
