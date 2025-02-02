# S7_Supervised.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# => ?extra used

# Supervised ----
#' @title Supervised
#'
#' @description
#' Superclass for supervised learning models.
#'
#' @author EDG
Supervised <- new_class(
  name = "Supervised",
  properties = list(
    algorithm = class_character,
    model = class_any,
    type = class_character,
    preprocessor_parameters = PreprocessorParameters | NULL,
    hyperparameters = Hyperparameters,
    tuner_parameters = TunerParameters | NULL,
    y_training = class_any,
    y_validation = class_any,
    y_testing = class_any,
    predicted_training = class_any,
    predicted_validation = class_any,
    predicted_testing = class_any,
    metrics_training = Metrics,
    metrics_validation = Metrics | NULL,
    metrics_testing = Metrics | NULL,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(algorithm,
                         model,
                         type,
                         preprocessor_parameters,
                         hyperparameters,
                         tuner_parameters,
                         y_training,
                         y_validation,
                         y_testing,
                         predicted_training,
                         predicted_validation,
                         predicted_testing,
                         metrics_training,
                         metrics_validation,
                         metrics_testing,
                         xnames,
                         varimp,
                         question,
                         extra) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      model = model,
      type = type,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      y_training = y_training,
      y_validation = y_validation,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      metrics_training = metrics_training,
      metrics_validation = metrics_validation,
      metrics_testing = metrics_testing,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /Supervised

# Print Supervised ----
#' Print Supervised
#'
#' Print Supervised object
#'
#' @param x Supervised object.
#' @param ... Not used
#'
#' @author EDG
#' @export
print.Supervised <- function(x, ...) {
  cat(gray(".:"))
  objcat(paste(x@type, "Model"))
  cat("  ",
    hilite(x@algorithm),
    " (", get_alg_desc(x@algorithm), ")\n\n",
    sep = ""
  )
  # padcat("Training Metrics")
  print(x@metrics_training)
  if (length(x@metrics_validation) > 0) {
    cat("\n")
    print(x@metrics_validation)
  }
  if (length(x@metrics_testing) > 0) {
    cat("\n")
    print(x@metrics_testing)
  }
} # /print.Supervised
method(print, Supervised) <- function(x) {
  print.Supervised(x)
}

# Plot Variable Importance ----
plot_varimp <- new_generic("plot_varimp", "x")
method(plot_varimp, Supervised) <- function(x,
                                            theme = rtTheme,
                                            filename = NULL, ...) {
  if (is.null(x@varimp)) {
    msg2(hilite2("No variable importance available."))
    return(invisible(NULL))
  }
  dplot3_varimp(x@varimp, theme = theme, filename = filename, ...)
} # /plot_varimp.Supervised

# Describe Supervised ----
describe <- new_generic("describe", "x")
method(describe, Supervised) <- function(x) {
  type <- x@type
  algorithm <- select_learn(x@algorithm, desc = TRUE)
  cat(algorithm, " was used for ", tolower(type), ".\n", sep = "")
  desc <- paste0(algorithm, " was used for ", tolower(type), ".")

  # Tuning ----
  if (length(x@tuner_parameters) > 0) {
    res <- x@tuner_parameters$resample_params
    n_resamples <- res$n_resamples
    resampler <- res$resampler
    resamples <- switch(resampler,
      strat.sub = " stratified subsamples",
      bootstrap = " bootstraps",
      strat.boot = " stratified bootstraps",
      kfold = "-fold crossvalidation",
      "custom resamples"
    )
    cat(" Hyperparameter tuning was performed using ",
      n_resamples, resamples, ".\n",
      sep = ""
    )
    desc <- paste0(
      desc,
      " Hyperparameter tuning was performed using ",
      n_resamples, resamples, "."
    )
    params <- x@tuner_parameters$params$search
    search.index <- which(lapply(params, length) > 1)
    fixed.index <- which(lapply(params, length) == 1)
    fixed <- searched <- list()
    for (i in seq_along(search.index)) {
      searched[[i]] <- params[[search.index[i]]]
    }
    names(searched) <- names(params)[search.index]
    for (i in seq(fixed.index)) {
      fixed[[i]] <- params[[fixed.index[i]]]
    }
    names(fixed) <- names(params)[fixed.index]
    metric <- x@tuner_parameters$metric

    if (length(fixed) > 0) {
      cat("The following parameters were fixed:\n")
      printls(fixed)
    }
    if (length(searched) > 0) {
      cat("Grid search was performed on:\n")
      printls(searched)
    }
    cat(metric, "was", ifelse(x@tuner_parameters$maximize, "maximized", "minimized"), "with:\n")
    printls(x@tuner_parameters$best_tune)
  }

  # Metrics ----
  if (type == "Classification") {
    cat(
      "Balanced accuracy was",
      ddSci(x@metrics_training$Overall$Balanced_Accuracy),
      "on the training set"
    )
    desc <- paste(
      desc, "Balanced accuracy was",
      ddSci(x@metrics_training$Overall$Balanced_Accuracy),
      "in the training set"
    )
    if (!is.null(x@metrics_testing$Overall$Balanced_Accuracy)) {
      cat(
        " and",
        ddSci(x@metrics_testing$Overall$Balanced_Accuracy),
        "in the testing set."
      )
      desc <- paste(
        desc, "and",
        ddSci(x@metrics_testing$Overall$Balanced_Accuracy),
        "in the testing set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  } else if (type == "Regression") {
    cat(
      "R-squared was",
      ddSci(x@metrics_training$Rsq),
      "in the training set"
    )
    desc <- paste(
      desc, "R-squared was",
      ddSci(x@metrics_training$Rsq),
      "on the training set"
    )
    if (!is.null(x@metrics_testing$Balanced_Accuracy)) {
      cat(
        " and",
        ddSci(x@metrics_testing$Rsq), "in the testing."
      )
      desc <- paste(
        desc, "and",
        ddSci(x@metrics_testing$Rsq),
        "on the testing set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  }
  cat("\n")
  invisible(desc)
} # / describe

# Classification ----
#' @title Classification
#' @description
#' Supervised subclass for classification models.
#'
#' @author EDG
#' @export
Classification <- new_class(
  name = "Classification",
  parent = Supervised,
  properties = list(
    predicted_prob_training = class_double | NULL,
    predicted_prob_validation = class_double | NULL,
    predicted_prob_testing = class_double | NULL
  ),
  constructor = function(algorithm = NULL,
                         model = NULL,
                         preprocessor_parameters = NULL, # PreprocessorParameters
                         hyperparameters = NULL, # Hyperparameters
                         tuner_parameters = NULL, # TunerParameters
                         y_training = NULL,
                         y_validation = NULL,
                         y_testing = NULL,
                         predicted_training = NULL,
                         predicted_validation = NULL,
                         predicted_testing = NULL,
                         xnames = NULL,
                         varimp = NULL,
                         question = NULL,
                         extra = NULL,
                         predicted_prob_training = NULL,
                         predicted_prob_validation = NULL,
                         predicted_prob_testing = NULL) {
    metrics_training <- classification_metrics(
      true = y_training,
      predicted = predicted_training,
      predicted_prob = predicted_prob_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      classification_metrics(
        true = y_validation,
        predicted = predicted_validation,
        predicted_prob = predicted_prob_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_testing <- if (!is.null(y_testing)) {
      classification_metrics(
        true = y_testing,
        predicted = predicted_testing,
        predicted_prob = predicted_prob_testing,
        sample = "Testing"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Classification",
        preprocessor_parameters = preprocessor_parameters,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        y_training = y_training,
        y_validation = y_validation,
        y_testing = y_testing,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_testing = predicted_testing,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_testing = metrics_testing,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_testing = predicted_prob_testing
    )
  }
) # /Clasiffication

# Regression ----
#' @title Regression
#' @description
#' Supervised subclass for regression models.
#'
#' @author EDG
#' @export
Regression <- new_class(
  name = "Regression",
  parent = Supervised,
  properties = list(
    se_training = NULL,
    se_validation = NULL,
    se_testing = NULL
  ),
  constructor = function(algorithm = NULL,
                         model = NULL,
                         preprocessor_parameters = NULL, # PreprocessorParameters
                         hyperparameters = NULL, # Hyperparameters
                         tuner_parameters = NULL, # TunerParameters
                         y_training = NULL,
                         y_validation = NULL,
                         y_testing = NULL,
                         predicted_training = NULL,
                         predicted_validation = NULL,
                         predicted_testing = NULL,
                         se_training = NULL,
                         se_validation = NULL,
                         se_testing = NULL,
                         xnames = NULL,
                         varimp = NULL,
                         question = NULL,
                         extra = NULL) {
    metrics_training <- regression_metrics(
      y_training, predicted_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      regression_metrics(
        y_validation, predicted_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_testing <- if (!is.null(y_testing)) {
      regression_metrics(
        y_testing, predicted_testing,
        sample = "Testing"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Regression",
        preprocessor_parameters = preprocessor_parameters,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        y_training = y_training,
        y_validation = y_validation,
        y_testing = y_testing,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_testing = predicted_testing,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_testing = metrics_testing,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_validation = se_validation,
      se_testing = se_testing
    )
  }
) # /Regression

# Make Supervised ----
make_Supervised <- function(
    algorithm = NULL,
    model = NULL,
    preprocessor_parameters = list(),
    hyperparameters = list(),
    tuner_parameters = list(),
    y_training = NULL,
    y_validation = NULL,
    y_testing = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_testing = NULL,
    predicted_prob_training = NULL,
    predicted_prob_validation = NULL,
    predicted_prob_testing = NULL,
    se_training = NULL,
    se_validation = NULL,
    se_testing = NULL,
    xnames = character(),
    varimp = NULL,
    question = character(),
    extra = NULL) {
  # Supervised ----
  if (is.factor(y_training)) {
    Classification(
      algorithm = algorithm,
      model = model,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      y_training = y_training,
      y_validation = y_validation,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_testing = predicted_prob_testing,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  } else {
    Regression(
      algorithm = algorithm,
      model = model,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      y_training = y_training,
      y_validation = y_validation,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      se_training = se_training,
      se_validation = se_validation,
      se_testing = se_testing,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /make_Supervised

# Write Supervised ----
write_Supervised <- function(object,
                             outdir = NULL,
                             save_mod = FALSE,
                             theme = rtTheme,
                             verbosity = 1L) {
  if (verbosity > 0L) {
    print(object)
  }
  if (!is.null(outdir)) {
    filename_train <- paste0(outdir, object@algorithm, "_Predicted_Training_vs_True.pdf")
    if (!is.null(object@y_testing)) {
      filename_test <- paste0(outdir, object@algorithm, "_Predicted_Testing_vs_True.pdf")
    }
  } else {
    filename_train <- filename_test <- NULL
  }

  if (save_mod) {
    rt_save(rt, outdir, verbosity = verbosity)
  }
} # /write_Supervised

# SupervisedCV ----
# fields metrics_training/metrics_validation/metrics_testing
# could be active bindings that are collected from @models
#' SupervisedCV
#'
#' @description
#' Superclass for cross-validated supervised learning models.
#'
#' @author EDG
#' @export
SupervisedCV <- new_class(
  name = "SupervisedCV",
  properties = list(
    algorithm = class_character,
    models = class_list,
    type = class_character,
    preprocessor_parameters = PreprocessorParameters | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_parameters = TunerParameters | NULL,
    crossvalidation_parameters = ResamplerParameters,
    y_training = class_any,
    y_testing = class_any,
    predicted_training = class_any,
    predicted_testing = class_any,
    metrics_training = MetricsCV,
    metrics_testing = MetricsCV,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(algorithm,
                         models,
                         type,
                         preprocessor_parameters,
                         hyperparameters,
                         tuner_parameters,
                         crossvalidation_parameters,
                         y_training,
                         y_testing,
                         predicted_training,
                         predicted_testing,
                         metrics_training,
                         metrics_testing,
                         metrics_training_mean,
                         metrics_testing_mean,
                         xnames,
                         varimp,
                         question,
                         extra) {
    new_object(
      S7::S7_object(),
      algorithm = algorithm,
      models = models,
      type = models[[1]]@type,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      crossvalidation_parameters = crossvalidation_parameters,
      y_training = y_training,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_testing = predicted_testing,
      metrics_training = metrics_training,
      metrics_testing = metrics_testing,
      # metrics_training_mean = metrics_training_mean,
      # metrics_testing_mean = metrics_testing_mean,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /SupervisedCV

# Print SupervisedCV ----
method(print, SupervisedCV) <- function(x) {
  cat(gray(".:"))
  objcat(paste("Crossvalidated", x@type, "Model"))
  cat("  ",
    hilite(x@algorithm),
    " (", get_alg_desc(x@algorithm), ")\n",
    sep = ""
  )
  cat("\n  Mean (SD) across", length(x@models), "models.\n\n")
  print(x@metrics_training)
  cat("\n")
  print(x@metrics_testing)
} # /SupervisedCV

# ClassificationCV ----
#' @title ClassificationCV
#'
#' @description
#' SupervisedCV subclass for cross-validated classification models.
#'
#' @author EDG
#' @export
ClassificationCV <- new_class(
  name = "ClassificationCV",
  parent = SupervisedCV,
  properties = list(
    predicted_prob_training = class_any,
    predicted_prob_testing = class_any
  ),
  constructor = function(algorithm,
                         models,
                         preprocessor_parameters,
                         hyperparameters,
                         tuner_parameters,
                         crossvalidation_parameters,
                         y_training,
                         y_validation = NULL,
                         y_testing = NULL,
                         predicted_training = NULL,
                         predicted_testing = NULL,
                         predicted_prob_training = NULL,
                         predicted_prob_testing = NULL,
                         xnames = NULL,
                         varimp = NULL,
                         question = NULL,
                         extra = NULL) {
    # metrics_training_l <- lapply(models, function(mod) mod@metrics_training)
    # metrics_training_l <- lapply(models, function(mod) mod@metrics_testing)
    # metrics_training_mean <- colMeans(do.call(rbind, lapply(metrics_training_l, function(x) x$Overall)))
    # metrics_testing_mean <- colMeans(do.call(rbind, lapply(metrics_training_l, function(x) x$Overall)))
    metrics_training <- ClassificationMetricsCV(
      sample = "Training",
      cv_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_testing <- ClassificationMetricsCV(
      sample = "Testing",
      cv_metrics = lapply(models, function(mod) mod@metrics_testing)
    )
    new_object(
      SupervisedCV(
        algorithm = algorithm,
        models = models,
        type = "Classification",
        preprocessor_parameters = preprocessor_parameters,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        crossvalidation_parameters = crossvalidation_parameters,
        y_training = y_training,
        y_testing = y_testing,
        predicted_training = predicted_training,
        predicted_testing = predicted_testing,
        metrics_training = metrics_training,
        metrics_testing = metrics_testing,
        # metrics_training_mean = metrics_training_mean,
        # metrics_testing_mean = metrics_testing_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_testing = predicted_prob_testing
    )
  }
) # /ClassificationCV

# RegressionCV ----
#' @title RegressionCV
#'
#' @description
#' SupervisedCV subclass for cross-validated regression models.
#'
#' @author EDG
#' @export
RegressionCV <- new_class(
  name = "RegressionCV",
  parent = SupervisedCV,
  properties = list(
    se_training = class_any,
    se_validation = class_any,
    se_testing = class_any
  ),
  constructor = function(algorithm,
                         models,
                         preprocessor_parameters,
                         hyperparameters,
                         tuner_parameters,
                         crossvalidation_parameters,
                         y_training,
                         y_validation = NULL,
                         y_testing = NULL,
                         predicted_training = NULL,
                         predicted_testing = NULL,
                         se_training = NULL,
                         se_testing = NULL,
                         xnames = NULL,
                         varimp = NULL,
                         question = NULL,
                         extra = NULL) {
    metrics_training <- lapply(models, function(mod) mod@metrics_training@metrics)
    metrics_testing <- lapply(models, function(mod) mod@metrics_testing@metrics)
    # metrics_training <- Filter(
    #   Negate(is.null), lapply(models, function(mod) mod@metrics_training@metrics)
    # )
    # metrics_testing <- Filter(
    #   Negate(is.null), lapply(models, function(mod) mod@metrics_testing@metrics)
    # )
    # metrics_training_mean <- colMeans(do.call(rbind, metrics_training))
    # metrics_testing_mean <- if (length(metrics_testing) > 0) {
    #   colMeans(do.call(rbind, metrics_testing))
    # } else {
    #   NULL
    # }
    # metrics_testing_mean <- try(colMeans(do.call(rbind, metrics_testing)), silent = TRUE)
    metrics_training <- RegressionMetricsCV(
      sample = "Training",
      cv_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_testing <- RegressionMetricsCV(
      sample = "Testing",
      cv_metrics = lapply(models, function(mod) mod@metrics_testing)
    )
    new_object(
      SupervisedCV(
        algorithm = algorithm,
        models = models,
        type = "Regression",
        preprocessor_parameters = preprocessor_parameters,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        crossvalidation_parameters = crossvalidation_parameters,
        y_training = y_training,
        y_testing = y_testing,
        predicted_training = predicted_training,
        predicted_testing = predicted_testing,
        metrics_training = metrics_training,
        metrics_testing = metrics_testing,
        # metrics_training_mean = metrics_training_mean,
        # metrics_testing_mean = metrics_testing_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_testing = se_testing
    )
  }
) # /RegressionCV

# Make SupervisedCV ----
#' Make SupervisedCV
#'
#' @author EDG
#' @keywords internal
# make_SupervisedCV <- function(algorithm, models, hyperparameters) {
#   SupervisedCV(
#     algorithm = algorithm,
#     models = models,
#     hyperparameters = hyperparameters
#   )
# } # /make_SupervisedCV
# => predict method for {Regression,Classification}CV with average_fn = "mean"

make_SupervisedCV <- function(
    algorithm,
    type,
    models,
    preprocessor_parameters,
    hyperparameters,
    tuner_parameters,
    crossvalidation_parameters,
    y_training,
    y_testing,
    predicted_training,
    predicted_testing,
    predicted_prob_training,
    predicted_prob_testing,
    se_training = NULL,
    se_testing = NULL,
    xnames = character(),
    varimp = NULL,
    question = character(),
    extra = NULL) {
  # Supervised ----
  if (type == "Classification") {
    ClassificationCV(
      algorithm = algorithm,
      models = models,
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
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  } else {
    RegressionCV(
      algorithm = algorithm,
      models = models,
      preprocessor_parameters = preprocessor_parameters,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      crossvalidation_parameters = crossvalidation_parameters,
      y_training = y_training,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_testing = predicted_testing,
      se_training = se_training,
      se_testing = se_testing,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /make_SupervisedCV
