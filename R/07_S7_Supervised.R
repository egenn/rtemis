# S7_Supervised.R
# ::rtemis::
# 2025 EDG rtemis.org

# Refs & Res
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7
# https://rconsortium.github.io/S7/articles/classes-objects.html?q=computed#computed-properties
# https://utf8-icons.com/

# => ?extra used

# Supervised ----
#' @title Supervised
#'
#' @description
#' Superclass for supervised learning models.
#'
#' @author EDG
#' @noRd
Supervised <- new_class(
  name = "Supervised",
  properties = list(
    algorithm = class_character,
    model = class_any,
    type = class_character,
    preprocessor = Preprocessor | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner = Tuner | NULL,
    y_training = class_any,
    y_validation = class_any,
    y_test = class_any,
    predicted_training = class_any,
    predicted_validation = class_any,
    predicted_test = class_any,
    metrics_training = Metrics,
    metrics_validation = Metrics | NULL,
    metrics_test = Metrics | NULL,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(
    algorithm,
    model,
    type,
    preprocessor,
    hyperparameters,
    tuner,
    y_training,
    y_validation,
    y_test,
    predicted_training,
    predicted_validation,
    predicted_test,
    metrics_training,
    metrics_validation,
    metrics_test,
    xnames,
    varimp,
    question,
    extra
  ) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      model = model,
      type = type,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      metrics_training = metrics_training,
      metrics_validation = metrics_validation,
      metrics_test = metrics_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /Supervised

# Predict Supervised ----
#' Predict Supervised
#'
#' Predict Method for Supervised objects
#'
#' @param object Supervised object.
#' @param newdata data.frame or similar: New data to predict.
#'
#' @noRd
method(predict, Supervised) <- function(object, newdata, ...) {
  check_inherits(newdata, "data.frame")
  predict_fn <- get_predict_fn(object@algorithm)
  do_call(
    predict_fn,
    list(model = object@model, newdata = newdata, type = object@type)
  )
} # /predict.Supervised

# Fitted Supervised ----
#' Fitted Supervised
#'
#' Fitted Method for Supervised objects
#'
#' @param object Supervised object.
#'
#' @keywords internal
#' @noRd
method(fitted, Supervised) <- function(object, ...) {
  object@predicted_training
} # /fitted.Supervised

# Standard Error Supervised ----
#' Standard Error Supervised
#'
#' Standard Error Method for Supervised objects
#'
#' @param object Supervised object.
#'
#' @keywords internal
#' @noRd
method(se, Supervised) <- function(x, ...) {
  x@se_training
} # /se.Supervised

# Make Supervised props `$`- accessible ----
method(`$`, Supervised) <- function(x, name) {
  prop(x, name)
}

# `$`-autocomplete Supervised props ----
method(`.DollarNames`, Supervised) <- function(x, pattern = "") {
  all_names <- names(props(x))
  grep(pattern, all_names, value = TRUE)
}

# Make Supervised props `[[`- accessible ----
method(`[[`, Supervised) <- function(x, name) {
  prop(x, name)
}

# Print Supervised ----
#' Print Supervised
#'
#' Print Supervised object
#'
#' @param x Supervised object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
print.Supervised <- function(x, ...) {
  cat(gray(".:"))
  objcat(paste(x@type, "Model"))
  cat(
    "  ",
    hilite(x@algorithm),
    " (",
    get_alg_desc(x@algorithm),
    ")\n",
    sep = ""
  )
  if (!is.null(x@tuner)) {
    cat(
      "  ",
      magenta("\U2699", bold = TRUE),
      " Tuned using ",
      desc(x@tuner),
      ".\n\n",
      sep = ""
    )
  } else {
    cat("\n")
  }
  if (prop_exists(x, "calibration_model")) {
    cat(
      "  ",
      green("\U27CB", bold = TRUE),
      " Calibrated using ",
      get_alg_desc(x@calibration_model@algorithm),
      ".\n\n",
      sep = ""
    )
  }
  if (prop_exists(x, "calibration_models")) {
    cat(
      "  ",
      green("\U27CB", bold = TRUE),
      " Calibrated using ",
      get_alg_desc(x@calibration_models[[1]]@algorithm),
      " with ",
      ".\n\n",
      sep = ""
    )
  }
  print(x@metrics_training)
  if (length(x@metrics_validation) > 0) {
    cat("\n")
    print(x@metrics_validation)
  }
  if (length(x@metrics_test) > 0) {
    cat("\n")
    print(x@metrics_test)
  }
  invisible(x)
} # /print.Supervised
method(print, Supervised) <- function(x, ...) {
  print.Supervised(x)
}

# Plot Variable Importance ----
#' Plot Variable Importance
#'
#' @description
#' Plot Variable Importance for Supervised objects.
#'
#' @param x Supervised object.
# @param theme Theme to use for the plot.
# @param filename Character: Filename to save the plot to. If NULL, the plot is not saved.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @export
plot_varimp <- new_generic("plot_varimp", "x")
method(plot_varimp, Supervised) <- function(
  x,
  theme = rtemis_theme,
  filename = NULL,
  ...
) {
  if (is.null(x@varimp)) {
    msg2(hilite2("No variable importance available."))
    return(invisible(NULL))
  }
  draw_varimp(x@varimp, theme = theme, filename = filename, ...)
} # /plot_varimp.Supervised

# Describe Supervised ----
method(describe, Supervised) <- function(x) {
  type <- x@type
  algorithm <- get_alg_desc(x@algorithm)
  cat(algorithm, " was used for ", tolower(type), ".\n", sep = "")
  desc <- paste0(algorithm, " was used for ", tolower(type), ".")

  # Tuning ----
  if (length(x@tuner) > 0) {
    describe(x@tuner)
  }

  # Metrics ----
  if (type == "Classification") {
    cat(
      "Balanced accuracy was",
      ddSci(x@metrics_training[["Overall"]][["Balanced_Accuracy"]]),
      "on the training set"
    )
    desc <- paste(
      desc,
      "Balanced accuracy was",
      ddSci(x@metrics_training[["Overall"]][["Balanced_Accuracy"]]),
      "in the training set"
    )
    if (!is.null(x@metrics_test[["Overall"]][["Balanced_Accuracy"]])) {
      cat(
        " and",
        ddSci(x@metrics_test[["Overall"]][["Balanced_Accuracy"]]),
        "in the test set."
      )
      desc <- paste(
        desc,
        "and",
        ddSci(x@metrics_test[["Overall"]][["Balanced_Accuracy"]]),
        "in the test set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  } else if (type == "Regression") {
    cat(
      "R-squared was",
      ddSci(x@metrics_training[["Rsq"]]),
      "in the training set"
    )
    desc <- paste(
      desc,
      "R-squared was",
      ddSci(x@metrics_training[["Rsq"]]),
      "on the training set"
    )
    if (!is.null(x@metrics_test[["Rsq"]])) {
      cat(
        " and",
        ddSci(x@metrics_test[["Rsq"]]),
        "in the test."
      )
      desc <- paste(
        desc,
        "and",
        ddSci(x@metrics_test[["Rsq"]]),
        "on the test set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  }
  cat("\n")
  invisible(desc)
} # / describe


# Calibration ----
#' @title Calibration
#'
#' @description
#' Calibration class.
#'
#' @author EDG
#' @noRd
Calibration <- new_class(
  name = "Calibration",
  properties = list(
    model = Supervised,
    brier_score_delta_training = class_numeric | NULL,
    brier_score_delta_test = class_numeric | NULL
  )
) # /Calibration

# Prin Calibration ----
method(print, Calibration) <- function(x, ...) {
  cat(gray(".:"))
  objcat("Calibration Model")
  cat(
    "  ",
    hilite(x@algorithm),
    " (",
    get_alg_desc(x@algorithm),
    ")\n",
    sep = ""
  )
}

# CalibrationCV ----
CalibrationCV <- new_class(
  name = "CalibrationCV",
  properties = list(
    models = class_list,
    resampler_parameters = ResamplerParameters
    # brier_score_delta_training = class_numeric | NULL,
    # brier_score_delta_test = class_numeric | NULL
  )
  # constructor = function(models, resampler_parameters) {

  # }
) # /CalibrationCV

# Print CalibrationCV ----
method(print, CalibrationCV) <- function(x, ...) {
  cat(gray(".:"))
  objcat("Cross-validated Calibration Model")
  cat(
    "  ",
    hilite(x@algorithm),
    " (",
    get_alg_desc(x@algorithm),
    ")\n",
    sep = ""
  )
}


# Get explain function ----
#' Get explain function
#'
#' @param algorithm Character: Algorithm name.
#'
#' @keywords internal
#' @noRd
get_explain_fn <- function(algorithm) {
  paste0("explain_", algorithm)
}

# Explain Supervised ----
#' Explain Supervised
#'
#' Explain Supervised Learning Model
#'
#' The explain API is under development.
#' Different models require different inputs.
#' Currently, different explain methods output different objects.
#' This will likely be replaced in the future using a custom S7 class.
#'
#' @param model Supervised object.
#' @param x data.frame or similar: Data to explain.
#' @param dat_training data.frame or similar: Training data.
#' @param method Character: Method to use.
#'
#' @export
explain <- function(model, x, dat_training = NULL, method = NULL) {
  check_is_S7(model, Supervised)
  check_inherits(x, "data.frame")
  explain_fn <- get_explain_fn(model@algorithm)
  # Test if a function by that name exists in the package
  if (!exists(explain_fn, envir = asNamespace("rtemis"))) {
    stop(paste0(
      "Explain support for ",
      model@algorithm,
      " is not currently available."
    ))
  }
  do_call(
    explain_fn,
    list(model = model, x = x, dat_training = dat_training, method = method)
  )
} # /explain


# Classification ----
#' @title Classification
#'
#' @description
#' Supervised subclass for classification models.
#'
#' @author EDG
#' @noRd
Classification <- new_class(
  name = "Classification",
  parent = Supervised,
  properties = list(
    predicted_prob_training = class_double | NULL,
    predicted_prob_validation = class_double | NULL,
    predicted_prob_test = class_double | NULL,
    binclasspos = class_integer
  ),
  constructor = function(
    algorithm = NULL,
    model = NULL,
    preprocessor = NULL, # Preprocessor
    hyperparameters = NULL, # Hyperparameters
    tuner = NULL, # Tuner
    y_training = NULL,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL,
    predicted_prob_training = NULL,
    predicted_prob_validation = NULL,
    predicted_prob_test = NULL,
    binclasspos = 2L
  ) {
    metrics_training <- classification_metrics(
      true_labels = y_training,
      predicted_labels = predicted_training,
      predicted_prob = predicted_prob_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      classification_metrics(
        true_labels = y_validation,
        predicted_labels = predicted_validation,
        predicted_prob = predicted_prob_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_test <- if (!is.null(y_test)) {
      classification_metrics(
        true_labels = y_test,
        predicted_labels = predicted_test,
        predicted_prob = predicted_prob_test,
        sample = "Test"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Classification",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner = tuner,
        y_training = y_training,
        y_validation = y_validation,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_test = metrics_test,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      binclasspos = binclasspos
    )
  }
) # /Classification


# CalibratedClassification ----
#' @title CalibratedClassification
#'
#' @description
#' Classification subclass for calibrated classification models.
#' The classification_model can be trained on any data, ideally different from any data used by
#' the classification model.
#'
#' @author EDG
#' @noRd
CalibratedClassification <- new_class(
  name = "CalibratedClassification",
  parent = Classification,
  properties = list(
    calibration_model = Supervised,
    predicted_training_calibrated = class_vector,
    predicted_validation_calibrated = class_vector | NULL,
    predicted_test_calibrated = class_vector | NULL,
    predicted_prob_training_calibrated = class_double,
    predicted_prob_validation_calibrated = class_double | NULL,
    predicted_prob_test_calibrated = class_double | NULL,
    metrics_training_calibrated = Metrics,
    metrics_validation_calibrated = Metrics | NULL,
    metrics_test_calibrated = Metrics | NULL
  ),
  constructor = function(classification_model, calibration_model) {
    # Predict calibrated probabilities of classification model datasets
    predicted_prob_training_calibrated <- predict(
      calibration_model,
      data.frame(
        predicted_probabilities = classification_model@predicted_prob_training
      ),
    )
    predicted_prob_validation_calibrated <- if (
      !is.null(classification_model@predicted_prob_validation)
    ) {
      predict(
        calibration_model,
        data.frame(
          predicted_probabilities = classification_model@predicted_prob_validation
        )
      )
    } else {
      NULL
    }
    predicted_prob_test_calibrated <- if (
      !is.null(classification_model@predicted_prob_test)
    ) {
      predict(
        calibration_model,
        data.frame(
          predicted_probabilities = classification_model@predicted_prob_test
        )
      )
    } else {
      NULL
    }
    # Predict calibrated labels of classification model datasets
    predicted_training_calibrated <- prob2categorical(
      predicted_prob_training_calibrated,
      levels = levels(classification_model@y_training)
    )
    predicted_validation_calibrated <- if (
      !is.null(classification_model@predicted_prob_validation)
    ) {
      prob2categorical(
        predicted_prob_validation_calibrated,
        levels = levels(classification_model@y_validation)
      )
    } else {
      NULL
    }
    predicted_test_calibrated <- if (
      !is.null(classification_model@predicted_prob_test)
    ) {
      prob2categorical(
        predicted_prob_test_calibrated,
        levels = levels(classification_model@y_test)
      )
    } else {
      NULL
    }
    metrics_training_calibrated <- classification_metrics(
      true_labels = classification_model@y_training,
      predicted_labels = predicted_training_calibrated,
      predicted_prob = predicted_prob_training_calibrated,
      sample = "Calibrated Training"
    )
    metrics_validation_calibrated <- if (
      !is.null(classification_model@y_validation)
    ) {
      classification_metrics(
        true_labels = classification_model@y_validation,
        predicted_labels = predicted_validation_calibrated,
        predicted_prob = predicted_prob_validation_calibrated,
        sample = "Calibrated Validation"
      )
    } else {
      NULL
    }
    metrics_test_calibrated <- if (!is.null(classification_model@y_test)) {
      classification_metrics(
        true_labels = classification_model@y_test,
        predicted_labels = predicted_test_calibrated,
        predicted_prob = predicted_prob_test_calibrated,
        sample = "Calibrated Test"
      )
    } else {
      NULL
    }
    new_object(
      classification_model,
      calibration_model = calibration_model,
      predicted_training_calibrated = predicted_training_calibrated,
      predicted_validation_calibrated = predicted_validation_calibrated,
      predicted_test_calibrated = predicted_test_calibrated,
      predicted_prob_training_calibrated = predicted_prob_training_calibrated,
      predicted_prob_validation_calibrated = predicted_prob_validation_calibrated,
      predicted_prob_test_calibrated = predicted_prob_test_calibrated,
      metrics_training_calibrated = metrics_training_calibrated,
      metrics_validation_calibrated = metrics_validation_calibrated,
      metrics_test_calibrated = metrics_test_calibrated
    )
  }
) # rtemmis::CalibratedClassification


# Print CalibratedClassification ----
# method(print, CalibratedClassification) <- function(x, ...) {
#   cat(gray(".:"))
#   objcat("Classification Model")
#   cat("  ",
#     hilite(x@algorithm),
#     " (", get_alg_desc(x@algorithm), ")\n",
#     sep = ""
#   )
#   cat(
#     "  ", green("\U27CB", bold = TRUE),
#     " Calibrated using ", get_alg_desc(x@calibration_model@algorithm), ".\n\n",
#     sep = ""
#   )
#   # => raw => calibrated values for Overall
#   print(x@metrics_training_calibrated)
#   cat("\n")
#   print(x@metrics_test_calibrated)
# } # /print.CalibratedClassification

# Predict CalibratedClassification ----
method(predict, CalibratedClassification) <- function(object, newdata, ...) {
  check_inherits(newdata, "data.frame")
  predict_fn <- get_predict_fn(object@algorithm)
  # Get the classification model's predicted probabilities
  raw_prob <- do_call(predict_fn, list(model = object@model, newdata = newdata))
  # Get the calibration model's predicted probabilities
  cal_prob <- predict(
    object@calibration_model,
    newdata = data.frame(predicted_probabilities = raw_prob)
  )
} # rtemis::predict.CalibratedClassification

se_compat_algorithms <- c("GLM", "GAM")

# Regression ----
#' @title Regression
#' @description
#' Supervised subclass for regression models.
#'
#' @author EDG
#' @noRd
Regression <- new_class(
  name = "Regression",
  parent = Supervised,
  properties = list(
    se_training = class_double | NULL,
    se_validation = class_double | NULL,
    se_test = class_double | NULL
  ),
  constructor = function(
    algorithm = NULL,
    model = NULL,
    preprocessor = NULL, # Preprocessor
    hyperparameters = NULL, # Hyperparameters
    tuner = NULL, # Tuner
    y_training = NULL,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_test = NULL,
    se_training = NULL,
    se_validation = NULL,
    se_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    # Metrics ----
    metrics_training <- regression_metrics(
      y_training,
      predicted_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      regression_metrics(
        y_validation,
        predicted_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_test <- if (!is.null(y_test)) {
      regression_metrics(
        y_test,
        predicted_test,
        sample = "Test"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Regression",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner = tuner,
        y_training = y_training,
        y_validation = y_validation,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_test = metrics_test,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test
    )
  }
) # /Regression

# Plot Regression ----
#' Plot Regression
#'
#' @param x Regression object.
#' @param what Character vector: What to plot. Can include "training", "validation", "test", or
#' "all", which will plot all available.
#' @param fit Character: Algorithm to use to draw fit line.
#' @param theme Character or Theme: Theme to use for the plot.
#' @param labelify Logical: If TRUE, labelify the axis labels.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @export
plot.Regression <- function(
  x,
  what = "all",
  fit = "glm",
  theme = rtemis_theme,
  labelify = TRUE,
  ...
) {
  if (length(what) == 1 && what == "all") {
    what <- c("training", "validation", "test")
  }
  true <- paste0("y_", what)
  true_l <- Filter(
    Negate(is.null),
    sapply(true, function(z) prop(x, z))
  )
  predicted <- paste0("predicted_", what)
  predicted_l <- Filter(
    Negate(is.null),
    sapply(predicted, function(z) prop(x, z))
  )
  if (labelify) {
    names(predicted_l) <- labelify(names(predicted_l))
  }
  draw_fit(
    x = true_l,
    y = predicted_l,
    fit = fit,
    theme = theme,
    ...
  )
} # /rtemis::plot.Regression

method(plot, Regression) <- function(
  x,
  what = "all",
  fit = "glm",
  theme = rtemis_theme,
  ...
) {
  plot.Regression(
    x = x,
    what = what,
    fit = fit,
    theme = theme,
    ...
  )
}

# Plot Classification ----
#' Plot Classification
#'
#' @param x Classification object.
#' @param what Character vector: What to plot. "training", "validation", "test"
#' @param theme Character or Theme: Theme to use for the plot.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @export
plot.Classification <- function(x, what = NULL, theme = "darkgraygrid", ...) {
  if (is.null(what)) {
    if (!is.null(x@metrics_test)) {
      what <- "test"
    } else if (!is.null(x@metrics_validation)) {
      what <- "validation"
    } else {
      what <- "training"
    }
  }
  .confmat <- if (what == "training") {
    x@metrics_training
  } else if (what == "validation") {
    x@metrics_validation
  } else if (what == "test") {
    x@metrics_test
  }
  draw_conf(
    .confmat,
    theme = theme,
    ylab = labelify(paste("Predicted", what)),
    ...
  )
}
method(plot, Classification) <- function(
  x,
  what = NULL,
  theme = "darkgraygrid",
  ...
) {
  plot.Classification(
    x = x,
    what = what,
    theme = theme,
    ...
  )
}

# plot_ROC Classification ----
method(plot_roc, Classification) <- function(
  x,
  what = NULL,
  theme = rtemis_theme,
  filename = NULL,
  ...
) {
  if (is.null(x@predicted_prob_training)) {
    msg2(hilite2("No predicted probabilities available."))
    return(invisible(NULL))
  }
  if (is.null(what)) {
    what <- if (!is.null(x@metrics_test)) {
      c("train", "test")
    } else {
      "training"
    }
  }
  labelsl <- probl <- list()

  if ("training" %in% what) {
    labelsl[["training"]] <- x@y_training
    probl[["training"]] <- x@predicted_prob_training
  }
  if ("test" %in% what && !is.null(x@predicted_prob_test)) {
    labelsl[["test"]] <- x@y_test
    probl[["test"]] <- x@predicted_prob_test
  }

  draw_roc(
    true_labels = labelsl,
    predicted_prob = probl,
    theme = theme,
    filename = filename,
    ...
  )
} # /plot_ROC.Classification

# make_Supervised() ----
make_Supervised <- function(
  algorithm = NULL,
  model = NULL,
  preprocessor = NULL,
  hyperparameters = NULL,
  tuner = NULL,
  y_training = NULL,
  y_validation = NULL,
  y_test = NULL,
  predicted_training = NULL,
  predicted_validation = NULL,
  predicted_test = NULL,
  predicted_prob_training = NULL,
  predicted_prob_validation = NULL,
  predicted_prob_test = NULL,
  se_training = NULL,
  se_validation = NULL,
  se_test = NULL,
  xnames = character(),
  varimp = NULL,
  question = character(),
  extra = NULL,
  binclasspos = 2L
) {
  # Supervised ----
  if (is.factor(y_training)) {
    Classification(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      binclasspos = binclasspos
    )
  } else {
    Regression(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /make_Supervised

# Write Supervised ----
write_Supervised <- function(
  object,
  outdir = NULL,
  save_mod = FALSE,
  theme = rtemis_theme,
  verbosity = 1L
) {
  if (verbosity > 0L) {
    print(object)
  }
  if (!is.null(outdir)) {
    filename_train <- paste0(
      outdir,
      object@algorithm,
      "_Predicted_Training_vs_True.pdf"
    )
    if (!is.null(object@y_test)) {
      filename_test <- paste0(
        outdir,
        object@algorithm,
        "_predicted_test_vs_True.pdf"
      )
    }
  } else {
    filename_train <- filename_test <- NULL
  }

  if (save_mod) {
    rt_save(rt, outdir, verbosity = verbosity)
  }
} # /write_Supervised

# Present Regression ----
# present method for Regression objects
# Plot training + test metrics, if available, side by side using `plotly::subplot()`
# + run `describe()` on the object
method(present, Regression) <- function(
  x,
  what = c("training", "test"),
  theme = rtemis_theme,
  col = rtpalette(rtemis_palette)[1:2],
  filename = NULL
) {
  # Training set plot
  if ("training" %in% what) {
    plot_training <- plot(
      x,
      what = "training",
      ylab = "Predicted Training",
      theme = theme,
      col = col[1],
      group_names = "Training",
      legend_trace = FALSE
    )
  } else {
    plot_training <- NULL
  }
  # Test set plot
  if ("test" %in% what && !is.null(x@y_test)) {
    plot_test <- plot(
      x,
      what = "test",
      ylab = "Predicted Test",
      theme = theme,
      col = col[2],
      group_names = "Test",
      legend_trace = FALSE
    )
  } else {
    plot_test <- NULL
  }

  # Describe the model
  describe(x)

  # Combined plot
  # regression: scatter plots left to right
  plotly::subplot(
    plot_training,
    plot_test,
    nrows = 1L,
    shareX = FALSE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )
} # /rtemis::present.Regression

# Present Classification ----
# present method for Classification objects
# Plot training + test metrics if available, side by side
method(present, Classification) <- function(
  x,
  what = c("training", "test"),
  theme = rtemis_theme,
  filename = NULL
) {
  # Training set plot
  if ("training" %in% what) {
    plot_training <- plot(
      x,
      what = "training",
      theme = theme,
      xlab = "Predicted Training"
    )
  } else {
    plot_training <- NULL
  }
  # Test set plot
  if ("test" %in% what && !is.null(x@y_test)) {
    plot_test <- plot(
      x,
      what = "test",
      theme = theme,
      xlab = "Predicted Test"
    )
  } else {
    plot_test <- NULL
  }

  # Describe the model
  describe(x)

  # Combined plot
  # classification: confusion matrices side by side
  plotly::subplot(
    plot_training,
    plot_test,
    nrows = 1L,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.01
  )
} # /rtemis::present.Classification

# SupervisedCV ----
# fields metrics_training/metrics_validation/metrics_test
# could be active bindings that are collected from @models
#' SupervisedCV
#'
#' @description
#' Superclass for cross-validated supervised learning models.
#'
#' @author EDG
#' @noRd
SupervisedCV <- new_class(
  name = "SupervisedCV",
  properties = list(
    algorithm = class_character,
    models = class_list,
    type = class_character,
    preprocessor = Preprocessor | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_parameters = TunerParameters | NULL,
    outer_resampler = Resampler,
    y_training = class_any,
    y_test = class_any,
    predicted_training = class_any,
    predicted_test = class_any,
    metrics_training = MetricsCV,
    metrics_test = MetricsCV,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(
    algorithm,
    models,
    type,
    preprocessor,
    hyperparameters,
    tuner_parameters,
    outer_resampler,
    y_training,
    y_test,
    predicted_training,
    predicted_test,
    metrics_training,
    metrics_test,
    metrics_training_mean,
    metrics_test_mean,
    xnames,
    varimp,
    question,
    extra
  ) {
    new_object(
      S7::S7_object(),
      algorithm = algorithm,
      models = models,
      type = models[[1]]@type,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      outer_resampler = outer_resampler,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      metrics_training = metrics_training,
      metrics_test = metrics_test,
      # metrics_training_mean = metrics_training_mean,
      # metrics_test_mean = metrics_test_mean,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /SupervisedCV

# Print SupervisedCV ----
method(print, SupervisedCV) <- function(x, ...) {
  cat(gray(".:"))
  objcat(paste("Crossvalidated", x@type, "Model"))
  cat(
    "  ",
    hilite(x@algorithm),
    " (",
    get_alg_desc(x@algorithm),
    ")\n",
    sep = ""
  )
  cat(
    "  ",
    orange("\U27F3", bold = TRUE),
    " Tested using ",
    desc(x@outer_resampler),
    ".\n",
    sep = ""
  )
  if (!is.null(x@tuner_parameters)) {
    cat(
      "  ",
      magenta("\U2699", bold = TRUE),
      " Tuned using ",
      desc(x@tuner_parameters),
      ".\n\n",
      sep = ""
    )
  } else {
    cat("\n")
  }
  # if (x@type == "Classification" && !is.null(x@calibration)) {
  #   cat("  ", green("\U27CB", bold = TRUE), " Calibrated using ", get_alg_desc(x@calibration@model@algorithm), ".\n\n", sep = "")
  # }
  print(x@metrics_training)
  cat("\n")
  print(x@metrics_test)
  invisible(x)
} # /SupervisedCV

# Predict SupervisedCV ----
#' Predict SupervisedCV
#'
#' Predict Method for SupervisedCV objects
#'
#' @param object SupervisedCV object.
#' @param newdata data.frame or similar: New data to predict.
#' @param type Character: Type of prediction to output: "avg" applies `avg_fn` (default "mean") to
#' the predictions of individual models, "all" returns the predictions of all models in a
#' data.frame. "metrics" returns a list of data.frames with a) predictions from each model, b)
#' the mean of the predictions, and c) the standard deviation of the predictions.
#' @param ... Not used.
#'
#' @keywords internal
#' @noRd
method(predict, SupervisedCV) <- function(
  object,
  newdata,
  type = c("avg", "all", "metrics"),
  avg_fn = "mean",
  ...
) {
  check_inherits(newdata, "data.frame")
  type <- match.arg(type)
  predict_fn <- get_predict_fn(object@algorithm)

  predicted <- sapply(
    object@models,
    function(mod)
      do_call(
        predict_fn,
        list(model = mod, newdata = newdata, type = object@type)
      )
  ) # -> data.frame n cases x n resamples

  if (type == "all") {
    return(predicted)
  } else if (type == "avg") {
    return(apply(predicted, 1, avg_fn))
  } else if (type == "metrics") {
    mean_predictions <- apply(predicted, 2, mean)
    sd_predictions <- apply(predicted, 2, sd)
    return(list(
      predictions = predicted,
      mean = mean_predictions,
      sd = sd_predictions
    ))
  }
} # rtemis::predict.SupervisedCV

# ClassificationCV ----
#' @title ClassificationCV
#'
#' @description
#' SupervisedCV subclass for cross-validated classification models.
#'
#' @author EDG
#' @noRd
ClassificationCV <- new_class(
  name = "ClassificationCV",
  parent = SupervisedCV,
  properties = list(
    predicted_prob_training = class_any,
    predicted_prob_test = class_any
  ),
  constructor = function(
    algorithm,
    models,
    preprocessor,
    hyperparameters,
    tuner_parameters,
    outer_resampler,
    y_training,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_test = NULL,
    predicted_prob_training = NULL,
    predicted_prob_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    metrics_training <- ClassificationMetricsCV(
      sample = "Training",
      cv_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_test <- ClassificationMetricsCV(
      sample = "Test",
      cv_metrics = lapply(models, function(mod) mod@metrics_test)
    )
    new_object(
      SupervisedCV(
        algorithm = algorithm,
        models = models,
        type = "Classification",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        outer_resampler = outer_resampler,
        y_training = y_training,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_test = metrics_test,
        # metrics_training_mean = metrics_training_mean,
        # metrics_test_mean = metrics_test_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_test = predicted_prob_test
    )
  }
) # /ClassificationCV


# CalibratedClassificationCV ----
#' @title CalibratedClassificationCV
#'
#' @description
#' ClassificationCV subclass for calibrated classification models.
#' The calibration models are trained on resamples of the `ClassificationCV`'s test data.
#'
#' @author EDG
#' @noRd
# We use getter functions to avoid duplicating data
CalibratedClassificationCV <- new_class(
  name = "CalibratedClassificationCV",
  parent = ClassificationCV,
  properties = list(
    calibration_models = class_list, # => create CalibrationCV class
    predicted_training_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_training
        })
      }
    ),
    predicted_test_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_test
        })
      }
    ),
    predicted_prob_training_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_prob_training
        })
      }
    ),
    predicted_prob_test_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_prob_test
        })
      }
    ),
    metrics_training_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@metrics_training
        })
      }
    ),
    metrics_test_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@metrics_test
        })
      }
    )
  ),
  constructor = function(classificationcv_model, calibrations_models) {
    new_object(
      classificationcv_model,
      calibration_models = calibrations_models
    )
  }
) # /CalibratedClassificationCV


# Print CalibratedClassificationCV ----
method(print, CalibratedClassificationCV) <- function(x, ...) {
  cat(gray(".:"))
  objcat("Crossvalidated Classification Model")
  cat(
    "  ",
    hilite(x@algorithm),
    " (",
    get_alg_desc(x@algorithm),
    ")\n",
    sep = ""
  )
  cat(
    "  ",
    orange("\U27F3", bold = TRUE),
    " Tested using ",
    desc(x@outer_resampler),
    ".\n",
    sep = ""
  )
  if (!is.null(x@tuner_parameters)) {
    cat(
      "  ",
      magenta("\U2699", bold = TRUE),
      " Tuned using ",
      desc(x@tuner_parameters),
      ".\n",
      sep = ""
    )
  }
  cat(
    "  ",
    green("\U27CB", bold = TRUE),
    " Calibrated using ",
    get_alg_desc(x@calibration_models[[1]]@algorithm),
    " with ",
    desc(x@calibration_models[[1]]@outer_resampler@parameters),
    ".\n\n",
    sep = ""
  )
  print(x@metrics_training)
  cat("\n")
  print(x@metrics_test)
} # /print.CalibratedClassificationCV


# Predict CalibratedClassificationCV ----
# =>tocomplete
method(predict, CalibratedClassificationCV) <- function(object, newdata, ...) {
  check_inherits(newdata, "data.frame")
  raw_prob <- predict(object, newdata = newdata)
  # Get the classification model's predicted probabilities
  raw_prob <- do_call(
    class_predict_fn,
    list(model = object@model, newdata = newdata)
  )
  # Get the calibration model's predicted probabilities
  cal_prob <- lapply(object@calibration_models, function(mod) {
    predict(mod, data.frame(predicted_probabilities = raw_prob))
  })
} # rtemis::predict.CalibratedClassificationCV

# RegressionCV ----
#' @title RegressionCV
#'
#' @description
#' SupervisedCV subclass for cross-validated regression models.
#'
#' @author EDG
#' @noRd
RegressionCV <- new_class(
  name = "RegressionCV",
  parent = SupervisedCV,
  properties = list(
    se_training = class_any,
    se_validation = class_any,
    se_test = class_any
  ),
  constructor = function(
    algorithm,
    models,
    preprocessor,
    hyperparameters,
    tuner_parameters,
    outer_resampler,
    y_training,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_test = NULL,
    se_training = NULL,
    se_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    metrics_training <- lapply(
      models,
      function(mod) mod@metrics_training@metrics
    )
    metrics_test <- lapply(models, function(mod) mod@metrics_test@metrics)
    metrics_training <- RegressionMetricsCV(
      sample = "Training",
      cv_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_test <- RegressionMetricsCV(
      sample = "Test",
      cv_metrics = lapply(models, function(mod) mod@metrics_test)
    )
    new_object(
      SupervisedCV(
        algorithm = algorithm,
        models = models,
        type = "Regression",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        outer_resampler = outer_resampler,
        y_training = y_training,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_test = metrics_test,
        # metrics_training_mean = metrics_training_mean,
        # metrics_test_mean = metrics_test_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_test = se_test
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
  preprocessor,
  hyperparameters,
  tuner_parameters,
  outer_resampler,
  y_training,
  y_test,
  predicted_training,
  predicted_test,
  predicted_prob_training,
  predicted_prob_test,
  se_training = NULL,
  se_test = NULL,
  xnames = character(),
  varimp = NULL,
  question = character(),
  extra = NULL
) {
  # Supervised ----
  if (type == "Classification") {
    ClassificationCV(
      algorithm = algorithm,
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
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  } else {
    RegressionCV(
      algorithm = algorithm,
      models = models,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner_parameters = tuner_parameters,
      outer_resampler = outer_resampler,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      se_training = se_training,
      se_test = se_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /make_SupervisedCV

early_stopping_algs <- c("LightGBM", "LightRF", "LightRuleFit")


# LightRuleFit ----
#' @title LightRuleFit
#'
#' @description
#' Class for LightRuleFit models.
#'
#' @author EDG
#' @noRd
LightRuleFit <- new_class(
  name = "LightRuleFit",
  properties = list(
    model_lightgbm = Supervised,
    model_glmnet = Supervised,
    rules = class_character,
    rules_coefs = class_data.frame,
    rules_index = class_integer,
    rules_selected = class_character,
    rules_selected_formatted = class_character,
    rules_selected_formatted_coefs = class_data.frame,
    y_levels = class_character | NULL,
    xnames = class_character,
    complexity_metrics = class_data.frame
  )
) # /LightRuleFit

# Print LightRuleFit ----
method(print, LightRuleFit) <- function(x, ...) {
  objcat("rtemis LightRuleFit Model")
  cat(
    "Trained using ",
    hilite(x@model_lightgbm@algorithm),
    " and ",
    hilite(x@model_glmnet@algorithm),
    ".\n",
    sep = ""
  )
  cat("Selected", hilite(length(x@rules_selected)), "rules.\n")
  invisible(x)
} # /rtemis::print.LightRuleFit
