# rtemis Supervised Learning Classes
# ::rtemis::
# 2025 EDG rtemis.org

# References
# R6: https://r6.r-lib.org
# roxygen2 for R6: https://roxygen2.r-lib.org/articles/rd-other.html#r6

# Supervised ----
# rtemis Supervised Learning Class
Supervised <- R6::R6Class(
  classname = "Supervised",
  public = list(
    algorithm = NULL,
    model = NULL,
    type = NULL, # not included in initialize, set by child classes
    preprocessor = NULL,
    hyperparameters = NULL,
    tuner = NULL,
    y_training = NULL,
    y_validation = NULL,
    y_testing = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_testing = NULL,
    error_training = NULL,
    error_validation = NULL,
    error_testing = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL,
    session_info = NULL,
    ## Initialize Supervised ----
    initialize = function(algorithm = NULL,
                          model = NULL,
                          preprocessor = NULL,
                          hyperparameters = NULL,
                          tuner = NULL,
                          y_training = NULL,
                          y_validation = NULL,
                          y_testing = NULL,
                          predicted_training = NULL,
                          predicted_validation = NULL,
                          predicted_testing = NULL,
                          xnames = NULL,
                          varimp = NULL,
                          question = NULL,
                          extra = NULL) {
      self$algorithm <- algorithm
      self$model <- model
      inherits_check(preprocessor, "Preprocessor")
      self$preprocessor <- preprocessor
      inherits_check(hyperparameters, "Hyperparameters")
      self$hyperparameters <- hyperparameters
      self$tuner <- tuner
      self$y_training <- y_training
      self$y_validation <- y_validation
      self$y_testing <- y_testing
      self$predicted_training <- predicted_training
      self$predicted_validation <- predicted_validation
      self$predicted_testing <- predicted_testing
      self$xnames <- xnames
      self$varimp <- varimp
      self$question <- question
      self$extra <- extra
      self$session_info <- sessionInfo()
    },
    ## Print ----
    print = function() {
      "print method for Supervised"
      objcat(paste(self$type, "Model"))
      cat(
        hilite(self$algorithm),
        " (", select_learn(self$algorithm, desc = TRUE), ")\n",
        sep = ""
      )
      boxcat("Training Error")
      print(self$error_training)
      if (length(self$error_testing) > 0) {
        boxcat("Testing Error")
        print(self$error_testing)
      }
    },
    ## Plot Variable Importance ----
    plot_varimp = function(theme = rtTheme,
                           filename = NULL, ...) {
      if (is.null(self$varimp)) {
        msg2(hilite2("No variable importance available."))
        return(invisible(NULL))
      }
      dplot3_varimp(self$varimp, theme = theme, filename = filename, ...)
    },
    ## Describe ----
    describe = function() {
      type <- self$type
      algorithm <- select_learn(self$algorithm, desc = TRUE)
      cat(algorithm, " was used for ", tolower(type), ".\n", sep = "")
      desc <- paste0(algorithm, " was used for ", tolower(type), ".")

      # Tuning ----
      if (length(self$gridsearch) > 0) {
        res <- self$gridsearch$resample_params
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
        params <- self$gridsearch$params$search
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
        metric <- self$gridsearch$metric

        if (length(fixed) > 0) {
          cat("The following parameters were fixed:\n")
          printls(fixed)
        }
        if (length(searched) > 0) {
          cat("Grid search was performed on:\n")
          printls(searched)
        }
        cat(metric, "was", ifelse(self$gridsearch$maximize, "maximized", "minimized"), "with:\n")
        printls(self$gridsearch$best.tune)
      }

      # Error ----
      if (type == "Classification") {
        cat(
          "Balanced accuracy was",
          ddSci(self$error_training$Overall$`Balanced Accuracy`),
          "on the training set"
        )
        desc <- paste(
          desc, "Balanced accuracy was",
          ddSci(self$error_training$Overall$`Balanced Accuracy`),
          "in the training set"
        )
        if (!is.null(self$error_testing$Overall$`Balanced Accuracy`)) {
          cat(
            " and",
            ddSci(self$error_testing$Overall$`Balanced Accuracy`),
            "in the testing set."
          )
          desc <- paste(
            desc, "and",
            ddSci(self$error_testing$Overall$`Balanced Accuracy`),
            "in the testing set."
          )
        } else {
          cat(".")
          desc <- paste0(desc, ".")
        }
      } else if (type == "Regression") {
        cat(
          "R-squared was",
          ddSci(self$error_training$Rsq),
          "(training)"
        )
        desc <- paste(
          desc, "R-squared was",
          ddSci(self$error_training$Rsq),
          "on the training set"
        )
        if (!is.null(self$error_testing$`Balanced Accuracy`)) {
          cat(
            " and",
            ddSci(self$error_testing$Rsq), "(testing)."
          )
          desc <- paste(
            desc, "and",
            ddSci(self$error_testing$Rsq),
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
  ), # /public
) # rtemis::Supervised

# Classification ----
Classification <- R6::R6Class(
  classname = "Classification",
  inherit = Supervised,
  public = list(
    predicted_prob_training = NULL,
    predicted_prob_validation = NULL,
    predicted_prob_testing = NULL,
    ## Initialize Classification ----
    initialize = function(...,
                          predicted_prob_training = NULL,
                          predicted_prob_validation = NULL,
                          predicted_prob_testing = NULL) {
      super$initialize(...)
      self$type <- "Classification"
      self$predicted_prob_training <- predicted_prob_training
      self$predicted_prob_validation <- predicted_prob_validation
      self$predicted_prob_testing <- predicted_prob_testing
      self$error_training <- mod_error(
        true = self$y_training,
        predicted = self$predicted_training,
        predicted_prob = self$predicted_prob_training
      )
      self$error_validation <- mod_error(
        true = self$y_validation,
        predicted = self$predicted_validation,
        predicted_prob = self$predicted_prob_validation
      )
      self$error_testing <- mod_error(
        true = self$y_testing,
        predicted = self$predicted_testing,
        predicted_prob = self$predicted_prob_testing
      )
    }, # /initialize
    ## Plot Classification ----
    plot_training = function(theme = rtTheme,
                             filename = NULL,
                             xlab = "Predicted (Training)",
                             ylab = "Reference", ...) {
      dplot3_conf(
        x = self$error_training$ConfusionMatrix,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    }, # /plot_training
    plot_validation = function(theme = rtTheme,
                               filename = NULL,
                               xlab = "Predicted (Validation)",
                               ylab = "Reference", ...) {
      if (is.null(self$y_validation)) {
        msg2(hilite2("No validation data available."))
        return(invisible(NULL))
      }
      dplot3_conf(
        x = self$error_validation$ConfusionMatrix,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    }, # /plot_validation
    plot_testing = function(theme = rtTheme,
                            filename = NULL,
                            xlab = "Predicted (Testing)",
                            ylab = "Reference", ...) {
      if (is.null(self$y_testing)) {
        msg2(hilite2("No testing data available."))
        return(invisible(NULL))
      }
      dplot3_conf(
        x = self$error_testing$ConfusionMatrix,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    } # /plot_testing
  ) # /public
) # rtemis::Classification

# Regression ----
Regression <- R6::R6Class(
  classname = "Regression",
  inherit = Supervised,
  public = list(
    se_error_training = NULL,
    se_error_validation = NULL,
    se_error_testing = NULL,
    ## Initialize Regression ----
    initialize = function(...,
                          se_error_training = NULL,
                          se_error_validation = NULL,
                          se_error_testing = NULL) {
      super$initialize(...)
      self$type <- "Regression"
      self$error_training <- mod_error(true = self$y_training, predicted = self$predicted_training)
      self$error_validation <- mod_error(true = self$y_validation, predicted = self$predicted_validation)
      self$error_testing <- mod_error(true = self$y_testing, predicted = self$predicted_testing)
      self$se_error_training <- se_error_training
      self$se_error_validation <- se_error_validation
      self$se_error_testing <- se_error_testing
    },
    ## Plot Regression ----
    plot_training = function(theme = rtTheme,
                             fit = "glm",
                             filename = NULL,
                             xlab = "True",
                             ylab = "Predicted (Training)", ...) {
      dplot3_fit(
        x = self$y_training,
        y = self$predicted_training,
        fit = fit,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    },
    plot_validation = function(theme = rtTheme,
                               fit = "glm",
                               filename = NULL,
                               xlab = "True",
                               ylab = "Predicted (Validation)", ...) {
      if (is.null(self$y_validation)) {
        msg2(hilite2("No validation data available."))
        return(invisible(NULL))
      }
      dplot3_fit(
        x = self$y_validation,
        y = self$predicted_validation,
        fit = fit,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    },
    plot_testing = function(theme = rtTheme,
                            fit = "glm",
                            filename = NULL,
                            xlab = "True",
                            ylab = "Predicted (Testing)", ...) {
      if (is.null(self$y_testing)) {
        msg2(hilite2("No testing data available."))
        return(invisible(NULL))
      }
      dplot3_fit(
        x = self$y_testing,
        y = self$predicted_testing,
        fit = fit,
        theme = theme,
        filename = filename,
        xlab = xlab,
        ylab = ylab,
        ...
      )
    }
  ),
) # rtemis::Regression

### Make Supervised
make_Supervised <- function(
    algorithm,
    model,
    preprocessor,
    hyperparameters,
    tuner,
    y_training,
    y_validation = NULL,
    y_testing = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_testing = NULL,
    predicted_prob_training = NULL,
    predicted_prob_validation = NULL,
    predicted_prob_testing = NULL,
    se_error_training = NULL,
    se_error_validation = NULL,
    se_error_testing = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL) {
  if (is.factor(y_training)) {
    Classification$new(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
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
    Regression$new(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      y_training = y_training,
      y_validation = y_validation,
      y_testing = y_testing,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_testing = predicted_testing,
      se_error_training = se_error_training,
      se_error_validation = se_error_validation,
      se_error_testing = se_error_testing,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # rtemis::make_Supervised


# write_Supervised ----
#' Write Supervised objects and plots to disk
#'
#' @param object Supervised object
#' @param print.plot Logical: If TRUE, print plot
#' @param plot.fitted Logical: If TRUE, print fitted values plot
#' @param plot.predicted Logical: If TRUE, print predicted values plot
#' @param y.test Test set predictions
#' @param algorithm Character: name of algorithm used to train model
#' @param outdir Character: Path to directory to save model
#' @param save_mod Logical: if TRUE, save trained model to `outdir`,
#' otherwise save only log, summary, and plots
#' @param verbose Logical: If TRUE, print messages in console
#' @param theme rtemis theme to use
#' @param ... Additional arguments passed to `plot(rt)`
#'
#' @author EDG
#' @keywords internal
#' @noRd

write_Supervised <- function(object,
                             outdir,
                             save_mod = TRUE,
                             verbose = TRUE,
                             theme = rtTheme) {
  if (verbose) {
    print(object)
  }

  if (!is.null(outdir)) {
    filename_train <- paste0(outdir, object$algorithm, "_Predicted_Training_vs_True.pdf")
    if (!is.null(object$y_testing)) {
      filename_test <- paste0(outdir, object$algorithm, "_Predicted_Testing_vs_True.pdf")
    }
  } else {
    filename_train <- filename_test <- NULL
  }

  if (save_mod) {
    rt_save(rt, outdir, verbose = verbose)
  }
} # rtemis::write_Supervised

#' Describe generic
#'
#' @param object rtemis object to describe.
#' @param ... Additional arguments passed to `describe`.
#'
#' @author EDG
#' @export
describe <- function(object, ...) UseMethod("describe")

describe.Supervised <- function(object, ...) {
  object$describe()
}

SupervisedCV <- R6::R6Class(
  classname = "SupervisedCV",
  inherit = Supervised,
  public = list(
    outer_resample = NULL
  )
)
