# rtMod.R
# ::rtemis::
# 2016-22 E.D. Gennatas www.lambdamd.org

# rtMod R6 ----
#' \pkg{rtemis} Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Supervised Models
#'
#' Note on terminology:
#' The training set ('x.train') is used to build a model and calculate training error
#' (rtMod$error.train). The testing set ('x.test') is not seen during training and
#' only used to create predictions (rtMod$predicted) using the trained model and
#' calculate error metrics (rtMod$error.test).
#' This reflects generalizability of the model and is the error we care the most about.
#' The validation set is used during model tuning. Within rtemis, validation sets are
#' created automatically by \link{resample} when appropriate, they are not generally
#' input by the user, with few exceptions, as documented in individual functions.
#' @docType class
#' @name rtMod-class
#' 
#' @field mod.name Learner algorithm name
#' @field y.train Training y data
#' @field y.test Testing y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field mod Trained model
#' @field type Classification, Regression, or Survival
#' @field gridsearch Grid search output
#' @field parameters List of hyperparameters used when building model
#' @field fitted Fitted values
#' @field se.fit Standard error of the fit
#' @field error.train Training error
#' @field predicted Predicted values
#' @field se.prediction Standard error of the prediction
#' @field error.test Testing error
#' @field varimp Variable Importance. Note that this is a general concept and different ML algorithms provide very
#' different approaches
#' @field question Question the model is hoping to answer
#' @field extra Algorithm-specific output
#' @field sessionInfo The output of \code{sessionInfo()} at the time the model was trained
#' 
#' @author E.D. Gennatas
#' @export
rtMod <- R6::R6Class("rtMod",
                     public = list(
                       ### Attributes
                       mod.name = NULL,
                       y.train = NULL,
                       y.test = NULL,
                       x.name = NULL,
                       y.name = NULL,
                       xnames = NULL,
                       mod = NULL,
                       type = NULL,
                       gridsearch = NULL,
                       parameters = NULL,
                       fitted = NULL,
                       se.fit = NULL,
                       error.train = NULL,
                       predicted = NULL,
                       se.prediction = NULL,
                       error.test = NULL,
                       varimp = NULL,
                       question = NULL,
                       extra = NULL,
                       sessionInfo = NULL,
                       # Initialize rtMod ----
                       #' @description
                       #' Initialize \code{rtMod} object
                       #' 
                       #' @param mod.name Character: Algorithm name
                       #' @param y.train Training set output
                       #' @param y.test Testing set output
                       #' @param x.name Character: Feature set name
                       #' @param y.name Character: Output name
                       #' @param xnames Character vector: Feature names
                       #' @param mod Trained model
                       #' @param type Character: Type of model (Regression, Classification, Survival)
                       #' @param gridsearch Grid search output
                       #' @param parameters List of training parameters
                       #' @param fitted Fitted values (training set predictions)
                       #' @param se.fit Standard error of fitted values
                       #' @param error.train Training set error
                       #' @param predicted Predicted values (Testing set predictions)
                       #' @param se.prediction Standard error of predicted values
                       #' @param error.test Testing set error
                       #' @param varimp Variable importance
                       #' @param question Question the model is trying to answer
                       #' @param extra List of extra model info
                       #' @param sessionInfo R session info at time of training
                       initialize = function(mod.name = character(),
                                             y.train = numeric(),
                                             y.test = numeric(),
                                             x.name = character(),
                                             y.name = character(),
                                             xnames = character(),
                                             mod = list(),
                                             type = character(),
                                             gridsearch = list(),
                                             parameters = list(),
                                             fitted = numeric(),
                                             se.fit = NULL,
                                             error.train = list(),
                                             predicted = NULL,
                                             se.prediction = NULL,
                                             error.test = NULL,
                                             varimp = NULL,
                                             question = character(),
                                             extra = list(),
                                             sessionInfo = NULL) {
                         self$mod.name <-  mod.name
                         self$y.train <- y.train
                         self$y.test <- y.test
                         self$x.name <- x.name
                         self$y.name <- y.name
                         self$xnames <- xnames
                         self$mod <- mod
                         self$type <- type
                         self$gridsearch <- gridsearch
                         self$parameters <- parameters
                         self$fitted <- fitted
                         self$se.fit <- se.fit
                         self$error.train <- error.train
                         self$predicted <- predicted
                         self$se.prediction <- se.prediction
                         self$error.test <- error.test
                         self$varimp <- varimp
                         self$question <- question
                         self$extra <- extra
                         self$sessionInfo <- sessionInfo()
                       },
                       ### Methods
                       # Print rtMod ----
                       #' @description
                       #' code{print} method for \code{rtMod} object
                       print = function() {
                         "show / print method for rtMod"
                         objcat("Supervised Model")
                         cat(hilite(self$mod.name), " (", modSelect(self$mod.name, desc = TRUE),
                             ")\n", sep = "")
                         boxcat("Training Error")
                         print(self$error.train)
                         if (length(self$error.test) > 0) {
                           boxcat("Testing Error")
                           print(self$error.test)
                         }
                       },
                       # Plot rtMod ----
                       #' @description
                       #' \code{rtMod} plot method
                       #' @param estimate Character: "fitted" or "predicted"
                       #' @param theme Theme to pass to plotting function
                       #' @param filename Character: Path to file to save plot
                       #' @param ... Additional arguments passed to plotting function
                       plot = function(estimate = NULL,
                                       theme = rtTheme,
                                       filename = NULL, ...) {
                         "Plot predicted vs. true if available, otherwise fitted vs. true"
                         if (!is.null(estimate)) {
                           if (estimate == "fitted") {
                             self$plotFitted(theme = theme,
                                             filename = filename, ...)
                           } else if (estimate == "predicted") {
                             self$plotPredicted(theme = theme,
                                                filename = filename, ...)
                           }
                         } else {
                           if (length(self$predicted) < 1) {
                             self$plotFitted(theme = theme,
                                             filename = filename, ...)
                           } else {
                             self$plotPredicted(theme = theme,
                                                filename = filename, ...)
                           }
                         }
                       },
                       # Plot fitted rtMod ----
                       #' @description
                       #' Plot fitted values
                       #' @param print.plot Logical: If TRUE show plot
                       #' @param theme Theme to be passed on to plotting function
                       #' @param main Character: main title
                       #' @param filename Character: path to file to save plot
                       #' @param ... Additional arguments passed to plotting function
                       plotFitted = function(print.plot = TRUE,
                                             theme = rtTheme,
                                             main = NULL,
                                             filename = NULL, ...) {
                         "Plot fitted vs. true values for Regression or confusion matrix for Classification"
                         if (self$type == "Regression") {
                           mplot3_fit(self$y.train, self$fitted,
                                      xlab = paste("True", self$y.name),
                                      ylab = paste("Fitted", self$y.name),
                                      main = if (is.null(main)) paste(self$mod.name, "Training") else main,
                                      theme = theme,
                                      filename = filename, ...)
                         } else if (self$type == "Classification") {
                           mplot3_conf(self$error.train,
                                       xlab = "Reference",
                                       ylab = "Fitted",
                                       main = if (is.null(main)) paste(self$mod.name, "Training") else main,
                                       theme = theme,
                                       filename = filename, ...)
                         } else {
                           mplot3_surv(list(True = self$y.train, Fitted = self$fitted),
                                       main = if (is.null(main)) paste(self$mod.name,
                                                    "Kaplan-Meier estimate") else main,
                                       normalize.time = TRUE,
                                       theme = theme,
                                       filename = filename, ...)
                         }
                       },
                       # Plot predicted rtMod ----
                       #' @description
                       #' Plot predicted values
                       #' @param print.plot Logical: If TRUE show plot
                       #' @param theme Theme to be passed on to plotting function
                       #' @param main Character: main title
                       #' @param filename Character: path to file to save plot
                       #' @param ... Additional arguments passed to plotting function
                       plotPredicted = function(print.plot = TRUE,
                                                theme = rtTheme,
                                                main = NULL,
                                                filename = NULL, ...) {
                         "Plot predicted vs. true values"
                         if (length(self$y.test) < 1 | length(self$predicted) < 1) {
                           warning("No testing data available")
                           return(NULL)
                         }
                         if (self$type == "Regression") {
                           mplot3_fit(self$y.test, self$predicted,
                                      xlab = paste("True", self$y.name),
                                      ylab = paste("Predicted", self$y.name),
                                      main = if (is.null(main)) paste(self$mod.name, "Testing") else main,
                                      theme = theme,
                                      filename = filename, ...)
                         } else  if (self$type == "Classification") {
                           mplot3_conf(self$error.test,
                                       xlab = "Reference",
                                       ylab = "Predicted",
                                       main = if (is.null(main)) paste(self$mod.name, "Testing") else main,
                                       theme = theme,
                                       filename = filename, ...)
                         } else {
                           mplot3_surv(list(True = self$y.test, Predicted = self$predicted),
                                       main = if (is.null(main)) paste(self$mod.name,
                                                    "Kaplan-Meier estimate") else main,
                                       normalize.time = TRUE,
                                       theme = theme,
                                       filename = filename, ...)
                         }
                       },
                       # Plot fitted & predicted rtMod ----
                       #' @description
                       #' Plot fitted and predicted values
                       #' @param print.plot Logical: If TRUE show plot
                       #' @param theme Theme to be passed on to plotting function
                       #' @param filename Character: path to file to save plot
                       #' @param ... Additional arguments passed to \code{mplot3_conf}
                       plotFittedPredicted = function(print.plot = TRUE,
                                                      theme = rtTheme,
                                                      filename = NULL, ...) {
                         "Plot fitted & predicted vs. true values"
                         if (self$type == "Classification") {
                           rtlayout(2, 1)
                           mplot3_conf(self$error.train,
                                       xlab = "Refernece",
                                       ylab = "Fitted",
                                       main = paste(self$mod.name, "Training"),
                                       theme = theme, ...)
                           mplot3_conf(self$error.test,
                                       xlab = "Reference",
                                       ylab = "Predicted",
                                       main = paste(self$mod.name, "Testing"),
                                       theme = theme, ...)
                           rtlayout()
                           invisible(list(self$y.name,
                                       self$error.train,
                                       self$y.test,
                                       self$error.test))
                         }
                         if (length(self$y.test) < 1 | length(self$predicted) < 1) {
                           warning("No testing data available")
                           return(NULL)
                         }
                         mplot3_fit(list(Train = self$y.train, Test = self$y.test),
                                    list(Train = self$fitted, Test = self$predicted),
                                    xlab = paste("True", self$y.name),
                                    ylab = paste("Predicted", self$y.name),
                                    main = paste(self$mod.name, "Training & Testing"),
                                    col = c("#18A3AC", "#F48024"),
                                    theme = theme,
                                    filename = filename, ...)
                       },
                       # Plot variable importance rtMod ----
                       #' @description
                       #' Plot variable importance
                       #' 
                       #' @param plot.top Integer: Plot this many top features
                       #' @param type Character: "barplot" or "lollipop"
                       #' @param theme Theme to be passed on to plotting function
                       #' @param ... Not used
                       plotVarImp = function(plot.top = 12,
                                             type = c("barplot", "lollipop"),
                                             theme = rtTheme, ...) {
                         if (length(self$varimp) == 0) {
                           warning("Variable importance is not available for this model")
                         } else {
                           type <- match.arg(type)
                           if (type == "lollipop") {
                             mplot3_lolli(self$varimp,
                                          plot.top = plot.top,
                                          xlab = "Variable Importance",
                                          theme = theme, ...)
                           } else {
                             mplot3_varimp(self$varimp,
                                           plot.top = plot.top,
                                           theme = theme, ...)
                           }
                         }
                       },
                       # Summary rtMod ----
                       #' @description
                       #' Summary method for \code{rtMod} object
                       #' @param plots Logical: If TRUE show plots
                       #' @param cex Numeric: Character expansion factor
                       #' @param fit.true.line Character: algorithm to use to fit
                       #' true vs predicted curve
                       #' @param resid.fit.line Character: algorithm to use to
                       #' fit residuals plot
                       #' @param fit.legend Logical: If TRUE, show legend in
                       #' fit plot
                       #' @param se.fit Logical: If TRUE, include standard error
                       #' band in fit plot
                       #' @param single.fig Logical: If TRUE, combine in 
                       #' single plot
                       #' @param theme Theme to pass to plotting functions
                       #' @param title.col Title color
                       #' @param ... Extra argument to pass to 
                       summary = function(plots = TRUE,
                                          cex = 1,
                                          fit.true.line = "lm",
                                          resid.fit.line = "gam",
                                          fit.legend = TRUE,
                                          se.fit = TRUE,
                                          single.fig = TRUE,
                                          theme = rtTheme,
                                          title.col = NULL, ...) {
                         "Get model summary"
                         summary.rtMod(self)
                       },
                       # Describe rtMod ----
                       #' @description
                       #' Describe model
                       describe = function() {

                         type <- self$type
                         algorithm <- modSelect(self$mod.name, desc = TRUE)
                         cat(algorithm, " was used for ",
                           tolower(type), ".\n",
                           sep = ""
                         )
                         desc <- paste0(
                           algorithm, " was used for ",
                           tolower(type), "."
                         )

                         # '- Tuning ----
                         if (length(self$gridsearch) > 0) {
                             res <- self$gridsearch$resample.rtset
                             n.resamples <- res$n.resamples
                             resampler <- res$resampler
                             resamples <- switch(resampler,
                                 strat.sub = " stratified subsamples",
                                 bootstrap = " bootstraps",
                                 strat.boot = " stratified bootstraps",
                                 kfold = "-fold crossvalidation",
                                 "custom resamples"
                             )
                             cat(" Hyperparameter tuning was performed using ",
                                 n.resamples, resamples, ".\n",
                                 sep = ""
                             )
                             desc <- paste0(
                                 desc,
                                 " Hyperparameter tuning was performed using ",
                                 n.resamples, resamples, "."
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
                             cat(metric, "was", ifelse(self$gridsearch$maximize,
                                 "maximized", "minimized"
                             ), "with:\n")
                             printls(self$gridsearch$best.tune)
                         }

                         # '- Error ----
                         if (type == "Classification") {
                           cat("Balanced accuracy was",
                             ddSci(self$error.train$Overall$`Balanced Accuracy`),
                             "(training)"
                           )
                           desc <- paste(
                             desc, "Balanced accuracy was",
                             ddSci(self$error.train$Overall$`Balanced Accuracy`),
                             "on the training set"
                           )
                           if (!is.null(self$error.test$Overall$`Balanced Accuracy`)) {
                             cat("and",
                               ddSci(self$error.test$Overall$`Balanced Accuracy`),
                               "(testing)."
                             )
                             desc <- paste(
                               desc, "and",
                               ddSci(self$error.test$Overall$`Balanced Accuracy`),
                               "on the testing set."
                             )
                           } else {
                             cat(".")
                             desc <- paste0(desc, ".")
                           }
                         } else if (type == "Regression") {

                           cat("R-squared was",
                             ddSci(self$error.train$Rsq),
                             "(training)"
                           )
                           desc <- paste(
                             desc, "R-squared was",
                             ddSci(self$error.train$Rsq),
                             "on the training set"
                           )
                           if (!is.null(self$error.test$`Balanced Accuracy`)) {
                             cat("and",
                               ddSci(self$error.test$Rsq), "(testing)."
                             )
                             desc <- paste(
                               desc, "and",
                               ddSci(self$error.test$Rsq),
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
                     )) # /rtMod

# rtMod S3 methods ----
#' \code{rtMod} S3 methods
#'
#' S3 methods for \code{rtMod} class.
#' Excludes functions \code{print} and \code{plot} defined within the \link{rtMod} class itself.
#'
#' @name rtMod-methods
NULL


#' \code{print.rtMod}: \code{print} method for \code{rtMod} object
#'
#' @method print rtMod
#' @param x \code{rtMod} object
#' @param ... Not use
#' @rdname rtMod-methods
#' @export
print.rtMod <- function(x, ...) {

  x$print()

} # rtemis::print.rtMod


#' \code{fitted.rtMod}: \code{fitted} method for \code{rtMod} object
#'
#' @method fitted rtMod
#' @param object \code{rtMod} object
#' @param ... Not used
#' @rdname rtMod-methods
#' @export
fitted.rtMod <- function(object, ...) {

  return(object$fitted)

} # rtemis::fitted.rtMod


#' \code{predict.rtMod}: \code{predict} method for \code{rtMod} object
#'
#' @method predict rtMod
#' @param newdata Features to use for prediction
#' @param trace Integer: Set trace level
#' @param verbose Logical: If TRUE, output messages to console
#' @param ... Extra arguments to pass to trained model's \code{predict} method
#' @rdname rtMod-methods
#' @export
predict.rtMod <- function(object,
                          newdata,
                          trace = 0,
                          verbose = TRUE, ...) {

  extraArgs <- list(...)

  if (missing(newdata)) {
    estimated <- object$fitted
  } else {
    newdata <- as.data.frame(newdata)
    # This is "GLM" which gets names "LOGISTIC" for better or worse
    if (object$mod.name == "LOGISTIC") {
      estimated.prob <- predict(object$mod, newdata = newdata, type = "response")
      estimated <- factor(ifelse(estimated.prob >= .5, 1, 0), levels = c(1, 0))
      levels(estimated) <- levels(object$y.train)
    } else if (object$mod.name == "ADDT") {
      if (is.null(extraArgs$learning.rate)) learning.rate <- object$parameters$learning.rate
      if (is.null(extraArgs$n.feat)) n.feat <- length(object$xnames)
      estimated <- predict(object$mod, newdata = newdata,
                           learning.rate = learning.rate,
                           n.feat = n.feat, ...)
    } else if (object$mod.name == "BRUTO") {
      # BRUTO: newdata must be matrix
      estimated <- predict(object$mod, newdata = as.matrix(newdata))
    } else if (object$mod.name == "CART") {
      if (object$type == "Classification") {
        estimated <- predict(object$mod, newdata = newdata, type = "class")
      } else {
        estimated <- predict(object$mod, newdata = newdata)
      }
    } else if (inherits(object$mod, "gamsel")) {
      # if (inherits(object$mod, "gamsel")) {
      if (is.null(extraArgs$which.lambda)) {
        which.lambda <- length(object$mod$lambdas)
      } else {
        which.lambda <- extraArgs$which.lambda
      }
      estimated <- predict(object$mod, newdata, index = which.lambda, type = "response")
      if (object$type == "Classification") {
        estimated <- factor(ifelse(estimated >= .5, 1, 0), levels = c(1, 0))
        levels(estimated) <- levels(object$y.train)
      }
      # } else {
      #   estimated <- predict(object$mod, data.frame(newdata))
      # }

    } else if (object$mod.name == "GBM" || object$mod.name == "GBM3") {
      if (is.null(extraArgs$n.trees)) {
        n.trees <- object$mod$n.trees
      } else {
        n.trees <- extraArgs$n.trees
      }
      ###
      mod <- object$mod
      type <- object$type
      distribution <- object$mod$distribution
      y <- object$y.train
      if (type == "Regression" || type == "Survival") {
        if (distribution == "poisson") {
          if (trace > 0) msg("Using predict for Poisson Regression with type = response")
          estimated <- predict(mod, newdata, n.trees = n.trees, type = "response")
        } else {
          if (verbose) msg("Using predict for", type, "with type = link")
          estimated <- predict(mod, newdata, n.trees = n.trees)
        }
      } else {
        if (distribution == "multinomial") {
          if (trace > 0) msg("Using predict for multinomial classification with type = response")
          # Get probabilities per class
          estimated.prob <- estimated <- predict(mod, newdata, n.trees = n.trees, type = "response")
          # Now get the predicted classes
          estimated <- apply(estimated, 1, function(x) levels(newdata)[which.max(x)])
        } else {
          # Bernoulli: convert {0, 1} back to factor
          if (trace > 0) msg("Using predict for classification with type = response")
          estimated.prob <- predict(mod, newdata, n.trees = n.trees, type = "response")
          estimated <- factor(levels(y)[as.numeric(estimated.prob >= .5) + 1])
        }
      }
      ###
    } else if (object$mod.name == "GLMNET") {
      newdata <- model.matrix(~ ., newdata)[, -1, drop = FALSE]
      # drop = FALSE needed when predicting on a single case,
      # will drop to vector otherwise and glmnet predict will fail
      estimated <- predict(object$mod, newx = newdata, ...)[, 1]
    } else if (object$mod.name == "RANGER") {
      predict.ranger <- getFromNamespace("predict.ranger", "ranger")
      estimated <- predict.ranger(object$mod, data = newdata, ...)$predictions
    } else if (object$mod.name %in% c("XGBOOST", "XGBLIN", "XGBDART")) {
      # XGB: must convert newdata to xgb.DMatrix
      if (any(sapply(newdata, is.factor))) {
        newdata <- preprocess(newdata, oneHot = TRUE)
      }
      estimated <- predict(object$mod,
        newdata = xgboost::xgb.DMatrix(as.matrix(newdata))
      )
    } else if (object$mod.name == "MXFFN") {
      estimated <- predict(object$mod,
        data.matrix(newdata),
        array.layout = "rowmajor"
      )
    } else if (object$mod.name == "H2ODL") {
      newdata <- h2o::as.h2o(newdata, "newdata")
      estimated <- c(as.matrix(predict(object$mod, newdata)))
    } else if (object$mod.name == "DN") {
      estimated <- deepnet::nn.predict(object$mod, x = newdata)
    } else if (object$mod.name == "MLRF") {
      spark.master <- extraArgs$spark.master
      if (is.null(spark.master)) {
        warning("spark.master was not specified, trying 'local'")
        spark.master <- "local"
      }
      sc <- sparklyr::spark_connect(master = spark.master, app_name = "rtemis")
      new.tbl <- sparklyr::sdf_copy_to(sc, newdata)
      estimated <- as.data.frame(sparklyr::sdf_predict(new.tbl, object$mod))$prediction
    # } else if (object$mod.name == "MXN") {
    #   predict.MXFeedForwardModel <- getFromNamespace("predict.MXFeedForwardModel", "mxnet")
    #   if (object$type == "Classification") {
    #     estimated.prob <- predict.MXFeedForwardModel(object$mod, data.matrix(newdata),
    #                                                  array.layout = "rowmajor")
    #     if (min(dim(estimated.prob)) == 1) {
    #       # Classification with Logistic output
    #       estimated <- factor(as.numeric(estimated.prob >= .5))
    #     } else {
    #       # Classification with Softmax output
    #       estimated <- factor(apply(estimated.prob, 2, function(i) which.max(i)))
    #     }
    #     levels(estimated) <- levels(object$y.train)
    #   } else {
    #     estimated <- c(predict.MXFeedForwardModel(object$mod,
    #                                               X = data.matrix(newdata),
    #                                               array.layout = "rowmajor"))
    #   }
    } else if (object$mod.name == "SGD") {
      estimated <- predict(object$mod, newdata = cbind(1, data.matrix(newdata)))
    } else {
      estimated <- predict(object$mod, newdata = newdata, ...)
    }
  }

  if (!is.null(object$type) && (object$type == "Regression" || object$type == "Survival")) {
    estimated <- as.numeric(estimated)
  }

  estimated

} # rtemis::predict.rtMod


#' \code{residuals.rtMod}: \code{residuals} method for \code{rtMod} object
#'
#' @method residuals rtMod
#' @param object \code{rtMod} object
#' @param ... Not used
#' @rdname rtMod-methods
#' @export
residuals.rtMod <- function(object, ...) {

  as.numeric(object$y.train - object$fitted)

}


#' \code{plot.rtMod}: \code{plot} method for \code{rtMod} object
#'
#' @method plot rtMod
#' @param x \code{rtMod} object
#' @param estimate Character: "fitted" or "predicted"
#' @param filename Character: Path to file to save plot
#' @rdname rtMod-methods
#' @export
plot.rtMod <- function(x, estimate = NULL,
                       theme = rtTheme,
                       filename = NULL, ...) {

  x$plot(estimate = estimate,
         theme = theme,
         filename = filename, ...)

} # rtemis::plot.rtemisRC


#' \code{summary.rtMod}: \code{summary} method for \code{rtMod} object
#'
#' @method summary rtMod
#' @param object \code{rtMod} object
#' @param plots Logical: If TRUE, print plots. Default = TRUE
#' @param cex Float: Character expansion factor
#' @param fit.true.line \pkg{rtemis} algorithm to use for fitted vs. true line
#'   Options: \code{modSelect()}
#' @param resid.fit.line \pkg{rtemis} algorithm to use for residuals vs. fitted line.
#'   Options: \code{modSelect()}
#' @param fit.legend Logical: If TRUE, print fit legend. Default  = TRUE
#' @param se.fit Logical: If TRUE, plot 2 * standard error bands. Default = TRUE
#' @param single.fig Logical: If TRUE, draw all plots in a single figure. Default = TRUE
#' @param summary Logical: If TRUE, print summary. Default = TRUE
#' @param theme Character: theme to use. Options: "box", "darkbox", "light", "dark"
#' @param title.col Color for main title
#' @param ... Additional arguments to be passed to \link{mplot3_xy}
#' @author E.D. Gennatas
#' @rdname rtMod-methods
#' @export
summary.rtMod <- function(object,
                          plots = TRUE,
                          cex = 1,
                          fit.true.line = "lm",
                          resid.fit.line = "gam",
                          fit.legend = TRUE,
                          se.fit = TRUE,
                          single.fig = TRUE,
                          summary = TRUE,
                          theme = rtTheme,
                          title.col = NULL, ...) {

  # [ Arguments ]
  do.test <- ifelse(length(object$y.test) > 1, TRUE, FALSE)

  # [ Data ]
  mod.name <- object$mod.name
  y.train <- object$y.train
  y.test <- object$y.test
  fitted <- object$fitted
  predicted <- object$predicted
  # as.numeric in order to convert factors for Classification
  residuals <- as.numeric(object$fitted) - as.numeric(object$y.train)
  error <- object$predicted - object$y.test
  question <- object$question

  # [ Print train and testing errors ]
  boxcat(".:rtemis Summary")
  if (length(question) > 0) cat("Question: ", question, "\n\n")
  cat(mod.name, "Training Error:\n")
  print(object$error.train)
  if (do.test) {
    cat(mod.name, "Testing Error:\n")
    print(object$error.test)
  }

  # [ Plot ]
  if (plots) {
    if (object$type == "Classification") {
      object$plot()
    } else {
      # par(mfrow = mfrow)
      par.orig <- par(no.readonly = TRUE)
      if (single.fig) {
        par(oma = c(0, 0, 2, 0))
        on.exit(suppressWarnings(par(par.orig)))
        if (do.test) layout(matrix(1:6, nrow = 2)) else layout(matrix(1:3, nrow = 1))
      }
      pr <- ifelse(single.fig, FALSE, TRUE)

      # 1. Fitted vs. True
      mplot3_xy(y.train, fitted,
                xlab = "True", ylab = "Fitted", main = "Training",
                fit = fit.true.line, fit.legend = fit.legend, se.fit = se.fit,
                axes.equal = TRUE, fit.error = TRUE, cex = cex,
                point.col = pennCol$lighterBlue, fit.col = pennCol$red,
                theme = theme, par.reset = pr, ...)

      # 2. Predicted vs. True
      if (do.test) {
        mplot3_xy(y.test, predicted,
                  xlab = "True", ylab = "Predicted", main = "Testing",
                  fit = fit.true.line, fit.legend = fit.legend, se.fit = se.fit,
                  axes.equal = TRUE,  fit.error = TRUE, cex = cex,
                  point.col = pennCol$lighterBlue, fit.col = pennCol$red,
                  theme = theme, par.reset = pr, ...)
      }

      # 3. Residuals vs. Fitted
      mplot3_xy(fitted, residuals,
                xlab = "Fitted", ylab = "Residuals", main = "Residuals",
                fit = resid.fit.line, fit.legend = fit.legend, se.fit = se.fit,
                point.col = pennCol$red, fit.col = pennCol$lighterBlue,
                cex = cex, theme = theme, par.reset = pr, ...)

      # 4. Prediction error vs. True
      if (do.test) {
        mplot3_xy(y.test, error^2, main = "Error", # old: predicted, error
                  xlab = "True", ylab = "Squared Error",
                  fit = resid.fit.line, fit.legend = fit.legend, se.fit = se.fit,
                  point.col = pennCol$red, fit.col = pennCol$lighterBlue,
                  cex = cex, theme = theme, par.reset = pr, ...)
      }

      # 5. Residuals Q-Q plot
      mplot3_x(residuals, type = "qqline",
               main = "Residuals Q-Q Plot",
               col = pennCol$blue, qqline.col = pennCol$green,
               cex = cex, theme = theme, par.reset = pr, ...)

      # 6. Error Q-Q plot
      if (do.test) {
        mplot3_x(error, type = "qqline",
                 main = "Error Q-Q Plot",
                 col = pennCol$blue, qqline.col = pennCol$green,
                 cex = cex, theme = theme, par.reset = pr, ...)
      }
      if (is.null(title.col)) title.col <- ifelse(theme == "box" |
                                                    theme == "light", "black", "white")
      mtext(paste(object$mod.name, "diagnostic plots"), outer = TRUE,
            cex = 1.5, line = -0.5, col = title.col)
    }
  } # /plots

} # rtemis::summary.rtMod


#' \code{coef.rtMod}: \code{coef} method for \code{rtMod} object
#'
#' @param object Trained model of class \code{rtMod}
#' @param verbose Logical: If TRUE, output messages to console
#' 
#' @author E.D. Gennatas
#' @rdname rtMod-methods
#' @export
coef.rtMod <- function(object, verbose = TRUE, ...) {

  if (!is.null(object$varimp)) {
    object$varimp
  } else {
    if (verbose) msg("Variable importance not available for model of type", object$mod.name)
  }

} # rtemis::coef.rtMod


# rtModClass R6 ----
#' \pkg{rtemis} Classification Model Class
#'
#' R6 Class for \pkg{rtemis} Classification Models
#'
#' @docType class
#' @name rtModClass-class
#' 
#' @field fitted.prob Training set probability estimates
#' @field predicted.prob Testing set probability estimates
rtModClass <- R6::R6Class("rtModClass",
                          inherit = rtMod,
                          public = list(fitted.prob = NULL,
                                        predicted.prob = NULL,
                                        # Initialize rtModClass ----
                                        #' @description
                                        #' Initialize \code{rtModClass} object
                                        #' @param mod.name Character: Algorithm name
                                        #' @param y.train Training set output
                                        #' @param y.test Testing set output
                                        #' @param x.name Character: Feature set name
                                        #' @param y.name Character: Output name
                                        #' @param xnames Character vector: Feature names
                                        #' @param mod Trained model
                                        #' @param type Character: Type of model (Regression, Classification, Survival)
                                        #' @param gridsearch Grid search output
                                        #' @param parameters List of training parameters
                                        #' @param fitted Fitted values (training set predictions)
                                        #' @param fitted.prob Training set probability estimates
                                        #' @param se.fit Standard error of the fit
                                        #' @param error.train Training set error
                                        #' @param predicted Predicted values (Testing set predictions)
                                        #' @param predicted.prob Testing set probability estimates
                                        #' @param se.prediction 
                                        #' @param error.test Testing set error
                                        #' @param varimp Variable importance
                                        #' @param question Question the model is trying to answer
                                        #' @param extra List of extra model info
                                        #' @param sessionInfo R session info at time of training
                                        initialize = function(mod.name = character(),
                                                              y.train = numeric(),
                                                              y.test = numeric(),
                                                              x.name = character(),
                                                              y.name = character(),
                                                              xnames = character(),
                                                              mod = list(),
                                                              type = character(),
                                                              gridsearch = list(),
                                                              parameters = list(),
                                                              fitted = numeric(),
                                                              fitted.prob = numeric(),
                                                              se.fit = numeric(),
                                                              error.train = list(),
                                                              predicted = NULL,
                                                              predicted.prob = NULL,
                                                              se.prediction = NULL,
                                                              error.test = NULL,
                                                              varimp = NULL,
                                                              question = character(),
                                                              extra = list()) {
                                          super$initialize(mod.name,
                                                           y.train,
                                                           y.test,
                                                           x.name,
                                                           y.name,
                                                           xnames,
                                                           mod,
                                                           type,
                                                           gridsearch,
                                                           parameters,
                                                           fitted,
                                                           se.fit,
                                                           error.train,
                                                           predicted,
                                                           se.prediction,
                                                           error.test,
                                                           varimp,
                                                           question,
                                                           extra)
                                          self$fitted.prob <- fitted.prob
                                          self$predicted.prob <- predicted.prob
                                        },
                                        ### Methods
                                        # Plot ROC rtModClass ----
                                        #' @description
                                        #' plot ROC. Uses testing set if available,
                                        #' otherwise training
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotROC = function(theme = rtTheme,
                                                           filename = NULL, ...) {
                                          if (length(self$fitted.prob) == 0)
                                            stop("Estimated probabilities are not available")
                                          if (length(self$predicted.prob) > 0) {
                                            self$plotROCpredicted(theme = theme,
                                                                  filename = filename, ...)
                                          } else {
                                            self$plotROCfitted(theme = theme,
                                                               filename = filename, ...)
                                          }
                                        },
                                        #' @description
                                        #' Plot training set ROC
                                        #' 
                                        #' @param main Character: Main title
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotROCfitted = function(main = "ROC Training",
                                                                 theme = rtTheme,
                                                                 filename = NULL, ...) {
                                          if (length(self$fitted.prob) > 0) {
                                            mplot3_roc(self$fitted.prob, self$y.train,
                                                       main = main,
                                                       theme = theme,
                                                       filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        #' @description
                                        #' plot testing set ROC
                                        #' 
                                        #' @param main Character: Main title
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotROCpredicted = function(main = "ROC Testing",
                                                                    theme = rtTheme,
                                                                    filename = NULL, ...) {
                                          if (length(self$predicted.prob) > 0) {
                                            mplot3_roc(self$predicted.prob, self$y.test,
                                                       main = main,
                                                       theme = theme,
                                                       filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        #' @description
                                        #' plot Precision-Recall curve. Uses testing set 
                                        #' if available, otherwise training
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotPR = function(theme = rtTheme,
                                                          filename = NULL, ...) {
                                          if (length(self$fitted.prob) == 0)
                                            stop("Estimated probabilities are not available")
                                          if (length(self$predicted.prob) > 0) {
                                            self$plotPRpredicted(theme = theme,
                                                                 filename = filename, ...)
                                          } else {
                                            self$plotPRfitted(theme = theme,
                                                              filename = filename, ...)
                                          }
                                        },
                                        #' @description
                                        #' Plot training set Precision-Recall curve.
                                        #' 
                                        #' @param main Character: Main title
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotPRfitted = function(main = "P-R Training",
                                                                theme = rtTheme,
                                                                filename = NULL, ...) {
                                          if (length(self$fitted.prob) > 0) {
                                            mplot3_pr(self$fitted.prob, self$y.train,
                                                      main = main,
                                                      theme = theme,
                                                      filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        #' @description
                                        #' plot testing set Precision-Recall curve.
                                        #' 
                                        #' @param main Character: Main title
                                        #' @param theme Theme to pass to plotting function
                                        #' @param filename Character: Path to file to save plot
                                        #' @param ... Extra arguments to pass to plotting function
                                        plotPRpredicted = function(main = "P-R Testing",
                                                                   theme = rtTheme,
                                                                   filename = NULL, ...) {
                                          if (length(self$predicted.prob) > 0) {
                                            mplot3_pr(self$predicted.prob, self$y.test,
                                                      main = main,
                                                      theme = theme,
                                                      filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        })
)


# rtModBag R6 ----
#' \pkg{rtemis} Bagged Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Bagged Supervised Models
#'
#' @docType class
#' @name rtModBag-class
#' 
#' @field bag.resample.rtset List of settings for \link{resample}.
#' Set using \link{rtset.bag.resample}
#' @field fitted.bag Base learners' fitted values 
#' @field se.fit.bag Base learners' fitted values' standard error
#' @field predicted.bag Base learners' predicted values
#' @field se.predicted.bag Base learners' predicted values' standard error
#' @field aggr.fn Function used to aggregated base learners' predictions
#' 
#' @export
rtModBag <- R6::R6Class("rtModBag",
                        inherit = rtMod,
                        public = list(
                          bag.resample.rtset = NULL,
                          fitted.bag = NULL,
                          se.fit.bag = NULL,
                          predicted.bag = NULL,
                          se.predicted.bag = NULL,
                          aggr.fn = NULL,
                          # Initialize rtModBar ----
                          #' @description
                          #' Initialize \code{rtModBag} object
                          #' 
                          #' @param mod.name Model (algorithm) name
                          #' @param y.train Training y data
                          #' @param y.test Testing y data
                          #' @param x.name Name of x data
                          #' @param y.name Name of y data
                          #' @param xnames Character vector: Column names of x
                          #' @param bag.resample.rtset List of settings for \link{resample}.
                          #' Set using \link{rtset.bag.resample}
                          #' @param mod Trained model
                          #' @param type Character: Type of model (Regression, Classification, Survival)
                          #' @param parameters List of training parameters
                          #' @param fitted.bag Base learners' fitted values
                          #' @param se.fit.bag Base learners' fitted values' standard error
                          #' @param fitted Fitted values
                          #' @param se.fit Standard error of the fit
                          #' @param error.train Training error
                          #' @param predicted.bag Base learners' predicted values
                          #' @param se.predicted.bag Base learners' predicted values' standard error
                          #' @param predicted Predicted values
                          #' @param se.prediction Standard error of the prediction
                          #' @param aggr.fn Aggregating function
                          #' @param error.test Testing error
                          #' @param varimp Variable importance
                          #' @param question Question the model is hoping to answer
                          #' @param extra Algorithm-specific output
                          initialize = function(mod.name = character(),
                                                # call = call("NULL"),
                                                y.train = numeric(),
                                                y.test = numeric(),
                                                x.name = character(),
                                                y.name = character(),
                                                xnames = character(),
                                                bag.resample.rtset = list(),
                                                mod = list(),
                                                type = character(),
                                                parameters = list(),
                                                fitted.bag = numeric(),
                                                fitted = numeric(),
                                                se.fit.bag = numeric(),
                                                se.fit = numeric(),
                                                error.train = list(),
                                                predicted.bag = numaric(),
                                                predicted = numeric(),
                                                se.predicted.bag = numeric(),
                                                se.prediction = numeric(),
                                                aggr.fn = character(),
                                                error.test = list(),
                                                varimp = NULL,
                                                question = character(),
                                                extra = list()) {
                            self$mod.name <- mod.name
                            self$y.train <- y.train
                            self$y.test <- y.test
                            self$x.name <- x.name
                            self$y.name <- y.name
                            self$xnames <- xnames
                            self$bag.resample.rtset <- bag.resample.rtset
                            self$mod <- mod
                            self$type <- type
                            self$parameters <- parameters
                            self$fitted.bag <- fitted.bag
                            self$fitted <- fitted
                            self$se.fit.bag <- se.fit.bag
                            self$se.fit <- se.fit
                            self$error.train <- error.train
                            self$predicted.bag <- predicted.bag
                            self$predicted <- predicted
                            self$se.predicted.bag <- se.predicted.bag
                            self$se.prediction <- se.prediction
                            self$aggr.fn <- aggr.fn
                            self$error.test <- error.test
                            self$varimp <- varimp
                            self$question <- question
                            self$extra <- extra
                          },
                          ### Methods
                          # Print rtModBag ----
                          #' @description
                          #' \code{print} method for \code{rtModBag} object
                          print = function() {
                            "show / print method for rtModBag"
                            objcat("Bagged Supervised Model")
                            cat(hilite(self$mod.name), " (",
                                modSelect(self$mod.name, desc = TRUE),
                                ")\n", sep = "")
                            .resamples <- switch(self$bag.resample.rtset$resampler,
                                                 strat.sub = "stratified subsamples",
                                                 bootstrap = "bootstraps",
                                                 strat.boot = "stratified bootstraps",
                                                 kfold = "stratified folds",
                                                 "custom resamples")
                            cat("Aggregating", self$bag.resample.rtset$n.resamples,
                                .resamples, "\n")
                            boxcat("Training Error")
                            print(self$error.train)
                            if (length(self$error.test) > 0) {
                              boxcat("Testing Error")
                              print(self$error.test)
                            }
                          }
                        )) # /rtModBag

# rtModBag S3 methods ####
#' rtModBag S3 methods
#'
#' S3 methods for \code{rtModBag} class that differ from those of the \code{rtMod} superclass
#'
#'
#' @name rtModBag-methods
NULL

#' \code{predict.rtModBag}: \code{predict} method for \code{rtModBag} object
#'
#' @method predict rtModBag
#' @param object \code{rtModBag} object
#' @param newdata Testing set features
#' @param aggr.fn Character: Function to aggregate models' prediction. 
#' If NULL, defaults to "median"
#' @param n.cores Integer: Number of cores to use
#' @param ... Not used
#' @rdname rtModBag-methods
#' @export
predict.rtModBag <- function(object, 
                             newdata,
                             aggr.fn = NULL,
                             n.cores = 1,
                             verbose = FALSE, ...) {

  if (verbose) {
    msg(
      "Calculating estimated values of",
      length(object$mod), "bag resamples"
    )
  }
  if (is.null(aggr.fn)) aggr.fn <- object$aggr.fn
  # estimated.df <- sapply(object$mod$mods, function(m) predict(m$mod1, newdata = newdata, ...))
  # estimated <- apply(estimated.df, 1, fn)
  # if (object$type == "Regression" | object$type == "Survival") {
  #   estimated <- as.numeric(estimated)
  # } else {
  #   estimated <- levels(object$y.train)[estimated]
  # }
  # No progress bar if not verbose
  if (!verbose) pbapply::pboptions(type = "none")

  estimated.bag <- pbapply::pblapply(object$mod$mods, function(k) predict(k$mod1, newdata),
                                     cl = n.cores)
  estimated.bag <- do.call(cbind, estimated.bag)
  if (object$type == "Classification") {
    estimated <- factor(round(apply(estimated.bag, 1, function(i) aggr.fn(i))))
    levels(estimated) <- levels(object$y.train)
  } else {
    estimated <- apply(estimated.bag, 1, aggr.fn)
  }
  attr(estimated, "names") <- NULL
  estimated

} # rtemis::predict.rtModBag


# rtModCV R6 ----
#' \pkg{rtemis} Cross-Validated Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Cross-Validated Supervised Models
#'
#' @docType class
#' @name rtModCV-class
#' 
#' @field mod \code{rtModCV} object
#' @field mod.name Model (algorithm) name
#' @field type "Classification", "Regression", or "Survival"
#' @field y.train Training set y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field parameters List of algorithm hyperparameters
#' @field n.repeats Integer: Number of repeats. This is the outermost iterator: i.e. You will run
#' \code{resampler} this many times.
#' @field resampler.params List of resampling parameters
#' @field resamples Resamples produced by \link{resample}
#' @field y.train.res Resamples' fitted values
#' @field y.train.res.aggr Aggregated training set outcomes
#' @field fitted.res Resamples' fitted values
#' @field fitted.res.aggr Aggregated fitted values 
#' @field error.train.res Resamples' training error
#' @field error.train.res.mean Resamples' mean training error
#' @field error.train.res.aggr Error of aggregated fitted values
#' @field error.train.repeats Mean training error for each repeat
#' @field error.train.repeats.mean Mean training error across all repeats
#' @field error.train.repeats.sd Standard deviation of training error across all repeats
#' @field y.test.res Resamples' predicted values
#' @field y.test.res.aggr Aggregated testing set outcomes
#' @field predicted.res Resamples' predicted values 
#' @field predicted.res.aggr Aggregated predicted values
#' @field error.test.res Resamples' testing error
#' @field error.test.res.mean Resamples' mean testing error
#' @field error.test.res.aggr Error of aggregated predicted values
#' @field error.test.repeats Mean testing error for each repeat
#' @field error.test.repeats.mean Mean testing error across all repeats
#' @field error.test.repeats.sd Standard deviation of testing error across all repeats
#' @field fitted.bag Bagged model's fitted values
#' @field error.bag Bagged model's error
#' @field varimp Resamples' variable importance
#' @field question Question the model is hoping to answer
#' @field call elevate call
#' @field sessionInfo R session info at time of training
#' 
#' @export
rtModCV <- R6::R6Class("rtModCV",
                       public = list(
                         ### Attributes
                         mod = NULL,
                         mod.name = NULL,
                         type = NULL,
                         y.train = NULL,
                         x.name = NULL,
                         y.name = NULL,
                         xnames = NULL,
                         parameters = NULL,
                         n.repeats = NULL,
                         resampler.params = NULL,
                         resamples = NULL,
                         y.train.res = NULL,
                         y.train.res.aggr = NULL,
                         fitted.res = NULL,
                         fitted.res.aggr = NULL,
                         error.train.res = NULL,
                         error.train.res.mean = NULL,
                         error.train.res.aggr = NULL,
                         error.train.repeats = NULL,
                         error.train.repeats.mean = NULL,
                         error.train.repeats.sd = NULL,
                         y.test.res = NULL,
                         y.test.res.aggr = NULL,
                         predicted.res = NULL,
                         predicted.res.aggr = NULL,
                         error.test.res = NULL,
                         error.test.res.mean = NULL,
                         error.test.res.aggr = NULL,
                         error.test.repeats = NULL,
                         error.test.repeats.mean = NULL,
                         error.test.repeats.sd = NULL,
                         fitted.bag = NULL,
                         error.bag = NULL,
                         varimp = NULL,
                         question = NULL,
                         call = NULL,
                         sessionInfo = NULL,
                         # Initialize rtModCV ----
                         #' @description
                         #' Initialize \code{rtModCV} object
                         #' 
                         #' @param mod \code{rtModCV} object
                         #' @param mod.name Model (algorithm) name
                         #' @param type "Classification", "Regression", or "Survival"
                         #' @param y.train Training set y data
                         #' @param y.test Testing set y data
                         #' @param x.name Name of x data
                         #' @param y.name Name of y data
                         #' @param xnames Character vector: Column names of x
                         #' @param parameters List of algorithm hyperparameters
                         #' @param resampler List of settings for \link{resample}.
                         #' Set using \link{rtset.cv.resample}
                         #' @param n.repeats Integer: Number of repeats. This is the outermost iterator: i.e. You will run
                         #' \code{resampler} this many times.
                         #' @param resampler.params List of resampling parameters
                         #' @param resamples Resamples produced by \link{resample}
                         #' @param y.train.res Resamples' fitted values
                         #' @param y.train.res.aggr Aggregated training set outcomes
                         #' @param fitted.res Resamples' fitted values
                         #' @param fitted.res.aggr Aggregated fitted values
                         #' @param error.train.res Resamples' training error
                         #' @param error.train.res.mean Resamples' mean training error
                         #' @param error.train.res.aggr Error of aggregated fitted values
                         #' @param error.train.repeats Mean training error for each repeat
                         #' @param error.train.repeats.mean Mean training error across all repeats
                         #' @param error.train.repeats.sd Standard deviation of training error across all repeats
                         #' @param y.test.res Resamples' predicted values
                         #' @param y.test.res.aggr Aggregated testing set outcomes
                         #' @param predicted.res Resamples' predicted values
                         #' @param predicted.res.aggr Aggregated predicted values
                         #' @param error.test.res Resamples' testing error
                         #' @param error.test.res.mean Resamples' mean testing error
                         #' @param error.test.res.aggr Error of aggregated predicted values
                         #' @param error.test.repeats Mean testing error for each repeat
                         #' @param error.test.repeats.mean Mean testing error across all repeats
                         #' @param error.test.repeats.sd Standard deviation of testing error across all repeats
                         #' @param fitted.bag Bagged model's fitted values
                         #' @param error.bag Bagged model's error
                         #' @param varimp Resamples' variable importance
                         #' @param question Question the model is hoping to answer
                         #' @param call elevate call
                         #' @param sessionInfo R session info at time of training
                         initialize = function(mod = NULL,
                                               mod.name = NULL,
                                               type = NULL,
                                               y.train = NULL,
                                               x.name = NULL,
                                               y.name = NULL,
                                               xnames = NULL,
                                               parameters = NULL,
                                               n.repeats = NULL,
                                               resampler.params = NULL,
                                               resamples = NULL,
                                               y.train.res = NULL,
                                               y.train.res.aggr = NULL,
                                               fitted.res = NULL,
                                               fitted.res.aggr = NULL,
                                               error.train.res = NULL,
                                               error.train.res.mean = NULL,
                                               error.train.res.aggr = NULL,
                                               error.train.repeats = NULL,
                                               error.train.repeats.mean = NULL,
                                               error.train.repeats.sd = NULL,
                                               y.test.res = NULL,
                                               y.test.res.aggr = NULL,
                                               predicted.res = NULL,
                                               predicted.res.aggr = NULL,
                                               error.test.res = NULL,
                                               error.test.res.mean = NULL,
                                               error.test.res.aggr = NULL,
                                               error.test.repeats = NULL,
                                               error.test.repeats.mean = NULL,
                                               error.test.repeats.sd = NULL,
                                               fitted.bag = NULL,
                                               error.bag = NULL,
                                               varimp = NULL,
                                               call = NULL,
                                               question = NULL) {
                           self$mod <- mod
                           self$mod.name <- mod.name
                           self$type <- type
                           self$y.train <- y.train
                           self$x.name <- x.name
                           self$y.name <- y.name
                           self$xnames <- xnames
                           self$parameters <- parameters
                           self$n.repeats <- n.repeats
                           self$resampler.params <- resampler.params
                           self$resamples <- resamples
                           self$y.train.res <- y.train.res
                           self$y.train.res.aggr <- y.train.res.aggr
                           self$fitted.res <- fitted.res
                           self$fitted.res.aggr <- fitted.res.aggr
                           self$error.train.res <- error.train.res
                           self$error.train.res.mean <- error.train.res.mean
                           self$error.train.res.aggr <- error.train.res.aggr
                           self$error.train.repeats <- error.train.repeats
                           self$error.train.repeats.mean <- error.train.repeats.mean
                           self$error.train.repeats.sd <- error.train.repeats.sd
                           self$y.test.res <- y.test.res
                           self$y.test.res.aggr <- y.test.res.aggr
                           self$predicted.res <- predicted.res
                           self$predicted.res.aggr <- predicted.res.aggr
                           self$error.test.res <- error.test.res
                           self$error.test.res.mean <- error.test.res.mean
                           self$error.test.res.aggr <- error.test.res.aggr
                           self$error.test.repeats <- error.test.repeats
                           self$error.test.repeats.mean <- error.test.repeats.mean
                           self$error.test.repeats.sd <- error.test.repeats.sd
                           self$fitted.bag <- fitted.bag
                           self$error.bag <- error.bag
                           self$varimp <- varimp
                           self$question <- question
                           self$call <- call
                           self$sessionInfo <- sessionInfo()
                         },
                         # Methods
                         # Print rtModCV ----
                         #' @description
                         #' \code{print} method for \code{rtModCV} object
                         print = function() {
                           "R6 show / print method for rtModCV"
                           objcat(paste("Cross-Validated", self$type, "Model"))
                           cat(hilite(self$mod.name), " (",
                               modSelect(self$mod.name, desc = TRUE),
                               ")\n", sep = "")
                          #  cat(
                          #   "             Algorithm: ", self$mod.name, " (",
                          #      modSelect(self$mod.name, desc = TRUE),
                          #      ")\n", sep = "")
                          #  cat("          Outer resampling: n = ",
                          #      self$resampler.params$n.resamples,
                          #      ", type = ", self$resampler.params$resampler, "\n", sep = "")
                          # cat("              N of repeats:", self$n.repeats, "\n")
                          cat(
                            "      Outer resampling: ",
                            print1(self$resamples[[1]], verbose = FALSE),
                            " (", singorplu(self$n.repeats, "repeat"),
                            ")\n", sep = ""
                          )
                          if (!is.null(self$mod[[1]][[1]]$mod1$gridsearch)) {
                            cat(
                              "      Inner resampling:",
                              print.resamplertset(
                                self$mod[[1]][[1]]$mod1$gridsearch$resample.rtset,
                                verbose = FALSE
                              ), "\n"
                            )
                          }
                           if (self$type == "Classification") {
                             cat("Mean Balanced Accuracy:",
                                 self$error.test.repeats.mean$Balanced.Accuracy, "\n")
                           } else {
                             cat("  Mean MSE % reduction:",
                                 self$error.test.repeats.mean$MSE.RED * 100, "\n")
                           }
                         },
                         # Plot rtModCV ----
                         #' @description
                         #' \code{plot} method for \code{rtModCV} object
                         #' @param which.repeat Integer: which repeat to plot
                         #' @param ... Additional arguments passed to plotting function
                         plot = function(which.repeat = 1, ...) {
                           "R6 plot method for rtModCV"
                           self$plotPredicted(which.repeat = which.repeat, ...)
                         },
                         # Plot predicted rtModCV ----
                         #' @description
                         #' Plot predicted values
                         #' @param which.repeat Integer: which repeat to plot
                         #' @param theme rtemis theme
                         #' @param filename Character: path to file to save plot
                         #' @param mar Numeric vector of plot margins
                         #' @param ... Additional arguments passed to plotting function
                         plotPredicted = function(which.repeat = 1,
                                                  theme = rtTheme,
                                                  filename = NULL,
                                                  mar = c(2.5, 3, 2.5, 1), ...) {
                           "R6 method: Plot aggregated predicted vs. true values"
                           predicted <- unlist(self$predicted.res[[which.repeat]],
                                               use.names = FALSE)
                           y.test <- unlist(self$y.test.res[[which.repeat]],
                                            use.names = FALSE)
                           main <- paste0(self$mod.name, " Testing\n(",
                                          self$resampler.params$n.resamples,
                                          " aggregated resamples",
                                          ifelse(self$n.repeats > 1, paste0("; repeat #",
                                                                            which.repeat, ")"),
                                                 ")"))
                           if (self$type == "Classification") {
                             conf <- classError(y.test, predicted)$ConfusionMatrix
                             mplot3_conf(conf, main = main,
                                         # mar = c(3, 3, 5, 3),
                                         dim.main = 2,
                                         theme = theme,
                                         filename = filename, ...)
                           } else if (self$type == "Regression") {
                             mplot3_fit(y.test, predicted,
                                        main = main,
                                        xlab = paste("True", self$y.name),
                                        ylab = paste("Predicted", self$y.name),
                                        theme = theme,
                                        filename = filename,
                                        mar = mar, ...)
                           } else {
                             msg("Plotting for survival not currently supported")
                           }
                         },
                         # Plot fitted rtModCV ----
                         #' @description
                         #' Plot fitted values
                         #' @param which.repeat Integer: which repeat to plot
                         #' @param theme rtemis theme
                         #' @param filename Character: path to file to save plot
                         #' @param mar Numeric vector of plot margins
                         #' @param ... Additional arguments passed to plotting function
                         plotFitted = function(which.repeat = 1,
                                               theme = rtTheme,
                                               filename = NULL,
                                               mar = c(2.5, 3, 2.5, 1), ...) {
                           "R6 method: Plot aggregated fitted vs. true values"
                           fitted <- unlist(self$fitted.res[[which.repeat]],
                                            recursive = TRUE, use.names = FALSE)
                           y.train <- unlist(self$y.train.res[[which.repeat]],
                                             recursive = TRUE, use.names = FALSE)
                           main <- paste0(self$mod.name, " Training\n(",
                                          self$resampler.params$n.resamples,
                                          " aggregated resamples",
                                          ifelse(self$n.repeats > 1, paste0("; repeat #",
                                                                            which.repeat, ")"),
                                                 ")"))
                           if (self$type == "Classification") {
                             conf <- classError(y.train, fitted)$ConfusionMatrix
                             mplot3_conf(conf, main = main,
                                         # mar = c(3, 3, 5, 3),
                                         filename = filename,
                                         theme = theme,
                                         dim.main = 2, ...)
                           } else if (self$type == "Regression") {
                             mplot3_fit(y.train, fitted,
                                        main = main,
                                        xlab = paste("True", self$y.name),
                                        ylab = paste("Fitted", self$y.name),
                                        theme = theme,
                                        filename = filename,
                                        mar = mar, ...)
                           } else {
                             msg("Plotting for survival not currently supported")
                           }
                         },
                         # Plot varimp rtModCV ----
                         #' @description
                         #' Plot variable importance
                         #' @param which.repeat Integer: which repeat to plot
                         #' @param type Character: "barplot" or "lollipop"
                         #' @param plot.top Integer: Plotting this many top features
                         #' @param theme rtemis theme to use
                         #' @param ... Additional arguments passed to plotting function
                         plotVarImp = function(which.repeat = 1,
                                               type = c("barplot", "lollipop"),
                                               plot.top = 12,
                                               theme = rtTheme, ...) {
                           varimp <- colMeans(self$varimp[[which.repeat]])
                           if (length(varimp) == 0) {
                             warning("Variable importance is not available for this model")
                           } else {
                             type <- match.arg(type)
                             if (type == "lollipop") {
                               mplot3_lolli(varimp,
                                            plot.top = plot.top,
                                            xlab = "Variable Importance",
                                            theme = theme, ...)
                             } else {
                               mplot3_varimp(varimp,
                                             plot.top = plot.top,
                                             theme = theme, ...)
                             }
                           }
                         },
                         # Describe rtModCV ----
                         #' @description
                         #' Describe \code{rtModCV}
                         describe = function() {

                           type <- self$type
                           algorithm <- modSelect(self$mod.name, desc = TRUE)
                           cat(type, " was performed using ", algorithm, ".",
                             sep = ""
                           )
                           desc <- paste0(
                             type, " was performed using ",
                             algorithm, "."
                           )

                           # '- Preprocessing ----
                           if (!is.null(self$parameters$preprocess)) {
                             preproc <- self$parameters$preprocess

                             cat(" Data was preprocessed by ")
                             pre <- paste(
                               c(if (preproc$impute) {
                                 paste("imputing missing values using", preproc$impute.type)
                               },
                               if (preproc$scale) "scaling variables",
                               if (preproc$center) "centering variables"
                               ), collapse = ", "
                             )
                             pre <- rev(gsub(",", ", and", rev(pre)))
                             cat(pre, ".", sep = "")
                             desc <- paste(
                               desc, "Data was preprocessed by",
                               pre, "."
                             )
                           }

                           # '- Decomposition ----
                           if (!is.null(self$parameters$decompose)) {
                             decom <- self$parameters$decompose
                             cat(" Input was projected to ", decom$k, " dimensions using ",
                               decomSelect(decom$decom, desc = TRUE), ".",
                               sep = ""
                             )
                             desc <- paste0(
                               desc, " Input was projected to ", decom$k, " dimensions using ",
                               decomSelect(decom$decom, desc = TRUE), "."
                             )
                           }

                           # '- Tuning ----
                           .gs <- length(self$mod[[1]][[1]]$mod1$gridsearch) > 0
                           if (.gs) {
                             res <- self$mod[[1]][[1]]$mod1$gridsearch$resample.rtset
                             n.resamples <- res$n.resamples
                             resampler <- res$resampler
                             resamples <- switch(resampler,
                                                 strat.sub = " stratified subsamples",
                                                 bootstrap = " bootstraps",
                                                 strat.boot = " stratified bootstraps",
                                                 kfold = "-fold crossvalidation")
                             cat(" Hyperparameter tuning was performed using ",
                               n.resamples, resamples, ".",
                               sep = ""
                             )
                             desc <- paste0(
                               desc, 
                               " Hyperparameter tuning was performed using ",
                               n.resamples, resamples, "."
                             )
                           }

                           # '- Performance ----
                           n.repeats <- self$n.repeats
                           n.resamples <- self$resampler.params$n.resamples
                           resampler <- self$resampler.params$resampler
                           resamples <- switch(resampler,
                                               strat.sub = " stratified subsamples",
                                               bootstrap = " bootstraps",
                                               strat.boot = " stratified bootstraps",
                                               loocv = "leave-one-out crossvalidation",
                                               kfold = "-fold cross validation")
                           cat(" Model generalizability was assessed using ")
                           desc <- paste0(
                             desc,
                             " Model generalizability was assessed using "
                           )
                           if (n.repeats > 1) {
                             cat(n.repeats, "repeats of ")
                             desc <- paste0(desc, n.repeats, "repeats of ")
                           }
                           if (resampler != "loocv") {
                             cat(n.resamples, resamples, ".", sep = "")
                             desc <- paste0(desc, n.resamples, resamples, ".")
                           } else {
                             cat(resamples, ".", sep = "")
                             desc <- paste(desc, resamples, ".")
                           }
                           if (type == "Classification") {
                             cat(" The mean Balanced Accuracy across all testing set resamples was ",
                               ddSci(self$error.test.repeats.mean$`Balanced.Accuracy`),
                               ".",
                               sep = ""
                             )
                             desc <- paste0(
                               desc, " The mean Balanced Accuracy across all testing set resamples was ",
                               ddSci(self$error.test.repeats.mean$`Balanced.Accuracy`),
                               "."
                             )
                           } else if (type == "Regression") {
                             cat(" The mean R-squared across all testing set resamples was ",
                               ddSci(self$error.test.repeats.mean$Rsq),
                               ".",
                               sep = ""
                             )
                             desc <- paste0(
                               desc, " The mean R-squared across all testing set resamples was ",
                               ddSci(self$error.test.repeats.mean$Rsq),
                               "."
                             )
                           } else {
                             # Survival
                           }
                           cat("\n")
                           invisible(desc)
                         } # /rtModCV$describe()
                       ))

# rtModCV S3 methods ####
#' S3 methods for \code{rtModCV} class that differ from those of the \code{rtMod} superclass
#'
#' @name rtModCV-methods
NULL


#' \code{plot.rtModCV}: \code{plot} method for \code{rtModCV} object
#'
#' @method plot rtModCV
#' 
#' @param x \code{rtModCV} object
#' @param ... Additional arguments to pass to plotting function
#' 
#' @rdname rtModCV-methods
#' @export
plot.rtModCV <- function(x, ...) {

  x$plot(...)

} # rtemis::plot.rtModCV


#' \code{summary.rtModCV}: \code{summary} method for \code{rtModCV} object
#'
#' @method summary rtModCV
#' 
#' @param object \code{rtModCV} object
#' @param ... Not used
#' 
#' @rdname rtModCV-methods
#' @export
summary.rtModCV <- function(object, ...) {

  object$print()

} # rtemis::summary.rtModCV


#' \code{predict.rtModCV}: \code{predict} method for \code{rtModCV} object
#'
#' @method predict rtModCV
#' @param object \code{rtModCV} object
#' @param newdata Set of predictors to use
#' @param which.repeat Integer: Which repeat to use for prediction
#' @param bag.fn Function to use to average predictions of different models
#' @param n.cores Integer: Number of cores to use
#' 
#' @rdname rtModCV-methods
#' @export
predict.rtModCV <- function(object, newdata,
                            which.repeat = 1,
                            bag.fn = mean,
                            n.cores = 1, ...) {

  # extraArgs <- list(...)
  mods <- object$mod[[which.repeat]]
  predicted <- as.data.frame(pbapply::pbsapply(
    mods, function(i) as.numeric(predict(i$mod1, newdata)),
                                               cl = n.cores))
  if (object$type == "Classification") {
    predicted <- apply(predicted, 1, function(j) round(bag.fn(j)))
    predicted <- factor(predicted)
    levels(predicted) <- levels(object$y.train)
  } else {
    predicted <- apply(predicted, 1, bag.fn)
  }

} # rtemis::predict.rtModCV


# rtModCVClass R6 ----
#' \pkg{rtemis} Cross-Validated Classification Model Class
#'
#' R6 Class for \pkg{rtemis} Cross-Validated Classification Models
#'
#' @docType class
#' @name rtModCVClass-class
#' 
#' @field fitted.prob.aggr Aggregated training set probability estimates
#' @field predicted.prob.aggr Aggregated testing set probability estimates
#' 
#' @author E.D. Gennatas
#' @export
rtModCVClass <- R6::R6Class("rtModCVClass",
                            inherit = rtModCV,
                            public = list(fitted.prob.aggr = NULL,
                                          predicted.prob.aggr = NULL,
                                          # Initialize ----
                                          #' @description
                                          #' Initialize \code{rtModCVClass} object
                                          #'
                                          #' @param mod \code{rtModCV} object
                                          #' @param mod.name Model (algorithm) name
                                          #' @param type "Classification", "Regression", or "Survival"
                                          #' @param y.train Training set y data
                                          #' @param y.test Testing set y data
                                          #' @param x.name Name of x data
                                          #' @param y.name Name of y data
                                          #' @param xnames Character vector: Column names of x
                                          #' @param parameters List of algorithm hyperparameters
                                          #' @param resampler List of settings for \link{resample}.
                                          #' Set using \link{rtset.cv.resample}
                                          #' @param n.repeats Integer: Number of repeats. This is the outermost iterator: i.e. You will run
                                          #' \code{resampler} this many times.
                                          #' @param resampler.params List of resampling parameters
                                          #' @param resamples Resamples produced by \link{resample}
                                          #' @param y.train.res Resamples' fitted values
                                          #' @param y.train.res.aggr Aggregated training set outcomes
                                          #' @param fitted.res Resamples' fitted values
                                          #' @param fitted.res.aggr Aggregated fitted values
                                          #' @param fitted.prob.aggr Aggregated training set probability estimates
                                          #' @param error.train.res Resamples' training error
                                          #' @param error.train.res.mean Resamples' mean training error
                                          #' @param error.train.res.aggr Error of aggregated fitted values
                                          #' @param error.train.repeats Mean training error for each repeat
                                          #' @param error.train.repeats.mean Mean training error across all repeats
                                          #' @param error.train.repeats.sd Standard deviation of training error across all repeats
                                          #' @param y.test.res Resamples' predicted values
                                          #' @param y.test.res.aggr Aggregated testing set outcomes
                                          #' @param predicted.res Resamples' predicted values
                                          #' @param predicted.res.aggr Aggregated predicted values
                                          #' @param error.test.res Resamples' testing error
                                          #' @param error.test.res.mean Resamples' mean testing error
                                          #' @param error.test.res.aggr Error of aggregated predicted values
                                          #' @param predicted.prob.aggr Aggregated testing set probability estimates
                                          #' @param error.test.repeats Mean testing error for each repeat
                                          #' @param error.test.repeats.mean Mean testing error across all repeats
                                          #' @param error.test.repeats.sd Standard deviation of testing error across all repeats
                                          #' @param fitted.bag Bagged model's fitted values
                                          #' @param error.bag Bagged model's error
                                          #' @param varimp Resamples' variable importance
                                          #' @param question Question the model is hoping to answer
                                          #' @param call elevate call
                                          #' @param sessionInfo R session info at time of training
                                          initialize = function(mod = NULL,
                                                                mod.name = NULL,
                                                                type = NULL,
                                                                y.train = NULL,
                                                                x.name = NULL,
                                                                y.name = NULL,
                                                                xnames = NULL,
                                                                parameters = NULL,
                                                                n.repeats = NULL,
                                                                resampler.params = NULL,
                                                                resamples = NULL,
                                                                y.train.res = NULL,
                                                                y.train.res.aggr = NULL,
                                                                fitted.res = NULL,
                                                                fitted.res.aggr = NULL,
                                                                fitted.prob.aggr = NULL,
                                                                error.train.res = NULL,
                                                                error.train.res.mean = NULL,
                                                                error.train.res.aggr = NULL,
                                                                error.train.repeats = NULL,
                                                                error.train.repeats.mean = NULL,
                                                                error.train.repeats.sd = NULL,
                                                                y.test.res = NULL,
                                                                y.test.res.aggr = NULL,
                                                                predicted.res = NULL,
                                                                predicted.res.aggr = NULL,
                                                                predicted.prob.aggr = NULL,
                                                                error.test.res = NULL,
                                                                error.test.res.mean = NULL,
                                                                error.test.res.aggr = NULL,
                                                                error.test.repeats = NULL,
                                                                error.test.repeats.mean = NULL,
                                                                error.test.repeats.sd = NULL,
                                                                fitted.bag = NULL,
                                                                error.bag = NULL,
                                                                varimp = NULL,
                                                                call = NULL,
                                                                question = NULL) {
                                            super$initialize(mod,
                                                             mod.name,
                                                             type,
                                                             y.train,
                                                             x.name,
                                                             y.name,
                                                             xnames,
                                                             parameters,
                                                             n.repeats,
                                                             resampler.params,
                                                             resamples,
                                                             y.train.res,
                                                             y.train.res.aggr,
                                                             fitted.res,
                                                             fitted.res.aggr,
                                                             error.train.res,
                                                             error.train.res.mean,
                                                             error.train.res.aggr,
                                                             error.train.repeats,
                                                             error.train.repeats.mean,
                                                             error.train.repeats.sd,
                                                             y.test.res,
                                                             y.test.res.aggr,
                                                             predicted.res,
                                                             predicted.res.aggr,
                                                             error.test.res,
                                                             error.test.res.mean,
                                                             error.test.res.aggr,
                                                             error.test.repeats,
                                                             error.test.repeats.mean,
                                                             error.test.repeats.sd,
                                                             fitted.bag,
                                                             error.bag,
                                                             varimp,
                                                             call,
                                                             question)
                                            self$fitted.prob.aggr <- fitted.prob.aggr
                                            self$predicted.prob.aggr <- predicted.prob.aggr
                                          },
                                          # Methods
                                          # Plot ROC ----
                                          #' @description
                                          #' Plot ROC
                                          #' @param which.repeat Integer: Which repeat to plot
                                          #' @param ... Additional arguments passed to plotting function
                                          plotROC = function(which.repeat = 1, ...) {
                                            self$plotROCpredicted(which.repeat = which.repeat, ...)
                                          },
                                          # Plot fitted ROC ----
                                          #' @description
                                          #' Plot ROC plot for fitted values
                                          #' @param which.repeat Integer: Which repeat to plot
                                          #' @param main Character: Main title
                                          #' @param ... Additional arguments passed to plotting function
                                          plotROCfitted = function(which.repeat = 1,
                                                                   main = "ROC Training", ...) {
                                            if (!is.null(self$fitted.prob.aggr[[which.repeat]])) {
                                              mplot3_roc(self$fitted.prob.aggr[[which.repeat]],
                                                         self$y.train.res.aggr[[which.repeat]],
                                                         main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          # Plot predicted ROC ----
                                          #' @description
                                          #' Plot ROC plot for predicted values
                                          #' @param which.repeat Integer: Which repeat to plot
                                          #' @param main Character: Main title
                                          #' @param ... Additional arguments passed to plotting function
                                          plotROCpredicted = function(which.repeat = 1,
                                                                      main = "ROC Testing", ...) {
                                            if (!is.null(self$predicted.prob.aggr[[which.repeat]])) {
                                              mplot3_roc(self$predicted.prob.aggr[[which.repeat]],
                                                         self$y.test.res.aggr[[which.repeat]],
                                                         main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          # Plot Precision-Recall curve ----
                                          #' @description
                                          #' Plot Precision-Recall curve
                                          #' @param which.repeat Integer: Which repeat to use
                                          #' @param ... Additional arguments passed to
                                          #' \code{plotPRpredicted} method
                                          plotPR = function(which.repeat = 1, ...) {
                                            self$plotPRpredicted(which.repeat = which.repeat, ...)
                                          },
                                          # Plot fitted Precision-Recall curve ----
                                          #' @description
                                          #' Plot fitted Precision-Recall curve
                                          #' @param which.repeat Integer: Which repeat to use
                                          #' @param main Character: main title
                                          #' @param ... Additional arguments passed to
                                          #' \code{mplot3_pr}
                                          plotPRfitted = function(which.repeat = 1,
                                                                  main = "P-R Training", ...) {
                                            if (!is.null(self$fitted.prob.aggr[[which.repeat]])) {
                                              mplot3_pr(self$fitted.prob.aggr[[which.repeat]],
                                                        self$y.train.res.aggr[[which.repeat]],
                                                        main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          # Plot predicted Precision-Recall curve ----
                                          #' @description
                                          #' Plot predicted Precision-Recall curve
                                          #' @param which.repeat Integer: Which repeat to use
                                          #' @param main Character: main title
                                          #' @param ... Additional arguments passed to
                                          #' \code{mplot3_pr}
                                          plotPRpredicted = function(which.repeat = 1,
                                                                     main = "P-R Testing", ...) {
                                            if (!is.null(self$predicted.prob.aggr[[which.repeat]])) {
                                              mplot3_pr(self$predicted.prob.aggr[[which.repeat]],
                                                        self$y.test.res.aggr[[which.repeat]],
                                                        main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          })
)

# rtMeta R6 ----
#' \pkg{rtemis} Meta Model Class
#'
#' R6 Class for \pkg{rtemis} Meta Models
#'
#' @docType class
#' @name rtMeta-class
#' @field grid Grid of algorithm names and resamples IDs
#' @field base.resample.rtset List of resampling parameters for base learner training
#' @field base.mod.names Character vector: names of base learner algorithms
#' @field base.params Named list of lists with base model parameters
#' @field base.res.y.test Base resamples' testing set outcomes
#' @field base.res.predicted Base resamples' predicted valuess
#' @field base.mods Base learners
#' @field base.mods.error.train Base learners' training error
#' @field base.mods.error.test Base learners' testing error
#' @field meta.mod.name Name of meta algorithm
#' @field meta.params List of meta model parameters
#' @field meta.mod Meta model
#' @field sessionInfo R session info at training time
#' @export
rtMeta <- R6::R6Class("rtMeta",
                      inherit = rtMod,
                      public = list(
                        grid = NULL,
                        base.resample.rtset = NULL,
                        base.mod.names = NULL,
                        base.params = NULL,
                        base.res.y.test = NULL,
                        base.res.predicted = NULL,
                        base.mods = NULL,
                        base.mods.error.train = NULL,
                        base.mods.error.test = NULL,
                        meta.mod.name = NULL,
                        meta.params = NULL,
                        meta.mod = NULL,
                        sessionInfo = NULL,
                        # Initialize rtMeta ----
                        #' @description
                        #' Initialize \code{rtMeta} object
                        #' @param mod.name Character: Algorithm name
                        #' @param y.train Training set output
                        #' @param y.test Testing set output
                        #' @param x.name Character: Feature set name
                        #' @param y.name Character: Output name
                        #' @param xnames Character vector: Feature names
                        #' @param grid Data frame with combinations of algorithm
                        #' names and resample IDs
                        #' @param base.resample.rtset List with resampling 
                        #' parameters for base learners
                        #' @param base.mod.names Base learner algorithm names
                        #' @param base.params Named list of lists with base model parameters
                        #' @param base.res.y.test Base learners' predicted values
                        #' @param base.res.predicted Base resamples' predicted valuess
                        #' @param base.res.mods Base learner fitted models
                        #' @param base.mods Base learners
                        #' @param base.mods.error.train Base learners' training error
                        #' @param base.mods.error.test Base learners' testing error
                        #' @param meta.mod.name Meta model algorithm name
                        #' @param meta.params Meta model hyperparameters
                        #' @param meta.mod Trained meta model
                        #' @param type Character: Type of model (Regression, Classification, Survival)
                        #' @param fitted Fitted values (training set predictions)
                        #' @param se.fit Standard error of fitted values
                        #' @param error.train Training set error
                        #' @param predicted Predicted values (Testing set predictions)
                        #' @param se.prediction Standard error of predicted values
                        #' @param error.test Testing set error
                        #' @param question Question the model is trying to answer
                        #' @param extra List of extra model info
                        #' @param sessionInfo R session info at time of training
                        initialize = function(mod.name = character(),
                                              # call = call("NULL"),
                                              y.train = numeric(),
                                              y.test = numeric(),
                                              x.name = character(),
                                              y.name = character(),
                                              xnames = character(),
                                              grid = data.frame(),
                                              base.resample.rtset = list(),
                                              base.mod.names = character(),
                                              base.params = list(),
                                              base.res.y.test = numeric(),
                                              base.res.predicted = numeric(),
                                              base.mods = list(),
                                              base.mods.error.train = list(),
                                              base.mods.error.test = list(),
                                              meta.mod.name = character(),
                                              meta.params = list(),
                                              meta.mod = list(),
                                              type = character(),
                                              fitted = numeric(),
                                              se.fit = numeric(),
                                              error.train = list(),
                                              predicted = numeric(),
                                              se.prediction = numeric(),
                                              error.test = list(),
                                              question = character(),
                                              extra = list()) {
                          self$mod.name <- mod.name
                          self$y.train <- y.train
                          self$y.test <- y.test
                          self$x.name <- x.name
                          self$y.name <- y.name
                          self$xnames <- xnames
                          self$grid <- grid
                          self$base.resample.rtset <- base.resample.rtset
                          self$base.mod.names <- base.mod.names
                          self$base.params <- base.params
                          self$base.res.y.test <- base.res.y.test
                          self$base.res.predicted <- base.res.predicted
                          self$base.mods <- base.mods
                          self$base.mods.error.train <- base.mods.error.train
                          self$base.mods.error.test <- base.mods.error.test
                          self$meta.mod.name <- meta.mod.name
                          self$meta.params <- meta.params
                          self$meta.mod <- meta.mod
                          self$type <- type
                          self$fitted <- fitted
                          self$se.fit <- se.fit
                          self$error.train <- error.train
                          self$predicted <- predicted
                          self$se.prediction <- se.prediction
                          self$error.test <- error.test
                          self$question <- question
                          self$extra <- extra
                          self$sessionInfo <- sessionInfo()
                        },
                        ### Methods
                        # Print rtMeta ----
                        #' @description
                        #' Plot method for \code{rtMeta} objects
                        print = function() {
                          "R6 show / print method for rtMeta"
                          objcat("Meta Model")
                          cat("   Base: ", hilite(paste(self$base.mod.names,
                                                                  collapse = ", ")), "\n")
                          cat("   Meta: ", hilite(self$meta.mod.name), "\n")
                          boxcat("Training Error")
                          print(self$error.train)
                          if (length(self$error.test) > 0) {
                            boxcat("Testing Error")
                            print(self$error.test)
                          }
                        }
                      ))

# rtMeta S3 methods ####
#' rtMeta S3 methods
#'
#' S3 methods for \code{rtMeta} class that differ from those of the \code{rtMod} superclass
#'
#'
#' @name rtMeta-methods
NULL

#' \code{predict} method for \code{rtMeta} object
#'
#' @method predict rtMeta
#' 
#' @param object \code{rtMeta} object
#' @param newdata Testing set features
#' @param fn Function to average predictions
#' 
#' @rdname rtMeta-methods
#' @export
predict.rtMeta <- function(object, 
                           newdata, 
                           fn = median, ...) {

  if (missing(newdata)) {
    predicted <- object$predicted
  } else {
    base.predicted <- as.data.frame(
      sapply(object$base.mods, function(mod) {
        predict(mod, newdata = newdata, ...)
      })
    )
    predicted <- predict(object$meta.mod, newdata = base.predicted)
  }

  if (object$type == "Regression") {
    predicted <- as.numeric(predicted)
  } else {
    predicted <- levels(object$y.train)[predicted]
  }
  return(predicted)

} # rtemis::predict.rtMeta


# rtModLite R6 ----
#' \pkg{rtemis} Lite Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Lite Supervised Models
#'
#' Note on terminology for all models:
#' The training set is used to build a model. The testing set is a separate set never touched during
#' training and only used to a. create predictions using the trained model and
#' b. estimate error metrics.
#' This reflects generalizability of the model and is the error we care the most about.
#' It is saved in rtemis models
#' as \code{error.test}.
#' The validation set is used during model tuning. Within rtemis, validation sets are created and
#' used automatically by \link{resample} when appropriate, they are not generally input by the user
#' (with few exceptions).
#' @docType class
#' @name rtModLite-class
#' 
#' @field mod.name Algorithm name
#' @field mod Trained model
#' @field fitted Vector of fitted values
#' 
#' @export
rtModLite <- R6::R6Class("rtModLite",
                         public = list(
                           ### Attributes
                           mod.name = NULL,
                           mod = NULL,
                           fitted = NULL,
                           # Initialize rtModLite ----
                           #' @description
                           #' Initialize \code{rtModLite} object
                           #' @param mod Trained model
                           #' @param mod.name Algorithm name
                           #' @param fitted Vector of fitted values
                           initialize = function(mod = list(),
                                                 mod.name = character(),
                                                 fitted = numeric()) {
                             self$mod.name <- mod.name
                             self$mod <- mod
                             self$fitted <- fitted
                           },
                           ### Methods
                           # Print rtModLite ----
                           #' @description
                           #' Plot method for \code{rtModLite} object
                           print = function() {
                             "show / print method for rtModLite"
                             objcat("Lite Supervised Model")
                             cat(self$mod.name, " (", modSelect(self$mod.name, desc = TRUE),
                                 ")\n", sep = "")
                           }
                         )) # /rtModLite

# rtModLite S3 methods ----
#' rtModLite S3 methods
#'
#' S3 methods for \code{rtModLite} class.
#'
#' @name rtModLite-methods
NULL


#' \code{print.rtModLite}: \code{print} method for \code{rtModLite} object
#'
#' @method print rtModLite
#' 
#' @param x \code{rtModLite} object
#' @param ... Not used
#' 
#' @rdname rtModLite-methods
#' @export
print.rtModLite <- function(x, ...) {

  x$print()

} # rtemis::print.rtModLite


#' \code{predict.rtModLite}: \code{predict} method for \code{rtModLite} object
#'
#' @method predict rtModLite
#' 
#' @param object \code{rtModLite} object
#' @param newdata Testing set features
#' @param ... Additional argument passed to \code{predict(object)}
#' 
#' @rdname rtMod-methods
#' @export

predict.rtModLite <- function(object, newdata, ...) {

  extraArgs <- list(...)

  if (missing(newdata)) {
    estimated <- object$fitted
  } else {
    if (object$mod.name == "ADDT") {
      if (is.null(extraArgs$learning.rate)) stop("Please provide learning.rate")
      if (is.null(extraArgs$n.feat)) stop("Please provide n.feat")
      estimated <- predict(object$mod, newdata = newdata,
                           learning.rate = extraArgs$learning.rate,
                           n.feat = extraArgs$n.feat, ...)
    } else if (object$mod.name == "BRUTO") {
      # BRUTO: newdata must be matrix
      estimated <- predict(object$mod, newdata = as.matrix(newdata))
    } else if (object$mod.name == "GBM") {
      # GBM: must supply n.trees
      if (is.null(extraArgs$n.trees)) stop("Please provide n.trees")
      estimated <- predict(object$mod, n.trees = extraArgs$n.trees, ...)
    } else if (object$mod.name %in% c("XGBOOST", "XGBLIN", "XGBDART")) {
      # XGB: must convert newdata to xgb.DMatrix
      if (any(sapply(newdata, is.factor))) {
        newdata <- preprocess(newdata, oneHot = TRUE)
      }
      estimated <- predict(object$mod, newdata = xgboost::xgb.DMatrix(as.matrix(newdata)))
    } else if (object$mod.name == "MXFFN") {
      estimated <- predict(object$mod, data.matrix(newdata), array.layout = "rowmajor")
    } else {
      estimated <- predict(object$mod, newdata = newdata, ...)
    }
  }

  if (object$type == "Regression" || object$type == "Survival") {
    estimated <- as.numeric(estimated)
  }

  estimated

} # rtemis::predict.rtModLite


# rtMod.out ----
# for all s. learners
# Print plots / save plots and mods to outdir
#' rtMod.out
#' 
#' @param rt rtemid object
#' @param print.plot Logical: If TRUE, print plot
#' @param plot.fitted Logical: If TRUE, print fitted values plot
#' @param plot.predicted Logical: If TRUE, print predicted values plot
#' @param y.test Test set predictions
#' @param mod.name Character: name of algorithm used to train model
#' @param outdir Character: Path to directory to save model
#' @param save.mod Logical: if TRUE, save trained model to \code{outdir},
#' otherwise save only log, summary, and plots
#' @param verbose Logical: If TRUE, print messages in console
#' @param theme rtemis theme to use
#' @param ... Additional arguments passed to \code{plot(rt)}
#' 
#' @author E.D. Gennatas
#' @keywords internal

rtMod.out <- function(rt,
                      print.plot,
                      plot.fitted,
                      plot.predicted,
                      y.test,
                      mod.name,
                      outdir,
                      save.mod,
                      verbose,
                      theme, ...) {

  if (!is.null(outdir)) {
    filename.train <- paste0(outdir, "s_", mod.name, "_Fitted.vs.True.pdf")
    if (!is.null(y.test)) {
      filename.test <- paste0(outdir, "s_", mod.name, "_Predicted.vs.True.pdf")
    }
  } else {
    filename.train <- filename.test <- NULL
  }

  if (print.plot || !is.null(outdir)) {
    if (plot.fitted || !is.null(outdir)) {
      plot(rt,
        estimate = "fitted",
        print.plot = plot.fitted,
        filename = filename.train,
        theme = theme, ...
      )
    }
    if (!is.null(y.test) && (plot.predicted || !is.null(outdir))) {
      plot(rt,
        estimate = "predicted",
        print.plot = plot.predicted,
        filename = filename.test,
        theme = theme, ...
      )
    }
  }
  if (save.mod) rtSave(rt, outdir, verbose = verbose)
} # rtemis::rtMod.out

cat("                                        
            d8P                        
         d888888P
  88bd88b  ?88'   d8888b  88bd8b,d88b   88b .d888b,
  88P'  `  88P   d8b_,dP  88P'`?8P'?8b  88P ?8b,
 d88       88b   88b     d88  d88  88P d88    `?8b
d88'       `?8b  `?888P'd88' d88'  88bd88' `?888P'

")
