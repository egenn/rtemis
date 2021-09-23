# rtMod.R
# ::rtemis::
# 2016-9 E.D. Gennatas lambdamd.org
# TODO: survival analysis in elevate

# rtMod R6 ====
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
#' @field mod.name Learner algorithm name
#' @field y.train Training y data
#' @field y.test Testing y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field mod Trained model
#' @field type Classification, Regression, or Survival
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
                       ### Initialize
                       initialize = function(mod.name = character(),
                                             y.train = numeric(),
                                             y.test = numeric(),
                                             x.name = character(),
                                             y.name = character(),
                                             xnames = character(),
                                             mod = list(),
                                             type = character(),
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
                       print = function() {
                         "show / print method for rtMod"
                         objcat("Supervised Model")
                         cat(rtHighlight$bold(self$mod.name), " (", modSelect(self$mod.name, desc = TRUE),
                             ")\n", sep = "")
                         boxcat("Training Error")
                         print(self$error.train)
                         if (length(self$error.test) > 0) {
                           boxcat("Testing Error")
                           print(self$error.test)
                         }
                       },
                       plot = function(estimate = NULL,
                                       theme = getOption("rt.fit.theme", "lightgrid"),
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
                       plotFitted = function(print.plot = TRUE,
                                             theme = getOption("rt.fit.theme", "lightgrid"),
                                             main = NULL,
                                             filename = NULL, ...) {
                         "Plot fitted vs. true values for Regression or confusion matrix for Classification"
                         if (self$type == "Regression") {
                           mplot3.fit(self$y.train, self$fitted,
                                      xlab = paste("True", self$y.name),
                                      ylab = paste("Fitted", self$y.name),
                                      main = if (is.null(main)) paste(self$mod.name, "Training") else main,
                                      theme = theme,
                                      filename = filename, ...)
                         } else if (self$type == "Classification") {
                           mplot3.conf(self$error.train,
                                       xlab = "Reference",
                                       ylab = "Fitted",
                                       main = if (is.null(main)) paste(self$mod.name, "Training") else main,
                                       theme = theme,
                                       filename = filename, ...)
                         } else {
                           mplot3.surv(list(True = self$y.train, Fitted = self$fitted),
                                       main = if (is.null(main)) paste(self$mod.name,
                                                    "Kaplan-Meier estimate") else main,
                                       normalize.time = TRUE,
                                       theme = theme,
                                       filename = filename, ...)
                         }
                       },
                       plotPredicted = function(print.plot = TRUE,
                                                theme = getOption("rt.fit.theme", "lightgrid"),
                                                main = NULL,
                                                filename = NULL, ...) {
                         "Plot predicted vs. true values"
                         if (length(self$y.test) < 1 | length(self$predicted) < 1) {
                           warning("No testing data available")
                           return(NULL)
                         }
                         if (self$type == "Regression") {
                           mplot3.fit(self$y.test, self$predicted,
                                      xlab = paste("True", self$y.name),
                                      ylab = paste("Predicted", self$y.name),
                                      main = if (is.null(main)) paste(self$mod.name, "Testing") else main,
                                      theme = theme,
                                      filename = filename, ...)
                         } else  if (self$type == "Classification") {
                           mplot3.conf(self$error.test,
                                       xlab = "Reference",
                                       ylab = "Predicted",
                                       main = if (is.null(main)) paste(self$mod.name, "Testing") else main,
                                       theme = theme,
                                       filename = filename, ...)
                         } else {
                           mplot3.surv(list(True = self$y.test, Predicted = self$predicted),
                                       main = if (is.null(main)) paste(self$mod.name,
                                                    "Kaplan-Meier estimate") else main,
                                       normalize.time = TRUE,
                                       theme = theme,
                                       filename = filename, ...)
                         }
                       },
                       plotFittedPredicted = function(print.plot = TRUE,
                                                      theme = getOption("rt.fit.theme", "lightgrid"),
                                                      filename = NULL, ...) {
                         "Plot fitted & predicted vs. true values"
                         if (self$type == "Classification") {
                           rtlayout(2, 1)
                           mplot3.conf(self$error.train,
                                       xlab = "Refernece",
                                       ylab = "Fitted",
                                       main = paste(self$mod.name, "Training"),
                                       theme = theme, ...)
                           mplot3.conf(self$error.test,
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
                         mplot3.fit(list(Train = self$y.train, Test = self$y.test),
                                    list(Train = self$fitted, Test = self$predicted),
                                    xlab = paste("True", self$y.name),
                                    ylab = paste("Predicted", self$y.name),
                                    main = paste(self$mod.name, "Training & Testing"),
                                    col = c("#18A3AC", "#F48024"),
                                    theme = theme,
                                    filename = filename, ...)
                       },
                       plotVarImp = function(plot.top = 12,
                                             type = c("barplot", "lollipop"),
                                             theme = getOption("rt.fit.theme", "lightgrid"), ...) {
                         if (length(self$varimp) == 0) {
                           warning("Variable importance is not available for this model")
                         } else {
                           type <- match.arg(type)
                           if (type == "lollipop") {
                             mplot3.lolli(self$varimp,
                                          plot.top = plot.top,
                                          xlab = "Variable Importance",
                                          theme = theme, ...)
                           } else {
                             mplot3.varimp(self$varimp,
                                           plot.top = plot.top,
                                           theme = theme, ...)
                           }
                         }
                       },
                       summary = function(plots = TRUE,
                                          cex = 1,
                                          fit.true.line = "lm",
                                          resid.fit.line = "gam",
                                          fit.legend = TRUE,
                                          se.fit = TRUE,
                                          single.fig = TRUE,
                                          summary = TRUE,
                                          theme = getOption("rt.fit.theme", "lightgrid"),
                                          title.col = NULL, ...) {
                         "Get model summary"
                         summary.rtMod(self)
                       },
                       describe = function() {

                         type <- self$type
                         algorithm <- modSelect(self$mod.name, desc = T)
                         cat(algorithm, " was used for ", tolower(type), ".", sep = "")

                         # Tuning ====
                         if (!is.null(self$extra$gridSearch)) {
                           res <- self$extra$gridSearch$resample.rtset
                           n.resamples <- res$n.resamples
                           resampler <- res$resampler
                           resamples <- switch(resampler,
                                               strat.sub = " stratified subsamples",
                                               bootstrap = " bootstraps",
                                               strat.boot = " stratified bootstraps",
                                               kfold = "-fold crossvalidation",
                                               "custom resamples")
                           cat(" Model tuning was performed using ", n.resamples, resamples, ". ", sep = "")
                           params <- self$extra$gridSearch$params$search
                           search.index <- which(lapply(params, length) > 1)
                           fixed.index <- which(lapply(params, length) == 1)
                           fixed <- searched <- list()
                           for (i in seq(search.index)) searched[[i]] <- params[[search.index[i]]]
                           names(searched) <- names(params)[search.index]
                           for (i in seq(fixed.index)) fixed[[i]] <- params[[fixed.index[i]]]
                           names(fixed) <- names(params)[fixed.index]
                           metric <- self$extra$gridSearch$metric

                           if (length(fixed) > 0) {
                             cat("The following parameters were fixed:\n")
                             printls(fixed)
                           }
                           if (length(searched) > 0) {
                             cat("Grid search was performed on:\n")
                             printls(searched)
                           }
                           cat(metric, "was", ifelse(self$extra$gridSearch$maximize,
                                                     "maximized", "minimized"), "with:")
                           printls(self$extra$gridSearch$best.tune)
                         }

                         # Error ====
                         if (type == "Classification") {
                           cat("Training balanced accuracy was ",
                               ddSci(self$error.train$Overall$`Balanced Accuracy`), sep = "")
                           if (!is.null(self$error.test$Overall$`Balanced Accuracy`)) {
                             cat(" and testing balanced accuracy was ",
                                 ddSci(self$error.test$Overall$`Balanced Accuracy`), ".", sep = "")
                           } else {
                             cat(".")
                           }
                         } else if (type == "Regression") {

                           cat(" Training R-squared was ",
                               ddSci(self$error.train$Rsq), sep = "")
                           if (!is.null(self$error.test$`Balanced Accuracy`)) {
                             cat(" and testing R-squared was ",
                                 ddSci(self$error.test$Rsq), ".", sep = "")
                           } else {
                             cat(".")
                           }
                         }
                       } # / describe
                     )) # /rtMod

# rtMod S3 methods ====
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
#' @rdname rtMod-methods
#' @export
print.rtMod <- function(x, ...) {

  x$print()

} # rtemis::print.rtMod


#' \code{fitted.rtMod}: \code{fitted} method for \code{rtMod} object
#'
#' @method fitted rtMod
#' @rdname rtMod-methods
#' @export
fitted.rtMod <- function(object, ...) {

  return(object$fitted)

} # rtemis::fitted.rtMod


#' \code{predict.rtMod}: \code{predict} method for \code{rtMod} object
#'
#' @method predict rtMod
#' @param newdata Testing set features
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

    } else if (object$mod.name == "GBM" | object$mod.name == "GBM3") {
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
      if (type == "Regression" | type == "Survival") {
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
    } else if (object$mod.name == "XGB" | object$mod.name == "XGBLIN" |
               object$mod.name == "XGBDART") {
      # XGB: must convert newdata to xgb.DMatrix
      estimated <- predict(object$mod, newdata = xgboost::xgb.DMatrix(as.matrix(newdata)))
    } else if (object$mod.name == "MXFFN") {
      estimated <- predict(object$mod, data.matrix(newdata), array.layout = "rowmajor")
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

  if (!is.null(object$type) && (object$type == "Regression" | object$type == "Survival")) {
    estimated <- as.numeric(estimated)
  }

  estimated

} # rtemis::predict.rtMod


#' \code{residuals.rtMod}: \code{residuals} method for \code{rtMod} object
#'
#' @method residuals rtMod
#' @rdname rtMod-methods
#' @export
residuals.rtMod <- function(object, ...) {

  as.numeric(object$y.train - object$fitted)

}


#' \code{plot.rtMod}: \code{plot} method for \code{rtMod} object
#'
#' @method plot rtMod
#' @rdname rtMod-methods
#' @export
plot.rtMod <- function(x, estimate = NULL,
                       theme = getOption("rt.fit.theme", "lightgrid"),
                       filename = NULL, ...) {

  x$plot(estimate = estimate,
         theme = theme,
         filename = filename, ...)

} # rtemis::plot.rtemisRC


#' \code{summary.rtMod}: \code{summary} method for \code{rtMod} object
#'
#' @method summary rtMod
#' @param object \pkg{rtemis} model
#' @param plots Logical: If TRUE, print plots. Default = TRUE
#' @param cex Float: Character expansion factor
#' @param fit.true.line \pkg{rtemis} model to use for fitted vs. true line
#'   Options: \code{modSelect()}
#' @param resid.fit.line \pkg{rtemis} model to use for residuals vs. fitted line.
#'   Options: \code{modSelect()}
#' @param fit.legend Logical: If TRUE, print fit legend. Default  = TRUE
#' @param se.fit Logical: If TRUE, plot 2 * standard error bands. Default = TRUE
#' @param single.fig Logical: If TRUE, draw all plots in a single figure. Default = TRUE
#' @param summary Logical: If TRUE, print summary. Default = TRUE
#' @param theme Character: theme to use. Options: "box", "darkbox", "light", "dark"
#' @param title.col Color for main title
#' @param ... Additional arguments to be passed to \link{mplot3.xy}
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
                          theme = getOption("rt.fit.theme", "lightgrid"),
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

  # [ PRINT TRAIN AND TEST ERRORS ]
  boxcat(".:rtemis Summary")
  if (length(question) > 0) cat("Question: ", question, "\n\n")
  cat(mod.name, "Training Error:\n")
  print(object$error.train)
  if (do.test) {
    cat(mod.name, "Testing Error:\n")
    print(object$error.test)
  }

  # [ PLOT ]
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

      # [ PLOTS ]
      # 1. Fitted vs. True
      mplot3.xy(y.train, fitted,
                xlab = "True", ylab = "Fitted", main = "Training",
                fit = fit.true.line, fit.legend = fit.legend, se.fit = se.fit,
                axes.equal = TRUE, fit.error = TRUE, cex = cex,
                point.col = pennCol$lighterBlue, fit.col = pennCol$red,
                theme = theme, par.reset = pr, ...)

      # 2. Predicted vs. True
      if (do.test) {
        mplot3.xy(y.test, predicted,
                  xlab = "True", ylab = "Predicted", main = "Testing",
                  fit = fit.true.line, fit.legend = fit.legend, se.fit = se.fit,
                  axes.equal = TRUE,  fit.error = TRUE, cex = cex,
                  point.col = pennCol$lighterBlue, fit.col = pennCol$red,
                  theme = theme, par.reset = pr, ...)
      }

      # 3. Residuals vs. Fitted
      mplot3.xy(fitted, residuals,
                xlab = "Fitted", ylab = "Residuals", main = "Residuals",
                fit = resid.fit.line, fit.legend = fit.legend, se.fit = se.fit,
                point.col = pennCol$red, fit.col = pennCol$lighterBlue,
                cex = cex, theme = theme, par.reset = pr, ...)

      # 4. Prediction error vs. True
      if (do.test) {
        mplot3.xy(y.test, error^2, main = "Error", # old: predicted, error
                  xlab = "True", ylab = "Squared Error",
                  fit = resid.fit.line, fit.legend = fit.legend, se.fit = se.fit,
                  point.col = pennCol$red, fit.col = pennCol$lighterBlue,
                  cex = cex, theme = theme, par.reset = pr, ...)
      }

      # 5. Residuals Q-Q plot
      mplot3.x(residuals, type = "qqline",
               main = "Residuals Q-Q Plot",
               col = pennCol$blue, qqline.col = pennCol$green,
               cex = cex, theme = theme, par.reset = pr, ...)

      # 6. Error Q-Q plot
      if (do.test) {
        mplot3.x(error, type = "qqline",
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


# rtModClass ====
#' \pkg{rtemis} Classification Model Class
#'
#' R6 Class for \pkg{rtemis} Classification Models
#'
rtModClass <- R6::R6Class("rtModClass",
                          inherit = rtMod,
                          public = list(fitted.prob = NULL,
                                        predicted.prob = NULL,
                                        ### Initialize
                                        initialize = function(mod.name = character(),
                                                              y.train = numeric(),
                                                              y.test = numeric(),
                                                              x.name = character(),
                                                              y.name = character(),
                                                              xnames = character(),
                                                              mod = list(),
                                                              type = character(),
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
                                        plotROC = function(theme = getOption("rt.fit.theme",
                                                                             "lightgrid"),
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
                                        plotROCfitted = function(main = "ROC Training",
                                                                 theme = getOption("rt.fit.theme",
                                                                                   "lightgrid"),
                                                                 filename = NULL, ...) {
                                          if (length(self$fitted.prob) > 0) {
                                            mplot3.roc(self$fitted.prob, self$y.train,
                                                       main = main,
                                                       theme = theme,
                                                       filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        plotROCpredicted = function(main = "ROC Testing",
                                                                    theme = getOption("rt.fit.theme",
                                                                                      "lightgrid"),
                                                                    filename = NULL, ...) {
                                          if (length(self$predicted.prob) > 0) {
                                            mplot3.roc(self$predicted.prob, self$y.test,
                                                       main = main,
                                                       theme = theme,
                                                       filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        plotPR = function(theme = getOption("rt.fit.theme",
                                                                            "lightgrid"),
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
                                        plotPRfitted = function(main = "P-R Training",
                                                                theme = getOption("rt.fit.theme",
                                                                                  "lightgrid"),
                                                                filename = NULL, ...) {
                                          if (length(self$fitted.prob) > 0) {
                                            mplot3.pr(self$fitted.prob, self$y.train,
                                                      main = main,
                                                      theme = theme,
                                                      filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        },
                                        plotPRpredicted = function(main = "P-R Testing",
                                                                   theme = getOption("rt.fit.theme",
                                                                                     "lightgrid"),
                                                                   filename = NULL, ...) {
                                          if (length(self$predicted.prob) > 0) {
                                            mplot3.pr(self$predicted.prob, self$y.test,
                                                      main = main,
                                                      theme = theme,
                                                      filename = filename, ...)
                                          } else {
                                            msg("Estimated probabilities are not available")
                                          }
                                        })
)


# rtModBag R6 ====
#' \pkg{rtemis} Bagged Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Bagged Supervised Models
#'
#' @docType class
#' @name rtModBag-class
#' @field mod.name Model (algorithm) name
#' @field y.train Training y data
#' @field y.test Testing y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field bag.resample.rtset List of settings for \link{resample}.
#' Set using \link{rtset.bag.resample}
#' @field mod Trained model
#' @field fitted Fitted values
#' @field se.fit Standard error of the fit
#' @field error.train Training error
#' @field predicted Predicted values
#' @field se.prediction Standard error of the prediction
#' @field error.test Testing error
#' @field question Question the model is hoping to answer
#' @field extra Algorithm-specific output
#' @export
rtModBag <- R6::R6Class("rtModBag",
                        inherit = rtMod,
                        public = list(
                          bag.resample.rtset = NULL,
                          fitted.bag = NULL,
                          se.fit.bag = NULL,
                          predicted.bag = NULL,
                          se.prediction.bag = NULL,
                          aggr.fn = NULL,
                          ### Initialize
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
                                                se.prediction.bag = numeric(),
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
                            self$se.prediction.bag <- se.prediction.bag
                            self$se.prediction <- se.prediction
                            self$aggr.fn <- aggr.fn
                            self$error.test <- error.test
                            self$varimp <- varimp
                            self$question <- question
                            self$extra <- extra
                          },
                          ### Methods
                          print = function() {
                            "show / print method for rtModBag"
                            objcat("Bagged Supervised Model")
                            cat(rtHighlight$bold(self$mod.name), " (",
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
#' @param newdata Testing set features
#' @param aggr.fn Character: Function to aggregate models' prediction. Default = "median"
#' @param ... Not used
#' @rdname rtModBag-methods
#' @export
predict.rtModBag <- function(object, newdata,
                             aggr.fn = NULL,
                             n.cores = 1,
                             verbose = FALSE, ...) {

  if (verbose) msg("Calculating estimated values of", length(object$mod), "bag resamples")
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


# rtModCV R6 ====
#' \pkg{rtemis} Cross-Validated Supervised Model Class
#'
#' R6 Class for \pkg{rtemis} Cross-Validated Supervised Models
#'
#' @docType class
#' @name rtModCV-class
#' @field mod.name Model (algorithm) name
#' @field y.train Training set y data
#' @field y.test Testing set y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field resampler List of settings for \link{resample}. Set using \link{rtset.cv.resample}
#' @field n.repeats Integer: Number of repeats. This is the outermost iterator: i.e. You will run
#' \code{resampler} this many times.
#' @field mod Trained model
#' @field fitted Fitted values
#' @field se.fit Standard error of the fit
#' @field error.train Training error
#' @field predicted Predicted values
#' @field se.prediction Standard error of the prediction
#' @field error.test Testing error
#' @field question Question the model is hoping to answer
#' @field extra Algorithm-specific output
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
                         sessionInfo = NULL,
                         ### Initialize
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
                           self$sessionInfo <- sessionInfo()
                         },
                         ### Methods
                         print = function() {
                           "R6 show / print method for rtModCV"
                           objcat("Cross-Validated Model")
                           cat(rtHighlight$bold(self$mod.name), " (",
                               modSelect(self$mod.name, desc = TRUE),
                               ")\n", sep = "")
                           cat("                 Algorithm: ", self$mod.name, " (",
                               modSelect(self$mod.name, desc = TRUE),
                               ")\n", sep = "")
                           cat("                Resampling: n = ",
                               self$resampler.params$n.resamples,
                               ", type = ", self$resampler.params$resampler, "\n", sep = "")
                           cat("              N of repeats:", self$n.repeats, "\n")
                           if (self$type == "Classification") {
                             cat(" Mean Balanced Accuracy across repeats =",
                                 self$error.test.repeats.mean$Balanced.Accuracy, "\n")
                           } else {
                             cat(" Mean MSE % reduction across repeats =",
                                 self$error.test.repeats.mean$MSE.RED * 100, "\n")
                           }
                         },
                         plot = function(which.repeat = 1, ...) {
                           "R6 plot method for rtModCV"
                           self$plotPredicted(which.repeat = which.repeat, ...)
                         },
                         plotPredicted = function(which.repeat = 1,
                                                  theme = getOption("rt.fit.theme", "lightgrid"),
                                                  filename = NULL, ...) {
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
                             mplot3.conf(conf, main = main,
                                         # mar = c(3, 3, 5, 3),
                                         dim.main = 2,
                                         theme = theme,
                                         filename = filename, ...)
                           } else if (self$type == "Regression") {
                             mplot3.fit(y.test, predicted,
                                        main = main,
                                        xlab = paste("True", self$y.name),
                                        ylab = paste("Predicted", self$y.name),
                                        theme = theme,
                                        filename = filename, ...)
                           } else {
                             msg("Plotting for survival not currently supported")
                           }
                         },
                         plotFitted = function(which.repeat = 1,
                                               theme = getOption("rt.fit.theme", "lightgrid"),
                                               filename = NULL, ...) {
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
                             mplot3.conf(conf, main = main,
                                         # mar = c(3, 3, 5, 3),
                                         filename = filename,
                                         theme = theme,
                                         dim.main = 2, ...)
                           } else if (self$type == "Regression") {
                             mplot3.fit(y.train, fitted,
                                        main = main,
                                        xlab = paste("True", self$y.name),
                                        ylab = paste("Fitted", self$y.name),
                                        theme = theme,
                                        filename = filename, ...)
                           } else {
                             msg("Plotting for survival not currently supported")
                           }
                         },
                         plotVarImp = function(which.repeat = 1,
                                               type = c("barplot", "lollipop"),
                                               plot.top = 12,
                                               theme = getOption("rt.theme", "lightgrid"), ...) {
                           varimp <- colMeans(self$varimp[[which.repeat]])
                           if (length(varimp) == 0) {
                             warning("Variable importance is not available for this model")
                           } else {
                             type <- match.arg(type)
                             if (type == "lollipop") {
                               mplot3.lolli(varimp,
                                            plot.top = plot.top,
                                            xlab = "Variable Importance",
                                            theme = theme, ...)
                             } else {
                               mplot3.varimp(varimp,
                                             plot.top = plot.top,
                                             theme = theme, ...)
                             }
                           }
                         },
                         describe = function() {

                           type <- self$type
                           algorithm <- modSelect(self$mod.name, desc = TRUE)
                           cat(type, " was performed using ", algorithm, ".", sep = "")

                           # Preprocessing ====
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

                           }

                           # Decomposition ====
                           if (!is.null(self$parameters$decompose)) {
                             decom <- self$parameters$decompose
                             cat(" Input was projected to ", decom$k, " dimensions using ",
                                 decomSelect(decom$decom, desc = TRUE), ".", sep = "")
                           }

                           # Tuning ====
                           .gs <- !is.null(self$mod[[1]][[1]]$mod1$extra$gridSearch)
                           if (.gs) {
                             res <- self$mod[[1]][[1]]$mod1$extra$gridSearch$resample.rtset
                             n.resamples <- res$n.resamples
                             resampler <- res$resampler
                             resamples <- switch(resampler,
                                                 strat.sub = " stratified subsamples",
                                                 bootstrap = " bootstraps",
                                                 strat.boot = " stratified bootstraps",
                                                 kfold = "-fold crossvalidation")
                             cat(" Model tuning was performed using ",
                                 n.resamples, resamples, ".", sep = "")
                           }

                           # Performance ====
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
                           if (n.repeats > 1) cat(n.repeats, "repeats of ")
                           if (resampler != "loocv") {
                             cat(n.resamples, resamples, ".", sep = "")
                           } else {
                             cat(resamples)
                           }
                           if (type == "Classification") {
                             cat(" The mean Balanced Accuracy across all resamples was ",
                                 ddSci(self$error.test.repeats.mean$`Balanced.Accuracy`),
                                 ".", sep = "")
                           } else if (type == "Regression") {
                             cat(" The mean R-squared across all resamples was ",
                                 ddSci(self$error.test.repeats.mean$Rsq),
                                 ".", sep = "")
                           } else {

                           }

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
#' @param x \code{rtModCV} object
#' @param ... Additional arguments to pass to \code{mplot3.fit}
#' @rdname rtModCV-methods
#' @export
plot.rtModCV <- function(x, ...) {

  x$plot(...)

} # rtemis::plot.rtModCV


#' \code{summary.rtModCV}: \code{summary} method for \code{rtModCV} object
#'
#' @method summary rtModCV
#' @rdname rtModCV-methods
#' @export
summary.rtModCV <- function(object, ...) {

  object$print()

} # rtemis::summary.rtModCV


#' \code{predict.rtModCV}: \code{predict} method for \code{rtModCV} object
#'
#' @method predict rtModCV
#' @param newdata Set of predictors to use
#' @rdname rtModCV-methods
#' @export
predict.rtModCV <- function(object, newdata,
                            which.repeat = 1,
                            bag.fn = mean,
                            verbose = TRUE,
                            n.cores = 1, ...) {

  # extraArgs <- list(...)
  mods <- object$mod[[which.repeat]]
  predicted <- as.data.frame(pbapply::pbsapply(mods, function(i) as.numeric(predict(i$mod1, newdata)),
                                               cl = n.cores))
  if (object$type == "Classification") {
    predicted <- apply(predicted, 1, function(j) round(bag.fn(j)))
    predicted <- factor(predicted)
    levels(predicted) <- levels(object$y.train)
  } else {
    predicted <- apply(predicted, 1, bag.fn)
  }

} # rtemis::predict.rtModCV


# rtModCVclass ====
#' \pkg{rtemis} Cross-Validated Classification Model Class
#'
#' R6 Class for \pkg{rtemis} Cross-Validated Classification Models
#'
#' @docType class
#' @name rtModCVclass-class
#' @export
rtModCVclass <- R6::R6Class("rtModCVclass",
                            inherit = rtModCV,
                            public = list(fitted.prob.aggr = NULL,
                                          predicted.prob.aggr = NULL,
                                          ### Initialize
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
                                                             question)
                                            self$fitted.prob.aggr <- fitted.prob.aggr
                                            self$predicted.prob.aggr <- predicted.prob.aggr
                                          },
                                          ### Methods
                                          plotROC = function(which.repeat = 1, ...) {
                                            self$plotROCpredicted(which.repeat = which.repeat, ...)
                                          },
                                          plotROCfitted = function(which.repeat = 1,
                                                                   main = "ROC Training", ...) {
                                            if (!is.null(self$fitted.prob.aggr[[which.repeat]])) {
                                              mplot3.roc(self$fitted.prob.aggr[[which.repeat]],
                                                         self$y.train.res.aggr[[which.repeat]],
                                                         main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          plotROCpredicted = function(which.repeat = 1,
                                                                      main = "ROC Testing", ...) {
                                            if (!is.null(self$predicted.prob.aggr[[which.repeat]])) {
                                              mplot3.roc(self$predicted.prob.aggr[[which.repeat]],
                                                         self$y.test.res.aggr[[which.repeat]],
                                                         main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          plotPR = function(which.repeat = 1, ...) {
                                            self$plotPRpredicted(which.repeat = which.repeat, ...)
                                          },
                                          plotPRfitted = function(which.repeat = 1,
                                                                  main = "P-R Training", ...) {
                                            if (!is.null(self$fitted.prob.aggr[[which.repeat]])) {
                                              mplot3.pr(self$fitted.prob.aggr[[which.repeat]],
                                                        self$y.train.res.aggr[[which.repeat]],
                                                        main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          },
                                          plotPRpredicted = function(which.repeat = 1,
                                                                     main = "P-R Testing", ...) {
                                            if (!is.null(self$predicted.prob.aggr[[which.repeat]])) {
                                              mplot3.pr(self$predicted.prob.aggr[[which.repeat]],
                                                        self$y.test.res.aggr[[which.repeat]],
                                                        main = main, ...)
                                            } else {
                                              msg("Estimated probabilities are not available")
                                            }
                                          })
)

# rtMeta R6 ====
#' \pkg{rtemis} Meta Model Class
#'
#' R6 Class for \pkg{rtemis} Meta Models
#'
#' @docType class
#' @name rtMeta-class
#' @field mod.name Model (algorithm) name
#' @field y.train Training y data
#' @field y.test Testing y data
#' @field x.name Name of x data
#' @field y.name Name of y data
#' @field xnames Character vector: Column names of x
#' @field mod Trained model
#' @field fitted Fitted values
#' @field se.fit Standard error of the fit
#' @field error.train Training error
#' @field predicted Predicted values
#' @field se.prediction Standard error of the prediction
#' @field error.test Testing error
#' @field question Question the model is hoping to answer
#' @field extra Algorithm-specific output
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
                        ### Initialize
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
                        print = function() {
                          "R6 show / print method for rtMeta"
                          objcat("Meta Model")
                          cat("   Base: ", rtHighlight$bold(paste(self$base.mod.names,
                                                                  collapse = ", ")), "\n")
                          cat("   Meta: ", rtHighlight$bold(self$meta.mod.name), "\n")
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

#' \code{predict.rtMeta}: \code{predict} method for \code{rtMeta} object
#'
#' @method predict rtMeta
#' @param newdata Testing set features
#' @rdname rtMeta-methods
#' @export
predict.rtMeta <- function(object, newdata, fn = median, ...) {

  if (missing(newdata)) {
    predicted <- object$predicted
  } else {
    base.predicted <- as.data.frame(sapply(object$base.mods, function(mod)
      predict(mod, newdata = newdata, ...)))
    predicted <- predict(object$meta.mod, newdata = base.predicted)
  }

  if (object$type == "Regression") {
    predicted <- as.numeric(predicted)
  } else {
    predicted <- levels(object$y.train)[predicted]
  }
  return(predicted)

} # rtemis::predict.rtMeta


# rtModLite R6 ====
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
#' @field mod.name Learner algorithm names
#' @field mod Trained model
#' @export
rtModLite <- R6::R6Class("rtModLite",
                         public = list(
                           ### Attributes
                           mod.name = NULL,
                           mod = NULL,
                           fitted = NULL,
                           ### Initialize
                           initialize = function(mod = list(),
                                                 mod.name = character(),
                                                 fitted = numeric()) {
                             self$mod.name <- mod.name
                             self$mod <- mod
                             self$fitted <- fitted
                           },
                           ### Methods
                           print = function() {
                             "show / print method for rtModLite"
                             objcat("Lite Supervised Model")
                             cat(self$mod.name, " (", modSelect(self$mod.name, desc = TRUE),
                                 ")\n", sep = "")
                           }
                         )) # /rtModLite

# rtModLite S3 methods ====
#' rtModLite S3 methods
#'
#' S3 methods for \code{rtModLite} class.
#'
#' @name rtModLite-methods
NULL


#' \code{print.rtModLite}: \code{print} method for \code{rtModLite} object
#'
#' @method print rtModLite
#' @rdname rtModLite-methods
#' @export
print.rtModLite <- function(x, ...) {

  x$print()

} # rtemis::print.rtModLite


#' \code{predict.rtModLite}: \code{predict} method for \code{rtModLite} object
#'
#' @method predict rtModLite
#' @param newdata Testing set features
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
    } else if (object$mod.name == "XGB" | object$mod.name == "XGBLIN" |
               object$mod.name == "XGBDART") {
      # XGB: must convert newdata to xgb.DMatrix
      estimated <- predict(object$mod, newdata = xgboost::xgb.DMatrix(as.matrix(newdata)))
    } else if (object$mod.name == "MXFFN") {
      estimated <- predict(object$mod, data.matrix(newdata), array.layout = "rowmajor")
    } else {
      estimated <- predict(object$mod, newdata = newdata, ...)
    }
  }

  if (object$type == "Regression" | object$type == "Survival") {
    estimated <- as.numeric(estimated)
  }

  estimated

} # rtemis::predict.rtModLite


# rtMod.out ====
# for all s. learners
# Print plots / save plots and mods to outdir
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
    filename.train <- paste0(outdir, "s.", mod.name, "_Fitted.vs.True.pdf")
    if (!is.null(y.test)) {
      filename.test <- paste0(outdir, "s.", mod.name, "_Predicted.vs.True.pdf")
    }
  } else {
    filename.train <- filename.test <- NULL
  }

  if (print.plot | !is.null(outdir)) {
    if (plot.fitted | !is.null(outdir)) plot(rt, estimate = "fitted",
                                             print.plot = plot.fitted,
                                             filename = filename.train,
                                             theme = theme, ...)
    if (!is.null(y.test) & (plot.predicted | !is.null(outdir))) plot(rt, estimate = "predicted",
                                                                     print.plot = plot.predicted,
                                                                     filename = filename.test,
                                                                     theme = theme, ...)
  }
  if (save.mod) rtSave(rt, outdir, verbose = verbose)
}


cat("
                                        d8,
            d8P                        `8P
         d888888P
  88bd88b  ?88'   d8888b  88bd8b,d88b   88b .d888b,
  88P'  `  88P   d8b_,dP  88P'`?8P'?8b  88P ?8b,
 d88       88b   88b     d88  d88  88P d88    `?8b
d88'       `?8b  `?888P'd88' d88'  88bd88' `?888P'

")
