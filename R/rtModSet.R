# rtModSet.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' \pkg{rtemis} model
#'
#' Creates an \pkg{rtemis} object of your choice of class. All available class systems are supported,
#' but at this point \code{R6} is the default and most widely tested.
#'
#' @param rtclass Character: Which class system to use. Previously "S3", "S4", "RC", "R6".
#' Currently, only "R6", aka "rtMod" is supported.
#' @param mod Trained model
#' @param mod.name Model (algorithm) name
#' @param type "Regression" or "Classification"
#' @param parameters List of model hyperparameters
#' @param call Originating call
#' @param y.train Training y data
#' @param y.test Validation y data
#' @param x.name Name of x data
#' @param y.name Name of y data
#' @param xnames Column names of x
#' @param fitted Fitted values
#' @param fitted.prob Fitted probabilities (Classification only)
#' @param se.fit Standard error of the fit
#' @param error.train Training error
#' @param predicted Predicted values
#' @param predicted.prob Predicted probabilities (Classification only)
#' @param se.prediction Standard error of the prediction
#' @param error.test Validation error
#' @param varimp Variable importance
#' @param question Question the model is attempting to answer
#' @param extra List with algorithm-specific extra fields
#' @author Efstathios D. Gennatas
#' @export

rtModSet <- function(rtclass = "rtMod",
                     mod = list(),
                     mod.name = character(),
                     type = character(),
                     parameters = list(),
                     call = "",
                     y.train = numeric(),
                     y.test = numeric(),
                     x.name = character(),
                     y.name = character(),
                     xnames = character(),
                     bag.resample.rtset = list(),
                     fitted.bag = numeric(),
                     fitted = numeric(),
                     fitted.prob = numeric(),
                     se.fit.bag = numeric(),
                     se.fit = numeric(),
                     error.train = list(),
                     predicted.bag = numeric(),
                     predicted = numeric(),
                     predicted.prob = numeric(),
                     se.prediction.bag = numeric(),
                     se.prediction = numeric(),
                     error.test = list(),
                     varimp = numeric(),
                     question = character(),
                     extra = list()) {

  # [ ARGUMENTS ]
  # rtclass will be NULL in all s.* learners so that it can be set to the same default
  if (is.null(rtclass)) rtclass <- "rtMod"
  if (rtclass == "R6") rtclass <- "rtMod"
  if (type == "Classification" && rtclass == "rtMod") rtclass <- "rtModClass"

  # In S3, we set missing attributes to NULL,
  # while in all other types we store them as empty vectors of a given type
  if (rtclass != "S3") {
    se.fit <- ifNotNull(se.fit, numeric)
    predicted <- ifNotNull(predicted, numeric)
    se.prediction <- ifNotNull(se.prediction, numeric)
    error.test <- ifNotNull(error.test, list)
  }

  if (rtclass == "S3") {
    # S3 ====
    s.out <- list(mod = mod,
                  mod.name = mod.name,
                  type = type,
                  call = call,
                  y.train = y.train,
                  y.test = y.test,
                  x.name = x.name,
                  y.name = y.name,
                  xnames = xnames,
                  fitted = fitted,
                  se.fit = se.fit,
                  error.train = error.train,
                  predicted = predicted,
                  se.prediction = se.prediction,
                  error.test = error.test,
                  question = question)
    class(s.out) <- c("rtemis", "list")
  } else if (rtclass == "S4") {
    # S4 ====
    s.out <- new("rtemisS4",
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 call = call,
                 y.train = y.train,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 se.fit = se.fit,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = se.prediction,
                 error.test = error.test,
                 question = question)
  # } else if (rtclass == "RC") {
  #   # RC ====
  #   s.out <- rtemisRC(mod = mod,
  #                     mod.name = mod.name,
  #                     type = type,
  #                     call = call,
  #                     y.train = y.train,
  #                     y.test = y.test,
  #                     x.name = x.name,
  #                     y.name = y.name,
  #                     xnames = xnames,
  #                     fitted = fitted,
  #                     se.fit = se.fit,
  #                     error.train = error.train,
  #                     predicted = predicted,
  #                     se.prediction = se.prediction,
  #                     error.test = error.test,
  #                     question = question)
  } else if (rtclass == "rtMod") {
    # R6: rtMod ====
    s.out <- rtMod$new(mod = mod,
                       mod.name = mod.name,
                       type = type,
                       parameters = parameters,
                       # call = call,
                       y.train = y.train,
                       y.test = ifNotNull(y.test, numeric),
                       x.name = ifNotNull(x.name, character),
                       y.name = ifNotNull(y.name, character),
                       xnames = ifNotNull(xnames, character),
                       fitted = fitted,
                       se.fit = ifNotNull(se.fit, numeric),
                       error.train = error.train,
                       predicted = ifNotNull(predicted, numeric),
                       se.prediction = ifNotNull(se.prediction, numeric),
                       error.test = ifNotNull(error.test, list),
                       varimp = ifNotNull(varimp, numeric),
                       question = ifNotNull(question, character),
                       extra = ifNotNull(extra, list))
  } else if (rtclass == "rtModClass") {
    # R6: rtModClass ====
    s.out <- rtModClass$new(mod = mod,
                            mod.name = mod.name,
                            type = type,
                            parameters = parameters,
                            y.train = y.train,
                            y.test = ifNotNull(y.test, numeric),
                            x.name = ifNotNull(x.name, character),
                            y.name = ifNotNull(y.name, character),
                            xnames = ifNotNull(xnames, character),
                            fitted = fitted,
                            fitted.prob = fitted.prob,
                            se.fit = ifNotNull(se.fit, numeric),
                            error.train = error.train,
                            predicted = predicted,
                            predicted.prob = predicted.prob,
                            se.prediction = se.prediction,
                            error.test = ifNotNull(error.test, list),
                            varimp = ifNotNull(varimp, numeric),
                            question = ifNotNull(question, character),
                            extra = ifNotNull(extra, list))
  }else if (rtclass == "rtModBag") {
    # R6: rtModBag ====
    s.out <- rtModBag$new(mod = mod,
                          mod.name = mod.name,
                          type = type,
                          call = call,
                          y.train = y.train,
                          y.test = ifNotNull(y.test, numeric),
                          x.name = ifNotNull(x.name, character),
                          y.name = ifNotNull(y.name, character),
                          xnames = ifNotNull(xnames, character),
                          bag.resample.rtset = bag.resample.rtset,
                          fitted.bag = fitted.bag,
                          fitted = fitted,
                          se.fit.bag = ifNotNull(se.fit.bag, numeric),
                          se.fit = ifNotNull(se.fit, numeric),
                          error.train = error.train,
                          predicted.bag = ifNotNull(predicted.bag, numeric),
                          predicted = ifNotNull(predicted, numeric),
                          se.prediction.bag = ifNotNull(se.prediction.bag, numeric),
                          se.prediction = ifNotNull(se.prediction, numeric),
                          error.test = ifNotNull(error.test, list),
                          question = ifNotNull(question, character),
                          extra = ifNotNull(extra, list))
  } else if (rtclass == "rtModLite") {
    # R6: rtModLite ====
    s.out <- rtModLite$new(mod = mod,
                           mod.name = mod.name,
                           fitted = fitted)
  }

  s.out

} # rtemis::rtModSet
