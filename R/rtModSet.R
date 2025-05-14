# rtModSet.R
# ::rtemis::
# 2016-2021 E.D. Gennatas rtemis.org

#' \pkg{rtemis} model
#'
#' Creates an \pkg{rtemis} object of your choice of class. All available class systems are supported,
#' but at this point `R6` is the default and most widely tested.
#'
#' @param rtclass Character: Object class
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
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

rtModSet <- function(
  mod,
  rtclass = "rtMod",
  mod.name = character(),
  type = character(),
  gridsearch = NULL,
  parameters = list(),
  call = "",
  y.train = numeric(),
  y.test = numeric(),
  x.name = character(),
  y.name = character(),
  xnames = character(),
  bag.resample.params = list(),
  fitted.bag = numeric(),
  fitted = numeric(),
  fitted.prob = numeric(),
  se.fit.bag = numeric(),
  se.fit = numeric(),
  error.train = list(),
  predicted.bag = numeric(),
  predicted = numeric(),
  predicted.prob = numeric(),
  se.predicted.bag = numeric(),
  se.prediction = numeric(),
  error.test = list(),
  varimp = numeric(),
  question = character(),
  extra = list()
) {
  # Arguments ----
  # rtclass will be NULL in all s.* learners so that it can be set to the same default
  if (is.null(rtclass)) rtclass <- "rtMod"
  if (type == "Classification" && rtclass == "rtMod") rtclass <- "rtModClass"

  se.fit <- ifNotNull(se.fit, numeric)
  predicted <- ifNotNull(predicted, numeric)
  se.prediction <- ifNotNull(se.prediction, numeric)
  error.test <- ifNotNull(error.test, list)

  if (rtclass == "rtMod") {
    # R6: rtMod ----
    s.out <- rtMod$new(
      mod = mod,
      mod.name = mod.name,
      type = type,
      gridsearch = gridsearch,
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
      extra = ifNotNull(extra, list)
    )
  } else if (rtclass == "rtModClass") {
    # R6: rtModClass ----
    s.out <- rtModClass$new(
      mod = mod,
      mod.name = mod.name,
      type = type,
      gridsearch = gridsearch,
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
      extra = ifNotNull(extra, list)
    )
  } else if (rtclass == "rtModBag") {
    # R6: rtModBag ----
    s.out <- rtModBag$new(
      mod = mod,
      mod.name = mod.name,
      type = type,
      call = call,
      y.train = y.train,
      y.test = ifNotNull(y.test, numeric),
      x.name = ifNotNull(x.name, character),
      y.name = ifNotNull(y.name, character),
      xnames = ifNotNull(xnames, character),
      bag.resample.params = bag.resample.params,
      fitted.bag = fitted.bag,
      fitted = fitted,
      se.fit.bag = ifNotNull(se.fit.bag, numeric),
      se.fit = ifNotNull(se.fit, numeric),
      error.train = error.train,
      predicted.bag = ifNotNull(predicted.bag, numeric),
      predicted = ifNotNull(predicted, numeric),
      se.predicted.bag = ifNotNull(se.predicted.bag, numeric),
      se.prediction = ifNotNull(se.prediction, numeric),
      error.test = ifNotNull(error.test, list),
      question = ifNotNull(question, character),
      extra = ifNotNull(extra, list)
    )
  } else if (rtclass == "rtModLite") {
    # R6: rtModLite ----
    s.out <- rtModLite$new(
      mod = mod,
      mod.name = mod.name,
      fitted = fitted
    )
  }

  s.out
} # rtemis::rtModSet
