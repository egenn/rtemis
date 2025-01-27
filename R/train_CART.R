# train_CART.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train a CART decision tree
#'
#' Train a CART decision tree using `rpart::rpart`
#'
#' CART does not need any special preprocessing.
#' It works with numeric and factor variables and handles missing values.
#' The "train_*" functions train a single model.
#' Use [train] for tuning and testing using nested cross-validation.
#'
#' @param dat_training data.frame or similar: Training set.
#' @param dat_validation data.frame or similar: Validation set.
#' @param dat_testing data.frame or similar: Testing set.
#' @param preprocessor `Preprocessor` object: make using [setup_preprocessor].
#' @param hyperparameters `CARTHyperparameters` object: make using [setup_CART].
#' @param tuner `Tuner` object: make using [setup_tuner].
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG

train_CART <- function(
    dat_training,
    dat_validation = NULL,
    dat_testing = NULL,
    preprocessor = setup_preprocessor(),
    hyperparameters = setup_CART(),
    tuner = setup_tuner(),
    verbosity = 1L) {
  # Dependencies ----
  check_dependencies("rpart")

  # Arguments ----
  if (missing(dat_training)) {
    print(args(train_CART))
    stop("dat_training is missing")
  }
  # Hyperparameters must be untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    stop("Hyperparameters must be fixed - use train() instead.")
  }

  # Preprocess ----
  # dat_training <- preprocessor$preprocess(dat_training)
  # if (!is.null(dat_validation)) dat_validation <- preprocessor$apply_preprocessing(dat_validation)
  # if (!is.null(dat_testing)) dat_testing <- preprocessor$apply_preprocessing(dat_testing)

  # Data ----
  check_supervised_data(
    dat_training = dat_training,
    dat_validation = dat_validation,
    dat_testing = dat_testing,
    verbosity = verbosity
  )
  type <- supervised_type(dat_training)

  # Train ----
  # if (verbosity > 0) {
  #   msg20("Training ", hilite("CART", type), "...")
  # }
  mod <- rpart::rpart(
    make_formula(dat_training),
    data = dat_training,
    control = rpart::rpart.control(
      minsplit = hyperparameters$minsplit,
      minbucket = hyperparameters$minbucket,
      cp = hyperparameters$cp,
      maxcompete = hyperparameters$maxcompete,
      maxsurrogate = hyperparameters$maxsurrogate,
      usesurrogate = hyperparameters$usesurrogate,
      surrogatestyle = hyperparameters$surrogatestyle,
      maxdepth = hyperparameters$maxdepth,
      xval = hyperparameters$xval
    )
  )

  check_inherits(mod, "rpart")

  mod
} # rtemis::train_CART

#' Predict from rpart model
#' 
#' @param model rpart model.
#' @param newdata data.frame or similar: Data to predict on.
#' @param binclasspos Integer: Position of positive class in factor levels.
#' 
#' @keywords internal
predict_CART <- function(model, newdata, binclasspos = 1L) {
  if (model$method == "class")  {
    # Classification
    # predict.rpart returns a matrix n_cases x n_classes,
    # with classes are ordered the same as factor levels
    predict(model, newdata = newdata)[, binclasspos]
  } else {
    predict(model, newdata = newdata)
  }
} # /rtemis::predict_CART

#' Get variable importance from rpart model
#' 
#' @param model rpart model.
#' 
#' @keywords internal
varimp_CART <- function(model) {
  model[["variable.importance"]]
} # /rtemis::varimp_CART
