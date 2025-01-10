# train_CART.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Setup CART hyperparameters
#'
#' Setup hyperparameters for CART training
#'
#' @author EDG
#' @export
setup_CART <- function(
    # tunable
    cp = 0.01,
    maxdepth = 20,
    minsplit = 2,
    minbucket = 1, # round(minsplit / 3),
    prune.cp = NULL,
    # fixed
    method = "auto",
    model = TRUE,
    maxcompete = 4,
    maxsurrogate = 5,
    usesurrogate = 2,
    surrogatestyle = 0,
    xval = 0,
    cost = NULL) {
  CARTHyperparameters$new(
    cp = cp,
    maxdepth = maxdepth,
    minsplit = minsplit,
    minbucket = minbucket,
    prune.cp = prune.cp,
    method = method,
    model = model,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    xval = xval,
    cost = cost
  )
}

# Test that all CART hyperparameters are set by setup_CART
stopifnot(all(c(CART_tunable, CART_fixed) %in% names(formals(setup_CART))))

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
#' @param verbose Logical: If TRUE, print messages to console.
#' 
#' @author EDG

train_CART <- function(
    dat_training,
    dat_validation = NULL,
    dat_testing = NULL,
    preprocessor = setup_preprocessor(),
    hyperparameters = setup_CART(),
    tuner = setup_tuner(),
    verbose = TRUE) {
  # Dependencies ----
  dependency_check("rpart")

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
    verbose = verbose
  )

  # Train ----
  if (verbose) msg2("Training CART...")
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

  inherits_check(mod, "rpart")

  mod

} # rtemis::train_CART

predict_CART <- function(object, newdata) {
  predict(object, newdata = newdata)
}

