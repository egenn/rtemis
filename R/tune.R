# tune.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Get Tuner Function
#'
#' @param type Character: Type of tuner.
#'
#' @author EDG
#' @keywords internal
get_tuner_fn <- function(type = "GridSearch") {
  type <- match_arg(type, c("GridSearch"))
  switch(type,
    "GridSearch" = "tune_GridSearch"
  )
} # /rtemis::get_tuner_fn

#' Tune Supervised Learning Model
#'
#' @param x data.frame or similar: Training set data.
#' @param hyperparameters `Hyperparameters` object: make using each learner's `setup_*` function.
#' @param tuner_parameters `TunerParameters` object: created with [setup_GridSearch].
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @export

tune <- function(
    x,
    hyperparameters,
    tuner_parameters,
    weights = NULL,
    verbosity = 1L) {
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(tuner_parameters, TunerParameters)
  stopifnot(needs_tuning(hyperparameters))
  tuner_fn <- get_tuner_fn(tuner_parameters@type)
  do_call(tuner_fn, list(
    x = x,
    hyperparameters = hyperparameters,
    tuner_parameters = tuner_parameters,
    weights = weights,
    verbosity = verbosity
  ))
} # /rtemis::tune
