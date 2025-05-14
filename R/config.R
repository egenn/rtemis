# config.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Create rtemis configuration file
#'
#' Defines a complete predictive modeling pipeline and saves it as a JSON file.
#'
#' @param data_path Character: Path to data file. Can be any file recognized by [read], commonly CSV,
#' Excel, or RDS.
#' @param target Character: Name of the target variable in the data. If not specified, the last
#' column of `data` will be used.
#' @param binclass_posid Integer: Position of the positive class in a binary classification problem.
#' @param alg Character: Algorithm to use. Any of `select_learn()`.
#' @param train.params List: Parameters for the training algorithm.
#' @param inner.resampling List: Resampling method for the inner loop, i.e. hyperparameter tuning,
#' a.k.a. model selection. Set using [setup.resample]
#' @param outer.resampling List: Resampling method for the outer loop, i.e. testing. Set using
#' [setup.resample]
#' @param config.path Character: Path to save configuration file.
#' @param model.outdir Character: Directory to save trained model and associated files. If NULL,
#' the directory of `config.path` will be used.
#' @param allow.overwrite Logical: If TRUE, overwrite existing config file.
#' @param verbose Logical: If TRUE, print messages
#'
#' @author EDG
#' @return config as list, invisibly.

create_config <- function(
  data_path,
  target = NULL,
  binclass_posid = 1,
  alg = "lightgbm",
  train.params = NULL,
  inner.resampling = setup.resample(resampler = "cv", n.resamples = 5),
  outer.resampling = setup.resample(resampler = "cv", n.resamples = 10),
  config.path = "rtemis-config.json",
  model.outdir = NULL,
  allow.overwrite = FALSE,
  verbose = TRUE
) {
  # Get dataset name
  data_path <- path.expand(data_path)
  # data_name <- tools::file_path_sans_ext(basename(data_path))
  # Get algorithm name
  alg <- select_learn(alg, name = TRUE)
  # Create output directory
  config.path <- path.expand(config.path)
  config.outdir <- dirname(config.path)
  if (file.exists(config.path) && !allow.overwrite) {
    stop(
      "Config file ",
      config.path,
      " already exists. Set allow.overwrite = TRUE to overwrite."
    )
  }
  if (!dir.exists(config.outdir)) {
    dir.create(config.outdir, recursive = TRUE)
  }
  if (is.null(model.outdir)) {
    model.outdir <- config.outdir
  }
  config <- list(
    data_path = data_path,
    target = target,
    alg = alg,
    train.params = train.params,
    inner.resampling = inner.resampling,
    outer.resampling = outer.resampling,
    outdir = model.outdir
  )
  jsonlite::write_json(
    config,
    config.path,
    simplifyVector = TRUE,
    null = "null",
    digits = NA
  )

  if (file.exists(config.path)) {
    if (verbose) {
      msg20("Configuration file saved to ", hilite(config.path), ".")
    }
  } else {
    stop("Configuration file not saved.")
  }
  invisible(config)
} # /rtemis::create_config


#' Read rtemis configuration file
#'
#' Reads rtemis configuration file.
#'
#' @param file Character: Path to configuration file created by [create_config].
#'
#' @return List.
#'
#' @author EDG
#' @export

read_config <- function(config.path, verbose = TRUE) {
  config.path <- path.expand(config.path)
  if (verbose) {
    msg2("Reading config file ", hilite(config.path), "...\n", sep = "")
  }
  if (!file.exists(config.path)) {
    stop("File not found: ", config.path, )
  }
  jsonlite::read_json(config.path, simplifyVector = TRUE)
} # /rtemis::read_config
