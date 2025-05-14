# decompose.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Perform Data Decomposition
#'
#' Perform linear or non-linear decomposition of numeric data.
#'
#' @param x Matrix or data frame: Input data.
#' @param algorithm Character: Decomposition algorithm.
#' @param parameters DecompositionParameters: Algorithm-specific parameters.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Decomposition object.
#'
#' @author EDG
#' @export
decompose <- function(x, algorithm = "ICA", parameters = NULL, verbosity = 1L) {
  # Checks ----
  if (is.null(parameters)) {
    parameters <- get_default_decomparams(algorithm)
  }
  check_is_S7(parameters, DecompositionParameters)

  # Intro ----
  start_time <- intro(verbosity = verbosity)

  # Data ----
  if (verbosity > 0L) {
    cat("\n")
    summarize_unsupervised_data(x)
    cat("\n")
  }

  # Decompose ----
  algorithm <- get_decom_name(algorithm)
  decom_fn <- get_decom_fn(algorithm)
  if (verbosity > 0L) {
    msg20("Decomposing with ", algorithm, "...")
  }
  decom <- do_call(
    fn = decom_fn,
    args = list(x = x, parameters = parameters)
  )

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Decomposition(
    algorithm = algorithm,
    parameters = parameters,
    decom = decom[["decom"]],
    transformed = decom[["transformed"]]
  )
} # /rtemis::decompose
