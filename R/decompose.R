# decompose.R
# ::rtemis::
# 2025 EDG rtemis.org

#' @name decompose
#' @aliases decompose
#' 
#' @title
#' Perform Decompo Data
#' 
#' @description
#' Perform linear or non-linear decomposition of numeric data.
#' 
#' @usage
#' ## S7 generic
#' decompose(x, ...)
#' ## S7 method for signature 'data.frame'
#' decompose(x, algorithm = "ICA", parameters = NULL, verbosity = 1L, ...)
#' 
#' @param x Matrix or data frame: Input data.
#' @param algorithm Character: Decomposition algorithm.
#' @param parameters DecompositionParameters: Algorithm-specific parameters.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used.
#' 
#' @return Decomposition object.
#'
#' @author EDG
#' @export
decompose <- new_generic("decompose", "x")
method(decompose, class_numeric | class_data.frame) <- function(x,
                                                                algorithm = "ICA",
                                                                parameters = NULL,
                                                                verbosity = 1L, ...) {
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
    msg20("Decomposing with ", algorithm, "...\n")
  }
  decom <- do.call(
    decom_fn,
    list(x = x, parameters = parameters)
  )

  # Outro ----
  outro(start_time, verbosity = verbosity)
  Decomposition(
    algorithm = algorithm,
    parameters = parameters,
    decom = decom$decom,
    transformed = decom$transformed
  )
} # /rtemis::decompose
