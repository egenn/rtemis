# S7_DecompositionParameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# DecompositionParameters ----
#' @title DecompositionParameters
#'
#' @description
#' Decomposition parameters class.
#'
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
DecompositionParameters <- new_class(
  name = "DecompositionParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /DecompositionParameters

# Make DecompositionParameters@parameters `$`-accessible
method(`$`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# Make DecompositionParameters@parameters `[[`-accessible
method(`[[`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete DecompositionParameters@parameters ----
method(`.DollarNames`, DecompositionParameters) <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Print DecompositionParameters ----
#' Print DecompositionParameters
#'
#' Print method for DecompositionParameters object.
#'
#' @param x DecompositionParameters object.
#' @param pad Integer: Left side padding.
#'
#' @author EDG
#' @export
method(print, DecompositionParameters) <- function(x, pad = 0L) {
  objcat(paste(x@algorithm, "DecompositionParameters"), pad = pad)
  cat("\n")
  printls(props(x)$parameters, pad = pad)
  invisible(x)
}

# ICAParameters ----
#' @title ICAParameters
#'
#' @description
#' DecompositionParameters subclass for Independent Component Analysis.
#'
#' @author EDG
#' @export
ICAParameters <- new_class(
  name = "ICAParameters",
  parent = DecompositionParameters,
  constructor = function(k, type, fun, alpha, row.norm, maxit, tol) {
    new_object(
      DecompositionParameters(
        algorithm = "ICA",
        parameters = list(
          k = k,
          type = type,
          fun = fun,
          alpha = alpha,
          row.norm = row.norm,
          maxit = maxit,
          tol = tol
        )
      )
    )
  }
) # /rtemis::ICAParameters

# setup_ICA ----
#' @title setup_ICA
#'
#' @description
#' Setup ICA parameters.
#'
#' @param k Integer: Number of components.
#' @param type Character: Type of ICA: "parallel" or "deflation".
#' @param fun Character: ICA function: "logcosh", "exp".
#' @param alpha Numeric [1, 2]: Used in approximation to neg-entropy with `fun = "logcosh"`.
#' @param row.norm Logical: If TRUE, normalize rows of `x` before ICA.
#' @param maxit Integer: Maximum number of iterations.
#' @param tol Numeric: Tolerance.
#'
#' @author EDG
#' @return ICAParameters object.
#' @export
setup_ICA <- function(
    k = 3L,
    type = c("parallel", "deflation"),
    fun = c("logcosh", "exp"),
    alpha = 1.0,
    row.norm = TRUE,
    maxit = 100L,
    tol = 1e-04) {
  k <- clean_posint(k)
  type <- match.arg(type)
  fun <- match.arg(fun)
  stopifnot(alpha >= 1, alpha <= 2)
  check_inherits(row.norm, "logical")
  maxit <- clean_posint(maxit)
  check_inherits(tol, "numeric")
  ICAParameters(
    k = k, type = type, fun = fun, alpha = alpha, row.norm = row.norm, maxit = maxit, tol = tol
  )
} # /rtemis::setup_ICA
