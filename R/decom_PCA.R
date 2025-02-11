# decom_PCA.R
# ::rtemis::
# 2025 EDG rtemis.org

#' PCA Decomposition
#'
#' @keywords internal
#' @noRd
decom_PCA <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, PCAParameters)
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  decom <- prcomp(
    x = x,
    center = parameters$center,
    scale. = parameters$scale,
    tol = parameters$tol,
    rank. = parameters$k
  )
  check_inherits(decom, "prcomp")
  list(decom = decom, transformed = decom$x)
} # /rtemis::decom_ICA
