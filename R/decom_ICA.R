# decom_ICA.R
# ::rtemis::
# 2025 EDG rtemis.org

#' ICA Decomposition
#' 
decom_ICA <- function(x, parameters, verbosity = 1L) {
  # Checks
  check_is_S7(parameters, ICAParameters)

  # Dependencies
  check_dependencies("fastICA")

  # Data
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose
  decom <- fastICA::fastICA(
    X = as.matrix(x),
    n.comp = parameters$k,
    alg.typ = parameters$type,
    fun = parameters$fun,
    alpha = parameters$alpha,
    method = "C",
    row.norm = parameters$row_norm,
    maxit = parameters$maxit,
    tol = parameters$tol,
    verbose = verbosity > 0L
  )

} # /rtemis::decom_ICA
