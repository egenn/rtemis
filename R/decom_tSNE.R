# decom_tSNE.R
# ::rtemis::
# 2025 EDG rtemis.org

#' tSNE Decomposition
#' 
decom_tSNE <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, tSNEParameters)
  check_dependencies("package")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  args <- c(list(X = x, dims = parameters$k), parameters@parameters)
  args$k <- NULL
  decom <- do_call(
    Rtsne::Rtsne, args,
    error_pattern_suggestion = list("Remove duplicates" = "Remove duplicates using `preprocess()")
  )
  check_inherits(decom, "Rtsne")
  list(decom = decom, transformed = decom$Y)

} # /rtemis::decom_tSNE
