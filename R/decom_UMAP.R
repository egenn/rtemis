# decom_UMAP.R
# ::rtemis::
# 2025 EDG rtemis.org

#' UMAP Decomposition
#'
#' @keywords internal
#' @noRd
decom_UMAP <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, UMAPParameters)
  check_dependencies("uwot")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  args <- c(list(X = x, n_components = parameters$k, ret_model = TRUE), parameters@parameters)
  args$k <- NULL
  decom <- do_call(
    uwot::umap, args,
    error_pattern_suggestion = list("as_cholmod_sparse" = "Try installing packages 'Matrix' and 'irlba' from source.")
  )
  # ret_model = TRUE returns list
  check_inherits(decom, "list")
  list(decom = decom, transformed = decom$embedding)
} # /rtemis::decom_UMAP
