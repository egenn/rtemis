# decom_Isomap.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Isomap Decomposition
#'
#' @keywords internal
#' @noRd
decom_Isomap <- function(x, parameters, verbosity = 1L) {
  # Checks ----
  check_is_S7(parameters, IsomapParameters)
  check_dependencies("vegan")
  check_unsupervised_data(x = x, allow_missing = FALSE)

  # Decompose ----
  dst <- vegan::vegdist(x = x, method = parameters[["dist_method"]])
  decom <- vegan::isomap(
    dist = dst,
    ndim = parameters[["k"]],
    k = parameters[["nsd"]],
    path = parameters[["path"]]
  )
  check_inherits(decom, "isomap")
  list(decom = decom, transformed = decom[["points"]])
} # /rtemis::decom_Isomap
