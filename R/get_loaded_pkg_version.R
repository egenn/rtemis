# get_loaded_pkg_version.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Get version of all loaded packages (namespaces)
#'
#' @author E.D. Gennatas
#' @return Data frame with columns "Package_Name" and "Version"
#' @export

get_loaded_pkg_version <- function() {
  loaded_ <- loadedNamespaces()

  data.frame(
    Package_Name = loaded_,
    Version = sapply(loaded_, function(i) as.character(packageVersion(i))),
    row.names = seq(loaded_)
  )
} # rtemis::get_loaded_pkg_version
