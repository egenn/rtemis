# loadedPackageVersions.R
# ::rtemis::
# E.D. Gennatas www.lambdamd.org

#' Get version of all loaded packages (namespaces)
#'
#' @author E.D. Gennatas
#' @return Data frame with columns "Package_Name" and "Version"
#' @export

loadedPackageVersions <- function() {

  loaded_ <- loadedNamespaces()

  data.frame(Package_Name = loaded_,
             Version = sapply(loaded_, function(i) as.character(packageVersion(i))),
             row.names = seq(loaded_))

} # rtemis::loadedPackageVersions
