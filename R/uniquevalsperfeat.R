#' Unique values per feature
#'
#' Get number of unique values per features
#'
#' @param x matrix or data frame input
#' @return Vector, integer of length \code{NCOL(x)} with number of unique values per column/feature
#' @export
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' uniquevalsperfeat(iris)
#' }

uniquevalsperfeat <- function(x) {

  apply(x, 2, function(i) length(unique(i)))

}
