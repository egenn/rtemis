# utils_zip.R
# ::rtemis::
# EDG rtemis.org

#' Get Longitude and Lattitude for zip code(s)
#'
#' Returns a data.table of longitude and lattitude for one or more zip codes,
#' given an input dataset
#'
#' @param x Character vector: Zip code(s)
#' @param zipdt data.table with "zip", "lng", and "lat" columns
#'
#' @return `data.table`.
#'
#' @author EDG
#' @export
zip2longlat <- function(x, zipdt) {
  data.table::setorder(
    merge(
      data.table(ID = seq_along(x), zip = x, key = "ID"),
      zipdt[zip %in% x],
      by = "zip",
      all.x = TRUE
    ),
    "ID"
  )[, -"ID"]
}

#' Get distance between pairs of zip codes
#'
#' @param x Character vector
#' @param y Character vector, same length as `x`
#' @param zipdt data.table with columns `zip`, `lng`, `lat`
#'
#' @return `data.table` with distances in meters
#'
#' @author EDG
#' @export

zipdist <- function(x, y, zipdt) {
  # Dependencies ----
  check_dependencies("geosphere")

  # distHaversine ----
  geosphere::distHaversine(
    zip2longlat(x, zipdt)[, -1],
    zip2longlat(y, zipdt)[, -1]
  )
} # rtemis::zipdist
