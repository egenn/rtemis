# zipops.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Get Longitude and Lattitude for zip code(s)
#'
#' Returns a data.table of longitude and lattitude for one or more zip codes,
#' given an input dataset
#'
#' @param x Character vector: Zip code(s)
#' @param zipdt data.table with "zip", "lng", and "lat" columns

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
#' @returns `data.table` with distances in meters
#'
#' @author E.D. Gennatas
#' @export

zipdist <- function(x, y, zipdt) {
  # Dependencies ----
  dependency_check("geosphere")

  # distHaversine ----
  geosphere::distHaversine(
    zip2longlat(x, zipdt)[, -1],
    zip2longlat(y, zipdt)[, -1]
  )
} # rtemis::zipdist
