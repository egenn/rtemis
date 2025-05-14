# dplot3_leaflet.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Plot interactive choropleth map using \pkg{leaflet}
#'
#' @param fips Character vector of FIPS codes. (If numeric, it will be
#' appropriately zero-padded)
#' @param values Values to map to `fips`
#' @param names Character vector: Optional county names to appear on hover
#' along `values`
#' @param fillOpacity Float: Opacity for fill colors. Default = 1
#' @param palette Character: Color palette to use
#' @param color.mapping Character: "Numeric" or "Bin"
#' @param col.lo Overlay color mapped to lowest value
#' @param col.hi Overaly color mapped to highest value
#' @param col.na Color mappes to NA values
#' @param col.highlight Hover border color. Default = "#FE8A4F" (orange)
#' @param col.interpolate Character: "linear" or "spline"
#' @param col.bins Integer: Number of color bins to create if
#' `color.mapping = "Bin"`.
#' Default = 21
#' @param domain Limits for mapping colors to values.
#' Default = NULL and set to range
#' @param weight Float: Weight of county border lines. Default = .5
#' @param color Color of county border lines. Default = "black"
#' @param alpha Float: Overaly transparency. Default = 1
#' @param bg.tile.provider Background tile (below overlay colors), one of
#' `leaflet::providers`
#' @param bg.tile.alpha Float: Background tile transparency. Default = .67
#' @param fg.tile.provider Foreground tile (above overlay colors), one of
#' `leaflet::providers`
#' @param legend.position Character: One of: "topright", "bottomright",
#' "bottomleft", "topleft".
#' Default = "topright"
#' @param legend.alpha Float: Legend box transparency. Default = .8
#' @param legend.title Character: Defaults to name of `values` variable.
#' @param init.lng Float: Center map around this longitude (in decimal form).
#' Default = -98.54180833333334 (US geographic center)
#' @param init.lat Float: Center map around this latitude (in decimal form).
#' Default = 39.207413888888894 (US geographic center)
#' @param init.zoom Integer: Initial zoom level (depends on device, i.e. window,
#' size). Default = 3
#' @param stroke Logical: If TRUE, draw polygon borders. Default = TRUE
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' fips <- c(06075, 42101)
#' population <- c(874961, 1579000)
#' names <- c("SF", "Philly")
#' dplot3_leaflet(fips, supervals, names)
#' }
# NA in legend issue: https://github.com/rstudio/leaflet/issues/615
#'
dplot3_leaflet <- function(
  fips,
  values,
  names = NULL,
  fillOpacity = 1,
  palette = NULL,
  color.mapping = c("Numeric", "Bin"),
  col.lo = "#0290EE",
  col.hi = "#FE4AA3",
  col.na = "#303030",
  col.highlight = "#FE8A4F",
  col.interpolate = c("linear", "spline"),
  col.bins = 21, # for color.mapping Bin
  domain = NULL,
  weight = .5,
  color = "black",
  alpha = 1,
  bg.tile.provider = leaflet::providers$Stamen.TonerBackground,
  bg.tile.alpha = .67,
  fg.tile.provider = leaflet::providers$Stamen.TonerLabels,
  legend.position = c(
    "topright",
    "bottomright",
    "bottomleft",
    "topleft"
  ),
  legend.alpha = .8,
  legend.title = NULL,
  init.lng = -98.54180833333334,
  init.lat = 39.207413888888894,
  init.zoom = 3,
  stroke = TRUE
) {
  # Dependencies ----
  dependency_check("leaflet", "geojsonio", "htmltools", "htmlwidgets", "sf")

  # Arguments ----
  vals.name <- deparse(substitute(values))
  color.mapping <- match.arg(color.mapping)
  col.interpolate <- match.arg(col.interpolate)
  if (is.null(palette)) {
    palette <- colorRamp(
      colors = c(col.lo, col.hi),
      interpolate = col.interpolate
    )
  }
  legend.position <- match.arg(legend.position)
  if (is.null(legend.title)) legend.title <- labelify(vals.name)

  # State vs. County data ----
  if (max(nchar(fips)) < 3) {
    geo <- geojsonio::geojson_read(
      system.file(
        "extdata",
        "us-states.json",
        package = "rtemis"
      ),
      what = "sp"
    )
    fips <- if (is.character(fips)) fips else sprintf("%02d", fips)
  } else {
    geo <- geojsonio::geojson_read(
      system.file(
        "extdata",
        "us-counties.json",
        package = "rtemis"
      ),
      what = "sp"
    )
    fips <- if (is.character(fips)) fips else sprintf("%05d", fips)
  }

  # Match input county-level data
  index <- match(geo$id, fips)
  geo[["val"]] <- values[index]

  # Colorscale ----
  if (color.mapping == "Numeric") {
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = domain,
      na.color = col.na,
      alpha = TRUE
    )
  } else {
    pal <- leaflet::colorBin(
      palette = palette,
      domain = domain,
      na.color = col.na,
      bins = col.bins
    )
  }

  # Hover labels ----
  .labs <- values[index]
  if (!is.null(names)) {
    .names <- names[index]
    labels <- lapply(seq_len(NROW(geo)), function(i) {
      if (is.na(.labs[i])) {
        '<div style="color:#7f7f7f;">N/A</div>'
      } else {
        sprintf("<strong>%s</strong><br/>%g", .names[i], .labs[i])
      }
    }) |>
      lapply(htmltools::HTML)
  } else {
    labels <- lapply(seq_len(NROW(geo)), function(i) {
      if (is.na(.labs[i])) {
        '<div style="color:#7f7f7f;">N/A</div>'
      } else {
        sprintf("%g", .labs[i])
      }
    }) |>
      lapply(htmltools::HTML)
  }
  geo[["labels"]] <- labels[index]

  # leaflet map ----
  map <- leaflet::leaflet(geo) |>
    leaflet::addProviderTiles(
      provider = bg.tile.provider,
      options = leaflet::providerTileOptions(opacity = bg.tile.alpha)
    ) |>
    leaflet::addMapPane("polygons", zIndex = 410) |>
    leaflet::addMapPane("tiles", zIndex = 420) |>
    leaflet::addPolygons(
      fillColor = ~ pal(val),
      fillOpacity = fillOpacity,
      opacity = alpha,
      weight = weight,
      color = color,
      stroke = stroke,
      group = legend.title,
      options = leaflet::pathOptions(pane = "polygons"),
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = col.highlight,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "2px 2px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    leaflet::addProviderTiles(
      provider = fg.tile.provider,
      options = leaflet::pathOptions(pane = "tiles")
    ) |>
    leaflet::addLegend(
      position = legend.position,
      pal = pal,
      values = geo$val,
      opacity = legend.alpha,
      title = legend.title
    ) |>
    leaflet::addLayersControl(overlayGroups = c(legend.title)) |>
    leaflet::setView(lng = init.lng, lat = init.lat, zoom = init.zoom)

  insert <- htmltools::tags$style(
    type = "text/css",
    "div.info.legend.leaflet-control br {clear: both;}"
  )
  map <- htmlwidgets::prependContent(map, insert)
  map
} # rtemis:: dplot3_leaflet
