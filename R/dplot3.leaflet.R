# dplot3.leaflet.R
# ::rtemis::
# 2020 Efstathios D. Gennatas rtemis.lambdamd.org

#' Plot choropleth map using \strong{leaflet}
#'
#' @param dat data.frame where first column is county FIPS codes and second column is values to be
#' plotted. County names can be provided on a third column and will appear as hover-over information
#' along with values from column 2
#' @param fillOpacity Float: Opacity for fill colors. Default = 1
#' @param palette String: Color palette to use
#' @param color.mapping String: "Numeric" or "Bin"
#' @param col.lo Overlay color mapped to lowest value
#' @param col.hi Overaly color mapped to highest value
#' @param col.na Color mappes to NA values
#' @param col.highlight Hover border color. Default = "#FE8A4F" (orange)
#' @param col.interpolate String: "linear" or "spline"
#' @param col.bins Integer: Number of color bins to create if \code{color.mapping = "Bin"}.
#' Default = 21
#' @param domain Limits for mapping colors to values. Default = NULL and set to range
#' @param weight Float: Weight of county border lines. Default = .5
#' @param color Color of county border lines. Default = "black"
#' @param alpha Float: Overaly transparency. Default = 1
#' @param bg.tile.provider Background tile (below overlay colors), one of \code{leaflet::providers}
#' @param bg.tile.alpha Float: Background tile transparency. Default = .67
#' @param fg.tile.provider Foreground tile (above overlay colors), one of \code{leaflet::providers}
#' @param legend.position String: One of: "topright", "bottomright", "bottomleft", "topleft".
#' Default = "topright"
#' @param legend.alpha Float: Legend box transparency. Default = .8
#' @param legend.title String: Defaults to \code{colnames(dat)[2]}
#' @param init.lng Float: Center map around this longitude (in decimal form).
#' Default = -98.54180833333334 (US geographic center)
#' @param init.lat Float: Center map around this latitude (in decimal form).
#' Default = 39.207413888888894 (US geographic center)
#' @param init.zoom Integer: Initial zoom level (depends on device, i.e. window, size). Default = 3
#' @param stroke Logical: If TRUE, draw polygon borders. Default = TRUE
#' @author Efstathios D. Gennatas
#' @export

dplot3.leaflet <- function(dat,
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
                           # fg.tile.alpha = .67,
                           legend.position = c("topright", "bottomright", "bottomleft", "topleft"),
                           legend.alpha = .8,
                           legend.title = NULL,
                           init.lng = -98.54180833333334,
                           init.lat = 39.207413888888894,
                           init.zoom = 3,
                           stroke = TRUE) {

  # '- Arguments ====
  color.mapping <- match.arg(color.mapping)
  if (is.null(palette)) {
    palette <- colorRamp(colors = c(col.lo, col.hi), interpolate = col.interpolate)
  }
  col.interpolate <- match.arg(col.interpolate)
  legend.position <- match.arg(legend.position)
  if (is.null(legend.title)) legend.title <- labelify(colnames(dat)[2])

  # '- Data ====
  counties <- geojsonio::geojson_read("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json",
                                      what = "sp")
  fips <- if (is.character(dat[, 1])) dat[, 1] else sprintf("%05d", dat[, 1])
  # Match input county-level data
  index <- match(counties$id, fips)
  counties[["val"]] <- dat[index, 2]

  # '- Colorscale ====
  # if (is.null(domain)) domain <- counties$val
  if (color.mapping == "Numeric") {
    pal <- leaflet::colorNumeric(palette = palette,
                                 domain = domain,
                                 na.color = col.na,
                                 alpha = TRUE)
  } else {
    pal <- leaflet::colorBin(palette = palette,
                             domain = domain,
                             na.color = col.na,
                             bins = col.bins)
  }

  # '- Hover labels ====
  if (ncol(dat) > 2) {
    labelshtml <- sprintf("<strong>%s</strong><br/>%g", dat[, 3], dat[, 2]) %>%
      lapply(htmltools::HTML)
    .labs <- dat[index, 2]
    .names <- dat[index, 3]
    labels <- lapply(seq(NROW(counties)), function(i) {
      if (is.na(.labs[i])) '<div style="color:#7f7f7f;">N/A</div>'
      else sprintf("<strong>%s</strong><br/>%g", .names[i], .labs[i])
    }) %>% lapply(htmltools::HTML)
  } else {
    labels <- lapply(seq(NROW(counties)), function(i) {
      if (is.na(.labs[i])) '<div style="color:#7f7f7f;">N/A</div>'
      else sprintf("%g", .labs[i])
    }) %>% lapply(htmltools::HTML)
  }
  counties[["labels"]] <- labels[index]

  # '- leaflet map ====
  map <- leaflet::leaflet(counties) %>%
    leaflet::addProviderTiles(provider = bg.tile.provider,
                              options = leaflet::providerTileOptions(opacity = bg.tile.alpha)) %>%
    leaflet::addMapPane("polygons", zIndex = 410) %>%
    leaflet::addMapPane("tiles", zIndex = 420) %>%
    leaflet::addPolygons(fillColor = ~pal(val),
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
                           bringToFront = TRUE),
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "2px 2px"),
                           textsize = "15px",
                           direction = "auto")
                         ) %>%
    leaflet::addProviderTiles(provider = fg.tile.provider,
                              options = leaflet::pathOptions(pane = "tiles")) %>%
    leaflet::addLegend(position = legend.position,
                       pal = pal,
                       values = counties$val,
                       opacity = legend.alpha,
                       title = legend.title) %>%
    leaflet::addLayersControl(overlayGroups = c(legend.title)) %>%
    leaflet::setView(lng = init.lng, lat = init.lat, zoom = init.zoom)
  map

}


# "geographic center" of the United States:
# latitude 39°12'26.686", longitude 98°32'30.506"
# lat: 39.207413888888894, long: -98.54180833333334
