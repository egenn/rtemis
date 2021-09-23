# gplot3.map
# ::rtemis::
# 2020 E.D. Gennatas rtemis.lambdamd.org

#' Plot US state or county choropleth map
#'
#' Plot US state or county choropleth map using \strong{ggplot2} based on
#' \code{usmap::plot_usmap()}
#'
#' @param dat data.frame with 2 columns: fips, value
#' @param regions Vector, character: "states", "state", "counties", "county"
#' @param include Vector, character: Names of states or counties to include
#' @param exclude Vector, character: Names of states or counties to exclude
#' @param col.lo Color: Color mapped ot lowest negative values. Default = "#0290EE"
#' @param col.mid Color: Color mapped to value \code{colorscale.midpoint}, only used if
#' \code{colorscale.midpoint} is not NULL. If set to NA, \code{colorscale.midpoint} will be
#' set to NULL. Default = "black".
#' @param col.hi Color: Color mapped to highest positive values. Default = "#FE4AA3"
#' @param col.na Color: Color mapped to NA values. Default = "gray40"
#' @param colorscale.midpoint Float: Midpoint for colorscale. Default = 0
#' @param colorbar.height FLoat: Colorbar height, will be used as
#' \code{ggplot2::unit(colorbar.height, "npc")}
#' @param colorbar.width FLoat: Colorbar width, will be used as
#' \code{ggplot2::unit(colorbar.width, "npc")}
#' @param border.width Float: Map border width. Default = .1
#' @param main Character: Main title. Default = NULL
#' @param legend.title Character: Legend title. Default = NULL
#' @param theme \strong{ggplot2} theme to use. Default = \code{rt_gtheme_map()}
#' @param labels Logical: If TRUE, label states. Default = FALSE
#' @param col.labels Color for \code{labels}. Default = "gray50"
#' @author E.D. Gennatas
#' @export

gplot3.map <- function(dat,
                       regions = c("states", "state", "counties", "county"),
                       include = NULL,
                       exclude = NULL,
                       limits = NULL,
                       trans = "identity",
                       col.lo = "#0290EE",
                       col.mid = "black",
                       col.hi = "#FE4AA3",
                       col.na = "grey40",
                       colorscale.midpoint = 0,
                       colorbar.height = .15,
                       colorbar.width = .03,
                       border.width = .1,
                       main = NULL,
                       legend.title = NULL,
                       scale.accuracy = .1,
                       labelify = TRUE,
                       theme = rt_gtheme_map(),
                       labels = FALSE,
                       col.labels = "gray50",
                       filename = NULL,
                       file.width = 7,
                       file.height = 5, ...) {

  # [ Dependencies ] ====
  if (!depCheck("ggplot2", "usmap", "scales", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  regions_ <- match.arg(regions)
  colnames(dat)[1] <- "fips"
  if (is.null(legend.title)) legend.title <- colnames(dat)[2]
  if (is.na(col.mid)) colorscale.midpoint <- NULL

  # geom_args ====
  geom_args <- list(...)

  if (is.null(geom_args[["colour"]]) & is.null(geom_args[["color"]])) {
    geom_args[["color"]] <- "gray50"
  }

  if (is.null(geom_args[["size"]])) {
    geom_args[["size"]] <- border.width
  }

  if (is.null(geom_args[["fill"]]) & nrow(dat) == 0) {
    geom_args[["fill"]] <- "white"
  } else if (!is.null(geom_args[["fill"]]) & nrow(dat) != 0) {
    warning("`fill` setting is ignored when `dat` is provided. Use `fill` to color regions with solid color when no data is being displayed.")
  }

  # data ====
  values <- colnames(dat)[2]
  map_df <- usmap::map_with_data(dat,
                                 values = values,
                                 include = include,
                                 exclude = exclude)
  geom_args[["mapping"]] <- ggplot2::aes(x = x, y = y,
                                         group = group,
                                         fill = map_df[, values])

  polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)

  # '- Labels ====
  if (labels) {
    centroidLabelsColClasses <- c("numeric", "numeric",
                                  "character", "character", "character")

    if (regions_ == "county" | regions_ == "counties") {
      centroidLabelsColClasses <- c(centroidLabelsColClasses,
                                    "character")
    }

    centroid_labels <- read.csv(system.file("extdata",
                                                   paste0("us_", regions_, "_centroids.csv"), package = "usmap"),
                                       colClasses = centroidLabelsColClasses, stringsAsFactors = FALSE)

    if (length(include) > 0) {
      centroid_labels <- centroid_labels[centroid_labels$full %in%
                                           include | centroid_labels$abbr %in% include |
                                           centroid_labels$fips %in% include, ]
    }

    if (length(exclude) > 0) {
      centroid_labels <- centroid_labels[!(centroid_labels$full %in%
                                             exclude | centroid_labels$abbr %in% exclude |
                                             centroid_labels$fips %in% exclude | substr(centroid_labels$fips,
                                                                                        1, 2) %in% exclude), ]
    }

    if (regions_ == "county" | regions_ == "counties") {
      label_layer <- ggplot2::geom_text(data = centroid_labels,
                                        ggplot2::aes(x = x, y = y,
                                                     label = sub(" County", "", county)),
                                        color = col.labels)
    } else {
      label_layer <- ggplot2::geom_text(data = centroid_labels,
                                        ggplot2::aes(x = x, y = y, label = abbr),
                                        color = col.labels)
    }
  } else {
    label_layer <- ggplot2::geom_blank()
  }

  # '- colorscale ====
  colorscale <- if (is.null(colorscale.midpoint)) {
    ggplot2::scale_fill_gradient(limits = limits,
                                 oob = scales::squish,
                                 trans = trans,
                                 low = col.lo,
                                 high = col.hi,
                                 space = "Lab",
                                 na.value = col.na,
                                 guide = "colorbar",
                                 aesthetics = "fill",
                                 labels = scales::number_format(accuracy = scale.accuracy))
  } else {
    ggplot2::scale_fill_gradient2(limits = limits,
                                  oob = scales::squish,
                                  trans = trans,
                                  low = col.lo,
                                  mid = col.mid,
                                  high = col.hi,
                                  midpoint = colorscale.midpoint,
                                  space = "Lab",
                                  na.value = col.na,
                                  guide = "colorbar",
                                  aesthetics = "fill",
                                  labels = scales::number_format(accuracy = scale.accuracy))
  }


  # '- colorbar ====
  guide <- ggplot2::guides(fill = ggplot2::guide_colorbar(barheight = ggplot2::unit(colorbar.height, "npc"),
                                                          barwidth = ggplot2::unit(colorbar.width, "npc"),
                                                          ticks.colour = NA,
                                                          ticks.linewidth = 1))

  # '- ggplot ====
  if (labelify) legend.title <- labelify(legend.title)
  plt <- ggplot2::ggplot(data = map_df) +
    polygon_layer + label_layer + ggplot2::coord_equal() + theme +
    colorscale + guide + ggplot2::labs(fill = legend.title) +
    ggplot2::ggtitle(main)

  if (!is.null(filename)) {
    ggplot2::ggsave(filename = filename,
                    plot = plt,
                    width = file.width,
                    height = file.height)
  } else {
    plt
  }

} # rtemis::gplot3.map

#' **ggplot2** map theme
#'
#' **ggplot2** map theme for use with \link{gplot3.map}
#' based on \code{usmap:::theme_map()}
#'
#' @param base_size Float: Character \code{base_size.} Default = 9
#' @param base_family Character: Font family. Default = "Lato"
#' @author E.D. Gennatas
#' @export

rt_gtheme_map <- function(base_size = 9, base_family = "Helvetica",
                          legend.position = c(.9, 0),
                          plot.margin = ggplot2::unit(c(.02, .1, .02, .02), "npc")) {

  blank <- ggplot2::element_blank()
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(axis.line = blank, axis.text = blank,
                   axis.ticks = blank, axis.title = blank,
                   panel.background = blank, panel.border = blank,
                   panel.grid = blank, plot.background = blank,
                   panel.spacing = ggplot2::unit(0, "lines"),
                   legend.justification = c(0, 0),
                   legend.position = legend.position,
                   plot.margin = plot.margin)

} # rtemis::rt_gtheme_map
