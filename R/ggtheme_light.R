# ggtheme_light.R
# ::rtemis::
# 2019 E.D. Gennatas

#' \pkg{rtemis} \code{ggplot2} light theme
#'
#' @param base_size Float: Base font size. Default = 14
#' @param base_family Character: Font family. Default = "Helvetica Neue"
#' @param base_line_size Float: Line size. Default = base_size/22
#' @param base_rect_size Float: Size for rect elements. Default = base_size/22
#' @param axis.text.size.rel Float: Relative size for axis text. Default = 1
#' @param legend.key.fill Color: Fill color for legend. Default = NA (no color)
#' @param legend.text.size.rel Float: Relative size for legend text. Default = 1
#' @param legend.position Character: Legend position, "top", "bottom", "right", "left" Default  = "right"
#' @param strip.background.fill Color: Fill color from facet labels. Default = "grey85"
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' (p <- ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
#' geom_point() +
#' ggtheme_light())
#' }

ggtheme_light <- function(base_size = 14,
                               base_family = "Helvetica Neue",
                               base_line_size = base_size/22,
                               base_rect_size = base_size/22,
                               axis.text.size.rel = 1,
                               legend.key.fill = NA,
                               legend.text.size.rel = 1,
                               legend.position = "right",
                               strip.background.fill = "grey85") {

  half_line <- base_size/2

  ggplot2::theme(line = ggplot2::element_line(colour = "black",
                                     size = base_line_size,
                                     linetype = 1,
                                     lineend = "butt"),
                 rect = ggplot2::element_rect(fill = "white",
                                     colour = "black",
                                     size = base_rect_size,
                                     linetype = 1),
                 text = ggplot2::element_text(family = base_family,
                                     face = "plain",
                                     colour = "black",
                                     size = base_size,
                                     lineheight = 0.9,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     angle = 0,
                                     margin = ggplot2::margin(),
                                     debug = FALSE),
                 axis.line = ggplot2::element_blank(),
                 axis.line.x = NULL,
                 axis.line.y = NULL,
                 axis.text = ggplot2::element_text(size = ggplot2::rel(axis.text.size.rel), # delta
                                          colour = "grey30"),
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line/2),
                                            vjust = 1),
                 axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line/2),
                                                vjust = 0),
                 axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line/2),
                                            hjust = 1),
                 axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line/2),
                                                  hjust = 0),
                 axis.ticks = ggplot2::element_line(colour = "grey20"),
                 axis.ticks.length = ggplot2::unit(half_line/2, "pt"),
                 axis.ticks.length.x = NULL,
                 axis.ticks.length.x.top = NULL,
                 axis.ticks.length.x.bottom = NULL,
                 axis.ticks.length.y = NULL,
                 axis.ticks.length.y.left = NULL,
                 axis.ticks.length.y.right = NULL,
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line/2),
                                             vjust = 1),
                 axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line/2),
                                                 vjust = 0),
                 axis.title.y = ggplot2::element_text(angle = 90,
                                             margin = ggplot2::margin(r = half_line/2),
                                             vjust = 1),
                 axis.title.y.right = ggplot2::element_text(angle = -90,
                                                   margin = ggplot2::margin(l = half_line/2),
                                                   vjust = 0),
                 legend.background = ggplot2::element_rect(colour = NA),
                 legend.spacing = ggplot2::unit(2 * half_line, "pt"),
                 legend.spacing.x = NULL,
                 legend.spacing.y = NULL,
                 legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
                 legend.key = ggplot2::element_rect(fill = legend.key.fill, # delta
                                           colour = "white"),
                 legend.key.size = ggplot2::unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = ggplot2::element_text(size = ggplot2::rel(legend.text.size.rel)), # delta
                 legend.text.align = NULL,
                 legend.title = ggplot2::element_text(hjust = 0),
                 legend.title.align = NULL,
                 legend.position = legend.position,
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                 legend.box.background = ggplot2::element_blank(),
                 legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
                 panel.background = ggplot2::element_rect(fill = "grey92",
                                                 colour = NA),
                 panel.border = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_line(colour = "white"),
                 panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
                 panel.spacing = ggplot2::unit(half_line, "pt"),
                 panel.spacing.x = NULL,
                 panel.spacing.y = NULL,
                 panel.ontop = FALSE,
                 strip.background = ggplot2::element_rect(fill = strip.background.fill,
                                                 colour = NA),
                 strip.text = ggplot2::element_text(colour = "grey10",
                                           size = ggplot2::rel(0.8),
                                           margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
                 strip.text.x = NULL,
                 strip.text.y = ggplot2::element_text(angle = -90),
                 strip.placement = "inside",
                 strip.placement.x = NULL,
                 strip.placement.y = NULL,
                 strip.switch.pad.grid = ggplot2::unit(half_line/2, "pt"),
                 strip.switch.pad.wrap = ggplot2::unit(half_line/2, "pt"),
                 plot.background = ggplot2::element_rect(colour = "white"),
                 plot.title = ggplot2::element_text(size = ggplot2::rel(1.2),
                                           hjust = 0,
                                           vjust = 1,
                                           margin = ggplot2::margin(b = half_line)),
                 plot.subtitle = ggplot2::element_text(hjust = 0,
                                              vjust = 1,
                                              margin = ggplot2::margin(b = half_line)),
                 plot.caption = ggplot2::element_text(size = ggplot2::rel(0.8),
                                             hjust = 1,
                                             vjust = 1,
                                             margin = ggplot2::margin(t = half_line)),
                 plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2),
                                         hjust = 0.5,
                                         vjust = 0.5),
                 plot.tag.position = "topleft",
                 plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
                 complete = TRUE)

} # rtemis::theme_rtemis
