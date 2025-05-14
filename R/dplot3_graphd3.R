# dplot3_graphd3
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Plot graph using \pkg{networkD3}
#'
#' @param net \pkg{igraph} network
#' @param groups Vector, length n nodes indicating group/cluster/community membership of nodes in
#' `net`
#' @param color.scale D3 colorscale (e.g. `networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20b);")`)
#' @param edge.col Color for edges
#' @param node.col Color for nodes
#' @param node.alpha Float \[0, 1\]: Node opacity. Default = .5
#' @param edge.alpha Float \[0, 1\]: Edge opacity. Default = .33
#' @param zoom Logical: If TRUE, graph is zoomable. Default = TRUE
#' @param legend Logical: If TRUE, display legend for groups
#' @param palette Vector of colors, or Character defining a builtin palette - get options with
#' `rtpalette()`
#' @param theme rtemis theme to use
#' @param ... Additional arguments to pass to `networkD3`
#'
#' @author E.D. Gennatas
#' @export
dplot3_graphd3 <- function(
  net,
  groups = NULL,
  color.scale = NULL,
  edge.col = NULL,
  node.col = NULL,
  node.alpha = .5,
  edge.alpha = .33,
  zoom = TRUE,
  legend = FALSE,
  palette = rtPalette,
  theme = rtTheme,
  ...
) {
  # Dependencies ----
  dependency_check("networkD3")

  # Theme ----
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), list())
  }

  netd3 <- networkD3::igraph_to_networkD3(net)
  if (is.null(groups)) {
    netd3$nodes$group <- "A"
  } else {
    netd3$nodes$group <- groups
  }

  # Colors ----
  if (is.null(node.col) && length(unique(netd3$nodes$group)) == 1) {
    node.col <- theme$fg
  }

  if (is.null(color.scale)) {
    if (length(unique(netd3$nodes$group)) == 1) {
      color.scale <- paste0(
        'd3.scaleOrdinal().domain(["A"]).range(["',
        adjustcolor(node.col, node.alpha),
        '"]);'
      )
    } else {
      if (is.character(palette))
        palette <- adjustcolor(unlist(rtpalette(palette)), node.alpha)
      ngroups <- length(unique(groups))
      .groups <- paste0(sort(unique(groups)), collapse = '", "')
      if (ngroups > length(palette)) {
        palette <- rep(palette, ngroups / length(palette))
      }
      .colors <- paste0(palette[seq(ngroups)], collapse = '", "')
      color.scale <- paste0(
        'd3.scaleOrdinal().domain(["',
        .groups,
        '"]).range(["',
        .colors,
        '"]);'
      )
    }
  }

  if (is.null(edge.col)) {
    if (is.null(groups)) {
      edge.col <- adjustcolor("#18A3AC", edge.alpha)
    } else {
      edge.col <- adjustcolor(theme$fg, edge.alpha)
    }
  } else {
    edge.col <- adjustcolor(edge.col, edge.alpha)
  }

  # Plot ----
  fn <- networkD3::forceNetwork(
    Links = netd3$links,
    Nodes = netd3$nodes,
    Source = "source",
    Target = "target",
    NodeID = "name",
    Group = "group",
    colourScale = color.scale,
    linkColour = edge.col,
    opacity = 1,
    legend = legend,
    zoom = zoom
  )

  # fn$x$nodes$border <- border.groups
  fn <- htmlwidgets::onRender(
    fn,
    'function(el, x) { d3.selectAll("circle").style("stroke", d => "#ffffff00"); }'
  )

  fn
} # rtemis::dplot3_graphd3
