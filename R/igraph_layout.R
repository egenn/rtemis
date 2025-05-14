# igraph_layout
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

igraph_layout <- function(net, layout = "mds", dims = 2) {
  if (layout == "bipartite") {
    igraph::layout_as_bipartite(net)
  } else if (layout == "star") {
    out <- igraph::layout_as_star(net)
    if (dims == 3) out <- cbind(out, 0)
    out
  } else if (layout == "circle") {
    out <- igraph::layout_in_circle(net)
    if (dims == 3) out <- cbind(out, 0)
    out
  } else if (layout == "sphere") {
    igraph::layout_on_sphere(net)
  } else if (layout == "tree") {
    out <- igraph::layout_as_tree(net)
    if (dims == 3) out <- cbind(out, 0)
    out
  } else if (layout == "nicely") {
    igraph::layout_nicely(net, dim = dims)
  } else if (layout == "grid") {
    igraph::layout_on_grid(net, dim = dims)
  } else if (layout %in% c("drl", "fr", "kk", "mds")) {
    do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net), dim = dims)
    )
  } else if (layout == "sugiyama") {
    out <- igraph::layout_with_sugiyama(net)$layout
    if (dims == 3) out <- cbind(out, 0)
    out
  } else {
    out <- do.call(
      getFromNamespace(paste0("layout_with_", layout), "igraph"),
      c(list(net))
    )
    if (dims == 3) out <- cbind(out, 0)
    out
  }
} # rtemis::igraph_layout

# dims: drl, fr, kk, mds
