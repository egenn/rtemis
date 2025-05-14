# dplot3_addtree
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Plot AddTree trees
#'
#' Plot AddTree trees trained with [s_AddTree] using `data.tree::plot.Node`
#'
#' Edge info and styles have been removed because of problems with `DiagrammeR`
#'
#' @inheritParams prune.addtree
#' @param addtree Additive Tree object created by [s_AddTree]
#' @param col.positive Color for outcome positive.
#' @param col.negative Color for negative outcome.
#' @param node.col Color for non-terminal leaves.
#' @param node.shape Character: Node shape, passed to `data.tree::SetNodeStyle`
#' @param node.labels Logical: If `TRUE`, show node labels.
#' @param node.labels.pct.pos Logical: If `TRUE`, show % positive cases in node labels.
#' @param pos.name Character: Name for "positive" outcome.
# @param edge.width Numeric: Edge width.
#' @param layout Character: Passed to `data.tree::SetGraphStyle`
#' @param rankdir Character: Passed to `data.tree::SetGraphStyle`
#' @param splines Character: Passed to `data.tree::SetGraphStyle`
#' @param fontname Character: Passed to `data.tree::SetGraphStyle`
#' @param overlap Character: Passed to `data.tree::SetGraphStyle`
#' @param prune Logical: If `TRUE`, prune AddTree.
#' @param prune.empty.leaves Logical: If `TRUE`, prune empty leaves.
#' @param edge.col Color for edges.
#' @param bg.color Background color.
# @param filename Character: Path to filename to save PDF
# Requires packages `DiagrammeRsvg` and `rsvg`. Default = NULL
#'
#' @author E.D. Gennatas
#' @export

dplot3_addtree <- function(
  addtree,
  col.positive = "#F48024DD",
  col.negative = "#18A3ACDD",
  node.col = "#666666",
  node.shape = "none",
  node.labels = TRUE,
  node.labels.pct.pos = NULL,
  pos.name = NULL,
  edge.col = "#999999",
  #  edge.width = 2,
  #  edge.labels = FALSE,
  #  arrowhead = "vee",
  layout = "dot",
  #  drop.leaves = FALSE,
  rankdir = "TB",
  splines = "polyline",
  fontname = "helvetica",
  bg.color = "#ffffff",
  overlap = "false",
  prune = NULL,
  prune.empty.leaves = TRUE,
  remove.bad.parents = FALSE
  #  filename = NULL
) {
  # Dependencies ----
  dependency_check("data.tree", "DiagrammeR")

  # Tree ----
  if (inherits(addtree, "Node")) {
    addtree <- data.tree::Clone(addtree)
    if (is.null(prune)) prune <- TRUE
  } else {
    if (!inherits(addtree$mod, "addtree")) stop("Input is not addtree object")
    if (is.null(addtree$mod$addtree.pruned)) {
      addtree <- data.tree::Clone(addtree$mod$addtree)
      if (is.null(prune)) prune <- TRUE
    } else {
      addtree <- data.tree::Clone(addtree$mod$addtree.pruned)
      if (is.null(prune)) prune <- FALSE
    }
  }

  # Arguments ----
  if (is.null(node.labels.pct.pos)) {
    node.labels.pct.pos <- if (is.null(addtree$pct.pos)) FALSE else TRUE
  }

  # Prune ----
  if (prune) {
    addtree <- prune.addtree(
      addtree,
      prune.empty.leaves = prune.empty.leaves,
      remove.bad.parents = remove.bad.parents
    )
  }

  # Graph Style ----
  data.tree::SetGraphStyle(
    addtree,
    layout = layout,
    rankdir = rankdir,
    splines = splines,
    bgcolor = bg.color,
    overlap = overlap,
    tooltip = paste(
      "AddTree tree\n---------------",
      "\nDepth =",
      addtree$height - 1,
      "\nN nodes =",
      addtree$totalCount - 1,
      "\nN leaves =",
      length(addtree$leaves)
    )
  )
  # Node Style ----
  .node.labels <- if (node.labels) {
    if (node.labels.pct.pos) {
      # Include % positive cases
      function(node) {
        paste(
          # gsub('[{}]', "", node$Condition),
          formatcondition(node$Condition),
          paste("N =", node$N),
          paste(
            paste0(ddSci(node$pct.pos, asNumeric = TRUE) * 100, "%"),
            pos.name
          ),
          sep = "\n"
        )
      }
    } else {
      function(node) {
        paste(formatcondition(node$Condition), paste("N =", node$N), sep = "\n")
      }
    }
  } else {
    ""
  }

  data.tree::SetNodeStyle(
    addtree,
    style = "filled",
    shape = node.shape,
    fillcolor = node.col,
    col = node.col,
    fontname = fontname,
    label = .node.labels,
    tooltip = function(node) {
      paste(
        paste("Node", node$n),
        paste("Value =", ddSci(node$Value)),
        sep = "\n"
      )
    },
    rank = function(node) node$Depth
  )

  # Edge Style ----
  # .edge.labels <- if (edge.labels) function(node) node$Condition else NULL
  # sometimes fails
  # data.tree::SetEdgeStyle(addtree,
  #                         keepExisting = TRUE,
  #                         arrowhead = arrowhead,
  #                         color = edge.col,
  #                         penwidth = edge.width,
  #                         fontname = fontname,
  #                         label = .edge.labels,
  #                         tooltip = function(node) node$Condition)
  # ?drop.leaves, keepExisting = TRUE

  # leaves.rank <- if (drop.leaves) addtree$height else NULL
  data.tree::Do(addtree$leaves, function(node) {
    data.tree::SetNodeStyle(
      node,
      # rank = leaves.rank,
      style = "filled",
      shape = node.shape,
      fillcolor = function(node) {
        ifelse(node$EstimateInt == 1 & node$isLeaf, col.positive, col.negative)
      }
    )
  })

  # Write to file ----
  # if (!is.null(filename)) {
  #   dependency_check("DiagrammeRsvg", "rsvg")
  #   filename <- file.path(filename)
  #   plt_svg <- DiagrammeRsvg::export_svg(plt)
  #   rsvg::rsvg_pdf(charToRaw(plt_svg), file = filename)
  # }

  plot(addtree)
} # rtemis::dplot3_addtree


formatcondition <- function(x, remove.chars = "[{}]", decimal.places = 2) {
  categorical <- any(grepl("{", x, fixed = TRUE))
  if (categorical) {
    xf <- gsub(remove.chars, "", x)
  } else {
    xf <- strsplit(x, " ")[[1]]
    xf[3] <- ddSci(xf[3], decimal.places = decimal.places)
    xf <- paste(xf, collapse = " ")
  }
  xf
}
