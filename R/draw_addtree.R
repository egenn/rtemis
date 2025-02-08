# draw_addtree
# ::rtemis::
# EDG rtemis.org

#' Plot AddTree trees
#'
#' Plot AddTree trees trained with [s_AddTree] using `data.tree::plot.Node`
#'
#' Edge info and styles have been removed because of problems with `DiagrammeR`
#'
#' @inheritParams prune.addtree
#' @param addtree Additive Tree object created by [s_AddTree]
#' @param col_positive Color for outcome positive.
#' @param col_negative Color for negative outcome.
#' @param node_col Color for non-terminal leaves.
#' @param node_shape Character: Node shape, passed to `data.tree::SetNodeStyle`
#' @param node_labels Logical: If `TRUE`, show node labels.
#' @param node_labels_pct_pos Logical: If `TRUE`, show % positive cases in node labels.
#' @param pos_name Character: Name for "positive" outcome.
# @param edge_width Numeric: Edge width.
#' @param layout Character: Passed to `data.tree::SetGraphStyle`
#' @param rankdir Character: Passed to `data.tree::SetGraphStyle`
#' @param splines Character: Passed to `data.tree::SetGraphStyle`
#' @param fontname Character: Passed to `data.tree::SetGraphStyle`
#' @param overlap Character: Passed to `data.tree::SetGraphStyle`
#' @param prune Logical: If `TRUE`, prune AddTree.
#' @param prune_empty_leaves Logical: If `TRUE`, prune empty leaves.
#' @param edge_col Color for edges.
#' @param bg_color Background color.
# @param filename Character: Path to filename to save PDF
# Requires packages `DiagrammeRsvg` and `rsvg`.
#'
#' @author EDG
#' @export

draw_addtree <- function(addtree,
                         col_positive = "#F48024DD",
                         col_negative = "#18A3ACDD",
                         node_col = "#666666",
                         node_shape = "none",
                         node_labels = TRUE,
                         node_labels_pct_pos = NULL,
                         pos_name = NULL,
                         edge_col = "#999999",
                         #  edge_width = 2,
                         #  edge_labels = FALSE,
                         #  arrowhead = "vee",
                         layout = "dot",
                         #  drop_leaves = FALSE,
                         rankdir = "TB",
                         splines = "polyline",
                         fontname = "helvetica",
                         bg_color = "#ffffff",
                         overlap = "false",
                         prune = NULL,
                         prune_empty_leaves = TRUE,
                         remove_bad_parents = FALSE
                         #  filename = NULL
) {
  # Dependencies ----
  check_dependencies("data.tree", "DiagrammeR")

  # Tree ----
  if (inherits(addtree, "Node")) {
    addtree <- data.tree::Clone(addtree)
    if (is.null(prune)) prune <- TRUE
  } else {
    if (!inherits(addtree$mod, "addtree")) stop("Input is not addtree object")
    if (is.null(addtree$mod$addtree_pruned)) {
      addtree <- data.tree::Clone(addtree$mod$addtree)
      if (is.null(prune)) prune <- TRUE
    } else {
      addtree <- data.tree::Clone(addtree$mod$addtree_pruned)
      if (is.null(prune)) prune <- FALSE
    }
  }

  # Arguments ----
  if (is.null(node_labels_pct_pos)) {
    node_labels_pct_pos <- if (is.null(addtree$pct_pos)) FALSE else TRUE
  }

  # Prune ----
  if (prune) {
    addtree <- prune.addtree(addtree,
      prune_empty_leaves = prune_empty_leaves,
      remove_bad_parents = remove_bad_parents
    )
  }

  # Graph Style ----
  data.tree::SetGraphStyle(addtree,
    layout = layout,
    rankdir = rankdir,
    splines = splines,
    bgcolor = bg_color,
    overlap = overlap,
    tooltip = paste(
      "AddTree tree\n---------------",
      "\nDepth =", addtree$height - 1,
      "\nN nodes =", addtree$totalCount - 1,
      "\nN leaves =", length(addtree$leaves)
    )
  )
  # Node Style ----
  .node_labels <- if (node_labels) {
    if (node_labels_pct_pos) {
      # Include % positive cases
      function(node) {
        paste(
          # gsub('[{}]', "", node$Condition),
          formatcondition(node$Condition),
          paste("N =", node$N),
          paste(paste0(ddSci(node$pct_pos, as_numeric = TRUE) * 100, "%"), pos_name),
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

  data.tree::SetNodeStyle(addtree,
    style = "filled",
    shape = node_shape,
    fillcolor = node_col,
    col = node_col,
    fontname = fontname,
    label = .node_labels,
    tooltip = function(node) {
      paste(paste("Node", node$n),
        paste("Value =", ddSci(node$Value)),
        sep = "\n"
      )
    },
    rank = function(node) node$Depth
  )

  # Edge Style ----
  # .edge_labels <- if (edge_labels) function(node) node$Condition else NULL
  # sometimes fails
  # data.tree::SetEdgeStyle(addtree,
  #                         keepExisting = TRUE,
  #                         arrowhead = arrowhead,
  #                         color = edge_col,
  #                         penwidth = edge_width,
  #                         fontname = fontname,
  #                         label = .edge_labels,
  #                         tooltip = function(node) node$Condition)
  # ?drop_leaves, keepExisting = TRUE

  # leaves_rank <- if (drop_leaves) addtree$height else NULL
  data.tree::Do(addtree$leaves, function(node) {
    data.tree::SetNodeStyle(node,
      # rank = leaves_rank,
      style = "filled",
      shape = node_shape,
      fillcolor = function(node) {
        ifelse(node$EstimateInt == 1 & node$isLeaf,
          col_positive, col_negative
        )
      }
    )
  })

  # Write to file ----
  # if (!is.null(filename)) {
  #   check_dependencies("DiagrammeRsvg", "rsvg")
  #   filename <- file.path(filename)
  #   plt_svg <- DiagrammeRsvg::export_svg(plt)
  #   rsvg::rsvg_pdf(charToRaw(plt_svg), file = filename)
  # }

  plot(addtree)
} # rtemis::draw_addtree


formatcondition <- function(x,
                            remove_chars = "[{}]",
                            decimal_places = 2) {
  categorical <- any(grepl("{", x, fixed = TRUE))
  if (categorical) {
    xf <- gsub(remove_chars, "", x)
  } else {
    xf <- strsplit(x, " ")[[1]]
    xf[3] <- ddSci(xf[3], decimal_places = decimal_places)
    xf <- paste(xf, collapse = " ")
  }
  xf
}
