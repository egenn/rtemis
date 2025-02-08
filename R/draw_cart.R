# draw_cart.R
# ::rtemis::
# EDG rtemis.org
# node.labels
# 1: condition; 2: probability; 4: N cases
# TODO: Fails sometimes for regression: must track down through DiagrammeR to graphviz
# Rewrite using visNetwork

#' Plot `rpart` decision trees
#'
#' Plot `rpart` decision trees using `data.tree::plot.Node`
#'
#' If you want to show split conditions as edge labels (`edge.labels = TRUE`),
#' it is recommened to set `rankdir = "LR"` and `node.cond = FALSE`.
#' Edge labels in graphviz are shown to the right of
#' the edge when `rankdir = "TB"` and above when `rankdir = "LR"`.
#'
#' @inheritParams draw_addtree
#' @param object Either `rpart` object or `rtMod` object trained with
#' [s_CART]
#' @param col_lo Low color for estimated outcome
#' @param col_mid Middle color for estimated outcome
#' @param col_hi High color for estimated outcome
#' @param node_shape Shape of node.
#' @param node_labels Logical: If TRUE, print the node labels.
#' @param node_cond Logical: If TRUE, print the splitting condition inside each
#' node.
#' @param node_prob Logical: If TRUE, print the probability estimate for the
#' first class of the outcome inside each node.
#' @param node_estimate Logical: If TRUE, print the estimated outcome level
#' inside each node.
#' @param node_n Logical: If TRUE, print the number of cases (from training
#' data) that matched this condition
#' @param edge_col Color for edges.
#' @param edge_width Width of edges.
#' @param edge_labels Logical: If TRUE, print the splitting condition on the
#' edge.
#' @param arrowhead Character: Arrowhead shape.
#' @param layout Character: Passed to `data.tree::SetGraphStyle`
#' @param drop_leaves Logical: If TRUE, position leaves at the bottom of the plot.
#' @param rankdir Character: Passed to `data.tree::SetGraphStyle`
#' @param splines Character: Passed to `data.tree::SetGraphStyle`
#' @param fontname Character: Passed to `data.tree::SetGraphStyle`
#' @param bg_color Background color.
#' @param overlap Character: Passed to `data.tree::SetGraphStyle`
#' @param prune Logical: If TRUE, prune tree using `rpart::prune.rpart`
#' @param rpart_cp Numeric: Complexity parameter for pruning. If NULL, no
#' pruning is performed.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @export

draw_cart <- function(object,
                      col_positive = "#F48024DD",
                      col_negative = "#18A3ACDD",
                      col_lo = "#80ffff",
                      col_mid = "gray20",
                      col_hi = "#F4A0FF",
                      node_col = "#666666",
                      node_shape = "none",
                      node_labels = TRUE,
                      node_cond = TRUE,
                      node_prob = TRUE,
                      node_estimate = NULL,
                      node_n = TRUE,
                      edge_col = "#999999",
                      edge_width = 2,
                      edge_labels = FALSE,
                      arrowhead = "vee",
                      layout = "dot",
                      drop_leaves = FALSE,
                      rankdir = "TB",
                      splines = "polyline",
                      fontname = "helvetica",
                      bg_color = "white",
                      overlap = "false",
                      prune = FALSE,
                      rpart_cp = NULL,
                      verbosity = 1L) {
  # Dependencies ----
  check_dependencies("data.tree")

  # Prune ----
  if (!is.null(rpart_cp)) {
    .tree <- rpart::prune.rpart(.tree, rpart_cp)
  }

  # data.tree ----
  if (inherits(object, "rtMod")) {
    if (inherits(object$mod, "rpart")) {
      .tree <- as.data.tree.rpart(object$mod)
      method <- object$mod$method
      y <- object$mod$model$y
      if (verbosity > 0L) msg2("Object is rtemis rpart model")
    } else {
      stop("Input must be of type rpart")
    }
  } else {
    if (inherits(object, "rpart")) {
      .tree <- as.data.tree.rpart(object)
      method <- object$method
      y <- object$model$y
      if (verbosity > 0L) msg2("Object is rpart model")
    }
  }
  type <- "rpart"
  if (is.null(node_estimate)) node_estimate <- if (method == "class") FALSE else TRUE

  # Graph Style ----
  data.tree::SetGraphStyle(.tree,
    layout = layout,
    rankdir = rankdir,
    splines = splines,
    bgcolor = bg_color,
    overlap = overlap,
    tooltip = paste(
      toupper(type), "tree\n---------------",
      "\nDepth =", .tree$height,
      "\nN nodes =", .tree$totalCount,
      "\nN leaves =", length(.tree$leaves)
    )
  )

  # Node Style ----
  .node_labels <- if (node_labels) {
    if (method == "class") {
      function(node) {
        paste0(
          if (node_cond) paste(node$name),
          if (node_n) paste("\nN =", node$N),
          if (node_prob) paste("\n", ddSci(node$ProbClass1)),
          if (node_estimate) paste("\n", node$EstimateLabel)
        )
      }
    } else if (method == "anova") {
      function(node) {
        paste0(
          if (node_cond) paste(node$name),
          if (node_n) paste("\nN =", node$N),
          if (node_estimate) paste("\n", ddSci(node$Estimate))
        )
      }
    }
  } else {
    ""
  }

  data.tree::SetNodeStyle(.tree,
    style = "filled,",
    shape = node_shape,
    fillcolor = node_col,
    col = node_col,
    fontname = fontname,
    label = .node_labels,
    tooltip = if (method == "class") {
      function(node) {
        paste(paste("Node", node$node.id),
          paste("Prob =", ddSci(node$ProbClass1)),
          paste("Estimate level =", node$Estimate),
          paste("Estimate label =", node$EstimateLabel),
          sep = "\n"
        )
      }
    } else if (method == "anova") {
      function(node) {
        paste(paste("Node", node$node.id),
          paste("Estimate =", ddSci(node$Estimate)),
          sep = "\n"
        )
      }
    },
    rank = function(node) node$Depth
  )

  # Edge Style ----
  .edge_labels <- if (edge_labels) function(node) node$name else NULL # was node$Condition
  data.tree::SetEdgeStyle(.tree,
    arrowhead = arrowhead,
    color = edge_col,
    penwidth = edge_width,
    fontname = fontname,
    label = .edge_labels,
    tooltip = function(node) node$name
  ) # was node$Condition
  # ?drop.leaves, keepExisting = TRUE

  # Leaves ----
  leaves_rank <- if (drop_leaves) .tree$height else NULL
  data.tree::Do(.tree$leaves, function(node) {
    data.tree::SetNodeStyle(node,
      rank = leaves_rank,
      fillcolor = if (method == "class") {
        function(node) {
          ifelse(node$Estimate == 1 & node$isLeaf,
            col_positive, col_negative
          )
        }
      } else if (method == "anova") {
        function(node) {
          colorgrad(101,
            lo = col_lo,
            mid = col_mid,
            hi = col_hi
          )[round(drange(c(
            node$Estimate,
            range(y)
          ), 0, 100)[1])]
        }
      }
    )
  })
  plot(.tree)
} # /rtemis::draw_cart
