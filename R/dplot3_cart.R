# dplot3_cart.R
# ::rtemis::
# E.D. Gennatas rtemis.org
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
#' @inheritParams dplot3_addtree
#' @param object Either `rpart` object or `rtMod` object trained with
#' [s_CART]
#' @param col.lo Low color for estimated outcome
#' @param col.mid Middle color for estimated outcome
#' @param col.hi High color for estimated outcome
#' @param node.shape Shape of node. Default = "none"
#' @param node.labels Logical: If TRUE, print the node labels.
#' @param node.cond Logical: If TRUE, print the splitting condition inside each
#' node.
#' @param node.prob Logical: If TRUE, print the probability estimate for the
#' first class of the outcome inside each node.
#' @param node.estimate Logical: If TRUE, print the estimated outcome level
#' inside each node.
#' @param node.n Logical: If TRUE, print the number of cases (from training
#' data) that matched this condition
#' @param edge.col Color for edges.
#' @param edge.width Width of edges.
#' @param edge.labels Logical: If TRUE, print the splitting condition on the
#' edge.
#' @param arrowhead Character: Arrowhead shape.
#' @param layout Character: Passed to `data.tree::SetGraphStyle`
#' @param drop.leaves Logical: If TRUE, position leaves at the bottom of the plot.
#' @param rankdir Character: Passed to `data.tree::SetGraphStyle`
#' @param splines Character: Passed to `data.tree::SetGraphStyle`
#' @param fontname Character: Passed to `data.tree::SetGraphStyle`
#' @param bg.color Background color.
#' @param overlap Character: Passed to `data.tree::SetGraphStyle`
#' @param prune Logical: If TRUE, prune tree using `rpart::prune.rpart`
#' @param rpart.cp Numeric: Complexity parameter for pruning. If NULL, no
#' pruning is performed.
#' @param verbose Logical: If TRUE, print messages.
#'
#' @author E.D. Gennatas
#' @export

dplot3_cart <- function(
  object,
  col.positive = "#F48024DD",
  col.negative = "#18A3ACDD",
  col.lo = "#80ffff",
  col.mid = "gray20",
  col.hi = "#F4A0FF",
  node.col = "#666666",
  node.shape = "none",
  node.labels = TRUE,
  node.cond = TRUE,
  node.prob = TRUE,
  node.estimate = NULL,
  node.n = TRUE,
  edge.col = "#999999",
  edge.width = 2,
  edge.labels = FALSE,
  arrowhead = "vee",
  layout = "dot",
  drop.leaves = FALSE,
  rankdir = "TB",
  splines = "polyline",
  fontname = "helvetica",
  bg.color = "white",
  overlap = "false",
  prune = FALSE,
  rpart.cp = NULL,
  verbose = TRUE
) {
  # Dependencies ----
  dependency_check("data.tree")

  # Prune ----
  if (!is.null(rpart.cp)) {
    .tree <- rpart::prune.rpart(.tree, rpart.cp)
  }

  # data.tree ----
  if (inherits(object, "rtMod")) {
    if (inherits(object$mod, "rpart")) {
      .tree <- as.data.tree.rpart(object$mod)
      method <- object$mod$method
      y <- object$mod$model$y
      if (verbose) msg2("Object is rtemis rpart model")
    } else {
      stop("Input must be of type rpart")
    }
  } else {
    if (inherits(object, "rpart")) {
      .tree <- as.data.tree.rpart(object)
      method <- object$method
      y <- object$model$y
      if (verbose) msg2("Object is rpart model")
    }
  }
  type <- "rpart"
  if (is.null(node.estimate))
    node.estimate <- if (method == "class") FALSE else TRUE

  # Graph Style ----
  data.tree::SetGraphStyle(
    .tree,
    layout = layout,
    rankdir = rankdir,
    splines = splines,
    bgcolor = bg.color,
    overlap = overlap,
    tooltip = paste(
      toupper(type),
      "tree\n---------------",
      "\nDepth =",
      .tree$height,
      "\nN nodes =",
      .tree$totalCount,
      "\nN leaves =",
      length(.tree$leaves)
    )
  )

  # Node Style ----
  .node.labels <- if (node.labels) {
    if (method == "class") {
      function(node) {
        paste0(
          if (node.cond) paste(node$name),
          if (node.n) paste("\nN =", node$N),
          if (node.prob) paste("\n", ddSci(node$ProbClass1)),
          if (node.estimate) paste("\n", node$EstimateLabel)
        )
      }
    } else if (method == "anova") {
      function(node) {
        paste0(
          if (node.cond) paste(node$name),
          if (node.n) paste("\nN =", node$N),
          if (node.estimate) paste("\n", ddSci(node$Estimate))
        )
      }
    }
  } else {
    ""
  }

  data.tree::SetNodeStyle(
    .tree,
    style = "filled,",
    shape = node.shape,
    fillcolor = node.col,
    col = node.col,
    fontname = fontname,
    label = .node.labels,
    tooltip = if (method == "class") {
      function(node) {
        paste(
          paste("Node", node$node.id),
          paste("Prob =", ddSci(node$ProbClass1)),
          paste("Estimate level =", node$Estimate),
          paste("Estimate label =", node$EstimateLabel),
          sep = "\n"
        )
      }
    } else if (method == "anova") {
      function(node) {
        paste(
          paste("Node", node$node.id),
          paste("Estimate =", ddSci(node$Estimate)),
          sep = "\n"
        )
      }
    },
    rank = function(node) node$Depth
  )

  # Edge Style ----
  .edge.labels <- if (edge.labels) function(node) node$name else NULL # was node$Condition
  data.tree::SetEdgeStyle(
    .tree,
    arrowhead = arrowhead,
    color = edge.col,
    penwidth = edge.width,
    fontname = fontname,
    label = .edge.labels,
    tooltip = function(node) node$name
  ) # was node$Condition
  # ?drop.leaves, keepExisting = TRUE

  # Leaves ----
  leaves.rank <- if (drop.leaves) .tree$height else NULL
  data.tree::Do(.tree$leaves, function(node) {
    data.tree::SetNodeStyle(
      node,
      rank = leaves.rank,
      fillcolor = if (method == "class") {
        function(node) {
          ifelse(node$Estimate == 1 & node$isLeaf, col.positive, col.negative)
        }
      } else if (method == "anova") {
        function(node) {
          colorGrad(101, lo = col.lo, mid = col.mid, hi = col.hi)[round(drange(
            c(
              node$Estimate,
              range(y)
            ),
            0,
            100
          )[1])]
        }
      }
    )
  })
  plot(.tree)
} # rtemis::dplot3_cart
