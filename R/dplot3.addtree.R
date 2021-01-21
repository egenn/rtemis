# dplot3.addtree
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Plot ADDTREE trees
#'
#' Plot ADDTREE trees trained with \link{s.ADDTREE} using \code{data.tree::plot.Node}
#'
#' Edge info and styles have been removed because of problems with \code{DiagrammeR}
#' @param col.positive Color for outcome positive. Default = "#F48024DD"
#' @param col.negative Color for negative outcome. Default = "#18A3ACDD"
#' @param node.col Color for non-terminal leaves. Default = "#666666"
#' @param edge.col Color for edges. Default = "#999999"
#' @param bg.color Background color. Default = "#ffffff"
#' @param filename String, Optional: Save PDF to this file. Requires packages \code{DiagrammeRsvg}
#' and \code{rsvg}. Default = NULL
#' @author E.D. Gennatas
#' @export

dplot3.addtree <- function(addtree,
                           col.positive = "#F48024DD",
                           col.negative = "#18A3ACDD",
                           node.col = "#666666",
                           node.shape = "none",
                           node.labels = TRUE,
                           node.labels.pct.pos = NULL,
                           pos.name = NULL,
                           edge.col = "#999999",
                           edge.width = 2,
                           edge.labels = FALSE,
                           arrowhead = "vee",
                           layout = "dot",
                           drop.leaves = FALSE,
                           rankdir = "TB",
                           splines = "polyline",
                           fontname = "helvetica",
                           bg.color = "#ffffff",
                           overlap = "false",
                           prune = NULL,
                           prune.empty.leaves = TRUE,
                           remove.bad.parents = FALSE,
                           filename = NULL) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("data.tree", "DiagrammeR", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ TREE ] ====
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

  # [ ARGUMENTS ] ====
  if (is.null(node.labels.pct.pos)) {
    node.labels.pct.pos <- if (is.null(addtree$pct.pos)) FALSE else TRUE
  }

  # [ PRUNE ] ====
  if (prune) {
    addtree <- prune.addtree(addtree,
                             prune.empty.leaves = prune.empty.leaves,
                             remove.bad.parents = remove.bad.parents)
  }

  # [ GPARH STYLE ] ====
  data.tree::SetGraphStyle(addtree,
                           layout = layout,
                           rankdir = rankdir,
                           splines = splines,
                           bgcolor = bg.color,
                           overlap = overlap,
                           tooltip = paste("ADDTREE tree\n---------------",
                                           "\nDepth =", addtree$height - 1,
                                           "\nN nodes =", addtree$totalCount - 1,
                                           "\nN leaves =", length(addtree$leaves)))
  # [ NODE STYLE ] ====
  .node.labels <- if (node.labels) {
    if (node.labels.pct.pos) {
      # Include % positive cases
      function(node) paste(
                           # gsub('[{}]', "", node$Condition),
                           formatcondition(node$Condition),
                           paste("N =", node$N),
                           paste(paste0(ddSci(node$pct.pos, asNumeric = TRUE) * 100, "%"), pos.name),
                           sep = "\n")
    } else {
      function(node) paste(formatcondition(node$Condition), paste("N =", node$N), sep = "\n")
    }
  } else {
    ""
  }

  data.tree::SetNodeStyle(addtree,
                          style = "filled",
                          shape = node.shape,
                          fillcolor = node.col,
                          col = node.col,
                          fontname = fontname,
                          label = .node.labels,
                          tooltip = function(node) paste(paste("Node", node$n),
                                                         paste("Value =", ddSci(node$Value)),
                                                         sep = "\n"),
                          rank = function(node) node$Depth)

  # [ EDGE STYLE ] ====
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
    data.tree::SetNodeStyle(node,
                            # rank = leaves.rank,
                            style = "filled",
                            shape = node.shape,
                            fillcolor = function(node) {
                              ifelse(node$EstimateInt == 1 & node$isLeaf,
                                     col.positive, col.negative)
                            })
  })

  # Write to file ====
  if (!is.null(filename)) {
    if (!depCheck("DiagrammeRsvg", "rsvg", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    filename <- file.path(filename)
    plt_svg <- DiagrammeRsvg::export_svg(plt)
    rsvg::rsvg_pdf(charToRaw(plt_svg), file = filename)
  }

  plot(addtree)

} # rtemis::dplot3.addtree


formatcondition <- function(x,
                            remove.chars = "[{}]",
                            decimal.places = 2) {

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
