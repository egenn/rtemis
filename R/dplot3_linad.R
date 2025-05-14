# dplot3_linad.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Plot a Linear Additive Tree trained by [s_LINAD] using
#' *visNetwork*
#'
#' @param x `rtMod` object trained using [s_LINAD]
#' @param main Character: Title.
#' @param bg Background color.
#' @param shape Character: Node shape; one of: "square", "triangle", "box", "circle",
#' "dot", "star", "ellipse", "database", "text", "diamond".
#' @param nodelabels Logical: If TRUE, inlcude node labels.
#' @param ncases.inlabels Logical: If TRUE, include number of cases with the
#' node labels.
#' @param rules.on.edges Logical: If TRUE, display rules on edges instead of
#' nodes.
#' @param top Integer: If not NULL, only show the top `top` coefficients.
#' @param root.col Color for root node.
#' @param node.col Color for nodes.
#' @param leaf.col Color for leaf nodes.
#' @param edge.col Color for edges.
#' @param edge.width Numeric: Width for edges.
#' @param arrow.scale Numeric: Scale factor for arrows.
#' @param arrow.middle Logical: If TRUE, draw arrows in the middle of edges.
#' @param col.highlight Color for surrounding edges when node is selected.
#' @param node.font.col Color for node labels. Default varies by `shape`,
#' black or white depending if
#' `visNetwork` draws labels on node or underneath
#' @param edge.font.col Color for edge labels.
#' @param sort.coefs Logical: If TRUE, sort each coefs table.
#' @param height Numeric: Height for `visNetwork`. Default = NULL, i.e. auto
#' @param width Numeric: Width for `visNetwork`. Default = NULL, i.e. auto
#' @param levelSeparation Numeric: N of pixels to separate tree levels.
#' @param tree.font.size Integer: Font size for tree labels. Default = 22
#' @param edgethickness.by.ncases Logical: If TRUE, scale edge thickness by
#' number of cases with weight = 1
#' @param font.family Character: Font to use throughout.
#' Default = 'Helvetica Neue', because otherwise it may fail on a
#' number of external viewers.
#' @param uselog Logical: If TRUE, use log10 scale for coefficient colors.
#' @param tooltip.coefs Logical: If TRUE, show html coefficient tables on hover
#' over nodes. This was placed here before a custom html table creation
#' function was made to replace some impossibly slow alternatives.
#' @param tooltip.delay Numeric: Delay (in milliseconds) on mouse over before
#' showing tooltip.
#' @param table.font.size Character: Font size for html coefficient on-hover
#' tables.
#' @param table.dat.padding Ignore, has no visible effect. Otherwise,
#' Character: html table padding.
#' @param table.lo.col Color for lowest coefficient values (negative)
#' @param table.hi.col Color for highest coefficient values (positive).
#' @param dragNodes Logical: If TRUE, allow dragging nodes.
#' @param zoomView Logical: If TRUE, allow zooming.
#' @param nodeSpacing Numeric: Spacing between nodes.
#' @param blockShifting Logical: If TRUE, allow block shifting.
#' @param edgeMinimization Logical: If TRUE, minimize edge length.
#' @param parentCentralization Logical: If TRUE, centralize parent nodes.
#' @param direction Character: Direction of tree. One of: "UD", "DU", "LR",
#' "RL".
#' @param trace Integer: If > 0, print info to console (not particularly
#' informative).
#'
#' @author E.D. Gennatas
#' @export

dplot3_linad <- function(
  x,
  main = NULL,
  bg = "#FFFFFF",
  shape = "box",
  nodelabels = TRUE,
  ncases.inlabels = TRUE,
  rules.on.edges = FALSE,
  top = NULL,
  root.col = "#202020",
  node.col = "#5a5a5a",
  leaf.col = "#178CCB",
  edge.col = "#848484",
  edge.width = 4,
  arrow.scale = .7,
  arrow.middle = FALSE,
  col.highlight = "#FE4AA3",
  # theme = # merge devel first,
  node.font.col = NULL,
  edge.font.col = "#000000",
  sort.coefs = FALSE,
  height = NULL,
  width = NULL,
  levelSeparation = 100,
  tree.font.size = 22,
  edgethickness.by.ncases = FALSE,
  font.family = "Lato",
  # Coef tables
  uselog = FALSE,
  tooltip.coefs = TRUE,
  tooltip.delay = 50,
  table.font.size = "16px",
  table.dat.padding = "0px",
  table.lo.col = "#0290EE",
  table.hi.col = "#FE4AA3",
  # visNetwork
  dragNodes = FALSE,
  zoomView = FALSE,
  nodeSpacing = 150,
  blockShifting = TRUE,
  edgeMinimization = TRUE,
  parentCentralization = TRUE,
  direction = "UD",
  trace = 0
) {
  # Dependencies ----
  dependency_check("visNetwork")

  # Arguments ----
  if (inherits(x, "rtMod")) {
    if (inherits(x$mod, "shytreeLeavesRC")) tree <- x$mod$tree
    if (inherits(x$mod, "linadleaves")) tree <- x$mod$tree
  } else {
    stop("Please provide an rtMod object with a LINAD model")
  }

  # Data ----
  nodeids <- as.numeric(names(tree))
  nodeids_depth <- floor(log(nodeids, 2))
  .nodelabels <- x$mod$all.step.leaves$rules$condition
  colors <- rep(node.col, length(nodeids))
  nodeterminal <- !c(nodeids * 2) %in% nodeids
  colors[nodeterminal] <- leaf.col
  # Make root gray
  colors[1] <- root.col

  coefs <- as.data.frame(sapply(tree, function(i) i$coef))
  rownames(coefs)[1] <- "(Int)"
  coefnames <- rownames(coefs)

  if (!is.null(top)) sort.coefs <- TRUE
  if (sort.coefs) {
    indexl <- vector("list", NCOL(coefs))
    coefsl <- lapply(seq(coefs), function(i) {
      indexl[[i]] <- c(1, order(abs(coefs[-1, i]), decreasing = TRUE) + 1)
      if (!is.null(top)) indexl[[i]] <- indexl[[i]][seq(top + 1)]
      data.frame(Var = coefnames[indexl[[i]]], Coef = coefs[indexl[[i]], i])
    })
  } else {
    coefsl <- lapply(seq(coefs), function(i) {
      # vertical
      # replace round with ddSci after calculating custom colors
      # data.frame(Var = coefnames, Coef = round(coefs[, i], digits = digits))
      data.frame(Var = coefnames, Coef = coefs[, i])
      # horizontal
      # dat <- data.frame(t(coefs[, i]))
      # colnames(dat) <- rownames(coefs)
    })
    # index <- seq(NROW(coefs))
  }

  # Theme ----
  # Need to merge devel first
  # if (is.null(bg)) bg <- theme$plot.background
  if (is.null(node.font.col)) {
    if (shape %in% c("box", "circle")) {
      node.font.col <- "#fff"
    } else {
      node.font.col <- "#000"
    }
  }

  # HTML tables for tooltips ----
  # for vertical coefsl
  if (tooltip.coefs) {
    # '- custom ----
    # exclude intercept so that it doesn't soak up all the range
    # account for sorting and/or top
    # coefsl.noint <- lapply(coefsl, function(i) i[-1, ])
    val <- if (uselog) {
      log10n(unlist(lapply(coefsl, function(i) i[-1, 2])))
    } else {
      unlist(lapply(coefsl, function(i) i[-1, 2]))
    }
    dat.colm <- matrix(
      colorgradient.x(
        val,
        symmetric = TRUE,
        lo.col = table.lo.col,
        hi.col = table.hi.col
      ),
      ncol = length(coefsl)
    )

    coefs.html <- lapply(seq(coefsl), function(i) {
      twocol2html(
        coefsl[[i]],
        font.family = font.family,
        dat.col = c("#333333", dat.colm[, i]),
        font.size = table.font.size,
        dat.padding = table.dat.padding
      )
    })
  } else {
    coefs.html <- NULL
  }

  source_index <- which(sapply(nodeids, function(i) (i * 2) %in% nodeids))
  source_index <- rep(source_index, each = 2)
  target_index <- 2:length(nodeids)
  Ncases <- sapply(tree, function(i) sum(i$weights == 1))

  # visNetwork ----
  if (!rules.on.edges) {
    if (ncases.inlabels) {
      .nodelabels <- paste0(.nodelabels, "\n(n=", Ncases, ")")
    }
    nodes <- data.frame(
      id = seq(nodeids),
      label = paste(.nodelabels),
      value = Ncases,
      shape = shape,
      level = nodeids_depth + 1,
      # title = coefs.html,# tooltip on mouseover
      color = colors,
      shadow = FALSE
    )
    edges <- data.frame(
      from = source_index,
      to = target_index
    )
  } else {
    nodes <- data.frame(
      id = seq(nodeids),
      label = paste("N = ", Ncases),
      value = Ncases,
      shape = shape,
      level = nodeids_depth + 1,
      # title = coefs.html,# tooltip on mouseover
      color = colors,
      shadow = FALSE
    )
    edges <- data.frame(
      from = source_index,
      to = target_index,
      label = .nodelabels[-1]
    )
  }

  if (tooltip.coefs) nodes$title <- as.character(coefs.html)
  if (!nodelabels) nodes$label <- NULL

  if (edgethickness.by.ncases) {
    edges$value <- Ncases[-1]
  }
  if (trace > 0) msg2("Drawing graph with visNetwork...")
  # '- visNetwork ----
  visNetwork::visNetwork(
    nodes,
    edges,
    width = width,
    height = height,
    main = main,
    background = bg
  ) |>
    # '- visHierarchicalLayout ----
    visNetwork::visHierarchicalLayout(
      levelSeparation = levelSeparation,
      nodeSpacing = nodeSpacing,
      blockShifting,
      blockShifting,
      edgeMinimization = edgeMinimization,
      parentCentralization = parentCentralization,
      direction = direction
    ) |>
    # '- visNodes ----
    visNetwork::visNodes(
      font = list(
        color = node.font.col,
        size = tree.font.size,
        face = font.family
      ),
      borderWidth = 1,
      color = list(
        highlight = col.highlight,
        hover = list(
          background = col.highlight,
          border = col.highlight
        )
      )
    ) |>
    # '- visEdges ----
    visNetwork::visEdges(
      width = edge.width,
      color = list(
        color = edge.col,
        highlight = col.highlight
      ),
      font = list(
        color = edge.font.col,
        size = tree.font.size,
        face = font.family
      ),
      # arrows = "to",
      arrows = list(
        to = list(
          enabled = !arrow.middle,
          scaleFactor = arrow.scale
        ),
        middle = list(
          enabled = arrow.middle,
          scaleFactor = arrow.scale
        )
      ),
      arrowStrikethrough = FALSE,
      hoverWidth = 0
    ) |>
    visNetwork::visInteraction(
      hover = TRUE,
      dragNodes = dragNodes,
      dragView = TRUE,
      zoomView = zoomView,
      tooltipDelay = tooltip.delay,
      tooltipStyle = "position:fixed;visibility:hidden;padding: 0px"
    ) -> plt
  plt
} # rtemis::dplot3_linad

log10n <- function(x) {
  sign <- rep(1, length(x))
  sign[x < 0] <- -1
  out <- sign * log10(abs(x))
  out[out == -Inf] <- 0
  out
}
