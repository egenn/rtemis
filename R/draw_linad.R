# draw_linad.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Plot a Linear Additive Tree trained by [s_LINAD] using
#' *visNetwork*
#'
#' @param x `rtMod` object trained using [s_LINAD]
#' @param main Character: Title.
#' @param bg Background color.
#' @param shape Character: Node shape; one of: "square", "triangle", "box", "circle",
#' "dot", "star", "ellipse", "database", "text", "diamond".
#' @param nodelabels Logical: If TRUE, inlcude node labels.
#' @param ncases_inlabels Logical: If TRUE, include number of cases with the
#' node labels.
#' @param rules_on_edges Logical: If TRUE, display rules on edges instead of
#' nodes.
#' @param top Integer: If not NULL, only show the top `top` coefficients.
#' @param root_col Color for root node.
#' @param node_col Color for nodes.
#' @param leaf_col Color for leaf nodes.
#' @param edge_col Color for edges.
#' @param edge_width Numeric: Width for edges.
#' @param arrow_scale Numeric: Scale factor for arrows.
#' @param arrow_middle Logical: If TRUE, draw arrows in the middle of edges.
#' @param col_highlight Color for surrounding edges when node is selected.
#' @param node_font_col Color for node labels. Default varies by `shape`,
#' black or white depending if
#' `visNetwork` draws labels on node or underneath
#' @param edge_font_col Color for edge labels.
#' @param sort_coefs Logical: If TRUE, sort each coefs table.
#' @param height Numeric: Height for `visNetwork`. Default = NULL, i.e. auto
#' @param width Numeric: Width for `visNetwork`. Default = NULL, i.e. auto
#' @param levelSeparation Numeric: N of pixels to separate tree levels.
#' @param tree_font_size Integer: Font size for tree labels.
#' @param edgethickness_by_ncases Logical: If TRUE, scale edge thickness by
#' number of cases with weight = 1
#' @param font_family Character: Font to use throughout.
#' Default = 'Helvetica Neue', because otherwise it may fail on a
#' number of external viewers.
#' @param uselog Logical: If TRUE, use log10 scale for coefficient colors.
#' @param tooltip_coefs Logical: If TRUE, show html coefficient tables on hover
#' over nodes. This was placed here before a custom html table creation
#' function was made to replace some impossibly slow alternatives.
#' @param tooltip_delay Numeric: Delay (in milliseconds) on mouse over before
#' showing tooltip.
#' @param table_font_size Character: Font size for html coefficient on-hover
#' tables.
#' @param table_dat_padding Ignore, has no visible effect. Otherwise,
#' Character: html table padding.
#' @param table_lo_col Color for lowest coefficient values (negative)
#' @param table_hi_col Color for highest coefficient values (positive).
#' @param dragNodes Logical: If TRUE, allow dragging nodes.
#' @param zoomView Logical: If TRUE, allow zooming.
#' @param nodeSpacing Numeric: Spacing between nodes.
#' @param blockShifting Logical: If TRUE, allow block shifting.
#' @param edgeMinimization Logical: If TRUE, minimize edge length.
#' @param parentCentralization Logical: If TRUE, centralize parent nodes.
#' @param direction Character: Direction of tree. One of: "UD", "DU", "LR",
#' "RL".
#' @param verbosity Integer: Verbosity level.
#' informative).
#'
#' @author EDG
#' @export

draw_linad <- function(x,
                       main = NULL,
                       bg = "#FFFFFF",
                       shape = "box",
                       nodelabels = TRUE,
                       ncases_inlabels = TRUE,
                       rules_on_edges = FALSE,
                       top = NULL,
                       root_col = "#202020",
                       node_col = "#5a5a5a",
                       leaf_col = "#178CCB",
                       edge_col = "#848484",
                       edge_width = 4,
                       arrow_scale = .7,
                       arrow_middle = FALSE,
                       col_highlight = "#FE4AA3",
                       # theme = # merge devel first,
                       node_font_col = NULL,
                       edge_font_col = "#000000",
                       sort_coefs = FALSE,
                       height = NULL,
                       width = NULL,
                       levelSeparation = 100,
                       tree_font_size = 22,
                       edgethickness_by_ncases = FALSE,
                       font_family = "Lato",
                       # Coef tables
                       uselog = FALSE,
                       tooltip_coefs = TRUE,
                       tooltip_delay = 50,
                       table_font_size = "16px",
                       table_dat_padding = "0px",
                       table_lo_col = "#0290EE",
                       table_hi_col = "#FE4AA3",
                       # visNetwork
                       dragNodes = FALSE,
                       zoomView = FALSE,
                       nodeSpacing = 150,
                       blockShifting = TRUE,
                       edgeMinimization = TRUE,
                       parentCentralization = TRUE,
                       direction = "UD",
                       verbosity = 0L) {
  # Dependencies ----
  check_dependencies("visNetwork")

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
  .nodelabels <- x$mod$all_step_leaves$rules$condition
  colors <- rep(node_col, length(nodeids))
  nodeterminal <- !c(nodeids * 2) %in% nodeids
  colors[nodeterminal] <- leaf_col
  # Make root gray
  colors[1] <- root_col

  coefs <- as.data.frame(sapply(tree, function(i) i$coef))
  rownames(coefs)[1] <- "(Int)"
  coefnames <- rownames(coefs)

  if (!is.null(top)) sort_coefs <- TRUE
  if (sort_coefs) {
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
  if (is.null(node_font_col)) {
    if (shape %in% c("box", "circle")) {
      node_font_col <- "#fff"
    } else {
      node_font_col <- "#000"
    }
  }

  # HTML tables for tooltips ----
  # for vertical coefsl
  if (tooltip_coefs) {
    # custom ----
    # exclude intercept so that it doesn't soak up all the range
    # account for sorting and/or top
    # coefsl_noint <- lapply(coefsl, function(i) i[-1, ])
    val <- if (uselog) {
      log10n(unlist(lapply(coefsl, function(i) i[-1, 2])))
    } else {
      unlist(lapply(coefsl, function(i) i[-1, 2]))
    }
    dat_colm <- matrix(
      colorgradient_x(val,
        symmetric = TRUE,
        lo_col = table_lo_col,
        hi_col = table_hi_col
      ),
      ncol = length(coefsl)
    )

    coefs_html <- lapply(seq(coefsl), function(i) {
      twocol2html(coefsl[[i]],
        font_family = font_family,
        dat_col = c("#333333", dat_colm[, i]),
        font_size = table_font_size,
        dat_padding = table_dat_padding
      )
    })
  } else {
    coefs_html <- NULL
  }

  source_index <- which(sapply(nodeids, function(i) (i * 2) %in% nodeids))
  source_index <- rep(source_index, each = 2)
  target_index <- 2:length(nodeids)
  Ncases <- sapply(tree, function(i) sum(i$weights == 1))

  # visNetwork ----
  if (!rules_on_edges) {
    if (ncases_inlabels) {
      .nodelabels <- paste0(.nodelabels, "\n(n=", Ncases, ")")
    }
    nodes <- data.frame(
      id = seq(nodeids),
      label = paste(.nodelabels),
      value = Ncases,
      shape = shape,
      level = nodeids_depth + 1,
      # title = coefs_html,# tooltip on mouseover
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
      # title = coefs_html,# tooltip on mouseover
      color = colors,
      shadow = FALSE
    )
    edges <- data.frame(
      from = source_index,
      to = target_index,
      label = .nodelabels[-1]
    )
  }

  if (tooltip_coefs) nodes$title <- as.character(coefs_html)
  if (!nodelabels) nodes$label <- NULL

  if (edgethickness_by_ncases) {
    edges$value <- Ncases[-1]
  }
  if (verbosity > 0L) msg2("Drawing graph with visNetwork...")

  # visNetwork ----
  plt <- visNetwork::visNetwork(nodes, edges,
    width = width,
    height = height,
    main = main,
    background = bg
  ) |>
    # visHierarchicalLayout ----
    visNetwork::visHierarchicalLayout(
      levelSeparation = levelSeparation,
      nodeSpacing = nodeSpacing,
      blockShifting, blockShifting,
      edgeMinimization = edgeMinimization,
      parentCentralization = parentCentralization,
      direction = direction
    ) |>
    # visNodes ----
    visNetwork::visNodes(
      font = list(
        color = node_font_col,
        size = tree_font_size,
        face = font_family
      ),
      borderWidth = 1,
      color = list(
        highlight = col_highlight,
        hover = list(
          background = col_highlight,
          border = col_highlight
        )
      )
    ) |>
    # visEdges ----
    visNetwork::visEdges(
      width = edge_width,
      color = list(
        color = edge_col,
        highlight = col_highlight
      ),
      font = list(
        color = edge_font_col,
        size = tree_font_size,
        face = font_family
      ),
      # arrows = "to",
      arrows = list(
        to = list(
          enabled = !arrow_middle,
          scaleFactor = arrow_scale
        ),
        middle = list(
          enabled = arrow_middle,
          scaleFactor = arrow_scale
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
      tooltipDelay = tooltip_delay,
      tooltipStyle = "position:fixed;visibility:hidden;padding: 0px"
    )
  plt
} # rtemis::draw_linad

log10n <- function(x) {
  sign <- rep(1, length(x))
  sign[x < 0] <- -1
  out <- sign * log10(abs(x))
  out[out == -Inf] <- 0
  out
}
