# dplot3.linad.R
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' Plot a Linear Additive Tree trained by \code{s.LINAD} using \strong{visNetwork}
#'
#' @param tree \code{s.LINAD} tree
#' @param main Character: Title. Default = NULL
#' @param bg Background color. Default = "#FFFFFF" (white)
#' @param shape Character: Node shape; one of: "square", "triangle", "box", "circle", "dot", "star", "ellipse", "database",
#' "text", "diamond". Default = "box"
#' @param nodelabels Logical: If TRUE, inlcude node labels. Default = TRUE
#' @param ncases.inlabels Logical: If TRUE, include number of cases with the node labels. Default = TRUE
#' @param rules.on.edges Logical: If TRUE, display rules on edges instead of nodes. Default = FALSE
#' @param node.col Color for nodes. Default = #7F7F7F" (some gray)
#' @param leaf.col Color for leaf nodes. Default = "#18A3AC" (teal)
#' @param edge.col Color for edges. Default = "#848484" (another gray)
#' @param col.highlight Color for surrounding edges when node is selected. Default = "#F48024" (orange)
#' @param node.font.col Color for node labels. Default varies by \code{shape}, black or white depending if
#' \code{visNetwork} draws labels on node or underneath
#' @param edge.font.col Color for edge labels. Default = "#000000" (black)
#' @param sort.coefs Logical: If TRUE, sort each coefs table. Default = FALSE
#' @param height Float: Height for \code{visNetwork}. Default = NULL, i.e. auto
#' @param width Float: Width for \code{visNetwork}. Default = NULL, i.e. auto
#' @param levelSeparation Float: N of pixels to separate tree levels. Default = 100
#' @param tree.font.size Integer: Font size for tree labels. Default = 22
#' @param edgethickness.by.ncases. Logical: If TRUE, scale edge thickness by number of cases with weight = 1
#' @param font.family Character: Font to use throughout. Default = 'Helvetica Neue', because otherwise it will fail on a
#' number of external viewers, but feel free to play around, esp. within RStudio
#' @param tooltip.coefs Logical: If TRUE, show html coefficient tables on hover over nodes. This was placed here before
#' a custom html table creation function was made to replace some impossibly slow alternatives.
#' @param tooltip.delay Float: Delay (in milliseconds) on mouse over before showing tooltip. Default = 50
#' @param table.font.size Character: Font size for html coefficient on-hover tables. Default = "14px"
#' @param table.dat.padding Ignore, has no visible effect. Otherwise, Character: html table padding. Default = "0px"
#' @param table.lo.col Color for lowest coefficient values (negative). Default = "#80FFFF" (light blue)
#' @param table.hi.col Color for highest coefficient values (positive). Default = "#FFBE00" (light orange)
#' @param trace Integer: If > 0, print info to console (not particularly informative). Default = 0
#'
#' @export

dplot3.linad <- function(x,
                         main = NULL,
                         bg = "#FFFFFF",
                         shape = "box",
                         nodelabels = TRUE,
                         ncases.inlabels = TRUE,
                         rules.on.edges = FALSE,
                         log = FALSE,
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
                         trace = 0) {

  # [ Dependencies ] ====
  if (!depCheck("visNetwork", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ Arguments ] ====
  if (inherits(x, "rtMod")) {
    if (inherits(x$mod, "shytreeLeavesRC")) tree <- x$mod$tree
    if (inherits(x$mod, "shytreegamleaves")) tree <- x$mod$tree
  } else {
    stop("Please provide an rtMod object with a LINAD model")
  }

  # Data ====
  nodeids <- as.numeric(names(tree))
  nodeids_depth <- floor(log(nodeids, 2))
  .nodelabels <-x$mod$all.step.leaves$rules$condition
  colors <- rep(node.col, length(nodeids))
  nodeterminal <- !c(nodeids*2) %in% nodeids
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

  # Theme ====
  # Need to merge devel first
  # if (is.null(bg)) bg <- theme$plot.background
  if (is.null(node.font.col)) {
    if (shape %in% c("box", "circle")) {
      node.font.col <- "#fff"
    } else {
      node.font.col <- "#000"
    }
  }

  # HTML tables for tooltips ====
  # for vertical coefsl
  if (tooltip.coefs) {
    # '- custom ====
    # exclude intercept so that it doesn't soak up all the range
    # account for sorting and/or top
    # coefsl.noint <- lapply(coefsl, function(i) i[-1, ])
    val <- if (log) {
      log10n(unlist(lapply(coefsl, function(i) i[-1, 2])))
    } else {
      unlist(lapply(coefsl, function(i) i[-1, 2]))
    }
    dat.colm <- matrix(colorgradient.x(val,
                                       symmetric = TRUE,
                                       lo.col = table.lo.col,
                                       hi.col = table.hi.col),
                       ncol = length(coefsl))

    coefs.html <- lapply(seq(coefsl), function(i) {
      twocol2html(coefsl[[i]], font.family = font.family,
                  dat.col = c("#333333", dat.colm[, i]),
                  font.size = table.font.size,
                  dat.padding = table.dat.padding)
    })
  } else {
    coefs.html <- NULL
  }

  source_index <- which(sapply(nodeids, function(i) (i*2) %in% nodeids))
  source_index <- rep(source_index, each = 2)
  target_index <- 2:length(nodeids)
  Ncases <- sapply(tree, function(i) sum(i$weights == 1))

  # visNetwork ====
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
    edges <- data.frame(from = source_index,
                        to = target_index)
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
    edges <- data.frame(from = source_index,
                        to = target_index,
                        label = .nodelabels[-1])
  }

  if (tooltip.coefs) nodes$title <- as.character(coefs.html)
  if (!nodelabels) nodes$label <- NULL

  if (edgethickness.by.ncases) {
    edges$value <- Ncases[-1]
  }
  if (trace > 0) msg("Drawing graph with visNetwork...")
  # '- visNetwork ====
  visNetwork::visNetwork(nodes, edges,
                         width = width,
                         height = height,
                         main = main,
                         background = bg) %>%
    # '- visHierarchicalLayout ====
    visNetwork::visHierarchicalLayout(levelSeparation = levelSeparation,
                                      nodeSpacing = nodeSpacing,
                                      blockShifting, blockShifting,
                                      edgeMinimization = edgeMinimization,
                                      parentCentralization = parentCentralization,
                                      direction = direction) %>%
    # '- visNodes ====
    visNetwork::visNodes(font = list(color = node.font.col,
                                     size = tree.font.size,
                                     face = font.family),
                         borderWidth = 1,
                         color = list(highlight = col.highlight,
                                      hover = list(background = col.highlight,
                                                   border = col.highlight))) %>%
    # '- visEdges ====
    visNetwork::visEdges(width = edge.width,
                         color = list(color = edge.col,
                                      highlight = col.highlight),
                         font = list(color = edge.font.col,
                                     size = tree.font.size,
                                     face = font.family),
                         # arrows = "to",
                         arrows = list(to = list(
                           enabled = !arrow.middle,
                           scaleFactor = arrow.scale),
                           middle = list(
                             enabled = arrow.middle,
                             scaleFactor = arrow.scale)),
                         arrowStrikethrough = F,
                         hoverWidth = 0) %>%
    visNetwork::visInteraction(hover = TRUE,
                               dragNodes = dragNodes,
                               dragView = TRUE,
                               zoomView = zoomView,
                               tooltipDelay = tooltip.delay,
                               tooltipStyle = 'position:fixed;visibility:hidden;padding: 0px') -> plt
  plt

} # rtemis::dplot3.linad

# twocol2html.R
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' Create html table from a data.frame with 2 columns: Var name, and Coefs
#'
#' @examples
#' \dontrun{
#' x <- data.frame(ID = c("Int", paste0("V", 1:10)),
#'    Coef = rnorm(11))
#' twocol2html(x)
#' }

twocol2html <- function(x,
                        font.family = "'Lato'",
                        font.col = "#ffffff",
                        font.size = "18px",
                        header.bg = "#404040",
                        table.bg = "#7F7F7F",
                        dat.col = rep("#525252", NROW(x)), # get color grad using all tables
                        dat.font.col = "#ffffff",
                        height = "50px",
                        # header
                        head.padding = "5px",
                        # table
                        dat.padding = "5px") {

  # 1. table style ====
  tablestyle <- paste0('<table style="font-family: ', font.family,
                       ', sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color:',
                       font.col, '; font-size: ', font.size,
                       '; padding: 0px; text-align: right; background-color: ',
                       table.bg, '; width: auto; border-top-style: none; border-bottom-style: none; overflow-y: scroll; height: ',
                       height, '; display: box">')

  # 2. header row ====
  header <- paste0('<tr><th style="font-weight: bold; padding: ', head.padding,
                   '; text-align: center;',
                   'background-color: ', header.bg,
                   '">', colnames(x)[1], '</th>
    <th style="font-weight: bold; padding:', head.padding,
    '; text-align: center;',
    'background-color: ', header.bg,
    '">',
    colnames(x)[2], '</th></tr>')

  # 3. Data rows ====
  tab <- vector("character", NROW(x))
  for (i in seq(tab)) {
    # first column: variable name; second column: coefficient
    tab[i] <- paste0('<tr><td>', x[i, 1],
                     '</td><td style="color: ', dat.font.col,
                     '; font-variant-numeric: tabular-nums; background-color: ',
                     dat.col[i],
                     '; padding: ', dat.padding,
                     '">', ddSci(x[i, 2], 3), '</td></tr>')
  }

  # '- convert minus to &minus;
  tab <- gsub(">-", ">&minus;", tab)
  tab <- paste(tab, collapse = "")

  # Combine
  paste(tablestyle, header, tab, '</table>', collapse = "")
}

log10n <- function(x) {
  sign <- rep(1, length(x))
  sign[x < 0] <- -1
  out <- sign * log10(abs(x))
  out[out == -Inf] <- 0
  out
}
