# dplot3.shytree.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io
# Work in progress: probably only one visualization framework will be supported
# visNetwork: https://datastorm-open.github.io/visNetwork/
# gt: https://gt.rstudio.com/reference/index.html
#' Plot a stepwise hybrid tree trained by \code{s.SHYTREE} using \code{plotly}
#'
#' @param tree \code{s.SHYTREE} tree
#' @param main String: Title. Default = NULL
#' @param bg Background color. Default = "#FFFFFF" (white)
#' @param shape String: Node shape; one of: "square", "triangle", "box", "circle", "dot", "star", "ellipse", "database",
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
#' @param font.family String: Font to use throughout. Default = 'Helvetica Neue', because otherwise it will fail on a
#' number of external viewers, but feel free to play around, esp. within RStudio
#' @param tooltip.coefs Logical: If TRUE, show html coefficient tables on hover over nodes. This was placed here before
#' a custom html table creation function was made to replace some impossibly slow alternatives.
#' @param table.font.size String: Font size for html coefficient on-hover tables. Default = "14px"
#' @param table.dat.padding Ignore, has no visible effect. Otherwise, String: html table padding. Default = "0px"
#' @param table.lo.col Color for lowest coefficient values (negative). Default = "#80FFFF" (light blue)
#' @param table.hi.col Color for highest coefficient values (positive). Default = "#80FFFF" (light orange)
#' @param trace Integer: If > 0, print info to console (not particularly informative). Default = 0
#'
#' @export
dplot3.shytree <- function(tree,
                           main = NULL,
                           bg = "#FFFFFF",
                           shape = "box",
                           nodelabels = TRUE,
                           ncases.inlabels = TRUE,
                           rules.on.edges = FALSE,
                           node.col = "#7F7F7F",
                           leaf.col = "#18A3AC",
                           edge.col = "#848484",
                           col.highlight = "#F48024",
                           # theme = # merge devel first,
                           node.font.col = NULL,
                           edge.font.col = "#000000",
                           sort.coefs = FALSE,
                           height = NULL,
                           width = NULL,
                           levelSeparation = 100,
                           tree.font.size = 22,
                           edgethickness.by.ncases = FALSE,
                           font.family = "Helvetica Neue",
                           # Coef tables
                           tooltip.coefs = TRUE,
                           table.font.size = "14px",
                           table.dat.padding = "0px",
                           table.lo.col = "#2B27F1",
                           table.hi.col = "#FFBE00",
                           trace = 0) {

  # Data ====
  nodeids <- as.numeric(names(tree))
  nodeids_depth <- floor(log(nodeids, 2))
  # .nodelabels <- sapply(tree, function(i) i$split.rule)
  .nodelabels <- sapply(tree, function(i) i$rule)
  .nodelabels <- gsub(".*&", "", .nodelabels)
  .nodelabels <- gsub("^ ", "", .nodelabels) # rules start with space for some reason, fix and remove this line
  .nodelabels_split <- strsplit(.nodelabels, " ")
  .nodelabels <- sapply(.nodelabels_split, function(i) paste(i[1], i[2], ddSci(i[3])))
  .nodelabels[1] <- "All cases"
  colors <- rep(node.col, length(nodeids))
  nodeterminal <- !c(nodeids*2) %in% nodeids
  colors[nodeterminal] <- leaf.col
  # Make root gray
  colors[1] <- "#404040"

  coefs <- as.data.frame(sapply(tree, function(i) i$coef))
  rownames(coefs)[1] <- "(Int)"
  coefnames <- rownames(coefs)

  if (sort.coefs) {
    coefsl <- lapply(seq(coefs), function(i) {
      index <- order(abs(coefs[, i]), decreasing = TRUE)
      data.frame(Var = coefnames[index], Coef = coefs[index, i])
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
      # dat.col1 <- matrix(colorgradient.x(unlist(coefs), symmetric = TRUE,
      #                                   lo.col = table.lo.col,
      #                                   hi.col = table.hi.col),
      #                   NROW(coefs))

    # exclude intercept so that it doesn't soak up all the range
      dat.col <- matrix(colorgradient.x(unlist(coefs[-1, ]), symmetric = TRUE,
                          lo.col = table.lo.col,
                          hi.col = table.hi.col),
                        NROW(coefs) - 1)
      dat.col <- rbind(rep("#333333", NCOL(coefs)), dat.col)

      coefs.html <- lapply(seq(coefsl), function(i) {
        twocol2html(coefsl[[i]], font.family = font.family,
                    dat.col = dat.col[, i], font.size = table.font.size,
                    dat.padding = table.dat.padding)
      })
  } else {
    coefs.html <- NULL
  }

  # coefs_char <- sapply(coefs, function(i) paste0(rownames(coefs), ":", ddSci(i), collapse = "; "))

  source_index <- which(sapply(nodeids, function(i) (i*2) %in% nodeids))
  # sourceids <- rep(nodeids[source_index], each = 2)
  source_index <- rep(source_index, each = 2)
  # targetids <- sourceids*2 + c(0, 1) # unnecessary, all nodes are target other than 1
  # if (!all(targetids == nodeids[-1])) stop("Error")
  target_index <- 2:length(nodeids)
  Ncases <- sapply(tree, function(i) sum(i$weights == 1))
  # labeldata <- paste0(.nodelabels, "          </br>N=", Ncases)
  if (ncases.inlabels) {
    .nodelabels <- paste0(.nodelabels, "\nN=", Ncases)
  }

  # visNetwork ====
  if (!rules.on.edges) {
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
  visNetwork::visNetwork(nodes, edges,
                         width = width,
                         height = height,
                         main = main,
                         background = bg) %>%
    visNetwork::visHierarchicalLayout(levelSeparation = levelSeparation) %>%
    visNetwork::visNodes(font = list(color = node.font.col,
                                     size = tree.font.size,
                                     face = font.family),
                         color = list(highlight = col.highlight)) %>%
    visNetwork::visEdges(arrows = "to",
                         font = list(color = edge.font.col,
                                     size = tree.font.size,
                                     face = font.family),
                         color = list(color = edge.col,
                                      highlight = col.highlight)) %>%
    visNetwork::visInteraction(dragNodes = FALSE,
                               dragView = TRUE,
                               zoomView = TRUE) %>%
    visNetwork::visInteraction(tooltipDelay = 200) -> plt
  plt

} # rtemis::dplot3.shytree

# twocol2html.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.github.io

#' Create html table from a data.frame with 2 columns: Var name, and Coefs
#'
#' @examples
#' \dontrun{
#' x <- data.frame(ID = c("Int", paste0("V", 1:10)),
#'    Coef = rnorm(11))
#' twocol2html(x)
#' }

twocol2html <- function(x,
                        font.family = "'Helvetica Neue'",
                        font.col = "#ffffff",
                        font.size = "14px",
                        header.bg = "#404040",
                        table.bg = "#7F7F7F",
                        dat.col = rep("#525252", NROW(x)), # get color grad using all tables
                        dat.font.col = "#ffffff",
                        height = "50px",
                        # header
                        head.padding = "4px",
                        # table
                        dat.padding = "4px") {

  # 1. table style ====
  tablestyle <- paste0('<table style="font-family: ', font.family,
                       ', sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color:',
                       font.col, '; font-size: ', font.size,
                       '; padding: 4px; text-align: right; background-color: ',
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

