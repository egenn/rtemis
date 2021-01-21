#' Coefficient heatmap for The Hybrid Tree
#'
#' @author E.D. Gennatas
#' @export

dplot3.shytreecoef <- function(tree,
                               Rowv = FALSE,
                               Colv = FALSE,
                               leaf.col = "#18A3AC",
                               lo = "#2B27F1",
                               mid = "black",
                               hi = "#FFBE00") {

  if (inherits(tree, "rtMod")) {
    if (inherits(tree$mod, "shytreeLeavesRC")) tree <- tree$mod$tree
  }

  # Data ====
  nodeids <- as.numeric(names(tree))
  # nodeids_depth <- floor(log(nodeids, 2))
  # .nodelabels <- sapply(tree, function(i) i$rule)
  # .nodelabels <- gsub(".*&", "", .nodelabels)
  # .nodelabels <- gsub("^ ", "", .nodelabels) # rules start with space for some reason, fix and remove this line
  # .nodelabels_split <- strsplit(.nodelabels, " ")
  # .nodelabels <- sapply(.nodelabels_split, function(i) paste(i[1], i[2], ddSci(i[3])))
  # .nodelabels[1] <- "All cases"
  # colors <- rep(node.col, length(nodeids))
  nodeterminal <- !c(nodeids*2) %in% nodeids
  colors[nodeterminal] <- leaf.col
  coefs <- t(as.data.frame(sapply(tree, function(i) i$coef)))
  colnames(coefs)[1] <- "(Int)"
  coefnames <- rownames(coefs)

  dplot3.heatmap(coefs,
                 Rowv = Rowv, Colv = Colv,
                 lo = lo, mid = mid, hi = hi)

}
