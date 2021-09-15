# prune.addtree.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Prune ADDTREE tree
#'
#' Prune an ADDTREE tree in Node format using \code{data.tree} to remove sister nodes with same
#' class estimate.
#'
#' @param addtree rtMod trained with \link{s.ADDTREE}
#' @param prune.empty.leaves Logical: If TRUE, remove leaves with 0 cases. Default = TRUE
#' @param remove.bad.parents Logical: If TRUE, remove nodes with no siblings but children and
#' give their children to their parent. Default = TRUE
#' @author E.D. Gennatasd
#' @export

prune.addtree <- function(addtree,
                          prune.empty.leaves = TRUE,
                          remove.bad.parents = TRUE,
                          verbose = TRUE) {

  # Dependencies ====
  if (!depCheck("data.tree", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Tree ====
  if (inherits(addtree, "Node")) {
    addtree <- data.tree::Clone(addtree)
  } else {
    if (!inherits(addtree$mod, "addtree")) stop("Input is not a Node or addtree object")
    addtree <- data.tree::Clone(addtree$mod$addtree)
  }

  # Prune empty leaves ====
  if (prune.empty.leaves) {
    k <- data.tree::Prune(addtree, pruneFun = function(node) !(node$N == 0))
    if (verbose & k > 0) msg("Pruned", k, "empty", ifelse(k == 1, "leaf", "leaves"))
  }

  # Prune leaf siblings ====
  # Remove siblings with same estimate
  i <- data.tree::Prune(addtree, pruneFun = function(node) !(node$isLeaf
                                                             && length(node$siblings) == 1
                                                             && node$siblings[[1]]$isLeaf
                                                             && node$Estimate == node$siblings[[1]]$Estimate))
  if (verbose & i > 0) msg("Pruned", i, "siblings with same estimate")

  # Prune solo leaves ====
  j <- data.tree::Prune(addtree, pruneFun = function(node) !(node$isLeaf
                                                             && length(node$siblings) == 0))
  if (verbose & j > 0) msg("Pruned", j, "solo", ifelse(j == 1, "leaf", "leaves"))

  # Loop ====
  while (i + j > 0) {
    i <- data.tree::Prune(addtree, pruneFun = function(node) !(node$isLeaf
                                                               && length(node$siblings) == 1
                                                               && node$siblings[[1]]$isLeaf
                                                               && node$Estimate == node$siblings[[1]]$Estimate))
    if (verbose & i > 0)  msg("Pruned", i, "siblings with same estimate")
    j <- data.tree::Prune(addtree, pruneFun = function(node)
      !(node$isLeaf
        && length(node$siblings) == 0))
    if (verbose & j > 0) msg("Pruned", j, "solo", ifelse(j == 1, "leaf", "leaves"))
  }

  # Prune empty leaves ====
  if (prune.empty.leaves) {
    k <- 1
    while (k > 0) {
      k <- data.tree::Prune(addtree, pruneFun = function(node) !(node$N == 0))
      if (verbose & k > 0) msg("Pruned", k, ifelse(k == 1, "leaf", "leaves"))
    }
  }

  # Remove bad parents; send children to grandma ====
  # Remove one bad parent at a time
  if (remove.bad.parents) {
    bad.parents <- data.tree::Traverse(addtree,
                                       filterFun = function(node) length(node$siblings) == 0 &&
                                         !node$isLeaf && !node$isRoot)
    i <- 0
    while (length(bad.parents) > 0 & i < 10) {
      i <- i + 1
      # Reduce Depth before removing parent
      try({
        bad.parents[[1]]$children[[1]]$Depth <- bad.parents[[1]]$children[[1]]$Depth - 1
        if (length(bad.parents[[1]]$children) > 1) {
          bad.parents[[1]]$children[[2]]$Depth <- bad.parents[[1]]$children[[2]]$Depth - 1
        }
        bad.parents[[1]]$parent$children <- bad.parents[[1]]$children
        if (verbose) msg(i, "- Removed 1 bad parent")
      })
      bad.parents <- data.tree::Traverse(addtree,
                                         filterFun = function(node) length(node$siblings) == 0 &&
                                           !node$isLeaf && !node$isRoot)
    }
  }

  if (verbose) msg("Done")
  addtree

} # rtemis::prune.addtree
