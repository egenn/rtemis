# ruleDist.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Rule distance
#'
#' Calculate pairwise distance among a set of rules or between two sets of rules,
#' where each rule defines a subpopulation
#'
#' If only rules1 is provided, computes pairwise distance among rules1, otherwise computes pairwise distance between
#' rules1 and rules2
#'
#' @param x Data frame / matrix: Input features (cases by features)
#' @param rules1 Character, vector: Rules as combination of conditions on the features of \code{x}
#' @param rules2 String, vector, Optional: Rules as combination of conditions on the features of \code{x}
#' @param print.plot Logical: If TRUE, plot heatmap for calculated distance
#' @param plot.type Character: "static", "interactive": type of graphics to use, base or plotly, respectively. Default =
#' "static"
#' @param heat.lo Color: Heatmap low color. Default = "black"
#' @param heat.mid Color: Heatmap mid color. Default = NA (i.e. create gradient from `heat.lo` to `heat.hi`)
#' @param heat.hi Colo: Heatmap hi colo. Default = "#F48024" (orange)
#' @param verbose Logical: If TRUE, print console messages. Default = TRUE
#' @author E.D. Gennatas
#' @export

ruleDist <- function(x,
                     rules1,
                     rules2 =  NULL,
                     print.plot = TRUE,
                     plot.type = c("static", "interactive"),
                     # heat.lo = "black",
                     heat.lo = "black",
                     heat.mid = NA,
                     heat.hi = "#F48024",
                     verbose = TRUE) {

  #  Match cases by rules
  cxr1 <- matchCasesByRules(x, rules1, verbose)

  if (!is.null(rules2)) cxr2 <- matchCasesByRules(x, rules2, verbose)

  # Rule by rule distance
  if (is.null(rules2)) {
    rxr.hamming <- apply(cxr1, 2, function(i) matrixStats::colSums2(i != cxr1))
  } else {
    rxr.hamming <- sapply(seq_along(rules1), function(i) matrixStats::colSums2(cxr1[, i] != cxr2))
  }

  # Rule total distance
  rules.total.dist1 <- matrixStats::colSums2(rxr.hamming)

  # Order rules by total distance
  rules.ordered1 <- order(rules.total.dist1, decreasing = TRUE)

  rules.total.dist2 <- rules.ordered2 <- NULL
  if (!is.null(rules2)) {
    rules.total.dist2 <- matrixStats::rowSums2(rxr.hamming)

    # Order rules by total distance
    rules.ordered2 <- order(rules.total.dist2, decreasing = TRUE)
  }

  # Plot ====
  if (print.plot) {
    plot.type <- match.arg(plot.type)
    if (plot.type == "static") {
      mplot3.heatmap(rxr.hamming,
                     Rowv = TRUE, Colv = TRUE,
                     autorange = FALSE,
                     zlim = c(0, nrow(x)),
                     lo = heat.lo, mid = heat.mid, hi = heat.hi)
    } else {
      dplot3.heatmap(rxr.hamming,
                     Rowv = TRUE, Colv = TRUE,
                     limits = c(0, nrow(x)),
                     lo = heat.lo, mid = heat.mid, hi = heat.hi)
    }
  }

  out <- list(rxr.hamming = rxr.hamming,
              rules.total.dist1 = rules.total.dist1,
              rules.ordered1 = rules.ordered1)

  if (!is.null(rules2)) {
    out$rules.total.dist2 <- rules.total.dist2
    out$rules.ordered2 <- rules.ordered2
  }

  class(out) <- c("rtRuleDist", "list")
  out

} # rtemis::ruleDist


hamming  <- function(x, y) {
  sum(x != y)
} # rtemis::hamming
