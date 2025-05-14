# graph_node_metrics.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Node-wise (i.e. vertex-wise) graph metrics
#'
#' @param x \pkg{igraph} network
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' datcor <- cor(rnormmat(20, 20, seed = 2021))
#' datcor[sample(seq(datcor), 250)] <- 0
#' x <- igraph::graph_from_adjacency_matrix(adjmatrix = datcor,
#'                                          mode = "lower",
#'                                          weighted = TRUE,
#'                                          diag = FALSE)
#'
#' graph_node_metrics(x)
#' }

graph_node_metrics <- function(x, verbose = TRUE) {
  if (!inherits(x, "igraph")) stop("Input must be igraph object")

  .nodes <- as.character(igraph::V(x))
  .edgeweights <- igraph::E(x)$weight
  .degree <- igraph::degree(x)
  .strength <- igraph::strength(x)

  if (any(.edgeweights < 0)) {
    if (verbose) {
      msg2("Negative weights found: Betweenness and Closeness will be NA")
    }
    .betweenness <- .closeness <- NA
  } else {
    .betweenness <- igraph::betweenness(x)
    .closeness <- igraph::closeness(x)
  }

  .eigen_centrality <- igraph::eigen_centrality(x)$vector
  .hub_score <- igraph::hub_score(x)$vector
  .page_rank <- igraph::page_rank(x)$vector
  .authority_score <- igraph::authority_score(x)$vector

  data.frame(
    Node = .nodes,
    Degree = .degree,
    Strength = .strength,
    Betwenness = .betweenness,
    Closeness = .closeness,
    Eigen_Centrality = .eigen_centrality,
    Hub_Score = .hub_score,
    Page_Rank = .page_rank,
    Authority_Score = .authority_score
  )
} # rtemis::graph_node_metrics
