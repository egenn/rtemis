# clustSelect.R
# ::rtemis::
# 2016-22 E.D. Gennatas www.lambdamd.org

#' Select \pkg{rtemis} Clusterer
#'
#' Accepts clusterer name (supports abbreviations) and returns \pkg{rtemis} 
#' function name or the function itself.
#' If run with no parameters, prints list of available algorithms.
#'
#' @param clust Character: Clustering algorithm name. Case insensitive, supports partial matching. e.g. "hop" for HOPACH
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name of algorithm \code{clust}
#' @return Name of function (Default) or function (\code{fn=TRUE}) or full name of algorithm (\code{desc=TRUE})
#' @author E.D. Gennatas
#' @export

clustSelect <- function(clust,
                        fn = FALSE,
                        desc = FALSE) {

  description <- list(
    "CMEANS" = "Fuzzy C-means Clustering",
    "DBSCAN" = "Density-based spatial clustering of applications with noise",
    "EMC" = "Expectation Maximization Clustering",
    "HARDCL" = "Hard Competitive Learning",
    "HOPACH" = "Hierarchical Ordered Partitioning And Collapsing Hybrid",
    "H2OKMEANS" = "H2O K-Means Clustering",
    "KMEANS" = "K-Means Clustering",
    "MEANSHIFT" = "Mean Shift Clustering",
    "NGAS" = "Neural Gas Clustering",
    "PAM" = "Partitioning Around Medoids",
    "PAMK" = "Partitioning Around Medoids with k estimation",
    "SPEC" = "Spectral Clustering"
  )
  description <- t(as.data.frame(description))
  description <- data.frame(Name = rownames(description), Description = description)

  if (missing(clust)) {
    cat(".:clustSelect\nrtemis supports the following clustering algorithms:\n\n")
    print(description, quote = FALSE, row.names = FALSE)
    return(invisible(9))
  }

  name.vec <- toupper(c("cmeans", "dbscan", "emc", "hardcl", "hopach", 
              "h2okmeans", "kmeans", "meanshift", "ngas", "pam", "pamk", 
              "spec"))
  name <- name.vec[pmatch(toupper(clust), name.vec)]
  if (is.na(name)) {
    print(description, quote = FALSE)
    stop("Incorrect clusterer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  if (name == "CMEANS") clusterer <- if (fn) c_CMEANS else "c_CMEANS"
  if (name == "DBSCAN") clusterer <- if (fn) c_DBSCAN else "c_DBSCAN"
  if (name == "EMC") clusterer <- if (fn) c_EMC else "c_EMC"
  if (name == "HARDCL") clusterer <- if (fn) c_HARDCL else "c_HARDCL"
  if (name == "HOPACH") clusterer <- if (fn) c_HOPACH else "c_HOPACH"
  if (name == "H2OKMEANS") clusterer <- if (fn) c_KMEANS else "c_H2OKMEANS"
  if (name == "KMEANS") clusterer <- if (fn) c_KMEANS else "c_KMEANS"
  if (name == "MEANSHIFT") clusterer <- if (fn) c_MEANSHIFT else "c_MEANSHIFT"
  if (name == "NGAS") clusterer <- if (fn) c_NGAS else "c_NGAS"
  if (name == "PAM") clusterer <- if (fn) c_PAM else "c_PAM"
  if (name == "PAMK") clusterer <- if (fn) c_PAMK else "c_PAMK"
  if (name == "SPEC") clusterer <- if (fn) c_SPEC else "c_SPEC"

  clusterer

} # rtemis::clustSelect
