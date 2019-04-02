# clustSelect.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Select \pkg{rtemis} Clusterer
#'
#' Accepts clusterer name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param clust String: Clustering algorithm name. Case insensitive, supports partial matching. e.g. "hop" for HOPACH
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name of algorithm \code{clust}
#' @return Name of function (Default) or function (\code{fn=TRUE}) or full name of algorithm (\code{desc=TRUE})
#' @author Efstathios D. Gennatas
#' @export

clustSelect <- function(clust,
                        fn = FALSE,
                        desc = FALSE) {

  description <- list(
    "CMEANS" = "Fuzzy C-means Clustering",
    "EMC" = "Expectation Maximization Clustering",
    "HARDCL" = "Hard Competitive Learning",
    "HOPACH" = "Hierarchical Ordered Partitioning And Collapsing Hybrid",
    "H2OKMEANS" = "H2O K-Means Clustering",
    "KMEANS" = "K-Means Clustering",
    "NGAS" = "Neural Gas Clustering",
    "PAM" = "Partitioning Around Medoids",
    "PAMK" = "Partitioning Around Medoids with k estimation",
    "SPEC" = "Spectral Clustering"
  )
  description <- t(as.data.frame(description))
  description <- data.frame(Name = rownames(description), Description = description)

  if (missing(clust)) {
    cat(".:clustSelect\nrtemis supports the following clustering algorithms:\n\n")
    print(description, quote = F, row.names = F)
    return(invisible(9))
  }

  name.vec <- toupper(c("cmeans", "emc", "hardcl", "hopach", "h2okmeans",
                        "kmeans", "ngas", "pam", "pamk", "spec"))
  name <- name.vec[pmatch(toupper(clust), name.vec)]
  if (is.na(name)) {
    print(description, quote = F)
    stop("Incorrect clusterer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  if (name == "CMEANS") clusterer <- if (fn) u.CMEANS else "u.CMEANS"
  if (name == "EMC") clusterer <- if (fn) u.EMC else "u.EMC"
  if (name == "HARDCL") clusterer <- if (fn) u.HARDCL else "u.HARDCL"
  if (name == "HOPACH") clusterer <- if (fn) u.HOPACH else "u.HOPACH"
  if (name == "H2OKMEANS") clusterer <- if (fn) u.KMEANS else "u.H2OKMEANS"
  if (name == "KMEANS") clusterer <- if (fn) u.KMEANS else "u.KMEANS"
  if (name == "NGAS") clusterer <- if (fn) u.NGAS else "u.NGAS"
  if (name == "PAM") clusterer <- if (fn) u.PAM else "u.PAM"
  if (name == "PAMK") clusterer <- if (fn) u.PAMK else "u.PAMK"
  if (name == "SPEC") clusterer <- if (fn) u.SPEC else "u.SPEC"

  clusterer

} # rtemis::clustSelect
