# labels2niftis.R
# ::rtemis::
# 2015 Efstathios D. Gennatas egenn.lambdamd.org

#' Write Matrix to Multiple Nifti Files
#'
#' Writes matrix /data frame to nifti files columnwise.
#' Each column of the matrix should correspond to the labels of a nifti file.
#' i.e. nrow of matrix = n of labels in labeledNifti
#' Niftis are written in parallel using \code{parallel::parApply}
#'
#' @param datamat Matrix / Data Frame: Input
#' @param labeledNifti Character: Path to labeled nifti file
#' @param prefix Character: File output prefix
#' @param verbose Logical: If TRUE, print messages to output
#' @param n.cores Integer: Number of cores to use
#'
#' @author Efstathios D. Gennatas
#' @export

labels2niftis <- function(datamat,
                          labeledNifti,
                          prefix,
                          verbose = TRUE,
                          n.cores = parallel::detectCores()) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("oro.nifti", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ {GRID FN} ] ====
  s.datacol2nii.g1 <- function(datamat, fnim, prefix, verbose) {
    index <- datamat[1]
    values <- datamat[2:length(datamat)]
    levels(fnim) <- c(0, values)
    k <- length(unique(values))
    # to convert from factor (levels) to numeric, must convert to character first
    fnim <- as.numeric(as.character(fnim))
    fnim.array <- array(fnim, dim = c(dim[1], dim[2], dim[3]))
    nim <- oro.nifti::nifti(fnim.array)
    outname <- paste0(prefix, "_", index, '_Clusters', k)
    oro.nifti::writeNIfTI(nim, outname)
    if (verbose) msg("+ + + Wrote nifti", outname)
  }

  # [ MAIN ] ====
  scriptVersion <- 0.2
  if (verbose) msg(date(), "\nlabels2niftis version ", scriptVersion, "\nHello, ",
                   Sys.getenv('USER'), ".\n", sep = "")
  labelednim <- oro.nifti::readNIfTI(labeledNifti)
  dim <- dim(labelednim)
  # labels <- labelednim[]
  # nim <- list()
  if (verbose) msg("Working on", NCOL(datamat), "files")
  nim <- labelednim
  # replace labels with cluster numbers using factor levels ----
  # create factor with levels = the labels
  fnim <- factor(labelednim[])
  # fastest assignment of weights to labels, ever
  # explanation: levels gives an ordered list of the factor levels;
  # as long as your data(results) is saved in the same order, this works
  # i.e. your nifti labels say go from 1:10
  # and assuming your results are saved in that order,
  # then by using the assignment levels(factor) <- results
  # replaces each occurence of label with its corresponding result
  if (verbose) msg("Starting cluster...")
  cluster <- makeCluster(n.cores)
  on.exit(if (!is.null(cluster)) { stopCluster(cluster) })
  # index <- as.list(1:ncol(datamat))
  # parallel::parLapply(cl = cluster, index, s.datamat2nii.g1,
  #                     datamat = datamat, fnim = fnim, prefix = prefix)
  datamat <- rbind(seq(NCOL(datamat)), datamat)
  parApply(cl = cluster, datamat, 2, s.datacol2nii.g1,
           fnim = fnim, prefix = prefix, verbose = verbose)
  # stopCluster(cluster)
  cluster <- NULL
  if (verbose) msg("labels2niftis cluster stopped.\n")
  if (verbose) msg(date(), "::: labels2niftis", scriptVersion, "completed.\n")

} # rtemis::labels2niftis
