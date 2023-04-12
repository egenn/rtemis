# labels2niftis.R
# ::rtemis::
# 2015 E.D. Gennatas www.lambdamd.org

#' Write Matrix to Multiple Nifti Files
#'
#' Writes matrix /data frame to nifti files columnwise.
#' Each column of the matrix should correspond to the labels of a nifti file.
#' i.e. nrow of matrix = n of labels in labeledNifti
#' Niftis are written in parallel using `parallel::parApply`
#'
#' @param datamat Matrix / Data Frame: Input
#' @param labeledNifti Character: Path to labeled nifti file
#' @param prefix Character: File output prefix
#' @param verbose Logical: If TRUE, print messages to output
#' @param n.cores Integer: Number of cores to use
#'
#' @author E.D. Gennatas
#' @export

labels2niftis <- function(datamat,
                          labeledNifti,
                          prefix,
                          verbose = TRUE,
                          n.cores = future::availableCores()) {

  # Dependencies ----
  dependency_check("oro.nifti")

  # {Grid fn} ----
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
    if (verbose) msg2("+ + + Wrote nifti", outname)
  }

  # Main ----
  scriptVersion <- 0.2
  if (verbose) msg2(date(), "\nlabels2niftis version ", scriptVersion, "\nHello, ",
                   Sys.getenv('USER'), ".\n", sep = "")
  labelednim <- oro.nifti::readNIfTI(labeledNifti)
  dim <- dim(labelednim)
  if (verbose) msg2("Working on", NCOL(datamat), "files")
  nim <- labelednim
  # replace labels with cluster numbers using factor levels ----
  # create factor with levels = the labels
  fnim <- factor(labelednim[])
  # fast assignment of weights to labels:
  # levels() gives an ordered list of the factor levels;
  # as long as your data(results) is saved in the same order, this works
  # i.e. your nifti labels say go from 1:10
  # and assuming your results are saved in that order,
  # then by using the assignment levels(factor) <- results
  # replaces each occurence of label with its corresponding result
  if (verbose) msg2("Starting cluster...")
  cluster <- makeCluster(n.cores)
  on.exit(if (!is.null(cluster)) { stopCluster(cluster) })
  datamat <- rbind(seq(NCOL(datamat)), datamat)
  parApply(cl = cluster, datamat, 2, s.datacol2nii.g1,
           fnim = fnim, prefix = prefix, verbose = verbose)
  cluster <- NULL
  if (verbose) msg2("labels2niftis cluster stopped.\n")
  if (verbose) msg2(date(), "::: labels2niftis", scriptVersion, "completed.\n")

} # rtemis::labels2niftis
