# labels2nii.R
# ::rtemis::
# 2015 E.D. Gennatas www.lambdamd.org

#' Write Data to a Nifti File
#'
#' Writes ROI values to nifti
#'
#' @param label.vals Vector: The index of this vector should correspond to label numbers.
#'   Its values will replace the corresponding label numbers to form the new nifti file.
#'   This means that the values of this vector need to be in the same order as the labels.
#'   For example if `labelefNifti` has four labels: 0 (background), 10, 11, 20, then your
#'   `label.vals` should should have three values and they will be assigned to labels
#'   10, 11, and 20.
#' @param labeledNifti Character: Path to the labeled file whose labels corresponds to the values in
#'   `label.vals`
#' @param prefix Character: Prefix of the output file. ".nii.gz" will be added automatically
# @param reorient.label Logical: If TRUE, enforce Qform/Sform transformations. See \code{oro.nifti::readNIfTI}
#' @param datatype Integer or "auto": Defines the datatype of the output. Options:
#'   2: uint8, 4: int16, 8: int32, 16: float (Default), 64: double. Default = "auto"
#' @param verbose Logical: If TRUE, print messages to output
#' @author E.D. Gennatas
#' @export

labels2nii <- function(label.vals,
                       labeledNifti,
                       prefix,
                       datatype = "auto",
                       verbose = TRUE) {

  # Dependencies ----
  dependency_check("RNifti")

  # Arguments ----
  if (!file.exists(labeledNifti)) stop("Error: Labeled nifti file not found")
  outdir <- dirname(prefix)
  if (!dir.exists(outdir)) {
    if (verbose) msg2("Creating", outdir)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  }

  # Main ----
  labelednim <- RNifti::readNifti(labeledNifti)
  dim <- dim(labelednim)
  labels <- as.array(labelednim)
  # k <- length(unique(label.vals))
  k <- length(label.vals)
  kl <- length(unique(c(labels)))
  # Check N label.vals is equal to N labels in labeledNifti
  if (k != kl - 1) {
    stop("Supplied 'label.vals' is length", k, "but 'labeledNifti' contains", kl - 1,
         "labels (plus background")
  }
  if (verbose) msg2("Working on ", k, " labels...", sep = "")

  # replace labels with cluster numbers using factor levels
  # create factor with levels = the labels
  fnim <- factor(labels)
  # fast assignment of weights to labels
  # levels() gives an ordered list of the factor levels;
  # as long as your data(results) is saved in the same order, this works
  # i.e. your nifti labels say go from 1:10
  # and assuming your results are saved in that order,
  # then by using the assignment levels(factor) <- results
  # replaces each occurence of label with its corresponding result
  levels(fnim) <- c(0, label.vals)
  # to convert from factor (levels) to numeric, must convert to character first
  fnim <- as.numeric(as.character(fnim))
  fnim.array <- array(fnim, dim = c(dim[1], dim[2], dim[3]))
  outname <- paste0(prefix, "_label.vals", k, ".nii.gz")
  RNifti::writeNifti(fnim.array, outname, template = labelednim, datatype = datatype)
  if (verbose) msg2("Wrote labeled nifti", outname)

} # rtemis::labels2nii
