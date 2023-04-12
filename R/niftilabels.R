#' Read nifti segmentation label file
#'
#' Read nifti label file, a space/tab separated, headerless text file
#' containing the following fields:
#' LabelID LabelName R G B A
#'
#' @param x Character: Path to nifti label file
#'
#' @author E.D. Gennatas
#' @export

readseglabels <- function(x) {

  .labels <- read.table(x)
  colnames(.labels) <- c("LabelID", "LabelName", "R", "G", "B", "A")
  .labels

}

#' Convert nifti segmentation label file to itksnap-compatible label file
#'
#' @param x Character: path to nifti label file (read with [readseglabels])
#' @param visible Integer, vector, length = number of labels in `x`: 1: visible; 0: invisible.
#' Normally, the background label is set to invisible
#' @param mesh Same as `visible` but for mesh renderings
#' @param filename Character: Path to filename to write to
#'
#' @author E.D. Gennatas
#' @export

seglabels2itksnap <- function(x,
                              visible = NULL,
                              mesh = NULL,
                              filename = NULL,
                              verbose = TRUE) {

  .in <- readseglabels(x)
  if (is.null(visible)) visible <- c(0, rep(1, NROW(.in) - 1))
  if (is.null(mesh)) mesh <- c(0, rep(1, NROW(.in) - 1))
  .out <- data.frame(IDX = .in$LabelID,
                     R = .in$R,
                     G = .in$G,
                     B = .in$B,
                     A = .in$A/255,
                     VIS = visible,
                     MSH = mesh,
                     LABEL = .in$LabelName)
  if (!is.null(filename)) {
    writeLines(paste("#", paste(colnames(.out), collapse = " ")), file(filename))
    write.table(.out, file = filename, append = T,
                quote = T, row.names = FALSE, col.names = FALSE)
    if (verbose) {
      if (file.exists(filename)) {
        msg2("Wrote", filename)
      } else {
        warning("Failed to write", filename)
      }
    }
  }
  invisible(.out)

}
