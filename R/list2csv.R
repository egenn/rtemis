# list2csv.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Write list elements to CSV files
#'
#' @param x List containing R objects to be written to CSV (e.g. data.frames,
#' matrices, etc.)
#' @param outdir Character: Path to output directory
#'
#' @author E.D. Gennatas
#' @export

list2csv <- function(x, outdir) {
  if (!inherits(x, "list")) {
    stop("Input must be a list")
  }

  xname <- deparse(substitute(x))
  outnames <- names(x)
  if (is.null(outnames)) {
    outnames <- paste(xname, seq_along(x), sep = "_")
  }
  outnames <- paste0(outnames, ".csv")
  for (i in seq_along(x)) {
    write.csv(x[[i]], file.path(outdir, outnames[i]), row.names = FALSE)
  }
} # rtemis::list2csv
