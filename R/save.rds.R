# save.rds.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Write data to RDS file
#'
#' @param object \code{R} object
#' @param outdir Path to output directory
#' @param verbose Logical
#' @author Efstathios D. Gennatas
#' @export

save.rds <- function(object, outdir, verbose = TRUE) {

  if (verbose) cat("Writing data to", outdir, "...")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  rdsPath <- paste0(outdir, "s.", object$mod.name, ".rds")

  saveRDS(object, rdsPath)
  if (file.exists(rdsPath)) {
    if (verbose) cat(" Done")
    } else {
      warning("Could not save data to ", outdir)
    }

} # rtemis::save.rds
