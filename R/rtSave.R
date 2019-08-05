# rtSave.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Write \pkg{rtemis} model to RDS file
#'
#' @param rtmod \pkg{rtemis} model
#' @param outdir Path to output directory
#' @param file.prefix Character: Prefix for filename
#' @param verbose Logical: If TRUE, print messages to output
#' @author Efstathios D. Gennatas
#' @export

rtSave <- function(rtmod,
                   outdir,
                   file.prefix = "s.",
                   verbose = TRUE) {
  
  if (verbose) cat("Writing data to", outdir, "...")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  rdsPath <- paste0(outdir, file.prefix, rtmod$mod.name, ".rds")
  try(saveRDS(rtmod, rdsPath))
  if (file.exists(rdsPath)) {
    if (verbose) cat(green$bold("Done"))
    # if (verbose) rtOut("Saved", rdsPath)
  } else {
    if (verbose) cat(red$bold("Failed"))
    warning("Could not save data to ", outdir)
  }
  
} # rtemis::rtSave
