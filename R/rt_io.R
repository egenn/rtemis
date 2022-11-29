# rt_io.R
# ::rtemis::
# 2022 EDG lambdamd.org

#' Write \pkg{rtemis} model to RDS file
#'
#' @param rtmod \pkg{rtemis} model
#' @param outdir Path to output directory
#' @param file.prefix Character: Prefix for filename
#' @param verbose Logical: If TRUE, print messages to output
#' @author E.D. Gennatas
#' @export

rt_save <- function(rtmod,
                    outdir,
                    file.prefix = "s.",
                    verbose = TRUE) {
    if (verbose) cat("Writing data to", outdir, "...")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    rdsPath <- paste0(outdir, file.prefix, rtmod$mod.name, ".rds")
    try(saveRDS(rtmod, rdsPath))
    if (file.exists(rdsPath)) {
        if (verbose) cat(green("Done", TRUE))
        # if (verbose) rtOut("Saved", rdsPath)
    } else {
        if (verbose) cat(red("Failed", TRUE))
        warning("Could not save data to ", outdir)
    }
} # rtemis::rt_save

#' Check file(s) exist
#'
#' @param paths Character vector of paths
#' @param verbose Logical: If TRUE, print messages to console
#' @param pad Integer: Number of spaces to pad to the left
#'
#' @author E.D. Gennatas
#' @export

check_files <- function(paths,
                        verbose = TRUE,
                        pad = 0) {
    if (verbose) msg2("Checking files:")

    for (f in paths) {
        if (file.exists(f)) {
            if (verbose) {
                yay(f, pad = pad)
            }
        } else {
            if (verbose) {
                nay(paste(f, red(" not found!")), pad = pad)
            }
            stop("File not found")
        }
    }
} # rtemis::check_files