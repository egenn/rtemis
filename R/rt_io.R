# rt_io.R
# ::rtemis::
# 2022 EDG rtemis.org

#' Write \pkg{rtemis} model to RDS file
#'
#' @param rtmod \pkg{rtemis} model
#' @param outdir Path to output directory
#' @param file.prefix Character: Prefix for filename
#' @param verbose Logical: If TRUE, print messages to output
#' @author E.D. Gennatas
#' @export

rt_save <- function(rtmod, outdir, file.prefix = "s_", verbose = TRUE) {
  outdir <- normalizePath(outdir, mustWork = FALSE)
  if (verbose) {
    start_time <- Sys.time()
    msg2(
      "Writing data to ",
      outdir,
      "... ",
      sep = "",
      caller = NA,
      newline = FALSE
    )
  }
  if (!dir.exists(outdir))
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  rdsPath <- file.path(outdir, paste0(file.prefix, rtmod$mod.name, ".rds"))
  try(saveRDS(rtmod, rdsPath))
  if (verbose) elapsed <- Sys.time() - start_time
  if (file.exists(rdsPath)) {
    if (verbose) {
      yay("[", format(elapsed, digits = 2), "]", gray(" [rt_save]"), sep = "")
      msg20(italic(
        "Reload with:",
        "> rtmod <- readRDS('",
        rdsPath,
        "')",
        sep = ""
      ))
    }
  } else {
    if (verbose) {
      nay(
        "[Failed after ",
        format(elapsed, digits = 2),
        "]",
        gray(" [rt_save]"),
        sep = ""
      )
    }
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

check_files <- function(paths, verbose = TRUE, pad = 0) {
  if (verbose) msg20("Checking ", singorplu(length(paths), "file"), ":")

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
