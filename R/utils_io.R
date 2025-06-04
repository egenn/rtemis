# rt_io.R
# ::rtemis::
# 2022 EDG rtemis.org

#' Write \pkg{rtemis} model to RDS file
#'
#' @param object Supervised object.
#' @param outdir Path to output directory.
#' @param file_prefix Character: Prefix for filename.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @keywords internal
#' @noRd

rt_save <- function(object, outdir, file_prefix = "s_", verbosity = 1L) {
  outdir <- normalizePath(outdir, mustWork = FALSE)
  if (verbosity > 0L) {
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
  rdsPath <- file.path(outdir, paste0(file_prefix, object@algorithm, ".rds"))
  try(saveRDS(object, rdsPath))
  if (verbosity > 0L) elapsed <- Sys.time() - start_time
  if (file.exists(rdsPath)) {
    if (verbosity > 0L) {
      yay("[", format(elapsed, digits = 2), "]", gray(" [rt_save]"), sep = "")
      msg20(italic(
        "Reload with:",
        "> object <- readRDS('",
        rdsPath,
        "')",
        sep = ""
      ))
    }
  } else {
    if (verbosity > 0L) {
      nay(
        "[Failed after ",
        format(elapsed, digits = 2),
        "]",
        gray(" [rt_save]"),
        sep = ""
      )
    }
    stop("Error: Saving model to ", outdir, " failed.")
  }
} # rtemis::rt_save

#' Check file(s) exist
#'
#' @param paths Character vector of paths
#' @param verbosity Integer: Verbosity level.
#' @param pad Integer: Number of spaces to pad to the left
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_files <- function(paths, verbosity = 1L, pad = 0) {
  if (verbosity > 0L) msg20("Checking ", singorplu(length(paths), "file"), ":")

  for (f in paths) {
    if (file.exists(f)) {
      if (verbosity > 0L) {
        yay(f, pad = pad)
      }
    } else {
      if (verbosity > 0L) {
        nay(paste(f, red(" not found!")), pad = pad)
      }
      stop("File not found")
    }
  }
} # rtemis::check_files


#' Write list elements to CSV files
#'
#' @param x List containing R objects to be written to CSV (e.g. data.frames,
#' matrices, etc.)
#' @param outdir Character: Path to output directory
#'
#' @return Nothing, writes CSV files to `outdir`.
#'
#' @author EDG
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
} # /rtemis::list2csv
