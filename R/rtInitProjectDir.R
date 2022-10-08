# rtInitProjectDir.R
# ::rtemis::
# 2019 E.D. Gennatas www.lambdamd.org

#' Initialize Project Directory
#'
#' Initializes Directory Structure: "R", "Data", "Results"
#'
#' @author E.D. Gennatas
#' @export

rtInitProjectDir <- function(verbose = TRUE) {

  wd <- getwd()
  if (verbose) msg("Initializing project directory...")
  if (verbose) cat("  Working in ", wd, "...\n", sep = "")

  # rtInit.log ----
  # if (verbose) cat(hilite("  Writing 'rtInit.log' file..."))
  sink("rtInit.log", append = TRUE)
  cat(".:rtemis Project Directory\n")
  cat(date(), "\n")
  cat("--------------------------\n")
  print(sessionInfo())
  sink()

  # ./R ./Data ./Results ----
  dirs <- c("R", "Data", "Results")
  for (i in dirs) {
    if (verbose) cat("  > Creating ", bold(i), " folder...", sep = "")
    if (!dir.exists(i)) {
      dir.create(i)
      if (dir.exists(i)) {
        if (verbose) cat(hilite(" Done\n"))
      } else {
        if (verbose) cat(red(" Failed", TRUE))
      }
    } else {
      if (verbose) cat(orange(" Already present\n", TRUE))
    }
  }

  if (verbose) cat(hilite("  All done\n"))

} # rtemis::rtInitProjectDir
