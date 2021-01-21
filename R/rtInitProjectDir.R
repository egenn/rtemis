# rtInitProjectDir.R
# ::rtemis::
# 2019 E.D. Gennatas lambdamd.org

#' Initialize Project Directory
#'
#' Initializes Directory Structure: "R", "Data", "Results"
#'
#' @author E.D. Gennatas
#' @export

rtInitProjectDir <- function(verbose = TRUE) {

  wd <- getwd()
  if (verbose) msg0(rtHighlight$bold("rtemis"), ": Initializing project directory...")
  if (verbose) cat("   Working in ", wd, "...\n", sep = "")

  # rtInit.log ====
  # if (verbose) cat(rtHighlight("  Writing 'rtInit.log' file..."))
  sink("rtInit.log", append = TRUE)
  cat(".:rtemis Project Directory\n")
  cat(date(), "\n")
  cat("--------------------------\n")
  print(sessionInfo())
  sink()

  # ./R ./Data ./Results ====
  dirs <- c("R", "Data", "Results")
  for (i in dirs) {
    if (verbose) cat("  Creating '", i,"' folder...", sep = "")
    if (!dir.exists(i)) {
      dir.create(i)
      if (dir.exists(i)) {
        if (verbose) cat(crayon::green$bold(" Done\n"))
      } else {
        if (verbose) cat(crayon::red$bold("Failed"))
      }
    } else {
      if (verbose) cat(rtOrange$bold(" Already present\n"))
    }
  }

  if (verbose) cat(rtHighlight$bold("  All done\n"))

} # rtemis::rtInitProjectDir
