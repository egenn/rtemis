# rtemis RStudio theme
# ::rtemis::
# Efstathios D. Gennatas egenn.github.io

#' Apply rtemis theme for RStudio
#'
#' Apply the rtemis RStudio theme, a slight adaptation of the excellent rscodeio theme
#' (https://github.com/anthonynorth/rscodeio)
#' Recommended to use the Fira Code font with the theme
#' (https://fonts.google.com/specimen/Fira+Code?query=fira+code)
#' @export

rstudio_theme_rtemis <- function() {

  # '- Check we are in RStudio ====
  if (!rstudioapi::isAvailable()) {
    stop("Please run this function from within RStudio")
  }

  # '- Check RStudio supports themes ====
  if (compareVersion(as.character(rstudioapi::versionInfo()$version), "1.2.0") == -1) {
    stop("Please update to the latest RStudio version (or at least 1.2.0)")
  }

  # '- Remove theme if already present ====
  if ("rtemis" %in% names(rstudioapi::getThemes())) {
    rstudioapi::removeTheme("rtemis")
  }

  # '- Add theme ====
  msg("Adding rtemis theme to RStudio...")
  rstudioapi::addTheme(
    system.file(fs::path("resources","rtemis.rstheme"), package = "rtemis")
  )

  # '- Activate theme ====
  rstudioapi::applyTheme("rtemis")

} # rtemis::rstudio_theme_rtemis
