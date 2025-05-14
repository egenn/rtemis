# rtemis RStudio theme
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Apply rtemis theme for RStudio
#'
#' Apply the rtemis RStudio theme, an adaptation of the rscodeio theme
#' (https://github.com/anthonynorth/rscodeio)
#' Recommended to use the Fira Code font with the theme
#' (https://fonts.google.com/specimen/Fira+Code?query=fira+code)
#'
#' @param theme Character: "dark" or "light"
#'
#' @author E.D. Gennatas
#' @export

rstudio_theme_rtemis <- function(theme = "dark") {
  .theme <- if (theme == "dark") "rtemis" else "rtemis-light"
  # '- Check we are in RStudio ----
  if (!rstudioapi::isAvailable()) {
    stop("Please run this function from within RStudio")
  }

  # '- Check RStudio supports themes ----
  if (
    compareVersion(as.character(rstudioapi::versionInfo()$version), "1.2.0") ==
      -1
  ) {
    stop("Please update to the latest RStudio version (or at least 1.2.0)")
  }

  # '- Remove theme if already present ----
  if (.theme %in% names(rstudioapi::getThemes())) {
    rstudioapi::removeTheme(.theme)
  }

  # '- Add theme ----
  msg2("Adding rtemis theme...")
  .theme <- rstudioapi::addTheme(
    system.file(
      fs::path("resources", paste0(.theme, ".rstheme")),
      package = "rtemis"
    )
  )

  # '- Activate theme ----
  rstudioapi::applyTheme(.theme)
  msg2(
    "You may need to restart RStudio for theme changes to take effect in the Options menu"
  )
} # rtemis::rstudio_theme_rtemis
