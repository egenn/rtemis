# zzz.R
# ::rtemis::
# 2016-24 E.D. Gennatas rtemis.org

rtenv <- new.env()
rtenv$binclasspos <- 1
rtemisalpha_version <- packageVersion("rtemisalpha")
.availableCores <- future::availableCores()

.onLoad <- function(libname, pkgname) {
  # Defaults ----
  rtPlan <- rtPlanInit()
  assign("rtPlan", rtPlan, envir = parent.env(environment()))
  rtCores <- rtCoresInit()
  assign("rtCores", rtCores, envir = parent.env(environment()))
  rtTheme <- rtThemeInit()
  assign("rtTheme", rtTheme, envir = parent.env(environment()))
  rtFont <- rtFontInit()
  assign("rtFont", rtFont, envir = parent.env(environment()))
  rtPalette <- rtPaletteInit()
  assign("rtPalette", rtPalette, envir = parent.env(environment()))
  rtDate <- rtDateInit() == "TRUE"
  assign("rtDate", rtDate, envir = parent.env(environment()))
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(paste0(
      rtasciitxt(),
      "  .:",
      pkgname,
      " ",
      rtemisalpha_version,
      " \U1F30A",
      " ",
      sessionInfo()[[2]],
      bold("\n  Defaults"),
      "\n  \u2502   ",
      italic(gray("Theme: ")),
      rtTheme,
      "\n  \u2502    ",
      italic(gray("Font: ")),
      rtFont,
      "\n  \u2502 ",
      italic(gray("Palette: ")),
      rtPalette,
      "\n  \u2502    ",
      italic(gray("Plan: ")),
      rtPlan,
      "\n  \u2514   ",
      italic(gray("Cores: ")),
      rtCores,
      "/",
      .availableCores,
      " available",
      bold("\n  Resources"),
      "\n  \u2502    ",
      italic(gray("Docs:")),
      " https://rtemis.org/rtemis",
      "\n  \u2502 ",
      italic(gray("Learn R:")),
      " https://rtemis.org/pdsr",
      "\n  \u2502  ",
      italic(gray("Themes:")),
      " https://rtemis.org/themes",
      "\n  \u2514    ",
      italic(gray("Cite:")),
      ' > citation("rtemisalpha")',
      bold("\n  Setup"),
      "\n  \u2514 ",
      italic(gray("Enable progress reporting:")),
      " > progressr::handlers(global = TRUE)",
      '\n                               > progressr::handlers("cli")',
      "\n\n  ",
      italic(bold(red(
        "PSA: Do not throw data at algorithms. Compute responsibly!"
      )))
    ))
  } else {
    packageStartupMessage(
      paste0(
        "  .:",
        pkgname,
        " ",
        rtemisalpha_version,
        " \U1F30A",
        " ",
        sessionInfo()[[2]]
      )
    )
  }
}


#' \pkg{rtemis}: Machine Learning and Visualization
#'
#' @description
#' Advanced Machine Learning made easy, efficient, reproducible
#'
#' @section Online Documentation and Vignettes:
#' <https://www.rtemis.org>
#' @section System Setup:
#' There are some options you can define in your .Rprofile (usually found in your home directory),
#' so you do not have to define each time you execute a function.
#' \describe{
#'     \item{rt.theme}{General plotting theme; set to e.g. "whiteigrid" or "darkgraygrid"}
#'     \item{rt.palette}{Name of default palette to use in plots. See options by running `rtpalette()`}
#'     \item{rt.font}{Font family to use in plots.}
#'     \item{rt.cores}{Number of cores to use. By default, rtemis will use available cores reported by
#'     future::availableCores(). In shared systems, you should limit this as appropriate.}
#'     \item{future.plan}{Default plan to use for parallel processing.}
#' }
#' @section Visualization:
#' Static graphics are handled using the `mplot3` family.
#' Dynamic graphics are handled using the `dplot3` family.
#' @section Supervised Learning:
#' Functions for Regression and Classification begin with `s_*`.
#' Run [select_learn] to get a list of available algorithms
#' The documentation of each supervised learning function indicates in
#' brackets, after the title whether the function supports classification,
#' regression, and survival analysis `[C, R, S]`
#' @section Clustering:
#' Functions for Clustering begin with `c_*`.
#' Run [select_clust] to get a list of available algorithms
#' @section Decomposition:
#' Functions for Decomposition and Dimensionality reduction begin with
#' `d_*`.
#' Run [select_decom] to get a list of available algorithms
#' @section Cross-Decomposition:
#' Functions for Cross-Decomposition begin with `x_*`.
#' Run [xselect_decom] to get a list of available algorithms
#' @section Meta-Modeling:
#' Meta models are trained using `meta*` functions.
#'
#' @section Notes:
#' Function documentation includes input type (e.g. "String", "Integer",
#' "Float"/"Numeric", etc) and
#' range in interval notation where applicable. For example, Float: [0, 1)"
#' means floats between 0 and 1 including 0, but excluding 1
#'
#' For all classification models, the outcome should be provided as a factor,
#' with the first level of the factor being the 'positive' class, if
#' applicable. A character vector supplied as outcome will be converted to
#' factors, where by default the levels are set alphabetically and therefore
#' the positive class may not be set correctly.
#'
# @useDynLib rtemisalpha, .registration = TRUE
# @importFrom Rcpp evalCpp
#' @name rtemisalpha-package
#' @import graphics grDevices methods stats utils data.table R6 future htmltools
"_PACKAGE"

NULL
