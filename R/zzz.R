# zzz.R
# ::rtemis::
# 2016-22 E.D. Gennatas www.lambdamd.org

rtenv <- new.env()
rtenv$binclasspos <- 1
rtemis.version <- packageVersion("rtemis")
.availableCores <- future::availableCores()

# # Set initial plan e.g. for s_ with gridSearchLearn,
# # **will be overwritten by resLearn for nested plan as appropriate**
# rtPlan <- getOption(
#     "future.plan",
#     ifelse(.Platform$OS.type == "unix", "multicore", "multisession")
# )
# # future::plan(rtPlan)
# # rtProgress <- getOption("rt.progress", "global")
# # if (rtProgress == "global") progressr::handlers(global = TRUE)
# # rtGSL <- getOption("rt.gsl", "future")
# # if (rtGSL == "future") gridSearchLearn <- gridSearchLearn

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
    # gridSearchLearn <- if (rtGridSearchLearnInit() == "future") {
    #     gridSearchLearn
    # } else {
    #     gridSearchLearn_pbapply
    # }
    # assign("gridSearchLearn", gridSearchLearn, envir = parent.env(environment()))
}

# .onAttach <- function(libname, pkgname) {
    
#     packageStartupMessage(paste0(
#         rtasciitxt(),
#         "  .:", pkgname, " ", rtemis.version, " \U1F30A", " ", sessionInfo()[[2]],
#         # "\n  Welcome, ", Sys.getenv("USER"),
#         bold("\n  Defaults"),
#         "\n  │   Theme: ", rtTheme,
#         "\n  │    Font: ", rtFont,
#         "\n  │ Palette: ", rtPalette,
#         "\n  │    Plan: ", rtPlan,
#         "\n  └   Cores: ", rtCores, "/", .availableCores, " available",
#         bold("\n  Resources"),
#         "\n  │    Docs: https://rtemis.lambdamd.org",
#         "\n  │ Learn R: https://class.lambdamd.org/pdsr",
#         "\n  │  Themes: https://egenn.lambdamd.org/software/#rtemis_themes",
#         '\n  └    Cite: `citation("rtemis")`',
#         bold("\n  Setup"),
#         "\n  └ Enable progress reporting: `progressr::handlers(global = TRUE)`"
#     ))
# }

.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage(paste0(
            rtasciitxt(),
            "  .:", pkgname, " ", rtemis.version, " \U1F30A", " ", sessionInfo()[[2]],
            # "\n  Welcome, ", Sys.getenv("USER"),
            bold("\n  Defaults"),
            "\n  │   ", italic(gray("Theme: ")), rtTheme,
            "\n  │    ", italic(gray("Font: ")), rtFont,
            "\n  │ ", italic(gray("Palette: ")), rtPalette,
            "\n  │    ", italic(gray("Plan: ")), rtPlan,
            "\n  └   ", italic(gray("Cores: ")), rtCores, "/", .availableCores, " available",
            bold("\n  Resources"),
            "\n  │    ", italic(gray("Docs:")), " https://rtemis.lambdamd.org",
            "\n  │ ", italic(gray("Learn R:")), " https://class.lambdamd.org/pdsr",
            "\n  │  ", italic(gray("Themes:")), " https://egenn.lambdamd.org/software/#rtemis_themes",
            "\n  └    ", italic(gray("Cite:")), ' `citation("rtemis")`',
            bold("\n  Setup"),
            # "\n  └ ", italic(gray("Enable progress reporting:")), " `progressr::handlers(global = TRUE)`",
            # "\n                               `options(progressr.handlers = progressr::handler_cli)`"
            "\n  └ ", italic(gray("Enable progress reporting:")), 
            " progressr::handlers(global = TRUE)",
            "\n                              options(progressr.handlers = progressr::handler_cli)"
        ))
    } else {
        packageStartupMessage(
            paste0(
                "  .:", pkgname, " ", rtemis.version, " \U1F30A", " ", sessionInfo()[[2]]
            )
        )
    }
    
}


#' \pkg{rtemis}: Machine Learning and Visualization
#'
#' Advanced Machine Learning made easy, efficient, reproducible
#'
#' @section Online Documentation and Vignettes:
#' <https://rtemis.lambdamd.org>
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
#' Run [modSelect] to get a list of available algorithms
#' The documentation of each supervised learning function indicates in 
#' brackets, after the title whether the function supports classification, 
#' regression, and survival analysis `[C, R, S]`
#' @section Clustering:
#' Functions for Clustering begin with `c_*`.
#' Run [clustSelect] to get a list of available algorithms
#' @section Decomposition:
#' Functions for Decomposition and Dimensionality reduction begin with 
#' `d_*`.
#' Run [decomSelect] to get a list of available algorithms
#' @section Cross-Decomposition:
#' Functions for Cross-Decomposition begin with `x_*`.
#' Run [xdecomSelect] to get a list of available algorithms
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
#' @useDynLib rtemis, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @docType package
#' @name rtemis-package
#' @import graphics grDevices methods stats utils data.table R6 future htmltools

NULL
