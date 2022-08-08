# zzz.R
# ::rtemis::
# 2016-22 E.D. Gennatas www.lambdamd.org

rtenv <- new.env()
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
# # if (rtGSL == "future") gridSearchLearn <- gridSearchLearn_future

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
}

.onAttach <- function(libname, pkgname) {
    
    packageStartupMessage(paste0(
        "  .:", pkgname, " ", rtemis.version, " \U1F30A", " ", sessionInfo()[[2]],
        # "\n  Welcome, ", Sys.getenv("USER"),
        "\n  Defaults",
        "\n  |   Theme: ", rtTheme,
        "\n  |    Font: ", rtFont,
        "\n  | Palette: ", rtPalette,
        "\n  |    Plan: ", rtPlan,
        "\n  |   Cores: ", rtCores, "/", .availableCores, " available",
        "\n  Resources",
        "\n  | Documentation: https://rtemis.lambdamd.org",
        "\n  |       Learn R: https://class.lambdamd.org/pdsr",
        "\n  | rtemis themes: https://egenn.lambdamd.org/software/#rtemis_themes",
        '\n  |          Cite: `citation("rtemis")`',
        "\n  Setup",
        "\n  | Enable progress reporting: `progressr::handlers(global = TRUE)`",
        "\n"
    ))

    # packageStartupMessage(
    #   "                                         d8,           ",
    #   "\n             d8P                        `8P           |",
    #   "\n          d888888P                                    |",
    #   "\n   88bd88b  ?88'   d8888b  88bd8b,d88b   8 8b .d888b, |",
    #   "\n   88P'  `  88P   d8b_,dP  88P'`?8P'?8b  88P ?8b,     |",
    #   "\n  d88       88b   88b     d88  d88  88P d88    `?8b   |",
    #   "\n d88'       `?8b  `?888P'd88' d88'  88bd88' `?888P'   |",
    # )

    try(
        if (interactive() && try(rstudioapi::isAvailable(), silent = TRUE)) {
            col <- sample(unlist(rtCol1), 1)
            n <- 20
            polyshadow(n, n, .8,
                text = paste0("  .:", pkgname, " ", 
                              rtemis.version, " ", sessionInfo()[[2]]),
                text.x = 2,
                text.y = 2,
                text.adj = c(0, 0),
                text.col = col,
                text.cex = 1,
                shadow = seq(.9, .96, length = n^2),
                col_lo = "#00000040",
                col_hi = col,
                color.progression = "prod"
            )
        },
        silent = TRUE
    )

}


#' \pkg{rtemis}: Machine Learning and Visualization
#'
#' Advanced Machine Learning made easy, efficient, reproducible
#'
#' @section Online Documentation and Vignettes:
#' \url{https://rtemis.lambdamd.org}
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
#' Static graphics are handled using the \code{mplot3} family.
#' Dynamic graphics are handled using the \code{dplot3} family.
#' @section Supervised Learning:
#' Functions for Regression and Classification begin with \code{s_*}.
#' Run \link{modSelect} to get a list of available algorithms
#' The documentation of each supervised learning function indicates in 
#' brackets, after the title whether the function supports classification, 
#' regression, and survival analysis \code{[C, R, S]}
#' @section Clustering:
#' Functions for Clustering begin with \code{c_*}.
#' Run \link{clustSelect} to get a list of available algorithms
#' @section Decomposition:
#' Functions for Decomposition and Dimensionality reduction begin with 
#' \code{d_*}.
#' Run \link{decomSelect} to get a list of available algorithms
#' @section Cross-Decomposition:
#' Functions for Cross-Decomposition begin with \code{x_*}.
#' Run \link{xdecomSelect} to get a list of available algorithms
#' @section Meta-Modeling:
#' Meta models are trained using \code{meta*} functions.
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
#' @docType package
#' @name rtemis-package
#' @import graphics grDevices methods stats utils data.table R6 future htmltools crayon
#' @importFrom crayon "%+%"

NULL
