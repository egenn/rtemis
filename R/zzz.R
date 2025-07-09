#             d8P
#          d888888P
#   88bd88b  ?88'   d8888b  88bd8b,d88b   88b .d888b,
#   88P'  `  88P   d8b_,dP  88P'`?8P'?8b  88P ?8b,
#  d88       88b   88b     d88  d88  88P d88    `?8b
# d88'       `?8b  `?888P'd88' d88'  88bd88' `?888P'
#
# zzz.R
# ::rtemis::
# 2016- EDG rtemis.org

# rtemis internal environment
live <- new.env()
live[["parallelized_learners"]] <- c(
  "LightCART",
  "LightGBM",
  "LightRF",
  "LightRuleFit",
  "Ranger"
)

# vars
rtemis_version <- packageVersion("rtemis")
.availableCores <- unname(future::availableCores())

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
  # Defaults ----
  rtemis_plan <- getOption("future.plan", "multicore")
  assign("rtemis_plan", rtemis_plan, envir = parent.env(environment()))
  rtemis_workers <- getOption("rtemis_workers", .availableCores)
  assign("rtemis_workers", rtemis_workers, envir = parent.env(environment()))
  rtemis_theme <- getOption("rtemis_theme", "darkgraygrid")
  assign("rtemis_theme", rtemis_theme, envir = parent.env(environment()))
  rtemis_font <- getOption("rtemis_font", "Helvetica")
  assign("rtemis_font", rtemis_font, envir = parent.env(environment()))
  rtemis_palette <- getOption("rtemis_palette", "rtCol3")
  assign("rtemis_palette", rtemis_palette, envir = parent.env(environment()))
  rtemis_date <- getOption("rtemis_date", TRUE)
  assign("rtemis_date", rtemis_date, envir = parent.env(environment()))
  rtemis_plotfileformat <- getOption("rtemis_plotfileformat", "svg")
  assign(
    "rtemis_plotfileformat",
    rtemis_plotfileformat,
    envir = parent.env(environment())
  )
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(paste0(
      rtlogo,
      "\n  .:",
      bold(pkgname),
      " v.",
      rtemis_version,
      " \U1F30A",
      " ",
      sessionInfo()[[2]],
      bold("\n  Defaults"),
      "\n  \u2502   ",
      italic(gray("Theme: ")),
      rtemis_theme,
      "\n  \u2502    ",
      italic(gray("Font: ")),
      rtemis_font,
      "\n  \u2502 ",
      italic(gray("Palette: ")),
      rtemis_palette,
      "\n  \u2502    ",
      italic(gray("Plan: ")),
      rtemis_plan,
      # "\n  \u2514   ", italic(gray("Cores: ")), rtemis_workers, "/", .availableCores, " available",
      "\n  \u2514   ",
      italic(gray("Cores: ")),
      future::availableCores(),
      " cores available.",
      bold("\n  Resources"),
      "\n  \u2502    ",
      italic(gray("Docs:")),
      " https://rdocs.rtemis.org",
      "\n  \u2502 ",
      italic(gray("Learn R:")),
      " https://pdsr.rtemis.org",
      "\n  \u2502  ",
      italic(gray("Themes:")),
      " https://rtemis.org/themes",
      "\n  \u2514    ",
      italic(gray("Cite:")),
      ' > citation("rtemis")',
      bold("\n  Setup"),
      "\n  \u2514 ",
      italic(gray("Enable progress reporting:")),
      " > progressr::handlers(global = TRUE)",
      '\n                               > progressr::handlers("cli")',
      "\n\n  ",
      red(bold("PSA:"), "Do not throw data at algorithms. Compute responsibly!")
    ))
  } else {
    packageStartupMessage(
      paste0(
        "  .:",
        pkgname,
        " ",
        rtemis_version,
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
#' <https://rdocs.rtemis.org>
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
#' Graphics are handled using the `draw` family, which produces interactive plots using`plotly` and
#' other packages.
#' @section Supervised Learning:
#' Regression and Classification is performed using `train()`.
#' This function allows you to preprocess, train, tune, and test models on multiple resamples.
#' Run [available_supervised] to get a list of available algorithms
#' @section Clustering:
#' Clustering is performed using `cluster()`.
#' Run [available_clustering] to get a list of available algorithms.
#' @section Decomposition:
#' Decomposition is performed using `decomp()`.
#' Run [available_decomposition] to get a list of available algorithms.
#'
#' @section Notes:
#' Function documentation includes input type (e.g. "String", "Integer",
#' "Float"/"Numeric", etc) and
#' range in interval notation where applicable. For example, Float: [0, 1)"
#' means floats between 0 and 1 including 0, but excluding 1
#'
#' For all classification models, the outcome should be provided as a factor,
#' with the *second* level of the factor being the 'positive' class.
#'
#' @name rtemis-package
#' @import graphics grDevices methods stats utils S7 data.table htmltools cli
"_PACKAGE"

NULL
