# zzz.R
# ::rtemis::
# 2015-8 Efstathios D. Gennatas egenn.github.io

#' \pkg{rtemis}: Machine Learning and Visualization
#'
#' @section System Setup:
#' There are some options you can define in your .Rprofile (usually found in your home directory),
#' so you do not have to define each time you execute a function.
#' \describe{
#'    \item{rt.theme}{General plotting theme; set to e.g. "light" or "dark"}
#'    \item{ft.fit.theme}{Plotting theme for true vs. fitted; set to e.g. "lightgrid" or "darkgrid"}
#'    \item{rtCores}{Number of cores to use. By default, rtemis will use available cores reported by
#'    future::availableCores(). In shared systems, you should limit this as appropriate.}
#' }
#' @section Visualization:
#' Static graphics are handled using the \code{mplot3} family.
#' Dynamic graphics are handled using the \code{dplot3} family.
#' @section Supervised Learning:
#' Functions for Regression and Classification begin with \code{s.*}.
#' Run \link{modSelect} to get a list of available algorithms
#' The documentation of each supervised learning function indicates in brackets, after the title
#' whether the function supports classification, regression, and survival analysis \code{[C, R, S]}
#' @section Clustering:
#' Functions for Clustering begin with \code{u.*}.
#' Run \link{clustSelect} to get a list of available algorithms
#' @section Decomposition:
#' Functions for Decomposition and Dimensionality reduction begin with \code{d.*}.
#' Run \link{decomSelect} to get a list of available algorithms
#' @section Cross-Decomposition:
#' Functions for Cross-Decomposition begin with \code{x.*}.
#' Run \link{xdecomSelect} to get a list of available algorithms
#' @section Meta-Modeling:
#' Meta models are trained using \code{meta*} functions.
#'
#' @section Notes:
#' Function documentation includes input type (e.g. "String", "Integer", "Float", etc) and
#' range in interval notation where applicable. For example, Float: [0, 1)" means floats between 0 and 1 including 0,
#' but excluding 1
#'
#' For all classification models, the outcome should be provided as a factor, with the first level
#' of the factor being the 'positive' class, if applicable. A character vector supplied as outcome
#' will be converted to factors, where by default the levels are set alphabetically and therefore
#' the positive class may not be set correctly.
#'
#' @docType package
#' @name rtemis-package
#' @import graphics grDevices methods parallel future stats utils data.table crayon
#' @importFrom magrittr "%>%"
#' @importFrom crayon "%+%"

.availableCores <- future::availableCores()
rtCores <- getOption("rt.cores", .availableCores)
globalVariables("rtCores")
as.Node.data.frame <- getFromNamespace("as.Node.data.frame", "data.tree")

.onAttach <- function(libname, pkgname) {
  rtemis.ver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                         fields = "Version")
  # packageStartupMessage(paste0("  .:", pkgname, " v", rtemis.ver, ": Welcome, ", Sys.getenv("USER"),
  #                              "\n  [", sessionInfo()[2], ": ", rtCores,
  #                              " cores available]"))

  packageStartupMessage(paste0("  .:", pkgname, " ", rtemis.ver, ": Welcome, ", Sys.getenv("USER"),
                               "\n  [", sessionInfo()[2], ": Defaulting to ", rtCores, "/", .availableCores,
                               " available cores]"))

  # Draw a harmonograph on startup if we are in RStudio
  try(if (try(rstudioapi::isAvailable(), silent = TRUE))
    mplot3.harmonograph(text = "rtemis", text.adj = .01, text.padj = -.2, text.col = "#72CDF4"), silent = TRUE)
}

#' \pkg{rtemis} internal: rtemis environment
#'
#' @keywords internal

rtenv <- new.env()
rtenv$rtCores <- getOption("rtCores", future::availableCores())
