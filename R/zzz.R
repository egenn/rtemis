# zzz.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

rtenv <- new.env()
.availableCores <- parallelly::availableCores()
rtCores <- getOption("rt.cores", .availableCores)
rtemis.version <- packageVersion("rtemis")
rtHome = getOption("rt.home", Sys.getenv("HOME"))

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(paste0("  .:", pkgname, " ", rtemis.version, ": Welcome, ", Sys.getenv("USER"),
                               "\n  [", sessionInfo()[[2]], ": Defaulting to ", rtCores, "/", .availableCores, " available cores]",
                               "\n  Documentation & vignettes: https://rtemis.lambdamd.org",
                               "\n  Learn R: https://class.lambdamd.org/pdsr",
                               '\n  See `citation("rtemis")` for how to cite'))
  
  try(
    if (interactive() && try(rstudioapi::isAvailable(), silent = TRUE)) {
      col <- sample(unlist(rtemis:::rtCol1), 1)
      n <- 20
      polyshadow2(n, n, .8, 
                  text = "rtemis",
                  text.x = 2,
                  text.y = 2,
                  text.adj = c(0, 0),
                  text.col = col,
                  text.cex = 1.2,
                  shadow = seq(.9, .96, length = n^2),
                  col_lo =  "#00000040",
                  col_hi =  col,
                  color.progression = "prod")
  }, 
  silent = TRUE)

  if (is.null(getOption("rt.theme"))) {
    options(rt.theme = "darkgraygrid")
  }

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
#'    \item{rt.theme}{General plotting theme; set to e.g. "light" or "dark"}
#'    \item{ft.fit.theme}{Plotting theme for true vs. fitted; set to e.g. "lightgrid" or "darkgrid"}
#'    \item{rtCores}{Number of cores to use. By default, rtemis will use available cores reported by
#'    parallelly::availableCores(). In shared systems, you should limit this as appropriate.}
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
#' @import graphics grDevices methods stats utils parallel R6 data.table htmltools crayon parallelly
#' @importFrom crayon "%+%"

NULL
