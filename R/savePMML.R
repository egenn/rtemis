# savePMML.R
# ::rtemis::
# 2020 Efstathios D. Gennatas egenn.lambdamd.org

#' Save rtemis model to PMML file
#'
#' @param x rtemis model
#' @param filename Character: path to file
#' @export
#' @author Efstathios D. Gennatas
savePMML <- function(x, filename) {

  if (!depCheck("pmml", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  supported <- c("GLM", "GBM", "CART", "SVM", "RF", "RFSRC")
  if (!x$mod.name %in% supported) stop("Unsupported model")

  mod_pmml <- pmml::pmml(x$mod)

  pmml::save_pmml(mod_pmml, name = filename)

} # rtemis::savePMML
