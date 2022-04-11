# savePMML.R
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' Save rtemis model to PMML file
#'
#' @param x rtemis model
#' @param filename Character: path to file
#'
#' @export
#' @author E.D. Gennatas

savePMML <- function(x, filename,
                     transforms = NULL,
                     model_name = NULL,
                     model_version = NULL,
                     description = NULL,
                     copyright = NULL, ...) {

    # Dependencies ----
    dependency_check("pmml")

    supported <- c("GLM", "LOGISTIC", "GBM", "CART", "SVM", "RF", "RFSRC")
    if (!x$mod.name %in% supported) stop("Unsupported model")

    mod_pmml <- pmml::pmml(
        model = x$mod,
        model_name = model_name,
        app_name = "rtemis",
        description = description,
        copyright = copyright,
        model_version = model_version,
        transforms = transforms, ...
    )

    pmml::save_pmml(mod_pmml, name = filename)
} # rtemis::savePMML
