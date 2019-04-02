# preprocess.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Data preprocessing
#'
#' Prepare data for data analysis
#'
#' By default, only removes constant features, everything else can be specified.
#'
#' Order of operations:
#'   * completeCases
#'   * removeCases.thres
#'   * removeFeatures.thres
#'   * integer2factor
#'   * nonzeroFactors
#'   * impute
#'   * scale/center
#'   * removeConstant
#'
#' @param x Input
#' @param completeCases Logical: If TRUE, only retain complete cases (no missing data).
#' Default = FALSE
#' @param impute Logical: If TRUE, impute missing cases. See \code{impute.discrete} and
#' \code{impute.numeric} for how
#' @param impute.type String: How to impute data: "missForest" uses the package of the same name to impute by iterative
#' random forest regression. "rfImpute" uses \code{randomForest::rfImpute} (see its documentation), "meanMode" will use
#' mean and mode by default or any custom function defined in \code{impute.discrete} and \code{impute.numeric}
#' @param impute.discrete Function that returns single value: How to impute discrete variables for 
#' \code{impute.type = "meanMode"}. Default = \link{getMode}
#' @param impute.numeric Function that returns single value: How to impute continuous variables for 
#' \code{impute.type = "meanMode"}.
#' Default = \code{mean}
#' @param integer2factor Logical: If TRUE, convert all integers to factors
#' @param integer2numeric Logical: If TRUE, convert all integers to numeric (will only work 
#' if \code{integer2factor = FALSE})
#' @param removeCases.thres Float: Remove cases with >= to this fraction of missing features.
#' Default = NULL
#' @param removeFeatures.thres Float: Remove features with missing values in >= to this fraction of
#' cases. Default = NULL
#' @param removeConstant Logical: If TRUE, remove all columns with zero variance. Default = TRUE
#' @param nonzeroFactors Logical: Shift factor values to exclude zeros. Default = FALSE
#' @param scale Logical: If TRUE, scale columns of \code{x}
#' @param center Logical: If TRUE, center columns of \code{x}
#' @param verbose Logical: If TRUE, write messages to console. Default = TRUE
#' @author Efstathios D. Gennatas
#' @export

preprocess <- function(x, ...) {

  UseMethod("preprocess", x)

} # rtemis::preprocess
