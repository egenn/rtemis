# dt_ops.R
# ::rtemis::
# 2022 EDG lambdamd.org

#' Number of unique values per feature
#'
#' @param x data.table
#' @param excludeNA Logical: If TRUE, exclude NA values
#'
#' @author E.D. Gennatas
#' @export

dt_Nuniqueperfeat <- function(x, excludeNA = FALSE) {
    sapply(x, \(i) uniqueN(i, na.rm = excludeNA))
} # rtemis::dt_Nuniqueperfeat
