# summarize.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Summarize numeric variables
#'
#' @param x data.frame or data.table (will be coerced to data.table)
#' @param varname Character, vector: Variable name(s) to summarize. Must be column names in \code{x}
#' of type numeric.
#' @param group_by Character, vector: Variable name(s) of factors to group by. Must be column names
#' in \code{x}. Default = NULL
#' @param type Character: "median-range" or "mean-sd". Default = "median-range"
#' @param na.rm Logical: Passed to \code{median} and \code{mean}. Default = TRUE
#'
#' @return \code{data.table} with summary
#'
#' @author E.D. Gennatas
#' @export

summarize <- function(x,
                      varname,
                      group_by = NULL,
                      type = c("median-range", "mean-sd"),
                      na.rm = TRUE) {

  UseMethod("summarize")

} # rtemis::summarize

#' @method summarize data.frame
#' @export
summarize.data.frame <- function(x,
                                 varname,
                                 group_by = NULL,
                                 type = c("median-range", "mean-sd"),
                                 na.rm = TRUE) {
  summarize(as.data.table(x),
            varname,
            group_by,
            type,
            na.rm)
} # rtemis::summarize.data.frame

#' @method summarize data.table
#' @export
summarize.data.table <- function(x,
                                 varname,
                                 group_by = NULL,
                                 type = c("median-range", "mean-sd"),
                                 na.rm = TRUE) {

  type <- match.arg(type)

  if (type == "median-range") {
    x[, .(Var = varname,
          N = sapply(.SD, length),
          Median = sapply(.SD, median, na.rm = na.rm),
          Range = sapply(.SD, catrange),
          `NA` = sapply(.SD, function(i) sum(is.na(i)))),
      .SDcols = varname,
      by = group_by]
  } else {
    x[, .(Var = varname,
          N = sapply(.SD, length),
          Mean = sapply(.SD, mean, na.rm = na.rm),
          SD = sapply(.SD, sd),
          `NA` = sapply(.SD, function(i) sum(is.na(i)))),
      .SDcols = varname,
      by = group_by]
  }

} # rtemis::summarize.data.table
