# factor_NA2missing.R
# ::rtemis::
# 2020 Efstathios D. Gennatas rtemis.lambdamd.org

#' Factor NA to "missing" level
#'
#' Set NA values of a factor vector to a new level indicating missingness
#'
#' @param x Factor
#' @param na_level_name Character: Name of new level to create that will be assigned to all current
#' NA values. Default = "missing"
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' x <- factor(sample(letters[1:3], 100, TRUE))
#' x[sample(1:100, 10)] <- NA
#' xm <- factor_NA2missing(x)

factor_NA2missing <- function(x, na_level_name = "missing") {

  if (!is.factor(x)) stop("x must be a factor, instead it is of class", class(x))
  if (anyNA(x)) {
    x <- factor(x, levels = c(levels(x), na_level_name))
    x[is.na(x)] <- "missing"
    x
  } else {
    x
  }


} # rtemis::factor_NA2missing
