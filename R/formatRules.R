# formatRules
# ::rtemis::
# Efstathios D. Gennatas egenn.lambdamd.org

#' Format rules
#'
#' Converts R-executable logical expressions to a more human-friendly format
#'
#' @param x Vector, string: Logical expressions
#' @param space.after.comma Logical: If TRUE, place spaces after commas. Default = false
#' @param decimal.places Integer: Limit all floats (numbers of the form 9.9) to this many
#' decimal places
#' @author Efstathios D. Gennatas
#' @export

formatRules <- function(x, space.after.comma = FALSE,
                         decimal.places = NULL) {

  x <- gsub("[&+]", "AND", x)
  x <- gsub(">", " > ", x)
  x <- gsub("<=", " <= ", x)
  x <- gsub("%in%", "IN", x)
  x <- gsub("c\\(", "{", x)
  x <- gsub("\\)", "}", x)
  x <- gsub("'", "", x)
  if (space.after.comma) x <- gsub(",", ", ", x)
  if (!is.null(decimal.places)) {
    x <- gsubfn::gsubfn("([0-9.]+[0-9])", function(i) ddSci(i, decimal.places = decimal.places), x)
  }
  x

} # rtemis::format.rules
