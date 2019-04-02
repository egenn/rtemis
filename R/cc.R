# cc.R
# ::rtemis::
# 2015 Efstathios D. Gennatas egenn.github.io

#' Concatenate Vectors
#' 
#' Concatenate that maintains factors
#'
#' A \code{c()} replacement that maintains factors as factors because it doesn't make sense not to.
#' If all inputs are factors, they are converted to character, concatenated and converted to factor again.
#' Otherwise, they are passed to \code{c()}
#'
#' @param ... Two or more vectors of any type
#' @return Concatenation of \code{...}
#' @author Efstathios D. Gennatas
#' @export

cc <- function(...) {

  inputs <- list(...)
  if (all(as.logical(lapply(inputs, function(i) class(i) == "factor")))) {
    as.factor(unlist(sapply(inputs, as.character)))
  } else {
    c(...)
  }

} # rtemis::cc


cf <- function(x, y) {
  if (class(x) == "factor" & class(y) == "factor") {
    f <- as.factor(c(as.character(x), as.character(y)))
  } else {
    f <- c(x, y)
  }
  # levels(f) <- unique(c(levels(x), levels(y)))
  f
} # rtemis::cf
