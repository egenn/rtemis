# labelify.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Convert text for label printing
#'
#' @param x Character: Input
#' @param underscoresToSpaces Logical: If TRUE, convert underscores to spaces. Default = TRUE
#' @param dotsToSpaces Logical: If TRUE, convert dots to spaces. Default = TRUE
#' @param toTitleCase Logical: If TRUE, convert to Title Case. Default = TRUE
#' @param capitalize.strings String, vector: Always capitalize these strings, if present. Default = "id"
#' @param stringsToSpaces String, vector: Replace these strings with spaces. Escape as needed for \code{gsub}.
#' Default = "\\$", which formats common input of the type \code{data.frame$variable}
#'
#' @author Efstathios D. Gennatas
#' @export

labelify <- function(x,
                     underscoresToSpaces = TRUE,
                     dotsToSpaces = TRUE,
                     toTitleCase = TRUE,
                     capitalize.strings = c("id"),
                     stringsToSpaces = c("\\$")) {

  xf <- x
  for (i in stringsToSpaces) {
    xf <- gsub(i, " ", x)
  }
  if (underscoresToSpaces) xf <- gsub("_", " ", xf)
  if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
  if (toTitleCase) xf <- tools::toTitleCase(xf)
  for (i in capitalize.strings) {
    xf <- gsub(tools::toTitleCase(i), toupper(i), xf)
  }
  xf

} # rtemis::labelify
