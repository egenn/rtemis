# labelify
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Convert text for label printing
#' 
#' @param x String: Input
#' @param underscoresToSpaces Logical: If TRUE, convert underscores to spaces. Default = TRUE
#' @param dotsToSpaces Logical: If TRUE, convert dots to spaces. Default = TRUE
#' @param toTitleCase Logical: If TRUE, convert to Title Case. Default = TRUE
#' @param capitalize.strings String, vector: Always capitalize these strings, if present. Default = "id"
#'
#' @author Efstathios D. Gennatas
#' @export

labelify <- function(x,
                     underscoresToSpaces = TRUE,
                     dotsToSpaces = TRUE,
                     toTitleCase = TRUE,
                     capitalize.strings = c("id")) {
  
  xf <- x
  if (underscoresToSpaces) xf <- gsub("_", " ", x)
  if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
  if (toTitleCase) xf <- tools::toTitleCase(xf)
  for (i in capitalize.strings) {
    xf <- gsub(tools::toTitleCase(i), toupper(i), xf)
  }
  xf
  
} # rtemis::labelify
