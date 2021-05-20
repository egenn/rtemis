# labelify.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org

#' Convert text for label printing
#'
#' @param x Character: Input
#' @param underscoresToSpaces Logical: If TRUE, convert underscores to spaces. Default = TRUE
#' @param dotsToSpaces Logical: If TRUE, convert dots to spaces. Default = TRUE
#' @param toLower Logical: If TRUE, convert to lowercase (precedes \code{toTitleCase}).
#' Default = FALSE (Good for getting all-caps words converted to title case, bad for abbreviations
#' you want to keep all-caps)
#' @param toTitleCase Logical: If TRUE, convert to Title Case. Default = TRUE (This does not change
#' all-caps words, set \code{toLower} to TRUE if desired)
#' @param capitalize.strings String, vector: Always capitalize these strings, if present. Default = "id"
#' @param stringsToSpaces String, vector: Replace these strings with spaces. Escape as needed for \code{gsub}.
#' Default = "\\$", which formats common input of the type \code{data.frame$variable}
#'
#' @author E.D. Gennatas
#' @export

labelify <- function(x,
                     underscoresToSpaces = TRUE,
                     dotsToSpaces = TRUE,
                     toLower = FALSE,
                     toTitleCase = TRUE,
                     capitalize.strings = c("id"),
                     stringsToSpaces = c("\\$")) {

  xf <- x
  for (i in stringsToSpaces) {
    xf <- gsub(i, " ", x)
  }
  if (underscoresToSpaces) xf <- gsub("_", " ", xf)
  if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
  if (toLower) xf <- tolower(xf)
  if (toTitleCase) xf <- tools::toTitleCase(xf)
  for (i in capitalize.strings) {
    xf <- gsub(tools::toTitleCase(i), toupper(i), xf)
  }
  xf

} # rtemis::labelify
