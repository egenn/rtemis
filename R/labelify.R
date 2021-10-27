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
#' @param capitalize.strings Character, vector: Always capitalize these strings, if present. Default = "id"
#' @param stringsToSpaces Character, vector: Replace these strings with spaces. Escape as needed for \code{gsub}.
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

  if (is.null(x)) return(NULL)
  xf <- x
  for (i in stringsToSpaces) {
    xf <- gsub(i, " ", x)
  }
  for (i in capitalize.strings) {
    xf <- gsub(paste0("^", i, "$"), toupper(i), xf)
  }
  if (underscoresToSpaces) xf <- gsub("_", " ", xf)
  if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
  if (toLower) xf <- tolower(xf)
  if (toTitleCase) xf <- tools::toTitleCase(xf)

  xf

} # rtemis::labelify

#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character, vector
#'
#' @author E.D. Gennatas
#' @export

clean_colnames <- function(x) {
  if (!inherits(x, "character")) {
    x <- if (inherits(x, "matrix")) colnames(x) else names(x)
  }
  out <- gsub("[[:punct:]{1,}|[:space:]]{1,}", "_", x)
  gsub("_$", "", out)
}
