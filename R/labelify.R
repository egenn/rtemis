# labelify.R
# ::rtemis::
# 2017 E.D. Gennatas www.lambdamd.org

#' Format text for label printing
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
                     stringsToSpaces = c("\\$", "`")) {
    if (is.null(x)) {
        return(NULL)
    }
    xf <- x
    for (i in stringsToSpaces) {
        xf <- gsub(i, " ", xf)
    }
    for (i in capitalize.strings) {
        xf <- gsub(paste0("^", i, "$"), toupper(i), xf)
    }
    if (underscoresToSpaces) xf <- gsub("_", " ", xf)
    if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
    if (toLower) xf <- tolower(xf)
    if (toTitleCase) xf <- tools::toTitleCase(xf)
    xf <- gsub(" {2,}", " ", xf)
    gsub(" $", "", xf)
} # rtemis::labelify

#' Clean names
#'
#' Replace all symbols and combinations of symbols in a character vector
#' with underscores
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' clean_names(colnames(iris))

clean_names <- function(x) {
    out <- gsub("[[:space:]|[:punct:]]{1,}", "_", x)
    gsub("_$", "", out)
}

#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character, vector
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' clean_colnames(iris)

clean_colnames <- function(x) {
    if (!inherits(x, "character")) {
        x <- if (inherits(x, "matrix")) colnames(x) else names(x)
    }
    clean_names(x)
}


#' Clean factor levels of data.frame
#' 
#' Finds all factors in a data.frame and cleans factor levels to include
#' only underscore symbols
#' 
#' @param x data.frame
#' 
#' @author E.D. Gennatas
#' @export
clean_factor_levels <- function(x) {
    idi <- which(sapply(x, is.factor))
    for (i in idi) {
        levels(idi[, i]) <- clean_names(levels(idi[, i]))
    }
}
