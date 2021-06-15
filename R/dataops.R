# dataops
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Get names by string matching
#'
#' @param x objects wiht \code{names()} method
#' @param pattern Character: pattern to match anywhere in names of x
#' @param starts_with Character: pattern to match in the beginning of names of x
#' @param ends_with Character: pattern to match at the end of names of x
#' @param ignore.case Logical: If TRUE, well, ignore case. Default = TRUE
#' @author E.D. Gennatas
#' @export

getnames <- function(x,
                     pattern = NULL, starts_with = NULL, ends_with = NULL,
                     ignore.case = TRUE) {
  .names <- names(x)
  if (!is.null(pattern)) {
    .names[grep(pattern, .names, ignore.case = ignore.case)]
  } else if (!is.null(starts_with)) {
    .names[grep(paste0("^", starts_with), .names, ignore.case = ignore.case)]
  } else if (!is.null(ends_with)) {
    .names[grep(paste0(ends_with, "$"), .names, ignore.case = ignore.case)]
  }
}

#' Get factor/numeric/logical/character names from data.frame/data.table
#'
#' @name get-names
#' @param x data.frame or data.table (or data.frame-compatible object)
#' @author E.D. Gennatas
#' @export

getfactornames <- function(x) names(x)[sapply(x, is.factor)]

#' @rdname getnames
#' @export
getnumericnames <- function(x) names(x)[sapply(x, is.numeric)]

#' @rdname getnames
#' @export
getlogicalnames <- function(x) names(x)[sapply(x, is.logical)]

#' @rdname getnames
#' @export
getcharacternames <- function(x) names(x)[sapply(x, is.character)]
