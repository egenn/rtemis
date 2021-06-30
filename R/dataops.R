# dataops
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Get names by string matching
#'
#' @param x object with \code{names()} method
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

#' Get names by string matching multiple patterns
#'
#' @param x Character vector or object with \code{names()} method
#' @param pattern Character vector: pattern(s) to match anywhere in names of x
#' @param starts_with Character: pattern to match in the beginning of names of x
#' @param ends_with Character: pattern to match at the end of names of x
#' @param ignore.case Logical: If TRUE, well, ignore case. Default = TRUE
#' @param return.index Logical: If TRUE, return integer index of matches instead of names
#'
#' @return Character vector of matched names or integer index
#' @author E.D. Gennatas
#' @export

mgetnames <- function(x,
                      pattern = NULL,
                      starts_with = NULL,
                      ends_with = NULL,
                      ignore.case = TRUE,
                      return.index = FALSE) {

  .names <- if(is.character(x)) x else names(x)
  idi <- numeric()
  if (!is.null(pattern)) {
    idi <- c(idi, unlist(lapply(pattern, function(p) grep(p, .names, ignore.case = ignore.case))))
  }
  if (!is.null(starts_with)) {
    idi <- c(idi, which(startsWith(.names, starts_with)))
  }
  if (!is.null(ends_with)) {
    idi <- c(idi, which(endsWith(.names, ends_with)))
  }
  idi <- unique(idi)
  if (return.index) {
    idi
  } else {
    .names[idi]
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

#' @rdname getnames
#' @export
getdatenames <- function(x) names(x)[sapply(x, function(v) inherits(v, "Date"))]


#' Size
#'
#' Get \code{NCOL(x)} and \code{NROW{x}}
#'
#' @param x R object (usually that inherits from matrix or data.frame)
#' @param verbose Logical: If TRUE, print NROW and NCOL to console. Default = TRUE
#'
#' @return vector of NROW, NCOL invisibly
#' @author E.D. Gennatas
#' @export

catsize <- function(x, verbose = TRUE) {
  .nrow <- NROW(x)
  .ncol <- NCOL(x)
  if (verbose) {
    if (inherits(x, c("matrix", "data.frame"))) {
      cat("There are", .nrow, "rows and", .ncol, "columns")
    } else if (inherits(x, "list")) {
      cat("There are", length(x), "elements")
    }
  }
  invisible(c(.nrow, .ncol))

}
