# dataops
# ::rtemis::
# 2021 E.D. Gennatas www.lambdamd.org

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

  .names <- if (is.character(x)) {
    x
  } else {
    names(x)
  }
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

  .names <- if (is.character(x)) x else names(x)
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

#' Get data.frame names and types
#'
#' @param x data.frame / data.table or similar
#' @returns character vector of column names with attribute "type" holding the class of each
#' column
#' @export

getnamesandtypes <- function(x) {
  xnames <- names(x)
  attr(xnames, "type") <- sapply(x, class)
  xnames
} # rtemis::namesandtypes


#' Check file(s) exist
#' 
#' @param paths Character vector of paths
#' @param verbose Logical: If TRUE, print messages to console
#' @param pad Integer: Number of spaces to pad to the left
#' 
#' @author E.D. Gennatas
#' @export

check_files <- function(paths,
                        verbose = TRUE,
                        pad = 0) {
    if (verbose) msg("Checking files:")
    
    for (f in paths) {
      if (file.exists(f)) {
        if (verbose) {
            yay(f, pad = pad)
        }
      } else {
          if (verbose) {
            nay(paste(f, red(" not found!")), pad = pad)
          }
          stop("File not found")
        }
      }
  } # rtemis::check_files
  