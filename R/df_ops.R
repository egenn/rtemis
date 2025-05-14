# dataops
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Get names by string matching
#'
#' @param x object with `names()` method
#' @param pattern Character: pattern to match anywhere in names of x
#' @param starts_with Character: pattern to match in the beginning of names of x
#' @param ends_with Character: pattern to match at the end of names of x
#' @param ignore.case Logical: If TRUE, well, ignore case. Default = TRUE
#' @author E.D. Gennatas
#' @export

getnames <- function(
  x,
  pattern = NULL,
  starts_with = NULL,
  ends_with = NULL,
  ignore.case = TRUE
) {
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
#' @param x Character vector or object with `names()` method
#' @param pattern Character vector: pattern(s) to match anywhere in names of x
#' @param starts_with Character: pattern to match in the beginning of names of x
#' @param ends_with Character: pattern to match at the end of names of x
#' @param ignore.case Logical: If TRUE, well, ignore case. Default = TRUE
#' @param return.index Logical: If TRUE, return integer index of matches instead of names
#'
#' @return Character vector of matched names or integer index
#' @author E.D. Gennatas
#' @export

mgetnames <- function(
  x,
  pattern = NULL,
  starts_with = NULL,
  ends_with = NULL,
  ignore.case = TRUE,
  return.index = FALSE
) {
  .names <- if (is.character(x)) x else names(x)
  idi <- numeric()
  if (!is.null(pattern)) {
    idi <- c(
      idi,
      unlist(lapply(
        pattern,
        function(p) grep(p, .names, ignore.case = ignore.case)
      ))
    )
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
getdatenames <- function(x) {
  date_id <- sapply(
    x,
    \(v) class(v)[1] %in% c("Date", "IDate", "POSIXct", "POSIXlt")
  )
  names(x)[date_id]
}

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


#' Unique values per feature
#'
#' Get number of unique values per features
#'
#' @param x matrix or data frame input
#' @param excludeNA Logical: If TRUE, exclude NA values from unique count.
#'
#' @return Vector, integer of length `NCOL(x)` with number of unique
#' values per column/feature
#' @export
#' @author E.D. Gennatas
#' @examples
#' \dontrun{
#' uniquevalsperfeat(iris)
#' }

uniquevalsperfeat <- function(x, excludeNA = FALSE) {
  if (excludeNA) {
    apply(x, 2, function(i) length(unique(na.exclude(i))))
  } else {
    apply(x, 2, function(i) length(unique(i)))
  }
} # rtemis::uniquevalsperfeat


# df_movecolumn.R
# ::rtemis::
# 2020 E.D. Gennatas rtemis.org

#' Move data frame column
#'
#' @param x data.frame
#' @param from String or Integer: Define which column holds the vector you want to move
#' @param to Integer: Define which column number you want the vector to be moved to.
#' Default = `ncol(x)` i.e. the last column.
#' @author E.D. Gennatas
#' @export
#' @examples
#' mtcars_hp <- df_movecolumn(mtcars, "hp")
#'
df_movecolumn <- function(x, from, to = ncol(x)) {
  if (!is.data.frame(x)) stop("Input must be data frame")

  if (is.character(from)) {
    from_name <- from
    from <- grep(from, colnames(x))
    if (length(from) == 0) stop("Did not find \"", from, "\" in input")
  } else {
    from_name <- colnames(x)[from]
  }

  v <- x[, from, drop = FALSE]
  x[, from] <- NULL

  if (to == ncol(x)) {
    cbind(x, v)
  } else {
    cbind(x[, 1:(to - 1), drop = FALSE], v, x[, (to + 1):ncol(x), drop = FALSE])
  }
} # rtemis::df_movecolumn
