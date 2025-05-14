# type_check.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Check type of object
#'
#' @param x Object to check
#' @param fn Function to check against, any `is.*` function, e.g. `is.character`
#'
#' @return Logical
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' is_check("papaya", is.character) # TRUE
#' is_check(c(1, 2.5, 3.2), is.integer) # FALSE
#' is_check(iris, is.list) # TRUE
#' }

is_check <- function(x, fn) {
  if (!fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    message(red(bold(input), "is not", bold(type)))
    return(FALSE)
  }
  return(TRUE)
} # /rtemis::is_check


#' Test type of object
#'
#' @inheritParams is_check
#'
#' @return NULL (invisibly)
#' @author EDG
#' @keywords internal

is_test <- function(x, fn) {
  if (!is.null(x) && !fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    stop(bold(input), " is not ", bold(type))
  }
  invisible(NULL)
} # /rtemis::is_test


#' Check class of object
#'
#' @param x Object to check
#' @param cl Character: class to check against
#'
#' @return Logical
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' inherits_check("papaya", "character") # TRUE
#' inherits_check(c(1, 2.5, 3.2), "integer") # FALSE
#' inherits_check(iris, "list") # FALSE, compare to is_check(iris, is.list)
#' }

inherits_test <- function(x, cl) {
  if (!inherits(x, cl)) {
    input <- deparse(substitute(x))
    message(red(bold(input), "is not", bold(cl)))
    return(FALSE)
  }
  return(TRUE)
} # /rtemis::inherits_check


#' Test class of object
#'
#' @inheritParams inherits_check
#'
#' @return NULL (invisibly)
#' @author EDG
#' @keywords internal

inherits_check <- function(x, cl) {
  if (!is.null(x) && !inherits(x, cl)) {
    input <- deparse(substitute(x))
    stop(bold(input), " must be ", bold(cl))
  }
  invisible(NULL)
} # /rtemis::inherits_test


#' Function that returns object if it is of a certain class
#'
#' @param object Object to check and return
#' @param class Character vector: class(es) to check against
#' @param allow_null Logical: if TRUE, allows NULL objects
#'
#' @return Object
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' strict("papaya", "character") # "papaya"
#' strict(c(1, 2.5, 3.2), "integer") # Error
#' strict(iris, "list") # Error
#' }
strict <- function(object, class, allow_null = TRUE) {
  if (allow_null && is.null(object)) {
    return(NULL)
  }
  if (inherits(object, class)) {
    return(object)
  } else {
    stop(bold(input), " must be ", bold(cl))
  }
} # /rtemis::strict
