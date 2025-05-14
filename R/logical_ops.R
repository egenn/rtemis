# logical_ops
# ::rtemis::
# 2023 EDG rtemis.org

#' Inverse of `%in%`
#'
#' data.table %notin% not currently on CRAN version
#'
#' @param x vector
#' @param set vector
#'
#' @returns logical vector
#' @keywords internal
#' @noRd
#'
#' @examples
#' 3 %notin% c(2, 4, 6) # TRUE
#' 4 %notin% 3:5 # FALSE
"%notin%" <- function(x, set) !x %in% set
