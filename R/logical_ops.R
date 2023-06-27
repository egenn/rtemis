# logical_ops
# ::rtemis::
# 2023 EDG lambdamd.org

#' Inverse of `%in%`
#' 
#' @param x vector
#' @param set vector
#' 
#' @returns logical vector
#' @export
#' 
#' @examples
#' 3 %notin% c(2, 4, 6) # TRUE
#' 4 %notin% 3:5 # FALSE
"%notin%" <- function(x, set) !x %in% set
