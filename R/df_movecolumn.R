# df_movecolumn.R
# ::rtemis::
# 2020 Efstathios D. Gennatas rtemis.lambdamd.org

#' Move data frame column
#'
#' @param x data.frame
#' @param from String or Integer: Define which column holds the vector you want to move
#' @param to Integer: Define which column number you want the vector to be moved to.
#' Default = \code{ncol(x)} i.e. the last column.
#' @author Efstathios D. Gennatas
#' @export
#' @examples
#' mtcars_hp <- df_movecolumn(mtcars, "hp")

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
