# strict.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Strict assignment by class or type
#'
#' Allow assignment only if input is of correct class and/or type
#'
#' @param x Value to be assigned if class and/or type check passes
#' @param accept.class Required class of `x`
#' @param accept.type Required type of `x`
#' @param non.null.val Function which should have a non-null value when applied to `x`
#'
#' @author E.D. Gennatas
#' @export

strict <- function(
  x,
  accept.class = NULL,
  accept.type = NULL,
  non.null.val = NULL
) {
  input.class <- class(x)
  input.type <- typeof(x)
  if (!is.null(accept.class)) {
    if (input.class != accept.class) {
      stop(
        "Incorrect class: Expected ",
        accept.class,
        ", got ",
        input.class,
        " instead"
      )
    }
  }

  if (!is.null(accept.type)) {
    if (input.type != accept.type) {
      stop(
        "Incorrect type: Expected ",
        accept.type,
        ", got ",
        input.type,
        " instead"
      )
    }
  }

  return(x)
} # rtemis::strict
