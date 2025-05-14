# format.call.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Format method for `call` objects
#'
#' @method format call
#'
#' @param x `call` object
#' @param as.html Logical: If TRUE, output HTML span element
# @param font.family Character: font family to use when \code{as.html = TRUE}
#' @param class Character: CSS class to assign to span containing code
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' irmod <- elevate(iris,
#'   mod = "cart",
#'   maxdepth = 2:3,
#'   n.resamples = 9,
#'   train.p = .85
#' )
#' format(irmod$call) |> cat()
#' }
format.call <- function(
  x,
  as.html = FALSE,
  # font.family = "monospace",
  class = "rtcode",
  ...
) {
  leftpad <- nchar(x[1])
  out <- paste0(format.default(x), collapse = "")
  out <- gsub(" +", " ", out)
  out <- gsub(
    ",",
    paste0(",\n", paste0(rep(" ", leftpad), collapse = "")),
    out
  )
  out <- paste0(out, "\n")
  if (as.html) {
    span(
      HTML(
        gsub(" ", "&nbsp;", gsub("\n", "<br>", out))
      ),
      # style = paste0("font-family: ", font.family)
      class = class
    )
  } else {
    out
  }
} # rtemis::format.call
