# format.call.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Format method for \code{call} objects
#'
#' @method format call
#'
#' @param x \code{call} object
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export
#' 
#' @examples
#' \dontrun{
#' irmod <- elevate(iris,
#'                  mod = "cart",
#'                  maxdepth = 2:3, 
#'                  n.resamples = 9, 
#'                  train.p = .85)
#' format(irmod$call) |> cat()
#' }

format.call <- function(x, ...) {
    # leftpad <- nchar(x[1]) + 1
    leftpad <- nchar(x[1])
    out <- paste0(format.default(x), collapse = "")
    out <- gsub(" +", " ", out)
    out <- gsub(
        ",",
        paste0(",\n", paste0(rep(" ", leftpad), collapse = "")),
        out
    )
    paste0(out, "\n")
}
