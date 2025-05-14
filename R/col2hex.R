# col2hex.R
# ::rtemis::
# EDG

#' Convert R color to hexadecimal code
#'
#' Convert a color that R understands into the corresponding hexadecimal code
#'
#' @param color Color(s) that R understands
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' col2hex(c("gray50", "skyblue"))

col2hex <- function(color) {
  .rgb <- col2rgb(color)
  sapply(seq(color), function(i) {
    paste0(
      "#",
      paste0(
        sprintf(
          "%02s",
          c(
            as.character(as.hexmode(.rgb[1, i])),
            as.character(as.hexmode(.rgb[2, i])),
            as.character(as.hexmode(.rgb[3, i]))
          )
        ),
        collapse = ""
      )
    )
  })
} # rtemis::col2hex

# col2hex <- function(col, alpha) rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)
