# boxcat.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Box cat
#'
#' `cat` with a box; a boxed cat
#'
#' @param x Character: Text to be output to console
#' @param style  Integer: {1, 2}: 1: vintage style, 2: modern style. Default = 2
#' @param col Color: Any color fn
#' @param newline.pre Logical: If TRUE, start with a new line. Default = TRUE
#' @param newline Logical: If TRUE, end with a new (empty) line. Default = FALSE
#' @param pad Integer: Pad message with this many spaces on the left. Default = 0
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

boxcat <- function(
  x,
  col = NULL,
  newline.pre = TRUE,
  newline = FALSE,
  pad = 0
) {
  x <- as.character(x)
  if (newline.pre) cat("\n")
  cat(rep(" ", pad), sep = "")
  cat(gray(".:"))
  if (!is.null(col)) {
    cat(col(x, TRUE))
  } else {
    cat(bold(x))
  }
  cat("\n")
  if (newline) cat("\n")
} # rtemis::boxcat

pastebox <- function(x, pad = 0) {
  paste0(paste0(rep(" ", pad), collapse = ""), ".:", x)
}

# objcat.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' `rtemis-internal`: Object cat
#'
#' @param x Character: Object description
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

objcat <- function(x) {
  cat(".:rtemis", orange(x, bold = TRUE), "\n")
} # rtemis::boxcat
