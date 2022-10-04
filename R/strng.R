# strng.R
# ::rtemis::
# 2022 E. D. Gennatas www.lambdamd.org

#' String formatting utilities
#' 
#' @rdname strng
bold <- function(...) {
    paste0("\033[1m", paste(...), "\033[0m")
}

#' @rdname strng
hilite <- function(..., bold = TRUE) {
    paste0(ifelse(bold, "\033[1m", ""), "\033[38;5;117m", paste(...), "\033[0m")
}

#' @rdname strng
red <- function(..., bold = FALSE) {
    paste0("\033[", ifelse(bold, "1;", ""), "91m", paste(...), "\033[0m")
}

#' @rdname strng
green <- function(..., bold = FALSE) {
    paste0("\033[", ifelse(bold, "1;", ""), "92m", paste(...), "\033[0m")
}

#' @rdname strng
orange <- function(..., bold = FALSE) {
    paste0(ifelse(bold, "\033[1m", ""), "\033[38;5;208m", paste(...), "\033[0m")
}

#' @rdname strng
cyan <- function(..., bold = FALSE) {
    paste0(ifelse(bold, "\033[1m", ""), "\033[36m", paste(...), "\033[0m")
}

#' @rdname strng
magenta <- function(..., bold = FALSE) {
    paste0(ifelse(bold, "\033[1m", ""), "\033[35m", paste(...), "\033[0m")
}

#' @rdname strng
gray <- function(..., bold = FALSE) {
    paste0(ifelse(bold, "\033[1m", ""), "\033[90m", paste(...), "\033[0m")
}

#' @rdname strng
reset <- function(...) {
    paste0("\033[0m", paste(...))
}
