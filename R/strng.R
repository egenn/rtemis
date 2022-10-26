# strng.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' String formatting utilities
#' 
#' @rdname strng
bold <- function(...) {
    paste0("\033[1m", paste(...), "\033[22m")
}

#' @rdname strng
italic <- function(...) {
    paste0("\033[3m", paste(...), "\033[23m")
}

#' @rdname strng
underline <- function(...) {
    paste0("\033[4m", paste(...), "\033[24m")
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

col256 <- function(x, col = 183) {
    paste0("\033[38;5;", col, "m", x, "\033[0m")
}

rtascii <- function() {
    cat(col256("▄▄▄  ▄▄▄▄▄▄▄▄ .• ▌ ▄ ·. ▪  .▄▄ ·\n", 92))
    cat(col256("▀▄ █·•██  ▀▄.▀··██ ▐███▪██ ▐█ ▀.\n", 128))
    cat(col256("▐▀▀▄  ▐█.▪▐▀▀▪▄▐█ ▌▐▌▐█·▐█·▄▀▀▀█▄\n", 196))
    cat(col256("▐█•█▌ ▐█▌·▐█▄▄▌██ ██▌▐█▌▐█▌▐█▄▪▐█\n", 208))
    cat(col256(".▀  ▀ ▀▀▀  ▀▀▀ ▀▀  █▪▀▀▀▀▀▀ ▀▀▀▀\n", 27))
}

rtasciitxt <- function() {
    paste0(
        paste0(col256("  ▄▄▄  ▄▄▄▄▄▄▄▄ .• ▌ ▄ ·. ▪  .▄▄ ·\n", 92)),
        paste0(col256("  ▀▄ █·•██  ▀▄.▀··██ ▐███▪██ ▐█ ▀.\n", 128)),
        paste0(col256("  ▐▀▀▄  ▐█.▪▐▀▀▪▄▐█ ▌▐▌▐█·▐█·▄▀▀▀█▄\n", 196)),
        paste0(col256("  ▐█•█▌ ▐█▌·▐█▄▄▌██ ██▌▐█▌▐█▌▐█▄▪▐█\n", 208)),
        paste0(col256("  .▀  ▀ ▀▀▀  ▀▀▀ ▀▀  █▪▀▀▀▀▀▀ ▀▀▀▀\n", 27))
    )
}
