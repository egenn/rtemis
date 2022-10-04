# msg.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Message with provenance
#'
#' Print message to output with a prefix including data and time, and calling function or full
#' call stack
#'
#' If \code{msg} is called directly from the console, it will print \code{[interactive>]} in place of
#'   the call stack.
#' \code{msg0}, similar to \code{paste0}, is \code{msg(..., sep = "")}
#'
# Add following to each function using \code{msg}:
# \code{current <- as.list(sys.call())[[1]]}
#'
#' @param ... Message to print
#' @param date Logical: if TRUE, include date and time in the prefix
#' @param caller Character: Name of calling function
#' @param call.depth Integer: Print the system call path of this depth. Default = NULL
#' @param caller.id Integer: Which function in the call stack to print
#' @param newline Logical: If TRUE end with a new line. Default = FALSE
#' @param newline.pre Logical: If TRUE begin with a new line. Default = FALSE
#' @param as.message Logical: if TRUE, print using \code{message()}
#' @param color Color fn
#' @param sep Character: Use to separate objects in \code{...}
#'
#' @return Invisibly: List with call, message, and date
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' msg("Your dataset has", nrow(iris), "rows and", ncol(iris), "columns")
#' }
#'
msg <- function(...,
                date = TRUE,
                caller = NULL,
                call.depth = 1,
                caller.id = 1,
                newline = TRUE,
                extraline = FALSE,
                newline.pre = FALSE,
                as.message = FALSE,
                color = NULL,
                sep = " ") {
    if (is.null(caller)) {
        callStack.list <- as.list(sys.calls())
        stack.length <- length(callStack.list)
        if (stack.length < 2) {
            caller <- ">"
        } else {
            call.depth <- call.depth + caller.id
            if (call.depth > stack.length) call.depth <- stack.length
            caller <- paste(lapply(
                rev(seq(call.depth)[-seq(caller.id)]),
                function(i) rev(callStack.list)[[i]][[1]]
            ), collapse = ">>")
        }
        # do.call and similar will change the call stack, it will contain the full function definition instead of
        # the name alone
        if (is.function(caller)) caller <- NULL
        if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
        if (!is.null(caller)) caller <- paste0(" ", caller)
    } else {
        caller <- paste0(" ", caller)
    }

    txt <- Filter(Negate(is.null), list(...))
    .dt <- if (date) paste0(as.character(Sys.time())) else NULL

    if (as.message) {
        message(paste0("[", .dt, caller, "] ", paste(txt, collapse = sep)))
    } else {
        if (newline.pre) cat("\n")
        if (is.null(color)) {
            cat(gray(paste0("[", .dt, bold(caller), "] ")))
            cat(paste(txt, collapse = sep), if (newline) "\n")
        } else {
            cat(gray(paste0("[", .dt, bold(caller), "] ")))
            cat(paste(color(txt), collapse = sep), if (newline) "\n")
        }
        if (extraline) cat("\n")
    }

    invisible(list(
        call = caller,
        txt = txt,
        dt = .dt
    ))
} # rtemis::msg


# The only issue this way is msg0 will not print (interactive) when used interactively,
# but who cares. edit: I do
# msg0 <- function(...,
#                  date = TRUE,
#                  call.depth = 1,
#                  caller.id = 1,
#                  newline = TRUE,
#                  extraline = FALSE,
#                  newline.pre = FALSE,
#                  as.message = FALSE,
#                  color = NULL) {

#   msg(..., date = date,
#       call.depth = call.depth,
#       caller.id = caller.id + 1,
#       newline = newline,
#       extraline = extraline,
#       newline.pre = newline.pre,
#       as.message = as.message,
#       color = color,
#       sep = "")

# } # rtemis::msg0

# Create copy of msg with different default sep
# to avoid issue with call.depth after calling msg from msg0
# yes, there are other workarounds
#' @rdname msg
#' @export
msg0 <- function(...,
                 date = TRUE,
                 caller = NULL,
                 call.depth = 1,
                 caller.id = 1,
                 newline = TRUE,
                 extraline = FALSE,
                 newline.pre = FALSE,
                 as.message = FALSE,
                 color = NULL,
                 sep = "") {
    if (is.null(caller)) {
        callStack.list <- as.list(sys.calls())
        stack.length <- length(callStack.list)
        if (stack.length < 2) {
            caller <- ">"
        } else {
            call.depth <- call.depth + caller.id
            if (call.depth > stack.length) call.depth <- stack.length
            caller <- paste(lapply(
                rev(seq(call.depth)[-seq(caller.id)]),
                function(i) rev(callStack.list)[[i]][[1]]
            ), collapse = ">>")
        }
        # do.call and similar will change the call stack, it will contain the full function definition instead of
        # the name alone
        if (is.function(caller)) caller <- NULL
        if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
        if (!is.null(caller)) caller <- paste0(" ", caller)
    } else {
        caller <- paste0(" ", caller)
    }

    txt <- Filter(Negate(is.null), list(...))
    .dt <- if (date) paste0(as.character(Sys.time())) else NULL

    if (as.message) {
        message(paste0("[", .dt, caller, "] ", paste(txt, collapse = sep)))
    } else {
        if (newline.pre) cat("\n")
        if (is.null(color)) {
            cat(gray(paste0("[", .dt, bold(caller), "] ")))
            cat(paste(txt, collapse = sep), if (newline) "\n")
        } else {
            cat(gray(paste0("[", .dt, bold(caller), "] ")))
            cat(paste(color(txt), collapse = sep), if (newline) "\n")
        }
        if (extraline) cat("\n")
    }

    invisible(list(
        call = caller,
        txt = txt,
        dt = .dt
    ))
} # rtemis::msg0

stopQuietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
}

rtStop <- function(...) {
    message <- paste(...)
    cat(magenta("[Error]", message))
    stopQuietly()
}

rtWarning <- function(...) {
    message <- paste(...)
    cat(bold(orange(paste("[Warning]", message, "\n"))))
}

rtOut <- function(...) {
    message <- paste(...)
    cat(bold(gray("[")), green("+++", bold(green(message))),
        bold(gray("]")),
        sep = ""
    )
}

info <- function(..., color = hilite) {
    msg(..., color = color)
}
