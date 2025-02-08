# msg.R
# ::rtemis::
# 2016- EDG rtemis.org

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

warn <- function(...) {
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

info <- function(..., format_fn = hilite2) {
  msg2(..., format_fn = format_fn, caller_id = 2)
}

suggest <- function(...) {
  message <- paste(...)
  cat(hilite2("Suggestion: ", message, "\n"))
}

#' Message with provenance
#'
#' Print message to output with a prefix including data and time, and calling function or full
#' call stack
#'
#' If `msg` is called directly from the console, it will print `[interactive>]` in place of
#'   the call stack.
#' `msg0`, similar to `paste0`, is `msg2(..., sep = "")`
#'
# Add following to each function using \code{msg}:
# \code{current <- as.list(sys.call())[[1]]}
#'
#' @param ... Message to print
#' @param date Logical: if TRUE, include date and time in the prefix
#' @param caller Character: Name of calling function
#' @param call_depth Integer: Print the system call path of this depth.
#' @param caller_id Integer: Which function in the call stack to print
#' @param newline_pre Logical: If TRUE begin with a new line.
#' @param newline Logical: If TRUE end with a new line.
#' @param as.message Logical: if TRUE, print using `message()`
#' @param color Color fn
#' @param sep Character: Use to separate objects in `...`
#'
#' @return Invisibly: List with call, message, and date
#' @author EDG
#' @keywords internal
#' @noRd
msg2 <- function(...,
                 date = rtemis_date,
                 caller = NULL,
                 call_depth = 1,
                 caller_id = 1,
                 newline_pre = FALSE,
                 newline = TRUE,
                 format_fn = plain,
                 sep = " ") {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call_depth <- call_depth + caller_id
      if (call_depth > stack.length) call_depth <- stack.length
      caller <- paste(lapply(
        rev(seq(call_depth)[-seq(caller_id)]),
        function(i) rev(callStack.list)[[i]][[1]]
      ), collapse = ">>")
    }
    # do.call and similar will change the call stack, it will contain the full
    # function definition instead of the name alone
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }

  txt <- Filter(Negate(is.null), list(...))
  if (newline_pre) message("")
  if (date) {
    .dt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(gray(paste0(.dt, gray(" "))), appendLF = FALSE)
  }
  message(format_fn(paste(txt, collapse = sep)), appendLF = FALSE)
  if (!is.null(caller) && !is.na(caller)) {
    message(gray(" [", caller, "]", sep = ""))
  } else if (newline) {
    message("")
  }
} # rtemis::msg2


msg20 <- function(...,
                  caller = NULL,
                  call_depth = 1,
                  caller_id = 1,
                  newline_pre = FALSE,
                  newline = TRUE,
                  format_fn = plain,
                  sep = "") {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call_depth <- call_depth + caller_id
      if (call_depth > stack.length) call_depth <- stack.length
      caller <- paste(lapply(
        rev(seq(call_depth)[-seq(caller_id)]),
        function(i) rev(callStack.list)[[i]][[1]]
      ), collapse = ">>")
    }
    # do.call and similar will change the call stack, it will contain the full
    # function definition instead of the name alone
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }

  txt <- Filter(Negate(is.null), list(...))
  .dt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (newline_pre) message("")
  message(gray(paste0(.dt, gray(" "))), appendLF = FALSE)
  message(format_fn(paste(txt, collapse = sep)), appendLF = FALSE)
  if (!is.null(caller) && !is.na(caller)) {
    message(gray(" [", caller, "]", sep = ""))
  } else if (newline) {
    message("")
  }
} # rtemis::msg20


#' Pad-cat
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' {
#'   msg2("Hello")
#'   pcat("super", "wow")
#'   pcat(NULL, "oooo")
#' }
#' }
pcat <- function(left, right, pad = 17, newline = TRUE) {
  lpad <- max(0, pad - 1 - max(0, nchar(left)))
  cat(pad(left), right)
  if (newline) cat("\n")
}

pad <- function(x, target = 17, char = " ") {
  lpad <- max(0, target - max(0, nchar(x)))
  paste0(
    paste(rep(char, lpad), collapse = ""), x
  )
}


#' msg2start
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msg2start <- function(...,
                      #  date = TRUE,
                      #  newline = TRUE,
                      #  extraline = FALSE,
                      newline_pre = FALSE,
                      #  as.message = FALSE,
                      #  color = NULL,
                      sep = " ") {
  txt <- Filter(Negate(is.null), list(...))
  .dt <- format(Sys.time(), "%m-%d-%y %H:%M:%S")
  if (newline_pre) cat("\n")
  cat(gray(paste0(.dt, gray(" "))))
  cat(paste(txt, collapse = sep))
} # rtemis::msg2start


#' msg2done
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msg2done <- function(caller = NULL,
                     call_depth = 1,
                     caller_id = 1,
                     sep = " ") {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call_depth <- call_depth + caller_id
      if (call_depth > stack.length) call_depth <- stack.length
      caller <- paste(lapply(
        rev(seq(call_depth)[-seq(caller_id)]),
        function(i) rev(callStack.list)[[i]][[1]]
      ), collapse = ">>")
    }
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }
  cat(" ")
  yay(end = "")
  cat(gray("[", caller, "]\n", sep = ""), sep = "")
} # rtemis::msg2done
