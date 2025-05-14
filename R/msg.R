# msg.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

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
#' @param call.depth Integer: Print the system call path of this depth. Default = NULL
#' @param caller.id Integer: Which function in the call stack to print
#' @param newline Logical: If TRUE end with a new line. Default = FALSE
#' @param newline.pre Logical: If TRUE begin with a new line. Default = FALSE
#' @param as.message Logical: if TRUE, print using `message()`
#' @param color Color fn
#' @param sep Character: Use to separate objects in `...`
#'
#' @return Invisibly: List with call, message, and date
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
msg <- function(
  ...,
  date = TRUE,
  caller = NULL,
  call.depth = 1,
  caller.id = 1,
  newline = TRUE,
  extraline = FALSE,
  newline.pre = FALSE,
  as.message = FALSE,
  color = NULL,
  sep = " "
) {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- ">"
    } else {
      call.depth <- call.depth + caller.id
      if (call.depth > stack.length) call.depth <- stack.length
      caller <- paste(
        lapply(
          rev(seq(call.depth)[-seq(caller.id)]),
          function(i) rev(callStack.list)[[i]][[1]]
        ),
        collapse = ">>"
      )
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
      cat(gray(paste0("[", .dt, bold(caller), gray("] "))))
      cat(paste(txt, collapse = sep), if (newline) "\n")
    } else {
      cat(gray(paste0("[", .dt, bold(caller), gray("] "))))
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

#   msg2(..., date = date,
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
#' @keywords internal
#' @noRd
msg0 <- function(
  ...,
  date = TRUE,
  caller = NULL,
  call.depth = 1,
  caller.id = 1,
  newline = TRUE,
  extraline = FALSE,
  newline.pre = FALSE,
  as.message = FALSE,
  color = NULL,
  sep = ""
) {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- ">"
    } else {
      call.depth <- call.depth + caller.id
      if (call.depth > stack.length) call.depth <- stack.length
      caller <- paste(
        lapply(
          rev(seq(call.depth)[-seq(caller.id)]),
          function(i) rev(callStack.list)[[i]][[1]]
        ),
        collapse = ">>"
      )
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
      cat(gray(paste0("[", .dt, bold(caller), gray("] "))))
      cat(paste(txt, collapse = sep), if (newline) "\n")
    } else {
      cat(gray(paste0("[", .dt, bold(caller), gray("] "))))
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
  cat(
    bold(gray("[")),
    green("+++", bold(green(message))),
    bold(gray("]")),
    sep = ""
  )
}

info <- function(..., color = hilite) {
  msg2(..., color = color)
}

#' msg2
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msg2 <- function(
  ...,
  date = rtDate,
  caller = NULL,
  call.depth = 1,
  caller.id = 1,
  #  newline = TRUE,
  #  extraline = FALSE,
  newline.pre = FALSE,
  newline = TRUE,
  #  as.message = FALSE,
  #  color = NULL,
  sep = " "
) {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call.depth <- call.depth + caller.id
      if (call.depth > stack.length) call.depth <- stack.length
      caller <- paste(
        lapply(
          rev(seq(call.depth)[-seq(caller.id)]),
          function(i) rev(callStack.list)[[i]][[1]]
        ),
        collapse = ">>"
      )
    }
    # do.call and similar will change the call stack, it will contain the full
    # function definition instead of the name alone
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }

  txt <- Filter(Negate(is.null), list(...))
  if (newline.pre) cat("\n")
  if (date) {
    .dt <- format(Sys.time(), "%m-%d-%y %H:%M:%S")
    cat(gray(paste0(.dt, gray(" "))))
  }
  cat(paste(txt, collapse = sep))
  if (!is.null(caller) && !is.na(caller)) {
    cat(gray(" :", caller, "\n", sep = ""), sep = "")
  } else if (newline) {
    cat("\n")
  }
} # rtemis::msg2


msg20 <- function(
  ...,
  #  date = TRUE,
  caller = NULL,
  call.depth = 1,
  caller.id = 1,
  #  newline = TRUE,
  #  extraline = FALSE,
  newline.pre = FALSE,
  newline = TRUE,
  #  as.message = FALSE,
  #  color = NULL,
  sep = ""
) {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call.depth <- call.depth + caller.id
      if (call.depth > stack.length) call.depth <- stack.length
      caller <- paste(
        lapply(
          rev(seq(call.depth)[-seq(caller.id)]),
          function(i) rev(callStack.list)[[i]][[1]]
        ),
        collapse = ">>"
      )
    }
    # do.call and similar will change the call stack, it will contain the full
    # function definition instead of the name alone
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }

  txt <- Filter(Negate(is.null), list(...))
  .dt <- format(Sys.time(), "%m-%d-%y %H:%M:%S")
  if (newline.pre) cat("\n")
  cat(gray(paste0(.dt, gray(" "))))
  cat(paste(txt, collapse = sep))
  if (!is.null(caller) && !is.na(caller)) {
    cat(gray(" :", caller, "\n", sep = ""), sep = "")
  } else if (newline) {
    cat("\n")
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
    paste(rep(char, lpad), collapse = ""),
    x
  )
}


#' msg2start
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msg2start <- function(
  ...,
  #  date = TRUE,
  #  newline = TRUE,
  #  extraline = FALSE,
  newline.pre = FALSE,
  #  as.message = FALSE,
  #  color = NULL,
  sep = " "
) {
  txt <- Filter(Negate(is.null), list(...))
  .dt <- format(Sys.time(), "%m-%d-%y %H:%M:%S")
  if (newline.pre) cat("\n")
  cat(gray(paste0(.dt, gray(" "))))
  cat(paste(txt, collapse = sep))
} # rtemis::msg2start


#' msg2done
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msg2done <- function(caller = NULL, call.depth = 1, caller.id = 1, sep = " ") {
  if (is.null(caller)) {
    callStack.list <- as.list(sys.calls())
    stack.length <- length(callStack.list)
    if (stack.length < 2) {
      caller <- NA
    } else {
      call.depth <- call.depth + caller.id
      if (call.depth > stack.length) call.depth <- stack.length
      caller <- paste(
        lapply(
          rev(seq(call.depth)[-seq(caller.id)]),
          function(i) rev(callStack.list)[[i]][[1]]
        ),
        collapse = ">>"
      )
    }
    if (is.function(caller)) caller <- NULL
    if (is.character(caller)) if (nchar(caller) > 25) caller <- NULL
  }
  cat(" ")
  yay(end = "")
  cat(gray("[", caller, "]\n", sep = ""), sep = "")
} # rtemis::msg2done
