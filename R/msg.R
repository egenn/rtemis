# msg.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

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
#' @param call.depth Integer: Print the system call path of this depth. Default = NULL
#' @param caller.id Integer: Which function in the call stack to print
#' @param newline Logical: If TRUE end with a new line. Default = FALSE
#' @param newline.pre Logical: If TRUE begin with a new line. Default = FALSE
#' @param as.message Logical: if TRUE, print using \code{message()}
#' @param color Crayon color for message e.g. \code{crayon::red}
#' @param sep Character: Use to separate objects in \code{...}
#' @return Invisibly: List with call, message, and date
#' @author Efstathios D. Gennatas
#' @export

msg <- function(...,
                date = TRUE,
                call.depth = 1,
                caller.id = 1,
                newline = FALSE,
                newline.pre = FALSE,
                as.message = FALSE,
                color = NULL,
                sep = " ") {

  callStack.list <- as.list(sys.calls())
  stack.length <- length(callStack.list)

  if (stack.length < 2) {
    .call <- "(interactive)"
  } else {
    call.depth <- call.depth + caller.id
    if (call.depth > stack.length) call.depth <- stack.length
    .call <- paste(lapply(rev(seq(call.depth)[-seq(caller.id)]),
                          function(i) rev(callStack.list)[[i]][[1]]), collapse = ">>")
  }

  # do.call and similar will change the call stack, it will contain the full function definition instead of
  # the name alone
  if (is.function(.call)) .call <- NULL
  if (is.character(.call)) if (nchar(.call) > 25) .call <- NULL
  if (!is.null(.call)) .call <- paste0(" ", .call)

  txt <- list(...)
  .dt <- if (date) paste0(as.character(Sys.time())) else NULL

  if (as.message) {
    message(paste0("[", .dt, .call, "] ", paste(txt, collapse = sep)))
  } else {
    if (newline.pre) cat("\n")
    if (is.null(color)) {
      cat(silver(paste0("[", .dt, bold(.call), "] ")))
      cat(paste(txt, collapse = sep), "\n")
    } else {
      cat(silver(paste0("[", .dt, bold(.call), "] ")))
      cat(paste(color(txt), collapse = sep), "\n")
    }
    if (newline) cat("\n")
  }

  invisible(list(call = .call,
                 txt = txt,
                 dt = .dt))
} # rtemis::msg


#' @rdname msg
#' @export
# The only issue this way is msg0 will not print (interactive) when used interactively,
# but who cares
msg0 <- function(...,
                 date = TRUE,
                 call.depth = 1,
                 caller.id = 1,
                 newline = FALSE,
                 newline.pre = FALSE,
                 as.message = FALSE,
                 color = NULL) {

  msg(..., date = date,
      call.depth = call.depth,
      caller.id = caller.id + 1,
      newline = newline,
      newline.pre = newline.pre,
      as.message = as.message,
      color = color,
      sep = "")

} # rtemis::msg0

stopQuietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

rtStop <- function(...) {
  message <- paste(...)
  cat(red("/// Stop:", message))
  stopQuietly()
}


rtWarning <- function(...) {
  message <- paste(...)
  cat(rtOrange("/// Warning:", message))
}


rtOut <- function(...) {
  message <- paste(...)
  cat(silver$bold("["), green("+++", green$bold(message)), silver$bold("]"), sep = "")
}
