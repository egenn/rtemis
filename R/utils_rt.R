# info
# ::rtemis::
# 2016- EDG rtemis.org

#' `rtemis-internals`: `intro`
#'
#' Intro
#'
#' Starts function execution timer and opens log file.
#' Pairs with `outro`. Unfortunately the errors and warnings (the stderr; sink(type = "message"))
#' cannot get split to file and console, so we keep in console only
#'
#' @keywords internal
#' @noRd
intro <- function(
  message = NULL,
  log_file = NULL,
  call_depth = 1,
  caller = NULL,
  newline_pre = FALSE,
  verbosity = 1L
) {
  if (!is.null(log_file)) {
    log_file <- normalizePath(log_file, mustWork = FALSE)
    outdir <- dirname(log_file)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (file.exists(log_file)) {
      log_file <- paste0(log_file, "_1")
    }
    sink(log_file, split = verbosity > 0L)
  }
  start_time <- proc.time()
  if (verbosity > 0L || !is.null(log_file)) {
    if (newline_pre) {
      cat("\n")
    }
    if (is.null(message)) {
      # Read first line from inst/resources/intro.utf8
      msg20(
        "Hello.",
        call_depth = call_depth,
        caller_id = 2,
        caller = caller
      )
    } else {
      msg2(
        message,
        call_depth = call_depth,
        sep = "",
        caller_id = 2,
        caller = caller
      )
    }
  }
  invisible(start_time)
} # rtemis::intro

# Function to output seconds if seconds < 60, otherwise output minutes
format_seconds <- function(seconds) {
  if (seconds < 60) {
    paste0(bold(ddSci(seconds)), " seconds")
  } else {
    paste0(bold(ddSci(round(seconds / 60))), " minutes")
  }
}

#' `rtemis-internals`: `outro`
#'
#' Outro
#'
#' Stops function execution timer and closes log file.
#'
#' Second part to `intro`
#'
#' @keywords internal
#' @noRd
outro <- function(
  start_time,
  message = NULL,
  sink_off = FALSE,
  #   color = gray,
  newline_pre = FALSE,
  real_user_system = FALSE,
  verbosity = 1L
) {
  elapsed <- as.numeric(proc.time() - start_time)
  if (verbosity > 0L || sink_off) {
    if (newline_pre) {
      cat("\n")
    }
    if (real_user_system) {
      msg20(
        paste0(
          "Done in ",
          format_seconds(elapsed[3]),
          " (",
          "Real:",
          ddSci(elapsed[3]),
          "/User:",
          ddSci(elapsed[1]),
          "/System:",
          ddSci(elapsed[2]),
          ")."
        ),
        caller_id = 2
      )
    } else {
      msg20(
        paste0(
          "Done in ",
          format_seconds(elapsed[3]),
          "."
        ),
        caller_id = 2
      )
    }
  }

  if (sink_off) {
    sink()
  }
  invisible(elapsed)
} # rtemis::outro


#' Summarize supervised inputs
#'
#' @param x data.frame or similar: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_test data.frame or similar: Test set data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
summarize_supervised_data <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL
) {
  # msg2("Input data summary:")
  msg20(
    if (!is.null(dat_validation)) "  ",
    "Training set: ",
    hilite(NROW(x)),
    " cases x ",
    hilite(NCOL(x) - 1),
    " features."
  )
  if (!is.null(dat_validation)) {
    msg20(
      "Validation set: ",
      hilite(NROW(dat_validation)),
      " cases x ",
      hilite(NCOL(dat_validation) - 1),
      " features."
    )
  }
  if (!is.null(dat_test)) {
    msg20(
      if (!is.null(dat_validation)) "  ",
      "    Test set: ",
      hilite(NROW(dat_test)),
      " cases x ",
      hilite(NCOL(dat_test) - 1),
      " features."
    )
  }
} # rtemis::summarize_supervised_data

#' Summarize unsupervised inputs
#'
#' @param x data.frame or similar: Training set data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
summarize_unsupervised_data <- function(x) {
  msg2(
    "Input:",
    hilite(NROW(x)),
    "cases x",
    hilite(NCOL(x)),
    "features."
  )
} # rtemis::summarize_unsupervised_data


#' `rtemis-internals`: `parameter_summary`
#'
#' Pretty print a collection of objects
#'
#' Any number of R objects can be supplied. Their names and corresponding values will be printed line by line.
#' If input is a single data frame or list, its elements names and values will be printed.
#' If input includes multiple objects, any list's or data frame's elements' names will be printed with the
#' name of the list or data frame prepended.
#' @param ... Variables to print
#' @param title Character: title
#' @param pad Integer: Pad for [printls]
#' @param newline_pre Logical: If TRUE, start with a newline.
#' @param newline Logical: If TRUE, end with anew (empty) line.
#'
#' @author EDG
#' @keywords internal
#' @noRd

parameter_summary <- function(
  ...,
  title = "Parameters",
  pad = 0,
  newline_pre = FALSE,
  newline = FALSE
) {
  if (newline_pre) {
    message()
  }
  if (length(list(...)) > 0) {
    x <- list(...)
    xnames <- as.character(substitute(list(...)))[-1L]
    names(x) <- xnames
    for (i in seq_along(x)) {
      if (is.list(x[[i]]) && length(x[[i]]) > 0) {
        if (is.null(names(x[[i]]))) {
          names(x[[i]]) <- paste0(
            xnames[i],
            ".",
            seq_len(length(x[[i]]))
          )
        }
      }
      if (is.null(x[[i]])) x[[i]] <- "NULL"
    }
    printls(x, pad = pad + 3, title = title, center_title = FALSE)
    if (newline) {
      message()
    }
  }
} # rtemis::parameter_summary
