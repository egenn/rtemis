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
    if (file.exists(log_file)) log_file <- paste0(log_file, "_1")
    sink(log_file, split = verbosity > 0L)
  }
  start_time <- proc.time()
  if (verbosity > 0L || !is.null(log_file)) {
    if (newline_pre) cat("\n")
    if (is.null(message)) {
      # Read first line from inst/resources/intro.utf8
      msg20(
        "\U1F47D",
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
  #   as.message = FALSE,
  sink_off = FALSE,
  #   color = gray,
  newline_pre = FALSE,
  verbosity = 1L
) {
  elapsed <- as.numeric(proc.time() - start_time)
  if (verbosity > 0L || sink_off) {
    if (newline_pre) cat("\n")
    msg20(
      gray(paste0(
        "Done in ",
        ddSci(elapsed[3] / 60),
        " minutes (",
        "Real: ",
        ddSci(elapsed[3]),
        "; User: ",
        ddSci(elapsed[1]),
        "; System: ",
        ddSci(elapsed[2]),
        ")."
      )),
      # as.message = as.message,
      caller_id = 2
    )
  }

  if (sink_off) {
    sink()
  }
  invisible(elapsed)
} # rtemis::outro


#' `rtemis-internals`: `data_summary`
#'
#' Print input data dimensions and test dimensions match
#'
#' @keywords internal
#' @noRd
data_summary <- function(
  x,
  y,
  x.test = NULL,
  y.test = NULL,
  type = NULL,
  testSet = TRUE
) {
  if (!is.null(type)) {
    padcat(paste(type, "Input Summary"), pad = 0)
  } else {
    padcat("Input Summary", pad = 0)
  }
  cat("Training features:", bold(NROW(x), "x", NCOL(x), "\n"))
  cat(" Training outcome:", bold(NROW(y), "x", NCOL(y), "\n"))

  if (testSet) {
    if (!is.null(x.test)) {
      cat("    Test features:", bold(NROW(x.test), "x", NCOL(x.test), "\n"))
    } else {
      cat("    Test features: Not available\n")
    }
    if (!is.null(y.test)) {
      cat("    Test outcome:", bold(NROW(y.test), "x", NCOL(y.test), "\n"))
    } else {
      cat("    Test outcome: Not available\n")
    }
  }
} # rtemis::data_summary

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
  cat(bold("  Input data summary:\n"))
  cat(
    "  Training set:",
    hilite(NROW(x)),
    "cases x",
    hilite(NCOL(x) - 1),
    "features.\n"
  )
  if (!is.null(dat_validation)) {
    cat(
      "Validation set:",
      hilite(NROW(dat_validation)),
      "cases x",
      hilite(NCOL(dat_validation) - 1),
      "features.\n"
    )
  }
  if (!is.null(dat_test)) {
    cat(
      "   Test set:",
      hilite(NROW(dat_test)),
      "cases x",
      hilite(NCOL(dat_test) - 1),
      "features.\n"
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
  cat(
    "  Input:",
    hilite(NROW(x)),
    "cases x",
    hilite(NCOL(x)),
    "features.\n"
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
  if (newline_pre) cat("\n")
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

    if (newline) cat("\n")
  }
} # rtemis::parameter_summary


#' `rtemis-internals`: `grid_summary`
#'
#' Pretty print list of parameters with more than one value,
#' which will be used in a grid search
#'
#' @keywords internal
#' @noRd
grid_summary <- function(...) {
  params <- list(...)
  xnames <- as.character(substitute(list(...)))[-1L]
  index <- sapply(params, function(x) length(x) > 1)
  tuned <- params[index]
  names(tuned) <- xnames[index]
  if (length(tuned) > 0) {
    padcat("Parameters to be tuned")
    printls(tuned)
    cat("\n")
  }
} # rtemis::grid_summary


#' `rtemis-internals`: `metrics_summary`
#'
#' Print Training and Test error
#'
#' @keywords internal
#' @noRd
metrics_summary <- function(error, algorithm = NULL, pre = NULL) {
  id <- deparse(substitute(error))
  if (class(error)[[1]] == "regError") {
    type <- "Regression"
  } else if (class(error)[[1]] == "surv_error") {
    type <- "Survival"
  } else {
    type <- "Classification"
  }
  # type <- if (class(error)[[1]] == "regError") "Regression" else "Classification"

  if (is.null(pre)) {
    if (grepl("train", id)) {
      pre <- "Training"
    } else if (grepl("test", id)) {
      pre <- "Test"
    } else {
      pre <- "Error"
    }
  }

  if (!is.null(algorithm)) {
    padcat(paste(algorithm, type, pre, "Summary"))
  } else {
    padcat(paste(type, pre, "Summary"))
  }
  print(error)
} # rtemis::metrics_summary


check_type <- function(type, allowed.types, algorithm) {
  if (!type %in% allowed.types) {
    rtStop(
      "You were attempting to perform",
      type,
      "but",
      algorithm,
      "only supports:\n",
      paste(allowed.types, collapse = ", ")
    )
  }
}

#' Initialize parallel processing and progress reporting
#'
#' @keywords internal
rtemis_init <- function(n_cores = 1, context = NULL, verbosity = 1L) {
  # Progress handlers
  if (is.null(live[["handlers_set"]])) {
    # progressr::handlers(global = TRUE)
    rtemis_progress <- getOption("rtemis_progress", "cli")
    progressr::handlers(rtemis_progress)
    live[["handlers_set"]] <- 1
    if (verbosity > 0L) {
      msg2("Progress handler set to", bold(rtemis_progress), color = magenta)
    }
  }
} # rtemis::rtemis_init
