# rtemisInit.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org

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
  verbose = TRUE,
  #   as.message = FALSE,
  #   color = gray,
  logFile = NULL,
  call.depth = 1,
  newline.pre = FALSE
) {
  if (!is.null(logFile)) {
    logFile <- normalizePath(logFile, mustWork = FALSE)
    outdir <- dirname(logFile)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (file.exists(logFile)) logFile <- paste0(logFile, "_1")
    sink(logFile, split = verbose)
  }
  start.time <- proc.time()
  if (verbose || !is.null(logFile)) {
    if (newline.pre) cat("\n")
    if (is.null(message)) {
      msg2(
        "Hello,",
        Sys.getenv("USER"),
        # as.message = as.message,
        call.depth = call.depth,
        caller.id = 2
      )
    } else {
      msg2(
        message,
        # as.message = as.message,
        call.depth = call.depth,
        sep = "",
        caller.id = 2
        # color = color
      )
    }
  }
  invisible(start.time)
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
  start.time,
  message = NULL,
  verbose = TRUE,
  #   as.message = FALSE,
  sinkOff = FALSE,
  #   color = gray,
  newline.pre = FALSE
) {
  elapsed <- as.numeric(proc.time() - start.time)
  if (verbose || sinkOff) {
    if (newline.pre) cat("\n")
    msg20(
      gray(paste0(
        "Completed in ",
        ddSci(elapsed[3] / 60),
        " minutes (",
        "Real: ",
        ddSci(elapsed[3]),
        "; User: ",
        ddSci(elapsed[1]),
        "; System: ",
        ddSci(elapsed[2]),
        ")"
      )),
      # as.message = as.message,
      caller.id = 2
    )
  }

  if (sinkOff) {
    sink()
  }
  invisible(elapsed)
} # rtemis::outro


#' `rtemis-internals`: `dataSummary`
#'
#' Print input data dimensions and test dimensions match
#'
#' @keywords internal
#' @noRd
dataSummary <- function(
  x,
  y,
  x.test = NULL,
  y.test = NULL,
  type = NULL,
  testSet = TRUE
) {
  if (!is.null(type)) {
    boxcat(paste(type, "Input Summary"), pad = 0)
  } else {
    boxcat("Input Summary", pad = 0)
  }
  cat("Training features:", bold(NROW(x), "x", NCOL(x), "\n"))
  cat(" Training outcome:", bold(NROW(y), "x", NCOL(y), "\n"))

  if (testSet) {
    if (!is.null(x.test)) {
      cat(" Testing features:", bold(NROW(x.test), "x", NCOL(x.test), "\n"))
    } else {
      cat(" Testing features: Not available\n")
    }
    if (!is.null(y.test)) {
      cat("  Testing outcome:", bold(NROW(y.test), "x", NCOL(y.test), "\n"))
    } else {
      cat("  Testing outcome: Not available\n")
    }
  }
} # rtemis::dataSummary


#' `rtemis-internals`: `parameterSummary`
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
#' @param newline.pre Logical: If TRUE, start with a newline. Default = TRUE
#' @param newline Logical: If TRUE, end with anew (empty) line. Default = FALSE
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

parameterSummary <- function(
  ...,
  title = "Parameters",
  pad = 0,
  newline.pre = FALSE,
  newline = FALSE
) {
  if (newline.pre) cat("\n")
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

    printls(x, pad = pad + 3, title = title, center.title = FALSE)

    if (newline) cat("\n")
  }
} # rtemis::parameterSummary


#' `rtemis-internals`: `gridSummary`
#'
#' Pretty print list of parameters with more than one value,
#' which will be used in a grid search
#'
#' @keywords internal
#' @noRd
gridSummary <- function(...) {
  params <- list(...)
  xnames <- as.character(substitute(list(...)))[-1L]
  index <- sapply(params, function(x) length(x) > 1)
  tuned <- params[index]
  names(tuned) <- xnames[index]
  if (length(tuned) > 0) {
    boxcat("Parameters to be tuned")
    printls(tuned)
    cat("\n")
  }
} # rtemis::gridSummary


#' `rtemis-internals`: `errorSummary`
#'
#' Print Fit and Validation mod_error
#'
#' @keywords internal
#' @noRd
errorSummary <- function(error, mod.name = NULL, pre = NULL) {
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
      pre <- "Testing"
    } else {
      pre <- "Error"
    }
  }

  if (!is.null(mod.name)) {
    boxcat(paste(mod.name, type, pre, "Summary"))
  } else {
    boxcat(paste(type, pre, "Summary"))
  }
  print(error)
} # rtemis::errorSummary


checkType <- function(type, allowed.types, mod.name) {
  if (!type %in% allowed.types) {
    rtStop(
      "You were attempting to perform",
      type,
      "but",
      mod.name,
      "only supports:\n",
      paste(allowed.types, collapse = ", ")
    )
  }
}

#' Initialize parallel processing and progress reporting
#'
#' @keywords internal

rtemis_init <- function(n.cores = 1, context = NULL, verbose = TRUE) {
  # Future plan ====
  # if (!is.null(context)) context <- paste0(context, ":")
  # if (n.cores > 1) {
  #     rtPlan <- getOption(
  #         "future.plan",
  #         ifelse(.Platform$OS.type == "unix", "multicore", "multisession")
  #     )
  #     future::plan(rtPlan)
  #     # rtenv$plan_set <- 1
  #     if (verbose) {
  #         msg2(context, "Future plan set to", bold(rtPlan),
  #             color = magenta,
  #             "with", bold(n.cores), "workers"
  #         )
  #     }
  # } else {
  #     future::plan("sequential")
  #     if (verbose) {
  #         msg2(context, "Future plan set to",
  #             bold("sequential"),
  #             color = magenta
  #         )
  #     }
  # }

  # Progress handlers
  if (is.null(rtenv$handlers_set)) {
    # progressr::handlers(global = TRUE)
    rtHandler <- getOption("rt.handler", "progress")
    progressr::handlers(rtHandler)
    rtenv$handlers_set <- 1
    if (verbose) {
      msg2("Progress handler set to", bold(rtHandler), color = magenta)
    }
  }
} # rtemis::rtemis_init

rtPlanInit <- function() {
  getOption(
    "future.plan",
    ifelse(.Platform$OS.type == "unix", "multicore", "multisession")
  )
}
rtCoresInit <- function() {
  getOption("rt.cores", .availableCores)
}
rtPaletteInit <- function() {
  getOption("rt.palette", "rtCol3")
}
rtThemeInit <- function() {
  getOption("rt.theme", "darkgraygrid")
}
rtFontInit <- function() {
  getOption("rt.font", "Helvetica")
}
# rtGridSearchLearnInit <- function() {
#     getOption("rt.gridSearchLearn", "future")
# }
# rtWarnInit <- function() {
#     getOption("rt.warn", 1)
# }
rtDateInit <- function() {
  getOption("rt.date", TRUE)
}
