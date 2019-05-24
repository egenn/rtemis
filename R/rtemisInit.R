# rtemisInit.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io

#' \code{rtemis-internals}: \code{intro}
#'
#' Intro
#'
#' Starts function execution timer and opens log file.
#' Pairs with \code{outro}. Unfortunately the errors and warnings (the stderr; sink(type = "message"))
#' cannot get split to file and console, so we keep in console only
#' @keywords internal
intro <- function(message = NULL,
                  verbose = TRUE,
                  as.message = FALSE,
                  color = silver,
                  logFile = NULL,
                  call.depth = 1,
                  newline.pre = FALSE) {

  if (!is.null(logFile)) {
    logFile <- normalizePath(logFile, mustWork = FALSE)
    outdir <- dirname(logFile)
    if (!dir.exists(outdir)) dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    if (file.exists(logFile)) logFile <- paste0(logFile, "_1")
    sink(logFile, split = verbose)
  }
  start.time <- proc.time()
  if (verbose | !is.null(logFile)) {
    if (newline.pre) cat("\n")
    if (is.null(message)) {
      msg("Hello,", Sys.getenv("USER"),
          as.message = as.message,
          call.depth = call.depth,
          caller.id = 2,
          color = color)
    } else {
      msg(message,
          as.message = as.message,
          call.depth = call.depth,
          sep = "",
          caller.id = 2,
          color = color)
    }
  }
  invisible(start.time)

} # rtemis::intro


#' \code{rtemis-internals}: \code{outro}
#'
#' Outro
#'
#' Stops function execution timer and closes log file.
#'
#' Second part to \code{intro}
#' @keywords internal
outro <- function(start.time,
                  message = NULL,
                  verbose = TRUE,
                  as.message = FALSE,
                  sinkOff = FALSE,
                  color = silver,
                  newline.pre = TRUE) {

  # sinkOff must be FALSE by default since there is no logging by default, would result in warning
  elapsed <- as.numeric(proc.time() - start.time)
  if (verbose | sinkOff) {
    if (newline.pre) cat("\n")
    msg("Run completed in ", ddSci(elapsed[3] / 60), " minutes (",
        "Real: ", ddSci(elapsed[3]), "; User: ", ddSci(elapsed[1]), "; System: ", ddSci(elapsed[2]), ")",
        sep = "", as.message = as.message, caller.id = 2, color = color)
  }

  if (sinkOff) {
    sink()
  }
  invisible(elapsed)

} # rtemis::outro


#' \code{rtemis-internals}: \code{dataSummary}
#'
#' Print input data dimensions and test dimensions match
#' @keywords internal
dataSummary <- function(x, y,
                        x.test = NULL, y.test = NULL,
                        type = NULL, testSet = TRUE) {
  # cat("------------------------------------------------------\n")
  # if (!is.null(type)) {
  #   cat(paste(type, "Input Summary\n"))
  # } else {
  #   cat("Input Summary\n")
  # }
  # cat("------------------------------------------------------\n")
  if (!is.null(type)) {
    boxcat(paste(type, "Input Summary"), pad = 0)
  } else {
    boxcat("Input Summary", pad = 0)
  }
  cat("   Training features:", bold(NROW(x), "x", NCOL(x), "\n"))
  cat("    Training outcome:", bold(NROW(y), "x", NCOL(y), "\n"))

  if (testSet) {
    if (!is.null(x.test)) {
      cat("    Testing features:", bold(NROW(x.test), "x", NCOL(x.test), "\n"))
    } else {
      cat("    Testing features: Not available\n")
    }
    if (!is.null(y.test)) {
      cat("     Testing outcome:", bold(NROW(y.test), "x", NCOL(y.test), "\n"))
    } else {
      cat("     Testing outcome: Not available\n")
    }
  }
} # rtemis::dataSummary


#' \code{rtemis-internals}: \code{parameterSummary}
#'
#' Pretty print a collection of objects
#'
#' Any number of R objects can be supplied. Their names and corresponding values will be printed line by line.
#' If input is a single data frame or list, its elements names and values will be printed.
#' If input includes multiple objects, any list's or data frame's elements' names will be printed with the
#' name of the list or data frame prepended.
#' @param ... Variables to print
#' @param title String: title
#' @param pad Integer: Pad for \link{printls}
#' @export
#' @author Efstathios D. Gennatas

parameterSummary <- function(...,
                             title = "Parameters",
                             pad = 0,
                             newline.pre = FALSE,
                             newline = FALSE) {

  if (newline.pre) cat("\n")
  if (length(list(...)) > 0) {
    x = list(...)
    xnames <- as.character(substitute(list(...)))[-1L]
    names(x) <- xnames
    for (i in seq(x)) {
      if (is.list(x[[i]]) && length(x[[i]]) > 0) {
        # x[[i]] <- as.list(x[[i]]) # because data.frame also gives is.list TRUE
        if (is.null(names(x[[i]]))) names(x[[i]]) <- paste0(xnames[i], ".", 1:length(x[[i]]))
      }
      if (is.null(x[[i]])) x[[i]] <- "NULL"
    }

    printls(x, pad = pad + 3, title = title, center.title = FALSE)

    if (newline) cat("\n")
  }

} # rtemis::parameterSummary


#' \code{rtemis-internals}: \code{gridSummary}
#'
#' Pretty print list of parameters with more than one value, which will be used in a grid search
#' @keywords internal
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


#' \code{rtemis-internals}: \code{errorSummary}
#'
#' Print Fit and Validation modError
#' @keywords internal
errorSummary <- function(error, mod.name = NULL, pre = NULL) {

  id <- deparse(substitute(error))
  if (class(error)[[1]] == "regError") {
    type <- "Regression"
  } else if (class(error)[[1]] == "survError") {
    type <- "Survival"
  } else {
    type <- "Classification"
  }
  # type <- if (class(error)[[1]] == "regError") "Regression" else "Classification"

if (is.null(pre)) {
  if (grepl('train', id)) {
    pre <- "Training"
  } else if (grepl('test', id)) {
    pre <- "Testing"
  } else {
    pre <- "Error"
  }
}

  if (!is.null(mod.name)) {
    boxcat(paste(toupper(mod.name), type, pre, "Summary"))
  } else {
    boxcat(paste(type, pre, "Summary"))
  }
  print(error)

} # rtemis::errorSummary


checkType <- function(type, allowed.types, mod.name) {

  if (!type %in% allowed.types) {
    rtStop("You were attempting to perform", type, "but", mod.name, "only supports", allowed.types)
  }

}
