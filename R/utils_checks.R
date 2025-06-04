# utils_checks.R
# ::rtemis::
# 2024- EDG rtemis.org

# clean_* functions performm checks and return clean inputs.
# check_* functions perform checks (do not return a value).

#' Check type of object
#'
#' @param x Object to check
#' @param fn Function to check against, any `is.*` function, e.g. `is.character`
#'
#' @return Logical
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' is_check("papaya", is.character) # TRUE
#' is_check(c(1, 2.5, 3.2), is.integer) # FALSE
#' is_check(iris, is.list) # TRUE
#' }
is_check <- function(x, fn) {
  if (!fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    message(red(bold(input), "is not", bold(type)))
    return(FALSE)
  }
  TRUE
} # /rtemis::is_check


#' Test type of object
#'
#' @inheritParams is_check
#'
#' @return NULL (invisibly)
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_test <- function(x, fn) {
  if (!is.null(x) && !fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    cli::cli_abort(bold(input), " is not ", bold(type))
  }
  invisible(NULL)
} # /rtemis::is_test


#' Check class of object
#'
#' @param x Object to check
#' @param cl Character: class to check against
#'
#' @return Logical
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' check_inherits("papaya", "character") # TRUE
#' check_inherits(c(1, 2.5, 3.2), "integer") # FALSE
#' check_inherits(iris, "list") # FALSE, compare to is_check(iris, is.list)
#' }
test_inherits <- function(x, cl) {
  if (!inherits(x, cl)) {
    input <- deparse(substitute(x))
    message(red(bold(input), "is not", bold(cl)))
    return(FALSE)
  }
  TRUE
} # /rtemis::test_inherits


#' Test class of object
#'
#' @inheritParams check_inherits
#'
#' @return NULL (invisibly)
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_inherits <- function(x, cl) {
  xname <- bold(underline(deparse(substitute(x))))
  if (!is.null(x) && !inherits(x, cl)) {
    input <- deparse(substitute(x))
    cli::cli_abort(
      hilite(xname),
      " must be of class ",
      bold(cl),
      ".",
      call. = FALSE
    )
  }
} # /rtemis::check_inherits


#' Function that returns object if it is of a certain class
#'
#' @param object Object to check and return
#' @param class Character vector: class(es) to check against
#' @param allow_null Logical: if TRUE, allows NULL objects
#'
#' @return Object
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' strict("papaya", "character") # "papaya"
#' strict(c(1, 2.5, 3.2), "integer") # Error
#' strict(iris, "list") # Error
#' }
strict <- function(object, class, allow_null = TRUE) {
  name. <- deparse(substitute(object))
  if (allow_null && is.null(object)) {
    return(NULL)
  }
  if (inherits(object, class)) {
    return(object)
  } else {
    cli::cli_abort(name., " must be ", bold(class))
  }
} # /rtemis::strict

#' Clean integer input
#'
#' @details
#' The goal is to return an integer vectors.
#' If the input is integer, it is returned as is.
#' If the input is numeric, it is coerced to integer only if the numeric values are integers,
#' otherwise an error is thrown.
#'
#' @param x Double or integer vector to check.
#'
#' @return Integer vector
#' @author EDG
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' clean_int(6L)
#' clean_int(3)
#' clean_int(12.1) # Error
#' clean_int(c(3, 5, 7))
#' clean_int(c(3, 5, 7.01)) # Error
#' }
clean_int <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (is.integer(x)) {
    return(x)
  } else if (is.numeric(x)) {
    if (all(x %% 1 == 0)) {
      return(as.integer(x))
    } else {
      cli::cli_abort(xname, " must be integer.")
    }
  } else if (is.null(x)) {
    return(NULL)
  }
  cli::cli_abort(xname, " must be integer.")
} # /rtemis::clean_int


#' Match Arguments Ignoring Case
#'
#' @param x Character: Argument to match.
#' @param choices Character vector: Choices to match against.
#'
#' @return Character: Matched argument.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' match_arg("papaya", c("AppleExtreme", "SuperBanana", "PapayaMaster"))
#' }
match_arg <- function(x, choices) {
  out <- match.arg(tolower(x), tolower(choices))
  grep(out, choices, value = TRUE, ignore.case = TRUE)
} # /rtemis::match_arg


#' Check logical
#'
#' @param x Vector to check
#'
#' @return nothing
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_logical <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(xname, " must not contain NAs.", call. = FALSE)
  }
  if (!is.logical(x)) {
    cli::cli_abort(xname, " must be logical.", call. = FALSE)
  }
} # /rtemis::check_logical

check_character <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(xname, " must not contain NAs.", call. = FALSE)
  }
  if (!is.character(x)) {
    cli::cli_abort(xname, " must be character.", call. = FALSE)
  }
} # /rtemis::check_character

check_floatpos <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x <= 0)) {
    cli::cli_abort(xname, " must be greater than 0.", call. = FALSE)
  }
} # /rtemis::check_floatpos

#' Check float between 0 and 1, exclusive
#'
#' @param x Vector to check
#'
#' @return Nothing, otherwise error.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' check_float01exc(0.5)
#' }
check_float01exc <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x < 0 | x > 1)) {
    cli::cli_abort(xname, " must be between 0 and 1, exclusive.", call. = FALSE)
  }
} # /rtemis::check_float01


#' Check float between 0 and 1, inclusive
#'
#' @param x Float vector.
#'
#' @return Nothing, otherwise error.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' check_float01inc(0.5)
#' }
check_float01inc <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(paste(xname, "must not contain NAs."))
  }
  if (any(x < 0 | x > 1)) {
    cli::cli_abort(paste(xname, " must be between 0 and 1, inclusive."))
  }
} # /rtemis::check_float01

check_floatpos1 <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(paste(xname, "must not contain NAs."))
  }
  if (any(x <= 0) || any(x > 1)) {
    cli::cli_abort(paste(
      xname,
      "must be greater than 0 and less or equal to 1."
    ))
  }
} # /rtemis::check_floatpos1

#' Check positive integer
#'
#' @param x Integer vector.
#'
#' @return x, otherwise error.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' clean_posint(5)
#' }
clean_posint <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (is.null(x)) {
    return(NULL)
  }
  if (anyNA(x)) {
    cli::cli_abort(paste(xname, "must not contain NAs."))
  }
  if (any(x <= 0)) {
    cli::cli_abort(paste(xname, "must contain only positive integers."))
  }
  clean_int(x)
} # /rtemis::clean_posint

check_float0pos <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    cli::cli_abort(paste(xname, "must not contain NAs."))
  }
  if (!is.null(x) && any(x < 0)) {
    cli::cli_abort(paste(xname, "must be zero or greater."))
  }
  if (!is.null(x) && any(x < 0)) {
    # cli::cli_abort(xname, " must be zero or greater.", call. = FALSE)
    cli::cli_abort(paste(xname, "must be zero or greater."))
  }
} # /rtemis::check_float0positive

#' Check future settings for learner
#'
#' Checks the proposed plan and number of workers and avoids overparallelization.
#'
#' @param learner Character: Name of learner.
#' @param plan Character: Name of future plan.
#' @param n_workers Integer: Number of workers.
#'
#' @return n_workers Integer n of workers.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_n_workers_for_learner <- function(
  algorithm,
  parallel_type,
  n_workers = NULL,
  verbosity = 1L
) {
  # If n_workers is not set, set it to available cores.
  # If learner uses parallelization and plan is run on single machine,
  # set n_workers to 1 to avoid overparallelization.
  single_machine_types <- c(
    "future::multicore",
    "future::callr",
    "future::multisession",
    "future.mirai::mirai_multisession",
    "mirai"
  )
  if (
    parallel_type %in%
      single_machine_types &&
      algorithm %in% live[["parallelized_learners"]]
  ) {
    if (verbosity > 0L && !is.null(n_workers) && n_workers > 1) {
      msg2(hilite2(
        "Running a parallelized learner and n_workers is greater than 1, but plan ",
        parallel_type,
        " is run on single machine. Setting n_workers to 1."
      ))
    }
    return(1L)
  }
  available_workers <- future::availableCores()
  if (!is.null(n_workers) && n_workers <= available_workers) {
    return(n_workers)
  } else {
    if (
      verbosity > 0L && !is.null(n_workers) && n_workers > available_workers
    ) {
      msg2(hilite2("Requested n_workers is greater than available cores."))
    }
  }
  max(future::availableCores() - 1L, 1L)
} # /rtemis::get_n_workers_for_learner


#' Abbreviate object class name
#'
#' @param x Object
#'
#' @return Character: Abbreviated class
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
abbreviate_class <- function(x, n = 4L) {
  paste0("<", abbreviate(class(x)[1], minlength = n), ">")
} # /rtemis::abbr_class


# check_dependencies.R
# ::rtemis::
# 2022 EDG rtemis.org
# replaced depCheck

#' \pkg{rtemis} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbosity Integer: Verbosity level.
#' Note: An error will always printed if dependencies are missing.
#' Setting this to FALSE stops it from printing
#' "Dependencies check passed".
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_dependencies <- function(..., verbosity = 0L) {
  ns <- as.list(c(...))
  err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))
  if (any(err)) {
    cli::cli_abort(
      paste0(
        "Please install the following ",
        ngettext(sum(err), "dependency", "dependencies"),
        ":\n",
        pastels(ns[err], bullet = "    -")
      )
    )
  } else {
    if (verbosity > 0L) msg2("Dependency check passed")
  }
} # rtemis::check_dependencies
