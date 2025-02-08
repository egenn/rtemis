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
#' @author EDG
#' @keywords internal
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
#' @author EDG
#' @keywords internal

is_test <- function(x, fn) {
  if (!is.null(x) && !fn(x)) {
    input <- deparse(substitute(x))
    type <- substr(deparse(substitute(fn)), 4, 99)
    stop(bold(input), " is not ", bold(type))
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
#' @author EDG
#' @keywords internal

check_inherits <- function(x, cl) {
  xname <- bold(underline(deparse(substitute(x))))
  if (!is.null(x) && !inherits(x, cl)) {
    input <- deparse(substitute(x))
    stop(hilite(xname), " must be of class ", bold(cl), ".", call. = FALSE)
  }
} # /rtemis::check_inherits


#' Function that returns object if it is of a certain class
#'
#' @param object Object to check and return
#' @param class Character vector: class(es) to check against
#' @param allow_null Logical: if TRUE, allows NULL objects
#'
#' @return Object
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' strict("papaya", "character") # "papaya"
#' strict(c(1, 2.5, 3.2), "integer") # Error
#' strict(iris, "list") # Error
#' }
strict <- function(object, class, allow_null = TRUE) {
  if (allow_null && is.null(object)) {
    return(NULL)
  }
  if (inherits(object, class)) {
    return(object)
  } else {
    stop(bold(input), " must be ", bold(cl))
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
#' @export
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
      stop(xname, " must be integer.")
    }
  } else if (is.null(x)) {
    return(NULL)
  }
  stop(xname, " must be integer.")
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
#' @keywords internal
check_logical <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (!is.logical(x)) {
    stop(xname, " must be logical.", call. = FALSE)
  }
} # /rtemis::check_logical

check_floatpos <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x <= 0)) {
    stop(xname, " must be greater than 0.", call. = FALSE)
  }
} # /rtemis::check_floatpos

#' Check float between 0 and 1, exclusive
#'
#' @param x Vector to check
#'
#' @return nothing
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' check_float01exc(0.5)
#' }
check_float01exc <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x < 0 | x > 1)) {
    stop(xname, " must be between 0 and 1, exclusive.", call. = FALSE)
  }
} # /rtemis::check_float01


#' Check float between 0 and 1, inclusive
#'
#' @param x Float vector
#'
#' @return Logical
#' @author EDG
#' @keywords internal
#' @examples
#' \dontrun{
#' check_float01inc(0.5)
#' }
check_float01inc <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x < 0 | x > 1)) {
    stop(xname, " must be between 0 and 1, inclusive.", call. = FALSE)
  }
} # /rtemis::check_float01

check_floatpos1 <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (any(x <= 0) || any(x > 1)) {
    stop(xname, " must be greater than 0 and less or equal to 1.", call. = FALSE)
  }
} # /rtemis::check_floatpos1

#' Check positive integer
#'
#' @param x Integer vector
#'
#' @return x, otherwise error
#' @author EDG
#' @keywords internal
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
    stop(xname, " must not contain NAs.")
  }
  if (any(x <= 0)) {
    stop(xname, " must contain only positive integers.")
  }
  clean_int(x)
} # /rtemis::clean_posint

check_float0pos <- function(x) {
  xname <- bold(underline(deparse(substitute(x))))
  if (anyNA(x)) {
    stop(xname, " must not contain NAs.", call. = FALSE)
  }
  if (!is.null(x) && any(x < 0)) {
    stop(xname, " must be zero or greater.", call. = FALSE)
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
get_n_workers_for_learner <- function(algorithm, plan, n_workers, verbosity = 1L) {
  # If learner uses parallelization and plan is run on single machine,
  # set n_workers to 1 to avoid overparallelization.
  single_machine_plans <- c("multisession", "multicore", "callr", "mirai_multisession")
  parallelized_learners <- c("LightCART", "LightGBM", "LightRF", "LightRuleFit", "Ranger")
  if (plan %in% single_machine_plans && algorithm %in% parallelized_learners) {
    if (verbosity > 0L && !is.null(n_workers) && n_workers > 1) {
      msg2(hilite2(
        "Running a parallelized learner and n_workers is greater than 1, but plan ", plan,
        " is run on single machine. Setting n_workers to 1."
      ))
    }
    n_workers <- 1L
  }
  if (is.null(n_workers) && !algorithm %in% parallelized_learners) {
    n_workers <- future::availableCores()
  }
  n_workers
} # /rtemis::get_n_workers_for_learner


common_errors <- list(
  "object '(.*)' not found" = "Check that the object exists and is spelled correctly.",
  "object of type 'closure' is not subsettable" = "Check that the object is a list or data.frame."
)
common_warnings <- list(
  "NAs introduced by coercion" = "Check that the input is of the correct type."
)
#' Do call with tryCatch and suggestion
#'
#' @param fn: Function to call.
#' @param args: List of arguments to pass to function.
#' @param pattern_suggestion: Named list of the form pattern = "suggestion". If the pattern is
#'  found in the error message, the suggestion is appended to the error message.
#'
#' @return: Result of function call.
#' @author EDG
#' @keywords internal
do_call <- function(
    fn,
    args,
    error_pattern_suggestion = NULL,
    warning_pattern_suggestion = NULL,
    call. = FALSE) {
  pat_sug <- c(common_errors, error_pattern_suggestion)
  wpat_sug <- c(common_warnings, warning_pattern_suggestion)
  fn_name <- deparse(substitute(fn))
  tryCatch(
    do.call(fn, args),
    error = function(e) {
      fnerr <- e$message
      errmsg <- paste0(fn_name, "() failed with error:\n\n", fnerr, "\n\n")
      for (pattern in names(pat_sug)) {
        suggestion <- pat_sug[[pattern]]
        if (grepl(pattern, fnerr)) {
          errmsg <- paste0(errmsg, bold(underline("Suggestion:"), italic(suggestion)))
        }
      }
      stop(errmsg, call. = call.)
    }, # /error
    warning = function(w) {
      fnwarn <- w$message
      warnmsg <- paste0(fn_name, "() generated a warning:\n\n", fnwarn, "\n\n")
      for (pattern in names(wpat_sug)) {
        suggestion <- wpat_sug[[pattern]]
        if (grepl(pattern, fnwarn)) {
          warnmsg <- paste0(warnmsg, bold(underline("Suggestion:"), italic(suggestion)))
        }
      }
      warning(warnmsg, call. = call.)
    } # /warning
  )
} # /rtemis::do_call

#' Abbreviate object class name
#'
#' @param x Object
#'
#' @return Character: Abbreviated class
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
#' @export

check_dependencies <- function(..., verbosity = 0L) {
  ns <- as.list(c(...))
  err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))
  if (any(err)) {
    stop(
      "Please install the following ", ngettext(sum(err), "dependency", "dependencies"), ":\n",
      pastels(ns[err], bullet = "    -")
    )
  } else {
    if (verbosity > 0L) msg2("Dependency check passed", as.message = FALSE)
  }
} # rtemis::check_dependencies
