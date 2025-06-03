# rt_ops.R
# ::rtemis::
# 2016-2023 EDG rtemis.org

#' \pkg{rtemis} internal: Get Variable Name from Arguments
#'
#' Get the name of the variable passed as argument, limit number of characters in case of failure
#'
#' One way to test is to use [learn] with x.name = NULL, y.name = NULL
#'
#' @param x Variable whose name you want to extract
#' @param alt Character: If name derived from `deparse(substitute(x))` exceeds `max.nchar` characters, use this name instead
#' @param max.nchar Integer: Maximum N of characters to allow for name
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

getName <- function(x, alt = "x", max.nchar = 20) {
  name <- deparse(substitute(x))

  if (nchar(name) > max.nchar) name <- alt

  name
} # rtemis::getName


#' Get the mode of a factor or integer
#'
#' Returns the mode of a factor or integer
#'
#' @param x Vector, factor or integer: Input data
#' @param na.exclude Logical: If TRUE, exclude NAs
#' @param getlast Logical: If TRUE, get
#' @param retain.class Logical: If TRUE, output is always same class as input
#'
#' @return The mode of `x`
#' @author E.D. Gennatas
#' @export
#' @examples
#' x <- c(9, 3, 4, 4, 0, 2, 2, NA)
#' get_mode(x)
#' x <- c(9, 3, 2, 2, 0, 4, 4, NA)
#' get_mode(x)
#' get_mode(x, getlast = FALSE)
#'
get_mode <- function(
  x,
  na.exclude = TRUE,
  getlast = TRUE,
  retain.class = TRUE
) {
  if (retain.class) .class <- class(x)
  if (na.exclude) x <- na.exclude(x)
  freq <- table(x)
  if (sum(freq) > 0) {
    if (getlast) {
      .vals <- unique(x)
      out <- .vals[rev(which(.vals %in% names(freq)[which(freq == max(freq))]))[
        1
      ]]
    } else {
      out <- names(freq)[which.max(freq)]
    }
    if (length(out) == 0) out <- NA
  } else {
    out <- NA
  }

  if (retain.class) {
    if (is.factor(x)) {
      out <- factor(out, levels = levels(x))
    } else {
      class(out) <- .class
    }
  }
  out
} # rtemis::get_mode


#' Check for constant columns
#'
#' Checks if any column of a data frame have zero variance
#'
#' @param x Input Data Frame
#' @author E.D. Gennatas
#' @export

any_constant <- function(x) {
  # var0 <- which(apply(x, 2, var) == 0)
  # if (length(var0) > 0) TRUE else FALSE
  constant.index <- which(apply(x, 2, function(x) all(duplicated(x)[-1L])))
  if (length(constant.index) > 0) TRUE else FALSE
} # rtemis::any_constant


#' Check if vector is constant
#'
#' @param x Vector: Input
#' @param skip_missing Logical: If TRUE, skip NA values before testing
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- rep(9, 1000000)
#' is_constant(x)
#' x[10] <- NA
#' is_constant(x)
#' is_constant(x, skip_missing = TRUE)
#' }
is_constant <- function(x, skip_missing = FALSE) {
  # all(duplicated(x)[-1L])
  if (skip_missing) x <- na.exclude(x)
  isTRUE(all(x == x[1]))
} # rtemis::is_constant


#' Check if variable is discrete (factor or integer)
#'
#' @param x Input
#'
#' @author E.D. Gennatas
#' @export

is_discrete <- function(x) {
  is.factor(x) || is.integer(x) || is.logical(x) || is.character(x)
} # rtemis::is_discrete


#' Logit transform
#'
#' @param x Float \[0, 1\] Input
#'
#' @export

logit <- function(x) {
  log(x / (1 - x))
} # rtemis::logit


#' Inverse Logit
#'
#' @param x Float: Input data
#'
#' @return The inverse logit of the input
#' @author E.D. Gennatas
#' @export

invlogit <- function(x) {
  exp(x) / (1 + exp(x))
} # rtemis::invlogit


#' Logistic function
#'
#' @param x Float: Input
#' @param x0 x-value of the midpoint.
#' @param L maximum value.
#' @param k steepness of the curve.
#'
#' @export

logistic <- function(x, x0 = 0, L = 1, k = 1) {
  L / (1 + exp(-k * (x - x0)))
} # rtemis::logistic


#' ReLU - Rectified Linear Unit
#'
#' @param x Numeric: Input
#'
#' @export
relu <- function(x) {
  unlist(Map(function(i) max(0, i), x))
} # rtemis::relu


#' Softplus function
#'
#' Softplus function:
#' \deqn{log(1 + e^x)}
#' @param x Vector, Float: Input
#'
#' @export

softplus <- function(x) {
  log(1 + exp(x))
} # rtemis::softplus


#' Sigmoid function
#'
#' @param x Vector, float: Input
#'
#' @export

sigmoid <- function(x) 1 / (1 + exp(-x))


#' Softmax function
#'
#' @param x Vector, Float: Input
#'
#' @export

softmax <- function(x) {
  logsumexp <- function(x) {
    y <- max(x)
    y + log(sum(exp(x - y)))
  }

  exp(x - logsumexp(x))
} # rtemis::softmax


#' Square
#'
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd

square <- function(x) x^2


#' Cube
#'
#' @param x Vector, Float: Input
#'
#' @keywords internal
#' @noRd

cube <- function(x) x^3


#' Collapse data.frame to vector by getting column max
#'
#' @param x Matrix or Data frame input
#' @param na.rm Logical: passed to `max`, If TRUE, ignore NA values,
#' otherwise if NA is present in any column, NA will be returned.
#'
#' @author E.D. Gennatas
#' @export

colMax <- function(x, na.rm = TRUE) {
  apply(x, 2, function(i) max(i, na.rm = na.rm))
} # rtemis::colMax


#' Collapse data.frame to vector by getting row max
#'
#' @param x Input vector
#' @param na.rm Logical. If TRUE, missing values are not considered.
#'
#' @author E.D. Gennatas
#' @export

rowMax <- function(x, na.rm = TRUE) {
  apply(x, 1, function(i) max(i, na.rm = na.rm))
} # rtrmis::rowMax


#' Combine rules
#'
#' @param ... Character: Rules
#'
#' @author E.D. Gennatas
#' @export

crules <- function(...) {
  rules <- c(...)
  paste0(rules, collapse = " & ")
} # rtemis::crules


#' Say No to `NULL`
#'
#' Returns the input, unless it is NULL, in which case it returns an empty vector / list, etc
#' of defined type
#'
#' This can be useful when creating S4, RC, or R6 objects
#'
#' @param x Input of any type, may be NULL
#' @param defType If `x` is NULL, return empty vector of this type. Options: list, numeric,
#' character, integer
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

ifNotNull <- function(x, defType) {
  if (!is.null(x)) {
    return(x)
  } else {
    return(defType())
  }
} # rtemis::ifNotNull


# psd.R
# ::rtemis::
# 2016 E.D. Gennatas rtemis.org

#' Population Standard Deviation
#'
#' Estimate the population standard deviation:
#' \deqn{sqrt(mean(x^2) - mean(x)^2)}
#'
#' This will be particularly useful when the machines finally collect data on all humans.
#' Caution is advised, however, as you never know how many may be hiding underground.
#'
#' @param x Numeric vector
#'
#' @return Population standard deviation
#' @author E.D. Gennatas
#' @export

psd <- function(x) {
  return(sqrt(mean(x^2) - mean(x)^2))
} # rtemis::psd


#' Random Normal Matrix
#'
#' Create a matrix or data frame of defined dimensions, whose columns are random normal vectors
#'
#' @param nrow Integer: Number of rows. Default = 10
#' @param ncol Integer: Number of columns. Default = 10
#' @param mean Float: Mean. Default = 0
#' @param sd Float: Standard deviation. Default = 1
#' @param return.df Logical: If TRUE, return data.frame, otherwise matrix. Default = TRUE
#' @param seed Integer: Set seed for `rnorm`. Default = NULL
#'
#' @author E.D. Gennatas
#' @export
rnormmat <- function(
  nrow = 10,
  ncol = 10,
  mean = 0,
  sd = 1,
  return.df = FALSE,
  seed = NULL
) {
  if (length(mean) < ncol) mean <- rep(mean, ncol / length(mean))
  if (length(sd) < ncol) sd <- rep(sd, ncol / length(sd))

  if (!is.null(seed)) set.seed(seed)
  mat <- sapply(seq_len(ncol), function(j) rnorm(nrow, mean = mean, sd = sd))
  if (return.df) mat <- as.data.frame(mat)
  mat
} # rtemis::rnormmat


#' Random Uniform Matrix
#'
#' Create a matrix or data frame of defined dimensions, whose columns are random uniform vectors
#'
#' @param nrow Integer: Number of rows.
#' @param ncol Integer: Number of columns.
#' @param min Float: Min.
#' @param max Float: Max.
#' @param return.df Logical: If TRUE, return data.frame, otherwise matrix.
#' @param seed Integer: Set seed for `rnorm`.
#'
#' @author E.D. Gennatas
#' @export
runifmat <- function(
  nrow = 10,
  ncol = 10,
  min = 0,
  max = 1,
  return.df = FALSE,
  seed = NULL
) {
  if (length(min) < ncol) min <- rep(min, ncol / length(min))
  if (length(max) < ncol) max <- rep(max, ncol / length(max))

  if (!is.null(seed)) set.seed(seed)
  mat <- sapply(seq_len(ncol), function(j) runif(nrow, min = min, max = max))
  if (return.df) mat <- as.data.frame(mat)
  mat
} # rtemis::runifmat


#' Construct an n-length vector of letters
#'
#' Returns an n-length vector of the latin alphabet, replicating for every 26 characters
#'
#' @param n Length of vector to return
#' @param caps Logical: If TRUE, return all caps
#'
#' @keywords internal
#' @noRd
rtLetters <- function(n = 100, caps = FALSE) {
  reps <- ceiling(n / 26)
  prtlet <- function(x = NULL) paste0(x, if (caps) LETTERS else letters)
  out <- NULL
  for (i in 1:reps) {
    out.length <- length(out)
    out <- c(out, prtlet(out[(out.length - 25):out.length]))
  }
  out[1:n]
} # rtemis::rtLetters


#' @keywords internal
#' @noRd
singorplu <- function(n, x) {
  switch(
    as.character(n),
    `0` = paste0("no ", x, "s"),
    `1` = paste("1", x),
    paste0(n, " ", x, "s")
  )
}

#' Round to nearest .5
#'
#' @param x numeric vector
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
roundtohalf <- function(x) {
  round(x * 2) / 2
}


#' @keywords internal
#' @noRd
roundtofrac <- function(x, t = .5) {
  round(x / t) * t
}


#' Print range of continuous variable
#'
#' @param x Numeric vector
#' @param ddSci Logical: If TRUE, use [ddSci] or range. Default = TRUE
#' @param decimal.places Integer: Number of decimal place to use if `ddSci = TRUE`. Default = 1
#' @param na.rm Logical: passed to `base::range`
#'
#' @author E.D. Gennatas
#' @export

catrange <- function(x, ddSci = TRUE, decimal.places = 1, na.rm = TRUE) {
  if (ddSci) {
    paste(
      ddSci(range(x, na.rm = na.rm), decimal.places = decimal.places),
      collapse = " to "
    )
  } else {
    paste(range(x, na.rm = na.rm), collapse = " to ")
  }
} # rtemis::catrange


#' `lsapply`
#'
#' @inheritParams base::lapply
#' @param outnames Character vector: Optional names to apply to output
#'
#' @export

lsapply <- function(X, FUN, ..., outnames = NULL, simplify = FALSE) {
  out <- if (simplify) sapply(X, FUN, ...) else lapply(X, FUN, ...)
  if (!is.null(outnames)) names(out) <- outnames
  out
} # rtemis::lsapply


#' @keywords internal
#' @noRd
null2na <- function(x) {
  if (is.null(x)) NA else x
}

#' Get rtemis and OS version info
#'
#' @export
rtversion <- function() {
  out <- c(
    list(rtemis_version = as.character(packageVersion("rtemisalpha"))),
    as.list(Sys.info())
  )
  printls(out)
  invisible(out)
} # rtemis::rtversion


#' @keywords internal
#' @noRd
popvar <- function(x) {
  mean((x - mean(x))^2)
}


#' Filter order
#'
#' @param x Input vector
#' @param idl Logical vector: Index of elements to filter
#' @param decreasing Logical: If TRUE, sort in descending order
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' x <- rnorm(10)
#' x
#' x[filter_order(x, x < 0)]
#' }
filter_order <- function(x, idl, decreasing = FALSE) {
  idi <- which(idl)
  flt_ord <- order(x[idi], decreasing = decreasing)
  idi[flt_ord]
}

#' @keywords internal
#' @noRd
pval_stars <- function(x) {
  cut(x, breaks = c(0, .001, .01, .05, 1), labels = c("***", "**", "*", ""))
}


#' Return object if it has length > 0
#'
#' @keywords internal
#' @noRd
iflengthy <- function(x) {
  if (length(x) > 0) x else NULL
}
