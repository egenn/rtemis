# rtfn.R
# ::rtemis::
# 2016-2022 EDG lambdamd.org

#' \pkg{rtemis} internal: Get Variable Name from Arguments
#'
#' Get the name of the variable passed as argument, limit number of characters in case of failure
#'
#' One way to test is to use \link{learn} with x.name = NULL, y.name = NULL
#'
#' @param x Variable whose name you want to extract
#' @param alt Character: If name derived from \code{deparse(substitute(x))} exceeds \code{max.nchar} characters, use this name instead
#' @param max.nchar Integer: Maximum N of characters to allow for name
#'
#' @author E.D. Gennatas
#' @keywords internal

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
#' @return The mode of \code{x}
#' @author E.D. Gennatas
#' @export
#' @examples
#' x <- c(9, 3, 4, 4, 0, 2, 2, NA)
#' get_mode(x)
#' x <- c(9, 3, 2, 2, 0, 4, 4, NA)
#' get_mode(x)
#' get_mode(x, getlast = FALSE)
#'
get_mode <- function(x,
                     na.exclude = TRUE,
                     getlast = TRUE,
                     retain.class = TRUE) {
    if (retain.class) .class <- class(x)
    if (na.exclude) x <- na.exclude(x)
    freq <- table(x)
    if (sum(freq) > 0) {
        if (getlast) {
            .vals <- unique(x)
            out <- .vals[rev(which(.vals %in% names(freq)[which(freq == max(freq))]))[1]]
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
#' @author E.D. Gennatas
#' @export

is_discrete <- function(x) {
    is.factor(x) || is.integer(x) || is.logical(x) || is.character(x)
} # rtemis::is_discrete


#' Logit transform
#'
#' @param x Float [0, 1] Input
#' @export

logit <- function(x) {
    log(x / (1 - x))
} # rtemis::logit


#' Inverse Logit
#'
#' @param x Float: Input data
#' @return The inverse logit of the input
#' @author E.D. Gennatas
#' @export

invlogit <- function(x) {
    exp(x) / (1 + exp(x))
} # rtemis::invlogit


#' Logistic function
#'
#' @param x Float: Input
#' @param x0 x-value of the midpoint. Default = 0
#' @param L maximum value. Default = 1
#' @param k steepness of the curve. Default = 1
#' @export

logistic <- function(x, x0 = 0, L = 1, k = 1) {
    L / (1 + exp(-k * (x - x0)))
} # rtemis::logistic


#' ReLU - Rectified Linear Unit
#'
#' @param x Numeric: Input
#' @export
relu <- function(x) {
    unlist(Map(function(i) max(0, i), x))
} # rtemis::relu


#' Softplus function
#'
#' Softplus function:
#' \deqn{log(1 + e^x)}
#' @param x Vector, Float: Input
#' @export

softplus <- function(x) {
    log(1 + exp(x))
} # rtemis::softplus


#' Sigmoid function
#'
#' @param x Vector, float: Input
#' @export

sigmoid <- function(x) 1 / (1 + exp(-x))


#' Softmax function
#'
#' @param x Vector, Float: Input
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

square <- function(x) x^2


#' Cube
#'
#' @param x Vector, Float: Input

cube <- function(x) x^3


#' Collapse data.frame to vector by getting column max
#'
#' @param x Matrix or Data frame input
#' @param na.rm Logical: passed to \code{max}, If TRUE, ignore NA values,
#' otherwise if NA is present in any column, NA will be returned. Default = TRUE
#' @author E.D. Gennatas
#' @export

colMax <- function(x, na.rm = TRUE) {
    apply(x, 2, function(i) max(i, na.rm = na.rm))
} # rtemis::colMax


#' Collapse data.frame to vector by getting row max
#'
#' @param x Input vector
#' @param na.rm Logical. If TRUE, missing values are not considered. Default = TRUE
#' @author E.D. Gennatas
#' @export
#'
rowMax <- function(x, na.rm = TRUE) {
    apply(x, 1, function(i) max(i, na.rm = na.rm))
} # rtrmis::rowMax


#' Combine rules
#'
#' @param ... Character: Rules
#' @author E.D. Gennatas
#' @export
#'

crules <- function(...) {
    rules <- c(...)
    paste0(rules, collapse = " & ")
} # rtemis::crules


#' Say No to \code{NULL}
#'
#' Returns the input, unless it is NULL, in which case it returns an empty vector / list, etc
#' of defined type
#'
#' This can be useful when creating S4, RC, or R6 objects
#'
#' @param x Input of any type, may be NULL
#' @param defType If \code{x} is NULL, return empty vector of this type. Options: list, numeric,
#' character, integer
#' @author E.D. Gennatas
#' @export

ifNotNull <- function(x, defType) {
    if (!is.null(x)) {
        return(x)
    } else {
        return(defType())
    }
} # rtemis::ifNotNull


# psd.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Population Standard Deviation
#'
#' Estimate the population standard deviation:
#' \deqn{sqrt(mean(x^2) - mean(x)^2)}
#'
#' This will be particularly useful when the machines finally collect data on all humans.
#' Caution is advised, however, as you never know how many may be hiding underground.
#'
#' @param x Numeric vector
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
#' @param seed Integer: Set seed for \code{rnorm}. Default = NULL
#' @author E.D. Gennatas
#' @export

rnormmat <- function(nrow = 10, ncol = 10,
                     mean = 0, sd = 1,
                     return.df = FALSE,
                     seed = NULL) {
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
#' @param nrow Integer: Number of rows. Default = 10
#' @param ncol Integer: Number of columns. Default = 10
#' @param min Float: Min Default = 0
#' @param max Float: Max. Default = 1
#' @param return.df Logical: If TRUE, return data.frame, otherwise matrix. Default = TRUE
#' @param seed Integer: Set seed for \code{rnorm}. Default = NULL
#' @author E.D. Gennatas
#' @export

runifmat <- function(nrow = 10, ncol = 10,
                     min = 0, max = 1,
                     return.df = FALSE,
                     seed = NULL) {
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
#' @export
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


singorplu <- function(n, x) {
    switch(as.character(n),
        `0` = paste0("no ", x, "s"),
        `1` = paste("1", x),
        paste0(n, " ", x, "s")
    )
}

#' Round to nearest .5
#'
#' @param x numeric vector
#' @author E.D. Gennatas
#' @export

roundtohalf <- function(x) {
    round(x * 2) / 2
}

roundtofrac <- function(x, t = .5) {
    round(x / t) * t
}


#' Print range of continuous variable
#'
#' @param x Numeric vector
#' @param ddSci Logical: If TRUE, use \link{ddSci} or range. Default = TRUE
#' @param decimal.places Integer: Number of decimal place to use if \code{ddSci = TRUE}. Default = 1
#' @param na.rm Logical: passed to \code{base::range}
#'
#' @author E.D. Gennatas
#' @export

catrange <- function(x,
                     ddSci = TRUE,
                     decimal.places = 1,
                     na.rm = TRUE) {
    if (ddSci) {
        paste(
            ddSci(range(x, na.rm = na.rm),
                decimal.places = decimal.places
            ),
            collapse = " to "
        )
    } else {
        paste(range(x, na.rm = na.rm), collapse = " to ")
    }
} # rtemis::catrange


#' \code{lsapply}
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

null2na <- function(x) {
    if (is.null(x)) NA else x
}

#' Get rtemis and OS version info
#'
#' @export
rtversion <- function() {
    out <- c(
        list(rtemis_version = as.character(packageVersion("rtemis"))),
        as.list(Sys.info())
    )
    printls(out)
    invisible(out)
} # rtemis::rtversion


popvar <- function(x) {
    mean((x - mean(x))^2)
}


#' Random \pkg{rtemis} art
#'
#' Draw random shapes and colors
#'
#' @param pch Point character to use
#' @param col Colors to use
#' @param text Text to print
#' @param text.col Color for text
#' @author E.D. Gennatas
#' @export

rtrandom <- function(pch = sample(15:18, 1),
                     col = rtCol3,
                     text = "rtemis",
                     text.col = "gray50",
                     text.as.legend = FALSE,
                     legend.bg = NULL,
                     legend.alpha = 1,
                     random.bg = TRUE) {
    x <- runif(100, 1, 50)
    y <- runif(100, 1, 50)
    alpha <- runif(100, .1, 1)
    color.alpha <- sapply(seq_along(col), function(x) adjustcolor(col[[x]], alpha[[x]]))
    bg <- col
    alpha <- runif(length(bg), .1, 1)
    bg.alpha <- sapply(seq_along(bg), function(x) adjustcolor(bg[[x]], alpha[[x]]))
    cex <- runif(length(col), 1, 10)
    par.orig <- par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(par.orig)))
    par(mar = c(0, 0, 0, 0))
    plot(x, y,
        pch = pch,
        col = color.alpha,
        bg = bg.alpha,
        cex = cex, ann = F, axes = F, lwd = 3
    )

    if (!text.as.legend) {
        text(
            x = sample(10:40, 1), y = sample(10:40, 1),
            labels = text, cex = 3, col = text.col
        )
    } else {
        legend(
            x = sample(10:40, 1), y = sample(10:40, 1),
            text,
            text.col = sample(col, 1),
            box.lty = 0,
            bg = adjustcolor(legend.bg, legend.alpha),
            cex = 2
        )
    }
} # rtemis::rtrandom


#' Filter order
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @examples
#' x <- rnorm(10)
#' x
#' x[filter_order(x, x < 0)]
filter_order <- function(x, idl, decreasing = FALSE) {
    idi <- which(idl)
    flt_ord <- order(x[idi], decreasing = decreasing)
    idi[flt_ord]
}

