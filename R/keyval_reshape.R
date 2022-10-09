# keyval_reshape.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Long to wide key-value reshaping
#'
#' Reshape a long format \code{data.table} using key-value pairs with
#' \code{data.table:dcast}
#'
#' @param x A \code{data.table} object
#' @param id_name Character: Name of column in \code{x} that defines the IDs
#' identifying individual rows
#' @param key_name Character: Name of column in \code{x} that holds the key
#' @param value_name Character: Name of column in \code{x} thatholds the values
#' that correspond to the key
#' @param positive Numeric or Character: Used to fill id ~ key combination 
#' present in the long format input \code{x}
#' @param negative Numeric or Character: Used to fill id ~ key combination
#' NOT present in the long format input \code{x}
#' @param verbose Logical: If TRUE, print messages to the console
#' 
#' @author E.D. Gennatas
#' @export

keyval_reshape <- function(x,
                           id_name,
                           key_name,
                           value_name,
                           positive = 1,
                           negative = 0,
                           xname = NULL,
                           verbose = TRUE) {
    if (is.null(xname)) {
        xname <- deparse(substitute(x))
    }
    stopifnot(inherits(x, "data.table"))

    # Assign positive value to all in long form
    x[, (value_name) := positive]

    .formula <- as.formula(
        paste(id_name, "~", key_name)
    )
    if (verbose) {
        msg(xname, "long format dimensions: ", newline = FALSE)
        catsize(x)
        msg("Reshaping", xname, "to wide format...")
    }
    # Reshape to wide, filling all absent with negative value
    x <- dcast(
        x,
        .formula,
        fun.aggregate = length,
        value.var = value_name,
        drop = FALSE,
        fill = negative
    )

    if (verbose) {
        msg(xname, "wide format dimensions:", newline = FALSE)
        catsize(x)
    }

    invisible(x)
} # rtemis::keyval_reshape
