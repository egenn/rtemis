# dt_ops.R
# ::rtemis::
# 2022-3 EDG lambdamd.org

#' Number of unique values per feature
#'
#' @param x data.table
#' @param excludeNA Logical: If TRUE, exclude NA values
#'
#' @author E.D. Gennatas
#' @export

dt_Nuniqueperfeat <- function(x, excludeNA = FALSE) {
    sapply(x, \(i) uniqueN(i, na.rm = excludeNA))
} # rtemis::dt_Nuniqueperfeat

#' Merge data.tables
#'
#' @param left data.table
#' @param right data.table
#' @param on Character: Name of column to join on
#' @param left_on Character: Name of column on left table
#' @param right_on Character: Name of column on right table
#' @param how Character: Type of join: "inner", "left", "right", "outer".
#' @param left_suffix Character: If provided, add this suffix to all left column names, 
#' excluding on/left_on
#' @param right_suffix Character: If provided, add this suffix to all right column names,
#' excluding on/right_on
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#' @export

dt_merge <- function(left,
                     right,
                     on = NULL,
                     left_on = NULL,
                     right_on = NULL,
                     how = "inner",
                     left_suffix = NULL,
                     right_suffix = NULL,
                     verbose = TRUE) {
    leftname <- deparse(substitute(left))
    rightname <- deparse(substitute(right))
    if (is.null(left_on)) left_on <- on
    if (is.null(right_on)) right_on <- on
    if (verbose) {
        icon <- switch(how,
            inner = "\u2A1D",
            left = "\u27D5",
            right = "\u27D6",
            "\u27D7"
        )
        msg20(bold(green(icon)), " Merging ", hilite(leftname), " & ", hilite(rightname), "...")
        catsize(left, leftname)
        catsize(right, rightname)
    }

    if (how == "left") {
        all.x <- TRUE
        all.y <- FALSE
    } else if (how == "right") {
        all.x <- FALSE
        all.y <- TRUE
    } else if (how == "inner") {
        all.x <- FALSE
        all.y <- FALSE
    } else {
        all.x <- all.y <- TRUE
    }
    if (!is.null(left_suffix)) {
        left_names <- setdiff(names(left), left_on)
        setnames(left, left_names, paste0(left_names, left_suffix))
    }
    if (!is.null(right_suffix)) {
        right_names <- setdiff(names(right), right_on)
        setnames(right, right_names, paste0(right_names, right_suffix))
    }
    dat <- merge(left, right,
        by.x = left_on, by.y = right_on,
        all.x = all.x, all.y = all.y
    )
    if (verbose) {
        catsize(dat, "Merged")
    }
    dat

} # rtemis::dt_merge
