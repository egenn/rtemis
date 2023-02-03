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


#' Long to wide key-value reshaping
#'
#' Reshape a long format \code{data.table} using key-value pairs with
#' \code{data.table::dcast}
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
#' @examples
#' \dontrun{
#' x <- data.table(
#'     ID = rep(1:3, each = 2),
#'     Dx = c("A", "C", "B", "C", "D", "A")
#' )
#' dt_keybin_reshape(x, id_name = "ID", key_name = "Dx")
#' }

dt_keybin_reshape <- function(x,
                              id_name,
                              key_name,
                              positive = 1,
                              negative = 0,
                              xname = NULL,
                              verbose = TRUE) {
    if (is.null(xname)) {
        xname <- deparse(substitute(x))
    }
    stopifnot(inherits(x, "data.table"))
    x <- copy(x)

    # Assign positive value to all in long form
    value_name <- "Bin__"
    x[, (value_name) := positive]

    .formula <- as.formula(paste(
        paste(id_name, collapse = " + "), 
        "~", key_name
    ))
    if (verbose) {
        msg2("Reshaping", hilite(xname), "to wide format...")
        catsize(x, "Input size")
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

    if (verbose) catsize(x, "Output size")
    x
} # rtemis::dt_keybin_reshape


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
                     how = "left",
                     left_name = NULL,
                     right_name = NULL,
                     left_suffix = NULL,
                     right_suffix = NULL,
                     verbose = TRUE, ...) {
    if (is.null(left_name)) left_name <- deparse(substitute(left))
    if (is.null(right_name)) right_name <- deparse(substitute(right))
    if (is.null(left_on)) left_on <- on
    if (is.null(right_on)) right_on <- on
    if (verbose) {
        icon <- switch(how,
            inner = "\u2A1D",
            left = "\u27D5",
            right = "\u27D6",
            "\u27D7"
        )
        if (left_on == right_on) {
            msg20(
                bold(green(icon)), " Merging ", hilite(left_name), " & ", hilite(right_name),
                " on ", hilite(left_on), "..."
            )
        } else {
            msg20(
                bold(green(icon)), " Merging ", hilite(left_name), " & ", hilite(right_name),
                " on ", hilite(left_on), " & ", hilite(right_on), "..."
            )
        }
        
        catsize(left, left_name)
        catsize(right, right_name)
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
        all.x = all.x, all.y = all.y, ...
    )
    if (verbose) {
        catsize(dat, "Merged")
    }
    dat
} # rtemis::dt_merge


#' Clean factor levels of data.table in-place
#'
#' Finds all factors in a data.table and cleans factor levels to include
#' only underscore symbols
#'
#' @param x data.table
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' x <- as.data.table(iris)
#' levels(x$Species) <- c("setosa:iris", "versicolor$iris", "virginica iris")
#' dt_set_cleanfactorlevels(x)
#' x
#' }
dt_set_cleanfactorlevels <- function(x) {
    stopifnot(inherits(x, "data.table"))
    idi <- names(x)[sapply(x, is.factor)]
    for (i in idi) {
        x[, (i) := factor(x[[i]], labels = clean_names(levels(x[[i]])))]
    }
} # rtemis::dt_set_cleanfactorlevels

#' Check if all levels in a column are unique
#' 
#' @param x data.frame or data.table
#' @param on Integer or character: column to check
#' 
#' @author E.D. Gennatas
#' @export
dt_check_unique <- function(x, on) {
    length(unique(x[[on]])) == NROW(x)
}

#' Get index of duplicate values
#'
#' @param x data.frame or data.table
#' @param on Integer or character: column to check
#'
#' @author E.D. Gennatas
#' @export
dt_get_duplicates <- function(x, on) {
    x[x[[on]] %in% x[[on]][duplicated(x[[on]])], ..on][[1]]
}


#' Index columns by attribute name & value
#' 
#' @param x data.frame or compatible
#' @param name Character: Name of attribute
#' @param name Character: Value of attribute
#' 
#' @author E.D. Gennatas
#' @export
dt_index_attr <- function(x, name, value) {
    colattr <- unlist(sapply(x, \(i) attr(i, name)))
    which(colattr == value)
}

#' Get N and percent match of values between two columns of two data.tables
#' 
#' @param x data.table
#' @param y data.table
#' @param on Integer or character: column to read in \code{x} and \code{y}, if it is the
#' same
#' @param left_on Integer or character: column to read in \code{x}
#' @param right_on Integer or character: column to read in \code{y}
#' @param verbose Logical: If TRUE, print messages to console
#' 
#' @author E.D. Gennatas
#' @export 
dt_pctmatch <- function(x, y,
    on = NULL, 
    left_on = NULL, 
    right_on = NULL, verbose = TRUE) {

    if (is.null(left_on)) left_on <- on
    if (is.null(right_on)) right_on <- on
    xv <- unique(x[[left_on]])
    n <- length(xv)
    yv <- unique(y[[right_on]])
    nmatch <- sum(xv %in% yv)
    matchpct <- nmatch / n * 100
    if (verbose) {
        by_final <- paste(unique(c(left_on, right_on)), collapse = ", ")
        msg20(
            "Matched ", hilite(nmatch), "/", hilite(n), " on ", bold(by_final),
            " (", hilite(ddSci(matchpct)), "%)"
        )
    }
    invisible(list(nmatch = nmatch, matchpct = matchpct))
    
}

#' Get percent of missing values from every column
#' 
#' @param x data.frame or data.table
#' @param verbose Logical: If TRUE, print messages to console
#' 
#' @author E.D. Gennatas
#' @export 
dt_pctmissing <- function(x, verbose = TRUE) {
    nmissing <- sapply(x, \(i) length(is.na(i)))
    pctmissing <- nmissing / NROW(x)
    if (verbose) {
        cat("Percet missing per column:\n")
        printls(nmissing)
    }
    invisible(list(nmissing = nmissing, pctmissing = pctmissing))
}
