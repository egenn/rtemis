# printls.R
# ::rtemis::
# 2016 E.D. Gennatas lambdamd.org

#' Pretty print list
#'
#' Pretty print a list (or data frame) recursively
#'
#' Data frames in R began life as lists
#' @param x list or object that will be converted to a list
#' @param prefix Character: Optional prefix for names
#' @param pad Integer: Pad output with this many spaces. Default = 2
#' @param center.title Logical: If TRUE, autopad title for centering, if present. Default TRUE
#' @param color \code{crayon} color to be applied when printing values. Default = NULL (do not use \code{crayon})
#' @author E.D. Gennatas
#' @export

printls <- function(x,
                    prefix = "",
                    pad = 3,
                    center.title = TRUE,
                    title = NULL,
                    title.newline = FALSE,
                    newline.pre = FALSE,
                    color = NULL) {
    if (newline.pre) cat("\n")
    if (is.null(x)) {
        if (!is.null(title)) {
            boxcat(title,
                pad = pad, newline = title.newline,
                newline.pre = FALSE
            )
        }
        cat(paste0(rep(" ", pad), collapse = ""), "NULL", sep = "")
    } else if (length(x) == 0) {
        cat(class(x), "of length 0\n")
    } else {
        x <- as.list(x)
        # Remove closures that will cause error
        is.fn <- which(sapply(x, is.function))
        if (any(is.fn)) for (i in is.fn) x[[i]] <- NULL

        null.index <- sapply(x, is.null)
        x[null.index] <- "NULL"
        xnames <- names(x)
        lhs <- max(nchar(paste0(prefix, xnames))) + pad
        if (!is.null(title)) {
            title.pad <- if (center.title) {
                max(0, lhs - round((.5 * nchar(title))) - 3)
            } else {
                0
            }
            boxcat(title,
                pad = title.pad, newline = title.newline,
                newline.pre = FALSE
            )
        }
        for (i in seq(x)) {
            if (is.list(x[[i]])) {
                if (length(x[[i]]) == 0) {
                    if (is.null(color)) {
                        cat(paste0(
                            format(paste0(
                                prefix, xnames[i]
                            ), width = lhs, justify = "right"),
                            ": (empty list)"
                        ), "\n")
                    } else {
                        cat(paste0(
                            format(paste0(prefix, xnames[i]),
                                width = lhs, justify = "right"
                            ), ": ",
                            color("(empty list)"), "\n"
                        ))
                    }
                } else {
                    cat(paste0(format(paste0(prefix, xnames[i]),
                        width = lhs, justify = "right"
                    ), ": "), "\n")
                    printls(x[[i]], pad = lhs + 2)
                }
            } else {
                if (is.null(color)) {
                    cat(paste0(
                        format(paste0(prefix, xnames[i]),
                            width = lhs, justify = "right"
                        ), ": ",
                        headdot(x[[i]])
                    ), "\n")
                } else {
                    cat(paste0(
                        format(paste0(prefix, xnames[i]),
                            width = lhs, justify = "right"
                        ), ": ",
                        color(headdot(x[[i]])), "\n"
                    ))
                }
            }
        }
    }
} # rtemis::printls


# printdf1
# ::rtemis::
# 2016 lambdamd.org
#' Print 1 x N data frame
#'
#' Pretty print a data frame containing 1 row of data with named columns as a vertical list
#'   of "       name : value"
#'      " other.name : other.value"
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces. Default = 2
#' @author E.D. Gennatas
#' @export

printdf1 <- function(x, pad = 2) {
    x <- as.data.frame(x)
    # df <- data.frame(Parameter = c(names(x)), Value = unlist(x), row.names = NULL)

    xnames <- colnames(x)
    lhs <- max(nchar(xnames)) + pad

    for (i in seq(ncol(x))) {
        cat(paste(format(xnames[i], width = lhs, justify = "right"), ":", x[1, i]), "\n")
    }
} # rtemis::printdf1


cpad <- function(x, length = NULL, adjust = c("right", "left")) {
    adjust <- match.arg(adjust)
    if (is.null(length)) {
        cat(x)
    } else {
        reps <- max(0, length - nchar(x))
        if (adjust == "right") {
            paste0(paste0(rep(" ", reps), collapse = ""), x)
        } else {
            paste0(x, paste0(rep(" ", reps), collapse = ""))
        }
    }
} # rtemis::cpad


#' Print data frame
#'
#' Pretty print a data frame
#'
#' By design, numbers will not be justified, but using ddSci.dp will convert to characters,
#' which will be justified. This is intentional for internal use.
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces. Default = 2
#' @param spacing Integer: Number of spaces between columns. Default = 1
#' @param ddSci.dp Integer: Number of decimal places to print using \link{ddSci}. Default = NULL for no
#' formatting
#' @param transpose Logical: If TRUE, transpose \code{x} before printing. Default = FALSE
#' @param justify Character: "right", "left". Default = "right"
#' @param colnames Logical: If TRUE, print column names. Default = TRUE
#' @param rownames Logical: If TRUE, print row names. Default = TRUE
#' @param column.col \code{crayon} color for printing column names. Default = \pkg{rtemis} default highlight
#' @param row.col \code{crayon} color for printing row names. Default = silver
#' @param newline.pre Logical: If TRUE, print a new line before printing data frame. Default = FALSE
#' @param newlin Logical: If TRUE, print a new line after printing data frame. Default = FALSE
#' @author E.D. Gennatas
#' @export

printdf <- function(x,
                    pad = 0,
                    spacing = 1,
                    ddSci.dp = NULL,
                    transpose = FALSE,
                    justify = "right",
                    colnames = TRUE,
                    rownames = TRUE,
                    column.col = rtHighlight$bold,
                    row.col = silver,
                    newline.pre = FALSE,
                    newline = FALSE) {

    if (transpose) x <- as.data.frame(t(x))
    xnames <- colnames(x)
    xrownames <- gsub(pattern = "\\.", replacement = " ", rownames(x))
    if (!is.null(ddSci.dp)) {
        xf <- as.data.frame(matrix(ddSci(x, decimal.places = ddSci.dp), NROW(x)))
        colnames(xf) <- xnames
        rownames(xf) <- xrownames
        x <- xf
    }

    col.char <- sapply(seq(xnames), \(i) {
        max(nchar(as.character(x[, i])), nchar(xnames[i]))
    })

    xrownames.spacing <- if (rownames) max(nchar(xrownames)) + pad else pad
    spacer <- paste0(rep(" ", spacing), collapse = "")
    if (newline.pre) cat("\n")
    if (colnames) {
        cat(paste0(rep(" ", xrownames.spacing), collapse = ""))
        if (justify == "left") cat(spacer)
        for (i in seq(NCOL(x))) {
            cat(column.col(format(xnames[i],
                width = col.char[i] + spacing,
                justify = justify
            )))
        }
        cat("\n")
    }

    # cat(silver$bold(" ]]\n"))
    if (rownames) {
        for (i in seq(NROW(x))) {
            # cat(row.col(cpad(xrownames[i], xrownames.spacing)))
            cat(row.col(format(xrownames[i],
                width = xrownames.spacing,
                justify = "right"
            )))
            for (j in seq(NCOL(x))) {
                cat(spacer,
                    paste(format(x[i, j], width = col.char[j], 
                    justify = justify)),
                    sep = ""
                )
            }
            cat("\n")
        }
    } else {
        for (i in seq(NROW(x))) {
            for (j in seq(NCOL(x))) {
                cat(spacer,
                    paste(format(x[i, j], width = col.char[j], 
                    justify = justify)),
                    sep = ""
                )
            }
            cat("\n")
        }
    }
    if (newline) cat("\n")
} # rtemis::printdf

printtable <- function(x, spacing = 2, pad = 2) {
    dimnames <- names(attr(x, "dimnames"))
    class.names <- attr(x, "dimnames")$Reference
    n.classes <- NCOL(x)
    mat <- matrix(c(x), NROW(x))
    colnames(mat) <- colnames(x)
    rownames(mat) <- rownames(x)
    # Column width without spacing
    col.width <- sapply(seq(class.names), \(i) {
        max(nchar(as.character(x[, i])), nchar(class.names[i]))
    })
    lhspad <- max(nchar(class.names), nchar(dimnames[1])) + spacing + pad
    # Top dimname
    cat(bold(format(dimnames[2],
        width = lhspad + nchar(dimnames[2]),
        justify = "right"
    )), "\n")
    # Left dimname
    cat(bold(format(dimnames[1], width = lhspad - spacing, justify = "right")))
    cat(paste0(rep(" ", spacing), collapse = ""))
    for (i in seq(n.classes)) {
        cat(rtHighlight$bold(format(class.names[i],
            width = col.width[i] + spacing, justify = "left"
        )))
    }

    printdf(mat,
        pad = lhspad - max(nchar(class.names)) - spacing,
        colnames = FALSE,
        row.col = rtHighlight$bold, newline.pre = TRUE,
        spacing = spacing
    )
} # rtemis::printtable


pastels <- function(x, bullet = "  -") {
    paste(paste(bullet, x, collapse = "\n"), "\n")
}


headdot <- function(x, maxlength = 9) {
    if (length(x) < maxlength) {
        paste(x, collapse = ", ")
    } else {
        paste0(paste(head(x, n = maxlength), collapse = ", "), "...")
    }
}

# twocol2html.R
# ::rtemis::
# 2020 E.D. Gennatas lambdamd.org

#' Create html table from a data.frame with 2 columns: Var name, and Coefs
#'
#' @examples
#' \dontrun{
#' x <- data.frame(
#'     ID = c("Int", paste0("V", 1:10)),
#'     Coef = rnorm(11)
#' )
#' twocol2html(x)
#' }
#' @keywords internal

twocol2html <- function(x,
                        font.family = "'Lato'",
                        font.col = "#ffffff",
                        font.size = "18px",
                        header.bg = "#404040",
                        table.bg = "#7F7F7F",
                        dat.col = rep("#525252", NROW(x)), # get color grad using all tables
                        dat.font.col = "#ffffff",
                        height = "50px",
                        # header
                        head.padding = "5px",
                        # table
                        dat.padding = "5px") {

    # 1. table style ----
    tablestyle <- paste0(
        '<table style="font-family: ', font.family,
        ", sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color:",
        font.col, "; font-size: ", font.size,
        "; padding: 0px; text-align: right; background-color: ",
        table.bg, "; width: auto; border-top-style: none; border-bottom-style: none; overflow-y: scroll; height: ",
        height, '; display: box">'
    )

    # 2. header row ----
    header <- paste0(
        '<tr><th style="font-weight: bold; padding: ', head.padding,
        "; text-align: center;",
        "background-color: ", header.bg,
        '">', colnames(x)[1], '</th>
    <th style="font-weight: bold; padding:', head.padding,
        "; text-align: center;",
        "background-color: ", header.bg,
        '">',
        colnames(x)[2], "</th></tr>"
    )

    # 3. Data rows ----
    tab <- vector("character", NROW(x))
    for (i in seq(tab)) {
        # first column: variable name; second column: coefficient
        tab[i] <- paste0(
            "<tr><td>", x[i, 1],
            '</td><td style="color: ', dat.font.col,
            "; font-variant-numeric: tabular-nums; background-color: ",
            dat.col[i],
            "; padding: ", dat.padding,
            '">', ddSci(x[i, 2], 3), "</td></tr>"
        )
    }

    # '- convert minus to &minus;
    tab <- gsub(">-", ">&minus;", tab)
    tab <- paste(tab, collapse = "")

    # Combine
    paste(tablestyle, header, tab, "</table>", collapse = "")
} # rtemis::twocol2html

#' Print Size
#'
#' Get \code{NCOL(x)} and \code{NROW{x}}
#'
#' @param x R object (usually that inherits from matrix or data.frame)
#' @param verbose Logical: If TRUE, print NROW and NCOL to console.
#' @param newline Logical: If TRUE, end with new line character.
#'
#' @return vector of NROW, NCOL invisibly
#' @author E.D. Gennatas
#' @export

catsize <- function(x, verbose = TRUE, newline = TRUE) {
    .nrow <- NROW(x)
    .ncol <- NCOL(x)
    if (verbose) {
        if (inherits(x, c("matrix", "data.frame"))) {
            cat("There are", .nrow, "rows and", .ncol, "columns")
        } else if (inherits(x, "list")) {
            cat("There are", length(x), "elements")
        }
        if (newline) cat("\n")
    }
    invisible(c(.nrow, .ncol))
}
