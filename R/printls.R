# printls.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Pretty print list
#'
#' Pretty print a list (or data frame) recursively
#'
#' Data frames in R began life as lists
#' @param x list or object that will be converted to a list
#' @param prefix String: Optional prefix for names
#' @param pad Integer: Pad output with this many spaces. Default = 2
#' @param center.title Logical: If TRUE, autopad title for centering, if present. Default TRUE
#' @param color \code{crayon} color to be applied when printing values. Default = NULL (do not use \code{crayon})
#' @author Efstathios D. Gennatas
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
    if (!is.null(title)) boxcat(title, pad = pad, newline = title.newline, newline.pre = FALSE)
    cat(paste0(rep(" ", pad), collapse = ""), "NULL", sep = "")
  } else {
    x <- as.list(x)
    null.index <- sapply(x, is.null)
    x[null.index] <- "NULL"
    xnames <- names(x)
    lhs <- max(nchar(paste0(prefix, xnames))) + pad
    if (!is.null(title)) {
      title.pad <- if (center.title) max(0, lhs - round((.5 * nchar(title))) - 3) else 0
      boxcat(title, pad = title.pad, newline = title.newline, newline.pre = FALSE)
    }
    for (i in seq(x)) {
      if (is.list(x[[i]])) {
        if (length(x[[i]]) == 0) {
          if (is.null(color)) {
            cat(paste0(format(paste0(prefix, xnames[i]), width = lhs, justify = "right"), ": (empty list)"), "\n")
          } else {
            cat(paste0(format(paste0(prefix, xnames[i]), width = lhs, justify = "right"), ": ", color("(empty list)"), "\n"))
          }
        } else {
          cat(paste0(format(paste0(prefix, xnames[i]), width = lhs, justify = "right"), ": "), "\n")
          printls(x[[i]], pad = lhs + 2)
        }
      } else {
        if (is.null(color)) {
          cat(paste0(format(paste0(prefix, xnames[i]), width = lhs, justify = "right"), ": ",
                     paste(x[[i]], collapse = ", ")), "\n")
        } else {
          cat(paste0(format(paste0(prefix, xnames[i]), width = lhs, justify = "right"), ": ",
                     color(paste(x[[i]], collapse = ", ")), "\n"))
        }
      }
    }
  }

} # rtemis::printls


# printdf1
# ::rtemis::
# 2016 egenn.github.io
#' Print 1 x N data frame
#'
#' Pretty print a data frame containing 1 row of data with named columns as a vertical list
#'   of "       name : value"
#'      " other.name : other.value"
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces. Default = 2
#' @author Efstathios D. Gennatas
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
#' @param justify String: "right", "left". Default = "right"
#' @param colnames Logical: If TRUE, print column names. Default = TRUE
#' @param rownames Logical: If TRUE, print row names. Default = TRUE
#' @param column.col \code{crayon} color for printing column names. Default = \pkg{rtemis} default highlight
#' @param row.col \code{crayon} color for printing row names. Default = silver
#' @param newline.pre Logical: If TRUE, print a new line before printing data frame. Default = FALSE
#' @param newlin Logical: If TRUE, print a new line after printing data frame. Default = FALSE
#' @author Efstathios D. Gennatas
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

  col.char <- sapply(seq(xnames), function(i) max(nchar(as.character(x[, i])), nchar(xnames[i])))

  xrownames.spacing <- if (rownames) max(nchar(xrownames)) + pad else pad
  spacer <- paste0(rep(" ", spacing), collapse = "")
  if (newline.pre) cat("\n")
  if (colnames) {
    cat(paste0(rep(" ", xrownames.spacing), collapse = ""))
    if (justify == "left") cat(spacer)
    for (i in seq(NCOL(x))) cat(column.col(format(xnames[i], width = col.char[i] + spacing,
                                                      justify = justify)))
    cat("\n")
  }

  # cat(silver$bold(" ]]\n"))
  if (rownames) {
    for (i in seq(NROW(x))) {
      # cat(row.col(cpad(xrownames[i], xrownames.spacing)))
      cat(row.col(format(xrownames[i], width = xrownames.spacing, justify = "right")))
      for (j in seq(NCOL(x))) cat(spacer, paste(format(x[i, j], width = col.char[j], justify = justify)), sep = "")
      cat("\n")
    }
  } else {
    for (i in seq(NROW(x))) {
      for (j in seq(NCOL(x))) cat(spacer, paste(format(x[i, j], width = col.char[j], justify = justify)), sep = "")
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
  col.width <- sapply(seq(class.names), function(i) max(nchar(as.character(x[, i])), nchar(class.names[i])))
  lhspad <- max(nchar(class.names), nchar(dimnames[1])) + spacing + pad
  # Top dimname
  cat(bold(format(dimnames[2], width = lhspad + nchar(dimnames[2]), justify = "right")), "\n")
  # Left dimname
  cat(bold(format(dimnames[1], width = lhspad - spacing, justify = "right")))
  cat(paste0(rep(" ", spacing), collapse = ""))
  for (i in seq(n.classes)) {
    cat(rtHighlight$bold(format(class.names[i], width = col.width[i] + spacing, justify = "left")))
  }

  printdf(mat,
          pad = lhspad - max(nchar(class.names)) - spacing,
          colnames = FALSE,
          row.col = rtHighlight$bold, newline.pre = TRUE,
          spacing = spacing)

} # rtemis::printtable


