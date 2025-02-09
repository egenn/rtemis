# print_ops.R
# ::rtemis::
# 2016-23 EDG rtemis.org

#' Pretty print list
#'
#' Pretty print a list (or data frame) recursively
#'
#' Data frames in R began life as lists
#'
#' @param x list or object that will be converted to a list.
#' @param prefix Character: Optional prefix for names.
#' @param pad Integer: Pad output with this many spaces.
#' @param center_title Logical: If TRUE, autopad title for centering, if present.
#' @param format_fn Formatting function.
#' @param print_class Logical: If TRUE, print abbreviated class of object.
#'
#' @author EDG
#' @keywords internal
#' @noRd

printls <- function(x,
                    prefix = "",
                    pad = 2,
                    item_format = bold,
                    maxlength = 4L,
                    center_title = TRUE,
                    title = NULL,
                    title_newline = FALSE,
                    newline_pre = FALSE,
                    format_fn_rhs = ddSci,
                    print_class = TRUE,
                    abbrev_class_n = 3L,
                    print_S4 = FALSE) {
  # Arguments ----
  if (newline_pre) cat("\n")
  if (is.null(x)) {
    if (!is.null(title)) {
      padcat(title,
        pad = pad, newline = title_newline,
        newline_pre = FALSE
      )
    }
    cat(paste0(rep(" ", pad), collapse = ""), "NULL", sep = "")
  } else if (length(x) == 0) {
    cat(class(x), "of length 0.\n")
  } else {
    x <- as.list(x)
    # Get class of each element
    classes_ <- sapply(x, class)
    # Remove closures that will cause error
    is_fn <- which(sapply(x, is.function))
    if (length(is_fn) > 0) {
      for (i in is_fn) x[[i]] <- paste0(as.character(head(deparse(x[[i]]), n = 1L)), "...")
    }
    # Remove NULLs
    null_index <- sapply(x, is.null)
    x[null_index] <- "NULL"
    xnames <- names(x)
    lhs <- max(nchar(paste0(prefix, xnames))) + pad
    if (!is.null(title)) {
      title.pad <- if (center_title) {
        max(0, lhs - round((.5 * nchar(title))) - 3)
      } else {
        0
      }
      padcat(title,
        pad = title.pad, newline = title_newline,
        newline_pre = FALSE
      )
    }
    for (i in seq_along(x)) {
      if (is.list(x[[i]])) {
        if (length(x[[i]]) == 0) {
          cat(paste0(
            item_format(format(paste0(prefix, xnames[i]),
              width = lhs, justify = "right"
            )), ": ",
            format_fn_rhs("(empty list)"), "\n"
          ))
        } else {
          cat(paste0(
            item_format(format(paste0(prefix, xnames[i]),
              width = lhs, justify = "right"
            )), ": "
          ), "\n")
          printls(x[[i]], pad = lhs + 2)
        }
      } else if (is.logical(x[[i]])) {
        cat(paste0(
          item_format(format(paste0(prefix, xnames[i]),
            width = lhs, justify = "right"
          )), ": ",
          if (print_class) gray(paste0("<", abbreviate("logical", abbrev_class_n), "> ")),
          ifelse(isTRUE(x[[i]]), "TRUE", "FALSE"), "\n"
        ))
      } else if (S7_inherits(x[[i]])) {
        cat(paste0(
          item_format(format(paste0(prefix, xnames[i]),
            width = lhs, justify = "right"
          )), ": "
        ), "\n")
        # Print S7 object
        print(x[[i]], pad = lhs + 2)
      } else if (isS4(x[[i]])) {
        cat(paste0(
          item_format(format(paste0(prefix, xnames[i]),
            width = lhs, justify = "right"
          )), ": "
        ))
        # Print S4 object
        if (print_S4) {
          cat("\n")
          print(x[[i]])
        } else {
          cat("(S4 object of class: '", class(x[[i]]), "')\n", sep = "")
        }
      } else {
        cat(paste0(
          item_format(format(paste0(prefix, xnames[i]),
            width = lhs, justify = "right"
          )), ": ",
          if (print_class) gray(paste0("<", abbreviate(classes_[[i]], abbrev_class_n), "> ")),
          headdot(x[[i]], maxlength = maxlength, format_fn = format_fn_rhs), "\n"
        ))
      }
    }
  }
} # /rtemis::printls

# printdf1
# ::rtemis::
# 2016 rtemis.org
#' Print 1 x N data frame
#'
#' Pretty print a data frame containing 1 row of data with named columns as a vertical list
#'   of "       name : value"
#'      " other.name : other.value"
#'
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces.
#'
#' @author EDG
#' @keywords internal
#' @noRd

printdf1 <- function(x, pad = 2) {
  x <- as.data.frame(x)
  # df <- data.frame(Parameter = c(names(x)), Value = unlist(x), row.names = NULL)

  xnames <- colnames(x)
  lhs <- max(nchar(xnames)) + pad

  for (i in seq_len(ncol(x))) {
    cat(paste(format(xnames[i], width = lhs, justify = "right"), ":", x[1, i]), "\n")
  }
} # /rtemis::printdf1


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
} # /rtemis::cpad


#' Print data frame
#'
#' Pretty print a data frame
#'
#' By design, numbers will not be justified, but using ddSci_dp will convert to characters,
#' which will be justified. This is intentional for internal use.
#' @param x data frame
#' @param pad Integer: Pad output with this many spaces.
#' @param spacing Integer: Number of spaces between columns.
#' @param ddSci_dp Integer: Number of decimal places to print using [ddSci]. Default = NULL for no
#' formatting
#' @param transpose Logical: If TRUE, transpose `x` before printing.
#' @param justify Character: "right", "left".
#' @param colnames Logical: If TRUE, print column names.
#' @param rownames Logical: If TRUE, print row names.
#' @param column_col Color fn for printing column names.
#' @param row_col Color fn for printing row names.
#' @param newline_pre Logical: If TRUE, print a new line before printing data frame.
#' @param newline Logical: If TRUE, print a new line after printing data frame.
#'
#' @author EDG
#' @keywords internal
#' @noRd


printdf <- function(x,
                    pad = 0,
                    spacing = 1,
                    ddSci_dp = NULL,
                    transpose = FALSE,
                    justify = "right",
                    colnames = TRUE,
                    rownames = TRUE,
                    column_col = hilite,
                    row_col = gray,
                    newline_pre = FALSE,
                    newline = FALSE) {
  if (transpose) x <- as.data.frame(t(x))
  xnames <- colnames(x)
  xrownames <- gsub(pattern = "\\.", replacement = " ", rownames(x))
  if (!is.null(ddSci_dp)) {
    xf <- as.data.frame(matrix(ddSci(x, decimal_places = ddSci_dp), NROW(x)))
    colnames(xf) <- xnames
    rownames(xf) <- xrownames
    x <- xf
  }

  col_char <- sapply(seq_along(xnames), \(i) {
    max(nchar(as.character(x[, i])), nchar(xnames[i]))
  })

  xrownames_spacing <- if (rownames) max(nchar(xrownames)) + pad else pad
  spacer <- paste0(rep(" ", spacing), collapse = "")
  if (newline_pre) cat("\n")
  if (colnames) {
    cat(paste0(rep(" ", xrownames_spacing), collapse = ""))
    if (justify == "left") cat(spacer)
    for (i in seq_len(NCOL(x))) {
      cat(column_col(format(xnames[i],
        width = col_char[i] + spacing,
        justify = justify
      )))
    }
    cat("\n")
  }

  # cat(gray(" ]]\n"))
  if (rownames) {
    for (i in seq_len(NROW(x))) {
      # cat(row_col(cpad(xrownames[i], xrownames_spacing)))
      cat(row_col(format(xrownames[i],
        width = xrownames_spacing,
        justify = "right"
      )))
      for (j in seq_len(NCOL(x))) {
        cat(spacer,
          paste(format(x[i, j],
            width = col_char[j],
            justify = justify
          )),
          sep = ""
        )
      }
      cat("\n")
    }
  } else {
    for (i in seq_len(NROW(x))) {
      for (j in seq_len(NCOL(x))) {
        cat(spacer,
          paste(format(x[i, j],
            width = col_char[j],
            justify = justify
          )),
          sep = ""
        )
      }
      cat("\n")
    }
  }
  if (newline) cat("\n")
} # /rtemis::printdf


#' @keywords internal
#' @noRd
printtable <- function(x, spacing = 2, pad = 2) {
  dimnames <- names(attr(x, "dimnames"))
  class.names <- attr(x, "dimnames")$Reference
  n.classes <- NCOL(x)
  mat <- matrix(c(x), NROW(x))
  colnames(mat) <- colnames(x)
  rownames(mat) <- rownames(x)
  # Column width without spacing
  col.width <- sapply(seq_along(class.names), \(i) {
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
  for (i in seq_len(n.classes)) {
    cat(hilite(format(class.names[i],
      width = col.width[i] + spacing, justify = "left"
    )))
  }

  printdf(mat,
    pad = lhspad - max(nchar(class.names)) - spacing,
    colnames = FALSE,
    row_col = hilite, newline_pre = TRUE,
    spacing = spacing
  )
} # /rtemis::printtable


#' @keywords internal
#' @noRd
pastels <- function(x, bullet = "  -") {
  paste(paste(bullet, x, collapse = "\n"), "\n")
} # /rtemis::pastels


#' @keywords internal
#' @noRd
headdot <- function(x, maxlength = 6, format_fn = identity) {
  if (length(x) < maxlength) {
    paste(format_fn(x), collapse = ", ")
  } else {
    paste0(paste(format_fn(head(as.vector(x), n = maxlength)), collapse = ", "), "...")
  }
} # /rtemis::headdot

# twocol2html.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Create html table from a data.frame with 2 columns: Var name, and Coefs
#'
#' @examples
#' \dontrun{
#' x <- data.frame(
#'   ID = c("Int", paste0("V", 1:10)),
#'   Coef = rnorm(11)
#' )
#' twocol2html(x)
#' }
#' @keywords internal
#' @noRd
twocol2html <- function(x,
                        font_family = "'Lato'",
                        font_col = "#ffffff",
                        font_size = "18px",
                        header_bg = "#404040",
                        table_bg = "#7F7F7F",
                        dat_col = rep("#525252", NROW(x)), # get color grad using all tables
                        dat_font_col = "#ffffff",
                        height = "50px",
                        # header
                        head_padding = "5px",
                        # table
                        dat_padding = "5px") {
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
  for (i in seq_along(tab)) {
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
} # /rtemis::twocol2html

#' Print Size
#'
#' Get `NCOL(x)` and \code{NROW{x}}
#'
#' @param x R object (usually that inherits from matrix or data.frame)
#' @param name Character: Name of input object
#' @param verbosity Integer: Verbosity level.
#' @param newline Logical: If TRUE, end with new line character.
#'
#' @return vector of NROW, NCOL invisibly
#' 
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' catsize(iris)
#' }
catsize <- function(x, name = NULL, verbosity = 1L, newline = TRUE) {
  if (inherits(x, c("matrix", "data.frame"))) {
    .nrow <- NROW(x)
    .ncol <- NCOL(x)
    nrows <- format(.nrow, big.mark = ",")
    ncols <- format(.ncol, big.mark = ",")
    if (verbosity > 0L) {
      pcat(
        name,
        paste(hilite(nrows), "x", hilite(ncols)),
        newline = newline
      )
    }
    invisible(c(.nrow, .ncol))
  } else {
    .nels <- length(x)
    nels <- format(.nels, big.mark = ",")
    if (verbosity > 0L) {
      cat(
        # "There",
        # ngettext(.nels, "is", "are"),
        name,
        hilite(nels),
        # ngettext(.nels, "element", "elements"),
        if (newline) "\n"
      )
    }
    invisible(.nels)
  }
} # /rtemis::catsize


#' Print single line of object info
#'
#' @param x object to print
#' @param ... Not used.
#'
#' @author EDG
#' @keywords internal
#' @noRd

print1 <- function(x, ...) {
  UseMethod("print1")
} # /rtemis::print1

#' @author EDG
#' @keywords internal
#' @noRd
print1.default <- function(x, ...) {
  head(x, 1)
} # /rtemis::print1.default


#' @author EDG
#' @keywords internal
#' @noRd
list2text <- function(x, sep = ": ", line = "\n") {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(.names[i], sep, x[[i]], line)
  }) |> paste0(collapse = "")
} # /rtemis::list2text


#' @author EDG
#' @keywords internal
#' @noRd
list2html <- function(x, sep = ": ",
                      col = "#16A0AC",
                      key.weight = 100,
                      value.weight = 300,
                      line = "<br>") {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(
      span(.names[i], style = paste0("font-weight:", key.weight, ";")),
      sep,
      span(x[[i]], style = paste0("color:", col, "; font-weight:", value.weight, ";")),
      line
    )
  }) |>
    paste0(collapse = "") |>
    htmltools::HTML()
} # /rtemis::list2html


#' @author EDG
#' @keywords internal
#' @noRd
printchar <- function(x, left_pad = 2) {
  target_length <- left_pad + max(nchar(x))
  for (i in x) {
    cat(
      hilite(leftpad(i, target_length)), "\n"
    )
  }
} # /rtemis::printchar
