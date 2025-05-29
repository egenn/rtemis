# strng.R
# ::rtemis::
# 2022 EDG rtemis.org

# References
# ANSI escape code numbers
# https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
# Xterm color names: https://jonasjacek.github.io/colors/
# CSS color keywords: https://www.uxgem.com/docs/css-color-keywords

# rt console colors
MediumSpringGreen <- "49;1"
CornflowerBlue <- "69;1"
SteelBlue1 <- "75;1"
Magenta3 <- "164;1"
MediumOrchid1 <- "171;1"
Violet <- "177;1"
DarkOrange <- "208;1"
Turquoise4 <- "30;1"
DarkCyan <- "36;1"

# primary highlight color
# hilite_col <- MediumSpringGreen
hilite_col <- DarkCyan
# secondary highlight color
hilite1_col <- SteelBlue1 # objcat default
hilite2_col <- "177" # Violet # info
hilite3_col <- Magenta3 # warning
rt_green <- DarkCyan # yay

#' String formatting utilities
#'
#' @param ... Character objects to format
#'

#' @keywords internal
#' @noRd
bold <- function(...) {
  paste0("\033[1m", paste(...), "\033[22m")
}

italic <- function(...) {
  paste0("\033[3m", paste(...), "\033[23m")
}

thin <- function(...) {
  paste0("\033[2m", paste(...), "\033[22m")
}

underline <- function(...) {
  paste0("\033[4m", paste(...), "\033[24m")
}


# blue for light and dark background: "69;1"
# green: "49;1"
hilite <- function(..., col = hilite_col) {
  paste0("\033[38;5;", col, "m", paste(...), "\033[0m")
}


# blue for light and dark background: "69;1"
# green: "49;1"
hilite1 <- function(..., col = hilite1_col, bold = TRUE) {
  paste0(
    ifelse(bold, "\033[1m", ""),
    "\033[38;5;",
    col,
    "m",
    paste(...),
    "\033[0m"
  )
}


hilite2 <- function(
  ...,
  col = hilite2_col,
  bold = FALSE,
  italic = FALSE,
  sep = ""
) {
  paste0(
    ifelse(bold, "\033[1m", ""),
    ifelse(italic, "\033[3m", ""),
    "\033[38;5;",
    hilite2_col,
    "m",
    paste(..., sep = sep),
    "\033[0m"
  )
}


#' @param x Numeric: Input
#'
#' @keywords internal
#' @noRd
hilitebig <- function(x) {
  hilite(format(x, scientific = FALSE, big.mark = ","))
}


red <- function(..., bold = FALSE) {
  paste0("\033[", ifelse(bold, "1;", ""), "91m", paste(...), "\033[0m")
}


# og green: "92m"
green <- function(..., bold = FALSE) {
  paste0(
    ifelse(bold, "\033[1m", ""),
    "\033[38;5;",
    rt_green,
    "m",
    paste(...),
    "\033[0m"
  )
}

blue <- function(..., bold = FALSE) {
  paste0("\033[", ifelse(bold, "1;", ""), "34m", paste(...), "\033[0m")
}


orange <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[38;5;208m", paste(...), "\033[0m")
}


cyan <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[36m", paste(...), "\033[0m")
}


magenta <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[35m", paste(...), "\033[0m")
}


gray <- function(..., bold = FALSE, sep = " ") {
  paste0(
    ifelse(bold, "\033[1m", ""),
    "\033[90m",
    paste(..., sep = sep),
    "\033[0m"
  )
}


reset <- function(...) {
  paste0("\033[0m", paste(...))
}

col256 <- function(x, col = 183) {
  paste0("\033[38;5;", col, "m", x, "\033[0m")
}

# Read UTF-8 strings from file, because R files should be ASCII-only.

## rtemis_logo.utf8
rtaart <- local({
  lines <- NULL
  function() {
    if (is.null(lines)) {
      file <- system.file(
        package = .packageName,
        "resources",
        "rtemis_logo.utf8"
      )
      bfr <- readLines(file)
      cols <- c(92, 128, 196, 208, 27)
      lines <<- mapply(bfr, cols, FUN = col256)
    }
    lines
  }
})

## rtemis_logo.utf8
rtlogo <- local({
  paste0(
    "  ",
    mapply(
      col256,
      readLines(system.file(
        package = .packageName,
        "resources",
        "rtemis_logo.utf8"
      )),
      c(92, 128, 196, 208, 27)
    ),
    collapse = "\n"
  )
})

## rtascii
rtascii <- function() {
  cat(rtaart(), sep = "\n")
}

rtasciitxt <- function() {
  paste(paste0(paste0("  ", rtaart(), "\n")), collapse = "")
}

yay <- function(..., sep = " ", end = "\n", pad = 0) {
  cat(
    rep(" ", pad),
    bold(green("\u2713 ")),
    paste(..., sep = sep),
    end,
    sep = ""
  )
}

nay <- function(..., sep = " ", end = "\n", pad = 0) {
  cat(rep(" ", pad), bold(red("\u2715 ")), paste(..., sep = sep), end, sep = "")
}


# labelify.R
# ::rtemis::
# 2017 EDG rtemis.org

#' Format text for label printing
#'
#' @param x Character: Input
#' @param underscoresToSpaces Logical: If TRUE, convert underscores to spaces.
#' @param dotsToSpaces Logical: If TRUE, convert dots to spaces.
#' @param toLower Logical: If TRUE, convert to lowercase (precedes `toTitleCase`).
#' Default = FALSE (Good for getting all-caps words converted to title case, bad for abbreviations
#' you want to keep all-caps)
#' @param toTitleCase Logical: If TRUE, convert to Title Case. Default = TRUE (This does not change
#' all-caps words, set `toLower` to TRUE if desired)
#' @param capitalize.strings Character, vector: Always capitalize these strings, if present. Default = `"id"`
#' @param stringsToSpaces Character, vector: Replace these strings with spaces. Escape as needed for `gsub`.
#' Default = `"\\$"`, which formats common input of the type `data.frame$variable`
#'
#' @author EDG
#' @export
labelify <- function(
  x,
  underscoresToSpaces = TRUE,
  dotsToSpaces = TRUE,
  toLower = FALSE,
  toTitleCase = TRUE,
  capitalize.strings = c("id"),
  stringsToSpaces = c("\\$", "`")
) {
  if (is.null(x)) {
    return(NULL)
  }
  xf <- x
  for (i in stringsToSpaces) {
    xf <- gsub(i, " ", xf)
  }
  for (i in capitalize.strings) {
    xf <- gsub(paste0("^", i, "$"), toupper(i), xf, ignore.case = TRUE)
  }
  if (underscoresToSpaces) xf <- gsub("_", " ", xf)
  if (dotsToSpaces) xf <- gsub("\\.", " ", xf)
  if (toLower) xf <- tolower(xf)
  if (toTitleCase) xf <- tools::toTitleCase(xf)
  xf <- gsub(" {2,}", " ", xf)
  xf <- gsub(" $", "", xf)

  # Remove [[X]], where X is any length of characters or numbers
  xf <- gsub("\\[\\[.*\\]\\]", "", xf)

  return(xf)
} # rtemis::labelify


#' Clean names
#'
#' Clean character vector by replacing all symbols and sequences of symbols with single
#' underscores, ensuring no name begins or ends with a symbol
#'
#' @param x Character vector.
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
#' x
#' clean_names(x)
#' }
clean_names <- function(x, prefix_digits = "V_") {
  xc <- gsub("[^[:alnum:]]{1,}", "_", x)
  xc <- gsub("^_|_$", "", xc)
  if (!is.na(prefix_digits)) {
    sn_idi <- grep("^[0-9]", xc)
    xc[sn_idi] <- paste0(prefix_digits, xc[sn_idi])
  }
  xc
}

#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character vector.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' clean_colnames(iris)
#' }
clean_colnames <- function(x) {
  if (!inherits(x, "character")) {
    x <- if (inherits(x, "matrix")) colnames(x) else names(x)
  }
  clean_names(x)
}


leftpad <- function(x, target_length, pad_char = " ") {
  lpad <- target_length - nchar(x)
  if (lpad > 0) {
    paste0(paste0(rep(pad_char, lpad), collapse = ""), x)
  } else {
    x
  }
}

#' Force plain text when using `message()`
#'
#' @param x Character: Text to be output to console.
#'
#' @return Character: Text with ANSI escape codes removed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
plain <- function(x) {
  paste0("\033[0m", x)
}

oxfordcomma <- function(..., format_fn = identity) {
  x <- unlist(list(...))
  if (length(x) > 2) {
    paste0(
      paste(sapply(x[-length(x)], format_fn), collapse = ", "),
      ", and ",
      format_fn(x[length(x)])
    )
  } else if (length(x) == 2) {
    paste(format_fn(x), collapse = " and ")
  } else {
    format_fn(x)
  }
} # rtemis::oxfordcomma

#' Padded cat
#'
#' @param x Character: Text to be output to console.
#' @param format_fn Function: Any function to be applied to `x`.
#' @param col Color: Any color fn.
#' @param newline_pre Logical: If TRUE, start with a new line.
#' @param newline Logical: If TRUE, end with a new (empty) line.
#' @param pad Integer: Pad message with this many spaces on the left.
#'
#' @author EDG
#' @keywords internal
#' @noRd
padcat <- function(
  x,
  format_fn = hilite,
  col = NULL,
  newline_pre = TRUE,
  newline = FALSE,
  pad = 2
) {
  x <- as.character(x)
  if (!is.null(format_fn)) {
    x <- format_fn(x)
  }
  if (newline_pre) cat("\n")
  cat(rep(" ", pad), sep = "")
  if (!is.null(col)) {
    cat(col(x, TRUE))
  } else {
    cat(bold(x))
  }
  cat("\n")
  if (newline) cat("\n")
} # rtemis::padcat

pastebox <- function(x, pad = 0) {
  paste0(paste0(rep(" ", pad), collapse = ""), ".:", x)
}

# objcat.R
# ::rtemis::
# 2019- EDG rtemis.org

#' `rtemis-internal`: Object cat
#'
#' @param x Character: Object description
#' @author EDG
#' @keywords internal
#' @noRd

objcat <- function(x, format_fn = hilite1, pad = 0) {
  cat(
    paste0(rep(" ", pad), collapse = ""),
    format_fn(x),
    "\n",
    sep = ""
  )
} # rtemis::objcat

# Emojis ----
# wave <- "\U1F30A"
# mountain <- "\U26F0\UFE0F"
# alien <- "\U1F47D"
