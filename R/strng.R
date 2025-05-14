# strng.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' String formatting utilities
#'
#' @param ... Character objects to format
#'
#' @rdname strng
bold <- function(...) {
  paste0("\033[1m", paste(...), "\033[22m")
}

#' @rdname strng
italic <- function(...) {
  paste0("\033[3m", paste(...), "\033[23m")
}

#' @rdname strng
underline <- function(...) {
  paste0("\033[4m", paste(...), "\033[24m")
}

#' @rdname strng
hilite <- function(..., col = "69;1", bold = TRUE) {
  paste0(
    ifelse(bold, "\033[1m", ""),
    "\033[38;5;",
    col,
    "m",
    paste(...),
    "\033[0m"
  )
}

#' @rdname strng
#' @param x Numeric: Input
hilitebig <- function(x) {
  hilite(format(x, scientific = FALSE, big.mark = ","))
}

#' @rdname strng
#' @param bold Logical: If TRUE, use bold font
red <- function(..., bold = FALSE) {
  paste0("\033[", ifelse(bold, "1;", ""), "91m", paste(...), "\033[0m")
}

#' @rdname strng
green <- function(..., bold = FALSE) {
  paste0("\033[", ifelse(bold, "1;", ""), "92m", paste(...), "\033[0m")
}

#' @rdname strng
orange <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[38;5;208m", paste(...), "\033[0m")
}

#' @rdname strng
cyan <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[36m", paste(...), "\033[0m")
}

#' @rdname strng
magenta <- function(..., bold = FALSE) {
  paste0(ifelse(bold, "\033[1m", ""), "\033[35m", paste(...), "\033[0m")
}

#' @rdname strng
#' @param sep Character: Separator
gray <- function(..., bold = FALSE, sep = " ") {
  paste0(
    ifelse(bold, "\033[1m", ""),
    "\033[90m",
    paste(..., sep = sep),
    "\033[0m"
  )
}

#' @rdname strng
reset <- function(...) {
  paste0("\033[0m", paste(...))
}

col256 <- function(x, col = 183) {
  paste0("\033[38;5;", col, "m", x, "\033[0m")
}

# Read UTF-8 strings from file, because R files should be ASCII-only.
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
# 2017 E.D. Gennatas rtemis.org

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
#' @author E.D. Gennatas
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
#' @param x Character vector
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
#' x
#' clean_names(x)
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
#' @param x Character, vector
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' clean_colnames(iris)
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
