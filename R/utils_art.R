# utils_art.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Color columns of text art
#'
#' This function accepts text input of 1 or more lines and two colors.
#' It will:
#' a) generate a color gradient between the two colors
#' b) apply the gradient to each column of the text, creating a left to right color gradient.
#'
#' @param x Character vector of text to colorize.
#' @param color_left Color for the left side of the gradient.
#' @param color_right Color for the right side of the gradient.
#' @param output_type Character: Output type. One of "ansi", "html", "plain".
#'   Default = "ansi".
#'
#' @return Character vector with color formatting applied to each column.
#'
#' @author EDG
#' @keywords internal
#' @noRd
color_txt_columns <- function(
  x,
  color_left,
  color_right,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  # Count number of columns in input text
  ncols <- max(nchar(x, type = "width"))

  if (ncols == 0) {
    return(x)
  }

  # Create color gradient from color_left to color_right with ncols steps
  gradient <- grDevices::colorRampPalette(c(color_left, color_right))(ncols)
  gradient_rgb <- grDevices::col2rgb(gradient)

  # Apply the colors to each column of the text
  result <- character(length(x))

  for (i in seq_along(x)) {
    line <- x[i]
    line_chars <- strsplit(line, "")[[1]]
    line_width <- nchar(line, type = "width")

    if (line_width == 0) {
      result[i] <- line
      next
    }

    colored_chars <- character(length(line_chars))

    for (j in seq_along(line_chars)) {
      char <- line_chars[j]
      if (char == " ") {
        colored_chars[j] <- char
      } else {
        # Use column position for gradient color
        col_pos <- min(j, ncols)
        r <- gradient_rgb[1, col_pos]
        g <- gradient_rgb[2, col_pos]
        b <- gradient_rgb[3, col_pos]
        colored_chars[j] <- col_rgb(char, r, g, b, output_type = output_type)
      }
    }

    result[i] <- paste0(colored_chars, collapse = "")
  }

  result
} # /rtemis::color_txt_columns


#' Color rows of text art
#'
#' This function accepts text input of 1 or more lines and two colors.
#' It will:
#' a) generate a color gradient between the two colors
#' b) apply the gradient to each row of the text, creating a top to bottom color gradient.
#'
#' @param x Character vector of text to colorize.
#' @param color_top Color for the top of the gradient.
#' @param color_bottom Color for the bottom of the gradient.
#' @param output_type Character: Output type. One of "ansi", "html", "plain".
#'   Default = "ansi".
#'
#' @return Character vector with color formatting applied to each row.
#'
#' @author EDG
#' @keywords internal
#' @noRd
color_txt_rows <- function(
  x,
  color_top,
  color_bottom,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  # Number of rows
  nrows <- length(x)

  if (nrows == 0) {
    return(x)
  }

  # Create color gradient from color_top to color_bottom with nrows steps
  gradient <- grDevices::colorRampPalette(c(color_top, color_bottom))(nrows)
  gradient_rgb <- grDevices::col2rgb(gradient)

  # Apply the colors to each row of the text
  result <- character(nrows)

  for (i in seq_along(x)) {
    line <- x[i]
    line_chars <- strsplit(line, "")[[1]]
    line_width <- nchar(line, type = "width")

    if (line_width == 0) {
      result[i] <- line
      next
    }

    colored_chars <- character(length(line_chars))

    # Use row position for gradient color
    r <- gradient_rgb[1, i]
    g <- gradient_rgb[2, i]
    b <- gradient_rgb[3, i]

    for (j in seq_along(line_chars)) {
      char <- line_chars[j]
      if (char == " ") {
        colored_chars[j] <- char
      } else {
        colored_chars[j] <- col_rgb(char, r, g, b, output_type = output_type)
      }
    }

    result[i] <- paste0(colored_chars, collapse = "")
  }

  result
} # /rtemis::color_txt_rows


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
      col = c(92, 128, 196, 208, 27)
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

## rtemis_logo2.utf8
# Read rtemis_logo2.utf8 and apply color_txt_columns()
rtlogo2 <- paste0(
  color_txt_columns(
    readLines(system.file(
      package = .packageName,
      "resources",
      "rtemis_logo2.utf8"
    )),
    color_left = "#6125f7",
    color_right = "#19f0be",
    output_type = "ansi"
  ),
  collapse = "\n"
)
