# mformat.R
# ::rtemis::
# 2025 EDG rtemis.org

# Description:
# mformat() function used internally to format messages as ANSI, HTML, or plain text.
# Must support a generic API for formatting, e.g. bold(), col_ansi(..., col = "79") that gets
# translated to the appropriate format based on the output_type argument.

# Smart output type detection and dispatcher

#' "multi-format": Format messages for different outputs
#'
#' This function allows formatting messages 1) using ANSI codes for console printing, 2) as HTML
#' for web applications/viewers, or 3) as plain text for logs.
#'
#' @param ... Messages to format. Can include the following formatting functions:
#' bold(), italic(), underline(), thin(), muted(), col256(), col_rgb(), col_named(),
#' header(), list_item(), code_block(), warning_msg(), error_msg(), success_msg(),
#' info_msg(), reset().
#' @param sep Character: Separator between elements.
#' @param timestamp Logical: If TRUE, include a date & time prefix before message.
#'
#' @return Formatted message as a character string.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Using ANSI output (default)
#' mformat("This is ", bold("bold"), " and ", col256("colored", "79"), " text")
#'
#' # HTML output
#' mformat("This is ", bold("bold"), " and ", col256("colored", "79"), " text",
#'         output_type = "html")
#'
#' # Plain text output
#' mformat("This is ", bold("bold"), " and ", col256("colored", "79"), " text",
#'         output_type = "plain")
#'
#' # You can also call formatting functions directly with type
#' bold("Hello", type = "html")
#' col256("Colored text", "79", type = "ansi")
#' }
mformat <- function(
  ...,
  output_type = c("ansi", "html", "plain"),
  timestamp = TRUE,
  sep = ""
) {
  output_type <- match.arg(output_type)

  # Get the unevaluated arguments but preserve the calling environment
  args_list <- substitute(list(...))
  calling_env <- parent.frame()

  # Remove the 'list' part to get individual arguments
  args <- if (length(args_list) > 1) args_list[-1] else list()

  # Process each argument
  formatted_args <- lapply(args, function(arg) {
    if (is.call(arg)) {
      # If it's a function call, check if it's one of our formatting functions
      if (length(arg) > 1 && is.name(arg[[1]])) {
        func_name <- as.character(arg[[1]])
        # Check if this is one of our formatting functions
        if (
          func_name %in%
            c(
              "bold",
              "italic",
              "underline",
              "thin",
              "muted",
              "col256",
              "col_rgb",
              "col_named",
              "header",
              "list_item",
              "code_block",
              "warning_msg",
              "error_msg",
              "success_msg",
              "info_msg",
              "reset"
            )
        ) {
          # Add output_type parameter if not already present
          if (!"output_type" %in% names(arg)) {
            arg$output_type <- output_type
          }
        }
      }
      # Evaluate in the calling environment (not parent.frame() which changes with lapply)
      eval(arg, envir = calling_env)
    } else {
      # For non-function calls, evaluate in the calling environment
      eval(arg, envir = calling_env)
    }
  })

  # Convert to character and paste
  char_args <- lapply(formatted_args, as.character)
  out <- paste0(char_args, collapse = sep)

  # prefix datetime()
  if (timestamp) {
    out <- glue::glue("{muted(datetime(), output_type = output_type)} {out}")
  }
  out
} # /rtemis::mformat

# Core formatting functions that adapt based on output type

#' Make text bold
#' @param text Character: Text to make bold
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with bold styling
#'
#' @keywords internal
#' @noRd
bold <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[1m", text, "\033[0m"),
    "html" = paste0("<strong>", text, "</strong>"),
    "plain" = text
  )
} # /rtemis::bold

#' Make text italic
#' @param text Character: Text to make italic
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with italic styling
#'
#' @keywords internal
#' @noRd
italic <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[3m", text, "\033[0m"),
    "html" = paste0("<em>", text, "</em>"),
    "plain" = text
  )
} # /rtemis::italic


#' Make text underlined
#'
#' @param text Character: Text to underline
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with underline styling
#'
#' @keywords internal
#' @noRd
underline <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[4m", text, "\033[0m"),
    "html" = paste0("<u>", text, "</u>"),
    "plain" = text
  )
} # /rtemis::underline


#' Make text thin/light
#'
#' @param text Character: Text to make thin
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with thin/light styling
#'
#' @keywords internal
#' @noRd
thin <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[2m", text, "\033[0m"), # ANSI thin (dimmed)
    "html" = paste0('<span style="font-weight: lighter;">', text, "</span>"),
    "plain" = text
  )
} # /rtemis::thin


#' Muted text
#'
#' @param x Character: Text to format
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with muted styling
#'
#' @keywords internal
#' @noRd
muted <- function(x, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[2m", x, "\033[0m"), # ANSI muted (dimmed)
    "html" = paste0('<span style="color: gray;">', x, "</span>"), # HTML muted
    "plain" = x # Plain text unformatted
  )
} # /rtemis::muted


#' Gray text
#'
#' @param x Character: Text to format
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with gray styling
#'
#' @details
#' Can be useful in contexts where musted is not supported.
#'
#' @keywords internal
#' @noRd
gray <- function(x, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(
    output_type,
    "ansi" = paste0("\033[90m", x, "\033[0m"), # ANSI gray
    "html" = paste0('<span style="color: gray;">', x, "</span>"), # HTML gray
    "plain" = x # Plain text unformatted
  )
} # /rtemis::gray


#' Convert ANSI 256 color code to HEX
#'
#' @param code Integer: ANSI 256 color code (0-255).
#' @return Character: HEX color string.
#' @keywords internal
#' @noRd
ansi256_to_hex <- function(code) {
  code <- as.integer(code)
  if (is.na(code) || code < 0 || code > 255) {
    return("#000000") # Return black for invalid codes
  }

  # Standard and high-intensity colors (0-15)
  if (code < 16) {
    return(c(
      "#000000",
      "#cd0000",
      "#00cd00",
      "#cdcd00",
      "#0000ee",
      "#cd00cd",
      "#00cdcd",
      "#e5e5e5",
      "#7f7f7f",
      "#ff0000",
      "#00ff00",
      "#ffff00",
      "#5c5cff",
      "#ff00ff",
      "#00ffff",
      "#ffffff"
    )[code + 1])
  }

  # 6x6x6 color cube (16-231)
  if (code >= 16 && code <= 231) {
    code <- code - 16
    r <- floor(code / 36)
    g <- floor((code %% 36) / 6)
    b <- code %% 6
    levels <- c(0, 95, 135, 175, 215, 255) # xterm levels
    return(grDevices::rgb(
      levels[r + 1],
      levels[g + 1],
      levels[b + 1],
      maxColorValue = 255
    ))
  }

  # Grayscale ramp (232-255)
  gray_level <- (code - 232) * 10 + 8
  grDevices::rgb(
    gray_level,
    gray_level,
    gray_level,
    maxColorValue = 255
  )
} # /rtemis::ansi256_to_hex


#' Apply 256-color formatting
#'
#' @param text Character: Text to color
#' @param col Character or numeric: Color (ANSI 256-color code, hex for HTML)
#' @param bg Logical: If TRUE, apply as background color
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with 256-color styling
#'
#' @keywords internal
#' @noRd
col256 <- function(
  text,
  col = "79",
  bg = FALSE,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = {
      if (bg) {
        paste0("\033[48;5;", col, "m", text, "\033[0m")
      } else {
        paste0("\033[38;5;", col, "m", text, "\033[0m")
      }
    },
    "html" = {
      # Convert ANSI color codes to hex colors if needed
      hex_col <- if (
        is.numeric(col) || (is.character(col) && !grepl("^#", col))
      ) {
        ansi256_to_hex(col)
      } else {
        col
      }
      if (bg) {
        paste0(
          '<span style="background-color: ',
          hex_col,
          '">',
          text,
          "</span>"
        )
      } else {
        paste0('<span style="color: ', hex_col, '">', text, "</span>")
      }
    },
    "plain" = text
  )
} # /rtemis::col256


#' Apply RGB color formatting
#'
#' @param text Character: Text to color
#' @param r,g,b Numeric: RGB values (0-255)
#' @param bg Logical: If TRUE, apply as background color
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with RGB color styling
#'
#' @keywords internal
#' @noRd
col_rgb <- function(
  text,
  r,
  g,
  b,
  bg = FALSE,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = {
      if (bg) {
        paste0("\033[48;2;", r, ";", g, ";", b, "m", text, "\033[0m")
      } else {
        paste0("\033[38;2;", r, ";", g, ";", b, "m", text, "\033[0m")
      }
    },
    "html" = {
      hex_col <- sprintf("#%02x%02x%02x", r, g, b)
      if (bg) {
        paste0(
          '<span style="background-color: ',
          hex_col,
          '">',
          text,
          "</span>"
        )
      } else {
        paste0('<span style="color: ', hex_col, '">', text, "</span>")
      }
    },
    "plain" = text
  )
} # /rtemis::col_rgb


#' Apply named color formatting
#'
#' @param text Character: Text to color
#' @param color Character: Color name (red, green, blue, yellow, etc.)
#' @param bg Logical: If TRUE, apply as background color
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with named color styling
#'
#' @keywords internal
#' @noRd
col_named <- function(
  text,
  color = "red",
  bg = FALSE,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  # ANSI color codes for basic colors
  ansi_colors <- list(
    black = 0,
    red = 1,
    green = 2,
    yellow = 3,
    blue = 4,
    magenta = 5,
    cyan = 6,
    white = 7,
    bright_black = 8,
    bright_red = 9,
    bright_green = 10,
    bright_yellow = 11,
    bright_blue = 12,
    bright_magenta = 13,
    bright_cyan = 14,
    bright_white = 15
  )

  switch(
    output_type,
    "ansi" = {
      color_code <- ansi_colors[[color]]
      if (is.null(color_code)) {
        color_code <- 7
      } # default to white

      if (bg) {
        paste0("\033[", 40 + color_code, "m", text, "\033[0m")
      } else {
        paste0("\033[", 30 + color_code, "m", text, "\033[0m")
      }
    },
    "html" = {
      if (bg) {
        paste0('<span style="background-color: ', color, '">', text, "</span>")
      } else {
        paste0('<span style="color: ', color, '">', text, "</span>")
      }
    },
    "plain" = text
  )
} # /rtemis::col_named


#' Reset all formatting
#'
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Reset string for the specified output type
#'
#' @keywords internal
#' @noRd
reset <- function(output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)
  switch(output_type, "ansi" = "\033[0m", "html" = "", "plain" = "")
} # /rtemis::reset


#' Create a formatted header
#'
#' @param text Character: Header text
#' @param level Numeric: Header level (1-6, for HTML)
#' @param color Character: Color for the header
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted header text
#'
#' @keywords internal
#' @noRd
header <- function(
  text,
  level = 1,
  color = "blue",
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = {
      formatted_text <- col_named(
        bold(text, output_type = output_type),
        color,
        output_type = output_type
      )
      if (level == 1) {
        paste0(
          "\n",
          formatted_text,
          "\n",
          paste0(rep("=", nchar(text)), collapse = ""),
          "\n"
        )
      } else if (level == 2) {
        paste0(
          "\n",
          formatted_text,
          "\n",
          paste0(rep("-", nchar(text)), collapse = ""),
          "\n"
        )
      } else {
        paste0("\n", formatted_text, "\n")
      }
    },
    "html" = {
      tag <- paste0("h", min(level, 6))
      style <- if (color != "") paste0(' style="color: ', color, '"') else ""
      paste0("<", tag, style, ">", text, "</", tag, ">")
    },
    "plain" = {
      if (level == 1) {
        paste0(
          "\n",
          text,
          "\n",
          paste0(rep("=", nchar(text)), collapse = ""),
          "\n"
        )
      } else if (level == 2) {
        paste0(
          "\n",
          text,
          "\n",
          paste0(rep("-", nchar(text)), collapse = ""),
          "\n"
        )
      } else {
        paste0("\n", text, "\n")
      }
    }
  )
} # /rtemis::header

#' Create a formatted list item
#'
#' @param text Character: List item text
#' @param bullet Character: Bullet character
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted list item text
#'
#' @keywords internal
#' @noRd
list_item <- function(
  text,
  bullet = "\u2022",
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0(col256(bullet, "46", output_type = output_type), " ", text),
    "html" = paste0("<li>", text, "</li>"),
    "plain" = paste0(bullet, " ", text)
  )
} # /rtemis::list_item

#' Create a formatted code block
#'
#' @param code Character: Code text
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted code block text
#'
#' @keywords internal
#' @noRd
code_block <- function(code, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0("\033[100m", code, "\033[0m"), # gray background
    "html" = paste0("<code>", code, "</code>"),
    "plain" = paste0("`", code, "`")
  )
} # /rtemis::code_block

#' Create a formatted warning message
#'
#' @param text Character: Warning text
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted warning message
#'
#' @keywords internal
#' @noRd
warning_msg <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0(
      col_named("\u26a0 WARNING: ", "yellow", output_type = output_type),
      text
    ),
    "html" = paste0(
      '<span style="color: orange; font-weight: bold;">\u26a0 WARNING: </span>',
      text
    ),
    "plain" = paste0("WARNING: ", text)
  )
} # /rtemis::warning_msg

#' Create a formatted error message
#'
#' @param text Character: Error text
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted error message
#'
#' @keywords internal
#' @noRd
error_msg <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0(
      col_named("\u2717 ERROR: ", "red", output_type = output_type),
      text
    ),
    "html" = paste0(
      '<span style="color: red; font-weight: bold;">\u2717 ERROR: </span>',
      text
    ),
    "plain" = paste0("ERROR: ", text)
  )
} # /rtemis::error_msg

#' Create a formatted success message
#'
#' @param text Character: Success text
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted success message
#'
#' @keywords internal
#' @noRd
success_msg <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0(
      col_named("\u2713 SUCCESS: ", "green", output_type = output_type),
      text
    ),
    "html" = paste0(
      '<span style="color: green; font-weight: bold;">\u2713 SUCCESS: </span>',
      text
    ),
    "plain" = paste0("SUCCESS: ", text)
  )
} # /rtemis::success_msg

# Convenience messaging functions that use mformat internally

# Smart output type detection and dispatcher

#' Detect appropriate output type based on context
#'
#' @param filename Character: Optional filename for output
#' @param force_type Character: Force a specific output type
#'
#' @return Character: Detected output type ("ansi", "html", or "plain")
#'
#' @keywords internal
#' @noRd
detect_output_type <- function(filename = NULL, force_type = NULL) {
  # If explicitly forced, use that
  if (!is.null(force_type)) {
    return(match.arg(force_type, c("ansi", "html", "plain")))
  }

  # If writing to file, determine by extension
  if (!is.null(filename)) {
    ext <- tolower(tools::file_ext(filename))
    return(switch(
      ext,
      "html" = "html",
      "htm" = "html",
      "md" = "html",
      "txt" = "plain",
      "log" = "plain",
      "csv" = "plain",
      "plain" # default for files
    ))
  }

  # Check if in interactive session vs script/batch
  if (!interactive()) {
    return("plain")
  }

  # Check if output supports ANSI (terminal capabilities)
  if (Sys.getenv("TERM") == "dumb" || Sys.getenv("NO_COLOR") != "") {
    return("plain")
  }

  # Check if in RStudio, Jupyter, or other HTML-capable environment
  if (Sys.getenv("RSTUDIO") == "1") {
    return("ansi") # RStudio supports ANSI in console
  }

  # Check for Jupyter notebook
  if (any(grepl("jupyter", commandArgs(), ignore.case = TRUE))) {
    return("html")
  }

  # Default to ANSI for interactive terminal sessions
  "ansi"
} # /rtemis::detect_output_type

#' Smart replacement for msg2() with automatic output type detection
#'
#' @param ... Arguments passed to mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @param output_type Character: Force specific output type (overrides auto-detection)
#' @param filename Character: Optional filename to write to
#' @param append Logical: Append to file if filename provided
#'
#' @return Character: Formatted message (invisibly)
#'
#' @keywords internal
#' @noRd
msg3 <- function(
  ...,
  timestamp = TRUE,
  caller = TRUE,
  call_depth = 3L,
  caller_id = 1L,
  output_type = NULL,
  filename = NULL,
  append = TRUE
) {
  # Detect output type
  detected_type <- detect_output_type(filename, output_type)

  # Get caller info if requested (do this here, not in mformat)
  caller_name <- if (caller) {
    get_caller_info(call_depth = call_depth, caller_id = caller_id)
  } else {
    NULL
  }

  # Format message
  formatted_msg <- mformat(
    ...,
    output_type = detected_type,
    timestamp = timestamp
  )

  # Suffix caller info
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- paste0(
      " ",
      muted(paste0("[", caller_name, "]"), output_type = detected_type)
    )
    formatted_msg <- paste0(formatted_msg, caller_suffix)
  }

  # Output the message
  if (!is.null(filename)) {
    # Write to file
    if (append && file.exists(filename)) {
      cat(formatted_msg, "\n", file = filename, append = TRUE)
    } else {
      cat(formatted_msg, "\n", file = filename)
    }
  } else {
    # Output to console
    message(formatted_msg)
  }

  invisible(formatted_msg)
} # /rtemis::msg3

#' Start/done messaging functions for processes

#' Start a process message using mformat
#'
#' @param ... Arguments passed to mformat
#' @param output_type Character: Force specific output type
#' @param newline_pre Logical: If TRUE, start with a new line
#' @param sep Character: Separator between elements
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
#' @noRd
msg3start <- function(
  ...,
  output_type = NULL,
  newline_pre = FALSE,
  sep = " "
) {
  # Auto-detect output type
  detected_type <- detect_output_type(force_type = output_type)

  if (newline_pre) {
    message("")
  }

  # Format the message with timestamp but no caller info
  formatted_msg <- mformat(
    ...,
    output_type = detected_type,
    timestamp = TRUE,
    sep = sep
  )

  # Output without newline (appendLF = FALSE)
  message(formatted_msg, appendLF = FALSE)
} # /rtemis::msg3start

#' Complete a process message using mformat with success indicator
#'
#' @param caller Character: Name of calling function (auto-detected if NULL)
#' @param call_depth Integer: Depth in call stack
#' @param caller_id Integer: Which function in call stack to extract
#' @param output_type Character: Force specific output type
#' @param sep Character: Separator between elements
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
#' @noRd
msg3done <- function(
  caller = NULL,
  call_depth = 1L,
  caller_id = 1L,
  output_type = NULL,
  sep = " "
) {
  # Auto-detect output type
  detected_type <- detect_output_type(force_type = output_type)

  # Get caller info if not provided
  if (is.null(caller)) {
    caller <- get_caller_info(
      call_depth = call_depth + 2L,
      caller_id = caller_id
    )
  }

  # Add a space first, then success indicator
  message(" ", appendLF = FALSE)

  # Create success indicator based on output type
  success_indicator <- col_named("\u2713", "green", output_type = detected_type)

  message(success_indicator, " ", appendLF = FALSE)

  # Add caller info if available
  if (!is.null(caller) && !is.na(caller) && nchar(caller) > 0L) {
    caller_msg <- muted(paste0("[", caller, "]"), output_type = detected_type)
    message(caller_msg)
  } else {
    message("")
  }
} # /rtemis::msg3done

#' Info message using msg3
#'
#' @param ... Arguments passed to msg3
#' @param output_type Character: Force specific output type
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#'
#' @return Character: Formatted message (invisibly)
#'
#' @keywords internal
#' @noRd
msg3_info <- function(
  ...,
  timestamp = TRUE,
  caller = TRUE,
  call_depth = 4L,
  caller_id = 2L,
  output_type = NULL
) {
  msg3(
    ...,
    output_type = output_type,
    timestamp = timestamp,
    caller = caller,
    call_depth = call_depth,
    caller_id = caller_id
  )
} # /rtemis::msg3_info

# ==============================================================================
# MFORMAT SYSTEM SUMMARY
# ==============================================================================
#
# This mformat system provides a clean, efficient way to format text for multiple
# output types (console/ANSI, HTML, plain text) using a unified API.
#
# KEY DESIGN PRINCIPLES:
# 1. Parameter passing: Type is specified once in mformat() and passed to all functions
# 2. Function flexibility: Each formatting function can also be called directly with type
# 3. Clean API: Minimal repetition, clear intent
# 4. No global state: All type information passed explicitly
#
# USAGE EXAMPLES:
#
# # Specify output type in mformat() - it gets passed to all formatting functions
# mformat("This is ", bold("bold"), " and ", col256("colored", "79"),
#         output_type = "html")
#
# # Or call formatting functions directly with explicit type
# bold("Hello", type = "html")
# col256("Colored text", "79", type = "ansi")
#
# # Mix and match as needed
# message <- mformat("Status: ", success_msg("Complete"), output_type = "ansi")
#
# AVAILABLE FORMATTING FUNCTIONS:
# - bold(), italic(), underline(), thin(): Basic text styling
# - col256(), col_rgb(), col_named(): Color formatting
# - header(), list_item(), code_block(): Structural elements
# - success_msg(), warning_msg(), error_msg(), info_msg(): Status messages
# - reset(): Clear all formatting
#
# All functions accept a 'type' parameter: "ansi", "html", or "plain"
#
# ADVANTAGES OF THIS APPROACH:
# 1. Clean, readable code - type specified once per mformat() call
# 2. Flexible - can call functions directly with explicit type when needed
# 3. No global state - everything is explicit and functional
# 4. Efficient - minimal overhead, direct parameter passing
# 5. Maintainable - clear function signatures, no hidden dependencies
# 6. Testable - easy to test each function independently
#
# ==============================================================================
# Helper function for caller detection (adapted from msg2 system)

#' Get caller information from call stack
#'
#' @param call_depth Integer: Depth in call stack to examine
#' @param caller_id Integer: Which function in the call stack to extract
#' @param max_char Integer: Maximum characters for caller name
#'
#' @return Character: Name of the calling function, or NA if not found
#'
#' @keywords internal
#' @noRd
get_caller_info <- function(call_depth = 3L, caller_id = 1L, max_char = 30L) {
  call_stack <- as.list(sys.calls())
  stack.length <- length(call_stack)

  if (stack.length < call_depth) {
    return(NA)
  }

  # Get just the immediate caller (not the whole chain)
  caller_call <- rev(call_stack)[[call_depth]]
  caller <- as.character(caller_call[[1]])

  # Handle S7 method calls
  if (!is.na(caller) && substr(caller, 1, 8) == "`method(") {
    caller <- sub("`method\\(([^,]+),.*\\)`", "\\1", caller)
  }

  # Handle cases where caller is a function object
  if (is.function(caller)) {
    caller <- tryCatch(
      {
        # Get the original call stack element as character
        call_str <- deparse(caller_call)
        # Extract function name from the call
        fn_match <- regexpr("^[a-zA-Z_][a-zA-Z0-9_\\.]*", call_str)
        if (fn_match > 0) {
          regmatches(call_str, fn_match)
        } else {
          "(fn)"
        }
      },
      error = function(e) "(fn)"
    )
  }

  # Truncate if too long
  if (is.character(caller) && nchar(caller) > max_char) {
    caller <- paste0(substr(caller, 1, max_char - 3), "...")
  }

  caller
} # /rtemis::get_caller_info


#' Info msg
#'
#' @param text Character: Info text.
#' @param output_type Character: Output type ("ansi", "html", "plain").
#'
#' @return Character: Formatted info message
#'
#' @keywords internal
#' @noRd
info_msg <- function(text, output_type = c("ansi", "html", "plain")) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = paste0(
      col_named("\u2139 INFO: ", color = col_info, output_type = output_type),
      text
    ),
    "html" = paste0(
      '<span style="color: blue; font-weight: bold;">\u2139 INFO: </span>',
      text
    ),
    "plain" = paste0("INFO: ", text)
  )
} # /rtemis::info_msg
