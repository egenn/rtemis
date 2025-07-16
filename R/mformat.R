# mformat.R
# ::rtemis::
# 2025 EDG rtemis.org

# Goal: mformat() function that uses mformat_ansi(), mformat_html(), or mformat_plain() to format messages.
# Must support a generic API for formatting, e.g. bold(), col_ansi(..., col = "79") that gets
# translated to the appropriate format based on the output_type argument.

#' Format messages for different outputs
#'
#' This function allows formatting messages 1) using ANSI codes for console printing, 2) as HTML
#' for web applications/viewers, or 3) as plain text for logs.
#'
#' @param ... Messages to format. Can include calls to formatting functions like bold(), col256(), etc.
#' @param output_type Character: "ansi", "html", or "plain".
#' @param caller Logical: Include calling function name in output
#' @param sep Character: Separator between elements (default: "")
#'
#' @return Formatted message as a character string.
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
#'
#' @author EDG
#' @keywords internal
#' @noRd
mformat <- function(
  ...,
  output_type = c("ansi", "html", "plain"),
  timestamp = TRUE,
  sep = ""
) {
  output_type <- match.arg(output_type)

  # Get the unevaluated arguments
  args_list <- substitute(list(...))

  # debug
  # return(args_list)

  # If timestamp, prepend the current date and time using muted(datetime())
  if (timestamp) {
    # Create the timestamp expression (no caller info here)
    dt <- paste0(datetime(), " ")
    timestamp_expr <- bquote(muted(.(dt), type = .(output_type)))

    # Convert call to list, prepend timestamp, and convert back to call
    temp_list <- as.list(args_list)
    new_args <- c(temp_list[1], list(timestamp_expr), temp_list[-1])
    args_list <- as.call(new_args)
  }

  # Remove the 'list' part to get individual arguments
  args <- if (length(args_list) > 1) args_list[-1] else list()

  # debug
  # return(args)

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
          # Add type parameter if not already present
          if (!"type" %in% names(arg)) {
            arg$type <- output_type
          }
        }
      }
      eval(arg, envir = parent.frame())
    } else {
      # For non-function calls, evaluate directly
      eval(arg, envir = parent.frame())
    }
  })

  # Convert to character and paste
  char_args <- lapply(formatted_args, as.character)
  out <- paste0(char_args, collapse = sep)
  out
} # rtemis::mformat

# Core formatting functions that adapt based on output type

#' Make text bold
#' @param text Character: Text to make bold
#' @param type Character: Output type ("ansi", "html", "plain")
#' @keywords internal
#' @noRd
bold <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)
  switch(
    type,
    "ansi" = paste0("\033[1m", text, "\033[0m"),
    "html" = paste0("<strong>", text, "</strong>"),
    "plain" = text
  )
}

#' Make text italic
#' @param text Character: Text to make italic
#' @param type Character: Output type ("ansi", "html", "plain")
#' @keywords internal
#' @noRd
italic <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)
  switch(
    type,
    "ansi" = paste0("\033[3m", text, "\033[0m"),
    "html" = paste0("<em>", text, "</em>"),
    "plain" = text
  )
}

#' Make text underlined
#' @param text Character: Text to underline
#' @param type Character: Output type ("ansi", "html", "plain")
#' @keywords internal
#' @noRd
underline <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)
  switch(
    type,
    "ansi" = paste0("\033[4m", text, "\033[0m"),
    "html" = paste0("<u>", text, "</u>"),
    "plain" = text
  )
}

#' Muted text
#'
#' @param x Character: Text to mute
#' @param type Character: Output type ("ansi", "html", "plain")
#' @keywords internal
#' @noRd
muted <- function(x, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)
  switch(
    type,
    "ansi" = paste0("\033[2m", x, "\033[0m"), # ANSI muted (dimmed)
    "html" = paste0('<span style="color: gray;">', x, "</span>"), # HTML muted
    "plain" = x # Plain text unformatted
  )
}

#' Apply 256-color formatting
#'
#' @param text Character: Text to color
#' @param col Character or numeric: Color (ANSI 256-color code, hex for HTML)
#' @param bg Logical: If TRUE, apply as background color
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
col256 <- function(
  text,
  col = "79",
  bg = FALSE,
  type = c("ansi", "html", "plain")
) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = {
      if (bg) {
        paste0("\033[48;5;", col, "m", text, "\033[0m")
      } else {
        paste0("\033[38;5;", col, "m", text, "\033[0m")
      }
    },
    "html" = {
      # Convert ANSI color codes to approximate hex colors if needed
      if (is.numeric(col) || (is.character(col) && !grepl("^#", col))) {
        # Simple mapping for common ANSI colors - extend as needed
        color_map <- c(
          "79" = "#5fd7af", # light cyan
          "196" = "#ff0000", # red
          "46" = "#00ff00", # green
          "21" = "#0000ff", # blue
          "226" = "#ffff00", # yellow
          "201" = "#ff00ff", # magenta
          "51" = "#00ffff" # cyan
        )
        hex_col <- color_map[as.character(col)]
        if (is.na(hex_col)) hex_col <- "#ffffff" # default to white
      } else {
        hex_col <- col
      }

      if (bg) {
        paste0(
          '<span style="background-color: ',
          hex_col,
          '">',
          text,
          '</span>'
        )
      } else {
        paste0('<span style="color: ', hex_col, '">', text, '</span>')
      }
    },
    "plain" = text
  )
}

#' Apply RGB color formatting
#'
#' @param text Character: Text to color
#' @param r,g,b Numeric: RGB values (0-255)
#' @param bg Logical: If TRUE, apply as background color
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
col_rgb <- function(
  text,
  r,
  g,
  b,
  bg = FALSE,
  type = c("ansi", "html", "plain")
) {
  type <- match.arg(type)

  switch(
    type,
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
}

#' Apply named color formatting
#'
#' @param text Character: Text to color
#' @param color Character: Color name (red, green, blue, yellow, etc.)
#' @param bg Logical: If TRUE, apply as background color
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
col_named <- function(
  text,
  color = "red",
  bg = FALSE,
  type = c("ansi", "html", "plain")
) {
  type <- match.arg(type)

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
    type,
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
        paste0('<span style="background-color: ', color, '">', text, '</span>')
      } else {
        paste0('<span style="color: ', color, '">', text, '</span>')
      }
    },
    "plain" = text
  )
}

#' Reset all formatting
#'
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
reset <- function(type = c("ansi", "html", "plain")) {
  type <- match.arg(type)
  switch(type, "ansi" = "\033[0m", "html" = "", "plain" = "")
}

# Utility functions for common formatting patterns

#' Create a formatted header
#'
#' @param text Character: Header text
#' @param level Numeric: Header level (1-6, for HTML)
#' @param color Character: Color for the header
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
header <- function(
  text,
  level = 1,
  color = "blue",
  type = c("ansi", "html", "plain")
) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = {
      formatted_text <- col_named(bold(text, type = type), color, type = type)
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
}

#' Create a formatted list item
#'
#' @param text Character: List item text
#' @param bullet Character: Bullet character
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
list_item <- function(text, bullet = "•", type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0(col256(bullet, "46", type = type), " ", text),
    "html" = paste0("<li>", text, "</li>"),
    "plain" = paste0(bullet, " ", text)
  )
}

#' Create a formatted code block
#'
#' @param code Character: Code text
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
code_block <- function(code, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0("\033[100m", code, "\033[0m"), # gray background
    "html" = paste0("<code>", code, "</code>"),
    "plain" = paste0("`", code, "`")
  )
}

#' Create a formatted warning message
#'
#' @param text Character: Warning text
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
warning_msg <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0(col_named("⚠ WARNING: ", "yellow", type = type), text),
    "html" = paste0(
      '<span style="color: orange; font-weight: bold;">⚠ WARNING: </span>',
      text
    ),
    "plain" = paste0("WARNING: ", text)
  )
}

#' Create a formatted error message
#'
#' @param text Character: Error text
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
error_msg <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0(col_named("✗ ERROR: ", "red", type = type), text),
    "html" = paste0(
      '<span style="color: red; font-weight: bold;">✗ ERROR: </span>',
      text
    ),
    "plain" = paste0("ERROR: ", text)
  )
}

#' Create a formatted success message
#'
#' @param text Character: Success text
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
success_msg <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0(col_named("✓ SUCCESS: ", "green", type = type), text),
    "html" = paste0(
      '<span style="color: green; font-weight: bold;">✓ SUCCESS: </span>',
      text
    ),
    "plain" = paste0("SUCCESS: ", text)
  )
}

#' Create a formatted info message
#'
#' @param text Character: Info text
#' @param type Character: Output type ("ansi", "html", "plain")
#'
#' @keywords internal
#' @noRd
info_msg <- function(text, type = c("ansi", "html", "plain")) {
  type <- match.arg(type)

  switch(
    type,
    "ansi" = paste0(col_named("ℹ INFO: ", "cyan", type = type), text),
    "html" = paste0(
      '<span style="color: blue; font-weight: bold;">ℹ INFO: </span>',
      text
    ),
    "plain" = paste0("INFO: ", text)
  )
}

# Convenience messaging functions that use mformat internally

#' Message with mformat
#' @param ... Arguments passed to mformat
#' @param output_type Character: Output type for mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @keywords internal
#' @noRd
msg <- function(..., output_type = "ansi", timestamp = TRUE, caller = TRUE) {
  # Get caller info if requested
  caller_name <- if (caller) get_caller_info() else NULL

  # Format the message (without caller info)
  formatted_msg <- mformat(
    ...,
    output_type = output_type,
    timestamp = timestamp
  )

  # Add caller info at the end
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- paste0(
      " ",
      muted(paste0("[", caller_name, "]"), type = output_type)
    )
    formatted_msg <- paste0(formatted_msg, caller_suffix)
  }

  message(formatted_msg)
}

#' Print with mformat (cat version)
#' @param ... Arguments passed to mformat
#' @param output_type Character: Output type for mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @keywords internal
#' @noRd
pmsg <- function(..., output_type = "ansi", timestamp = TRUE, caller = TRUE) {
  # Get caller info if requested
  caller_name <- if (caller) get_caller_info() else NULL

  # Format the message (without caller info)
  formatted_msg <- mformat(
    ...,
    output_type = output_type,
    timestamp = timestamp
  )

  # Add caller info at the end
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- paste0(
      " ",
      muted(paste0("[", caller_name, "]"), type = output_type)
    )
    formatted_msg <- paste0(formatted_msg, caller_suffix)
  }

  cat(formatted_msg, "\n")
}

#' HTML message with mformat
#' @param ... Arguments passed to mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @keywords internal
#' @noRd
hmsg <- function(..., timestamp = TRUE, caller = TRUE) {
  # Get caller info if requested
  caller_name <- if (caller) get_caller_info() else NULL

  # Format the message (without caller info)
  formatted_msg <- mformat(
    ...,
    output_type = "html",
    timestamp = timestamp
  )

  # Add caller info at the end
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- paste0(
      " ",
      muted(paste0("[", caller_name, "]"), type = "html")
    )
    formatted_msg <- paste0(formatted_msg, caller_suffix)
  }

  message(formatted_msg)
}

#' Plain text message with mformat
#' @param ... Arguments passed to mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @keywords internal
#' @noRd
tmsg <- function(..., timestamp = TRUE, caller = TRUE) {
  # Get caller info if requested
  caller_name <- if (caller) get_caller_info() else NULL

  # Format the message (without caller info)
  formatted_msg <- mformat(
    ...,
    output_type = "plain",
    timestamp = timestamp
  )

  # Add caller info at the end
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- paste0(" [", caller_name, "]")
    formatted_msg <- paste0(formatted_msg, caller_suffix)
  }

  message(formatted_msg)
}

# Smart output type detection and dispatcher

#' Detect appropriate output type based on context
#' @param filename Character: Optional filename for output
#' @param force_type Character: Force a specific output type
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
  return("ansi")
}

#' Smart replacement for msg2() with automatic output type detection
#'
#' @param ... Arguments passed to mformat
#' @param timestamp Logical: Include timestamp
#' @param caller Logical: Include caller info
#' @param output_type Character: Force specific output type (overrides auto-detection)
#' @param filename Character: Optional filename to write to
#' @param append Logical: Append to file if filename provided
#'
#' @keywords internal
#' @noRd
msg3 <- function(
  ...,
  timestamp = TRUE,
  caller = TRUE,
  output_type = NULL,
  filename = NULL,
  append = TRUE
) {
  # Auto-detect output type
  detected_type <- detect_output_type(filename, output_type)

  # Get caller info if requested (do this here, not in mformat)
  caller_name <- if (caller) get_caller_info() else NULL

  # Format the message (without caller info)
  formatted_msg <- mformat(
    ...,
    output_type = detected_type,
    timestamp = timestamp
  )

  # Add caller info at the end like msg2() does
  if (!is.null(caller_name) && !is.na(caller_name) && nchar(caller_name) > 0L) {
    caller_suffix <- switch(
      detected_type,
      "ansi" = paste0(" ", muted(paste0("[", caller_name, "]"), type = "ansi")),
      "html" = paste0(" ", muted(paste0("[", caller_name, "]"), type = "html")),
      "plain" = paste0(" [", caller_name, "]")
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
}

#' Print method that automatically adapts to context
#' @param x Object to print
#' @param filename Character: Optional filename to write to
#' @param output_type Character: Force specific output type
#' @param ... Additional arguments passed to mformat
#' @keywords internal
#' @noRd
smart_print <- function(x, filename = NULL, output_type = NULL, ...) {
  # Auto-detect output type
  detected_type <- detect_output_type(filename, output_type)

  # Create appropriate representation based on object type
  if (is.data.frame(x) || is.matrix(x)) {
    # For tabular data
    if (detected_type == "html") {
      # Could integrate with DT, knitr::kable, etc.
      output <- paste(
        "<table>",
        paste(capture.output(print(x)), collapse = "\n"),
        "</table>"
      )
    } else {
      output <- capture.output(print(x))
    }
  } else {
    # For other objects, use standard representation
    output <- capture.output(print(x))
  }

  # Format and output
  if (!is.null(filename)) {
    cat(output, file = filename, sep = "\n")
  } else {
    cat(output, sep = "\n")
  }

  invisible(x)
}

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
# - bold(), italic(), underline(): Basic text styling
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
#' @param call_depth Integer: Depth in call stack to examine
#' @param caller_id Integer: Which function in the call stack to extract
#' @param max_char Integer: Maximum characters for caller name
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
}
