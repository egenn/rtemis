# check_data.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Check Data
#'
#' @param x data.frame, data.table or similar structure
#' @param name Character: Name of dataset
#' @param get_duplicates Logical: If TRUE, check for duplicate cases
#' @param get_na_case_pct Logical: If TRUE, calculate percent of NA values per
#' case
#' @param get_na_feature_pct Logical: If TRUE, calculate percent of NA values
#' per feature
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' n <- 1000
#' x <- rnormmat(n, 50, return.df = TRUE)
#' x$char1 <- sample(letters, n, TRUE)
#' x$char2 <- sample(letters, n, TRUE)
#' x$fct <- factor(sample(letters, n, TRUE))
#' x <- rbind(x, x[1, ])
#' x$const <- 99L
#' x[sample(nrow(x), 20), 3] <- NA
#' x[sample(nrow(x), 20), 10] <- NA
#' x$fct[30:35] <- NA
#' check_data(x)
#' }
#'
check_data <- function(
  x,
  name = NULL,
  get_duplicates = TRUE,
  get_na_case_pct = FALSE,
  get_na_feature_pct = FALSE
) {
  if (is.null(name)) name <- deparse(substitute(x))
  x <- as.data.table(x)
  n_rows <- NROW(x)
  n_cols <- NCOL(x)

  # Data Types ----
  classes <- sapply(x, \(v) class(v)[1])
  counts <- table(classes)

  ## Numeric ----
  n_numeric <- max0(counts["numeric"])

  ## Integers ----
  # index_integer <- which(sapply(x, is.integer))
  # n_integer <- length(index_integer)
  n_integer <- max0(counts["integer"])

  ## Characters ----
  # index_character <- which(sapply(x, is.character))
  # n_character <- length(index_character)
  n_character <- max0(counts["character"])

  ## Factors ----
  index_factor <- which(sapply(x, is.factor))
  n_factor <- length(index_factor)
  index_ordered <- which(sapply(x, is.ordered))
  n_ordered <- length(index_ordered)
  # index_gt2levels_nonordered <- which(
  #     sapply(
  #         x[, setdiff(index_factor, index_ordered), drop = FALSE],
  #         \(x) length(levels(x))
  #     ) > 2
  # )
  # n_gt2levels_nonordered <- length(index_gt2levels_nonordered)

  ## Dates ----
  # index_date <- which(
  #     sapply(x, \(col) inherits(col, "Date"))
  # )
  # n_date <- length(index_date)
  n_date <- sum(
    max0(counts["Date"]),
    max0(counts["IDate"]),
    max0(counts["POSIXct"]),
    max0(counts["POSIXlt"])
  )

  # Issues ----

  ## Constants ----
  index_constant <- which(sapply(x, is_constant))
  n_constant <- length(index_constant)

  ## Duplicates ----
  # cindex_dups <- which(duplicated(x))
  # n_dups <- length(cindex_dups)
  n_dups <- if (get_duplicates) {
    n_rows - uniqueN(x)
  } else {
    NA
  }

  ## NAs ----
  cols_anyna <- which(sapply(x, anyNA))
  n_cols_anyna <- length(cols_anyna)
  index_na <- which(is.na(x))
  n_na <- length(index_na)

  ## Get percent of NA values per feature and per case
  if (n_cols_anyna > 0) {
    na_feature_pct <- if (get_na_feature_pct) {
      data.frame(
        Feature = names(cols_anyna),
        Pct_NA = sapply(seq_len(n_cols_anyna), \(i) {
          sum(is.na(x[[cols_anyna[1]]])) / n_cols
        })
      )
    } else {
      NULL
    }

    index_incomplete <- which(!complete.cases(x))
    n_incomplete <- length(index_incomplete)

    na_case_pct <- if (get_na_case_pct) {
      data.frame(
        Case = index_incomplete,
        Pct_NA = sapply(seq_len(n_incomplete), \(i) {
          sum(is.na(x[index_incomplete[i], ])) / n_rows
        })
      )
    } else {
      NULL
    }

    # Get types of features with NA
    classes_na <- table(classes[cols_anyna])
  } else {
    classes_na <- NULL
    na_feature_pct <- na_case_pct <- rep(0, n_cols)
  }

  # Output ----
  cd <- list(
    class = class(x)[1],
    name = name,
    n_rows = n_rows,
    n_cols = n_cols,
    n_numeric = n_numeric,
    n_integer = n_integer,
    n_character = n_character,
    n_factor = n_factor,
    n_ordered = n_ordered,
    n_date = n_date,
    n_constant = n_constant,
    n_dups = n_dups,
    n_cols_anyna = n_cols_anyna,
    n_na = n_na,
    classes_na = classes_na,
    na_feature_pct = na_feature_pct,
    na_case_pct = na_case_pct
  )
  class(cd) <- c("CheckData", "list")
  cd
} # rtemis::check_data

# chck <- function(x) {
#     setDT(x)
#     cat("Input has", NROW(x), "rows\n")
# }
# x <- iris
# class(x)
# chck(x)
# class(x)

# x <- data.frame(
#     ID = c(101L, 102L, 103L),
#     V1 = rnorm(3),
#     V3 = c(3L, 5L, 7L)
# )
# sapply(x, is.double)

max0 <- function(x) max(x, 0, na.rm = TRUE)

#' Generate `CheckData` object description in HTML
#'
#' @param x `CheckData` object
#' @param name Character: Name of the data set
#' @param css List: CSS styles
#'
#' @author E.D. Gennatas
#' @export
tohtml <- function(
  x,
  name = NULL,
  css = list(
    font.family = "Helvetica",
    color = "#fff",
    background.color = "#242424"
  )
) {
  n_rows <- x$n_rows
  n_cols <- x$n_cols
  n_numeric <- x$n_numeric
  n_integer <- x$n_integer
  n_character <- x$n_character
  n_factor <- x$n_factor
  n_ordered <- x$n_ordered
  n_date <- x$n_date
  n_constant <- x$n_constant
  n_dups <- x$n_dups
  n_cols_anyna <- x$n_cols_anyna
  n_na <- x$n_na
  classes_na <- x$classes_na
  na_feature_pct <- x$na_feature_pct
  na_case_pct <- x$na_case_pct

  ## Data Types ----
  numeric <- HTML(paste(
    strong(n_numeric),
    "numeric",
    ngettext(n_numeric, "feature", "features")
  ))
  integer <- HTML(paste(
    strong(n_integer),
    "integer",
    ngettext(n_integer, "feature", "features")
  ))
  categorical <- HTML(paste0(
    strong(n_factor),
    ngettext(n_factor, " factor", " factors"),
    if (n_factor == 1) {
      paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
    } else if (n_factor > 1) {
      paste(
        ", of which",
        strong(n_ordered),
        ngettext(n_ordered, "is", "are"),
        "ordered"
      )
    }
  ))
  # .col <- if (n_character > 0) html_orange else strong
  .col <- strong
  characters <- HTML(paste(
    .col(n_character),
    "character",
    ngettext(n_character, "feature", "features")
  ))
  dates <- HTML(paste(
    strong(n_date),
    "date",
    ngettext(n_date, "feature", "features")
  ))

  ## Issues ----
  .col <- if (n_constant > 0) html_red else strong
  constants <- HTML(paste(
    .col(n_constant),
    "constant",
    ngettext(n_constant, "feature", "features")
  ))
  .col <- if (n_dups > 0) html_orange else strong
  duplicates <- HTML(paste(
    .col(n_dups),
    "duplicate",
    ngettext(n_dups, "case", "cases")
  ))

  .col <- if (n_cols_anyna > 0) html_orange else strong
  nas <- if (n_cols_anyna > 0) {
    HTML(paste(
      .col(n_cols_anyna),
      ngettext(n_cols_anyna, "feature includes", "features include"),
      "'NA' values; ",
      .col(n_na),
      "'NA'",
      ngettext(n_na, "value", "values"),
      "total",
      tags$ul(
        lapply(seq_along(classes_na), \(i) {
          tags$li(HTML(paste(
            .col(classes_na[i]),
            tolower(names(classes_na)[i])
            # ngettext(classes_na[i], "feature", "features")
          )))
        })
      )
    ))
  } else {
    HTML(paste(strong("0"), "missing values"))
  }

  ## Recs ----
  rec_char <- NULL
  rec_constant <- if (n_constant > 0) {
    tags$li(HTML(paste(html_orange(
      "Remove the constant",
      ngettext(n_constant, "feature", "features")
    ))))
  } else {
    NULL
  }

  rec_dups <- if (n_dups > 0) {
    tags$li(HTML(paste(html_orange(
      "Consider removing the duplicate",
      ngettext(n_dups, "case", "cases")
    ))))
  } else {
    NULL
  }

  # rec_na <- if (n_cols_anyna > 0) {
  #     tags$li(HTML(paste(html_orange("Consider imputing missing values or use complete cases only"))))
  # } else {
  #     NULL
  # }

  rec_na <- if (n_cols_anyna > 0) {
    list(
      if (isTRUE(classes_na["factor"] > 0)) {
        tags$li(HTML(paste(html_orange(
          "Consider assigning factor 'NA' values to new 'missing' level"
        ))))
      },
      tags$li(HTML(paste(html_orange(
        "Consider imputing missing values or using algorithms that can handle missing values"
      ))))
    )
  } else {
    NULL
  }

  recs <- if (sum(n_constant, n_dups, n_cols_anyna) == 0) {
    tags$li(html_success("Everything looks good"))
  } else {
    list(
      rec_constant,
      rec_dups,
      rec_na
    )
  }
  ## out ----
  div(
    p(
      div(
        html_highlight(name),
        ": A",
        x$class,
        "with",
        html_highlight(n_rows),
        ngettext(n_rows, "row", "rows"),
        "and",
        html_highlight(n_cols),
        ngettext(n_cols, "feature", "features"),
        class = "checkdata-header"
      )
    ),
    p(
      span(strong("Data types"), class = "sidelined"),
      tags$ul(
        tags$li(numeric),
        tags$li(integer),
        tags$li(categorical),
        tags$li(characters),
        tags$li(dates)
      )
    ), # p Data Types
    p(
      span(strong("Issues"), class = "sidelined"),
      tags$ul(
        tags$li(constants),
        tags$li(duplicates),
        tags$li(nas)
      )
    ), # p Issues
    p(
      span(strong("Recommendations"), class = "sidelined"),
      tags$ul(
        recs
      )
    ), # p Recommendations
    class = "checkData",
    style = paste0(
      "font-family:",
      css$font.family,
      "; color:",
      css$color,
      "; background-color:",
      css$background.color,
      ";"
    )
  )
}

#' Print `CheckData` object
#'
#' @method print CheckData
#' @param x `CheckData` object.
#' @param type Character: Output type: "plaintext" or "html".
#' @param name Character: Dataset name.
#' @param check_integers Logical: If TRUE and there are integer features, prints a
#' message to consider converting to factors.
#' @param css List with `font.family`, `color`, and `background.color` elements.
#' @param ... Not used.
#'
#' @author E.D. Gennatas
#' @export
print.CheckData <- function(
  x,
  type = c("plaintext", "html"),
  name = NULL,
  check_integers = FALSE,
  css = list(
    font.family = "Helvetica",
    color = "#fff",
    background.color = "#242424"
  ),
  ...
) {
  if (is.null(name)) {
    name <- x$name
    if (is.null(name)) name <- deparse(substitute(x))
  }
  type <- match.arg(type)

  n_rows <- x$n_rows
  n_cols <- x$n_cols
  n_numeric <- x$n_numeric
  n_integer <- x$n_integer
  n_character <- x$n_character
  n_factor <- x$n_factor
  n_ordered <- x$n_ordered
  n_date <- x$n_date
  n_constant <- x$n_constant
  n_dups <- x$n_dups
  n_cols_anyna <- x$n_cols_anyna
  n_na <- x$n_na
  na_feature_pct <- x$na_feature_pct
  na_case_pct <- x$na_case_pct

  if (type == "plaintext") {
    # plaintext out ----
    out <- paste0(
      "  ",
      hilite(name),
      paste(
        ": A",
        x$class,
        "with",
        hilite(n_rows),
        ngettext(n_rows, "row", "rows"),
        "and",
        hilite(n_cols),
        ngettext(n_cols, "column", "columns")
      )
    )
    ## Data Types ----
    out <- paste(
      out,
      bold("\n  Data types"),
      paste(
        "  *",
        bold(n_numeric),
        "numeric",
        ngettext(n_numeric, "feature", "features")
      ),
      paste(
        "  *",
        bold(n_integer),
        "integer",
        ngettext(n_integer, "feature", "features")
      ),
      sep = "\n"
    )
    isOrdered <- if (n_factor == 1) {
      paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
    } else if (n_factor > 1) {
      paste(
        ", of which",
        bold(n_ordered),
        ngettext(n_ordered, "is", "are"),
        "ordered"
      )
    } else {
      ""
    }
    out <- paste(
      out,
      paste0(
        "  * ",
        bold(n_factor),
        ngettext(n_factor, " factor", " factors"),
        isOrdered
      ),
      sep = "\n"
    )
    out <- paste(
      out,
      paste(
        "  *",
        bold(n_character),
        "character",
        ngettext(n_character, "feature", "features")
      ),
      sep = "\n"
    )
    out <- paste(
      out,
      paste(
        "  *",
        bold(n_date),
        "date",
        ngettext(n_date, "feature", "features")
      ),
      sep = "\n"
    )
    ## Issues ----
    out <- paste(out, bold("\n  Issues"), sep = "\n")
    fmt <- ifelse(n_constant > 0, red, I)
    out <- paste(
      out,
      paste(
        "  *",
        bold(fmt(n_constant)),
        "constant",
        ngettext(n_constant, "feature", "features")
      ),
      sep = "\n"
    )
    fmt <- ifelse(n_dups > 0, orange, I)
    out <- paste(
      out,
      paste(
        "  *",
        bold(fmt(n_dups)),
        "duplicate",
        ngettext(n_dups, "case", "cases")
      ),
      sep = "\n"
    )
    nas <- if (n_cols_anyna > 0) {
      classes_na <- x$classes_na
      .col <- if (n_cols_anyna > 0) orange else I
      paste(
        bold(.col(n_cols_anyna)),
        ngettext(n_cols_anyna, "feature includes", "features include"),
        "'NA' values;",
        bold(.col(n_na)),
        "'NA'",
        ngettext(n_na, "value", "values"),
        "total\n    *",
        paste0(
          sapply(seq_along(classes_na), \(i) {
            paste(
              bold(.col(classes_na[i])),
              tolower(names(classes_na)[i])
            )
          }),
          collapse = "\n    * "
        )
      )
    } else {
      paste(bold("0"), "missing values")
    }
    out <- paste0(out, "\n  * ", nas)

    ## Recommendations ----
    out <- paste(out, bold("\n  Recommendations"), sep = "\n")
    if (sum(n_character, n_constant, n_dups, n_cols_anyna) > 0) {
      if (n_character > 0) {
        out <- paste(
          out,
          bold(orange(
            "  * Consider converting character features to factors or excluding them"
          )),
          sep = "\n"
        )
      }
      if (n_constant > 0) {
        out <- paste(
          out,
          bold(red(paste(
            "  * Remove the constant",
            ngettext(n_constant, "feature", "features")
          ))),
          sep = "\n"
        )
      }

      if (n_dups > 0) {
        out <- paste(
          out,
          bold(orange(paste(
            "  * Consider removing the duplicate",
            ngettext(n_dups, "case", "cases")
          ))),
          sep = "\n"
        )
      }

      if (n_cols_anyna > 0) {
        out <- paste(
          out,
          bold(orange(paste(
            "  * Consider imputing missing values or use complete cases only"
          ))),
          sep = "\n"
        )
      }
      if (check_integers && n_integer > 0) {
        out <- paste(
          out,
          paste0(
            "  * Check the",
            ifelse(n_integer > 1, paste("", n_integer, ""), " "),
            "integer",
            ngettext(n_integer, " feature", " features"),
            " and consider if",
            ngettext(n_integer, " it", " they"),
            " should be converted to ",
            ngettext(n_integer, "factor", "factors")
          ),
          sep = "\n"
        )
      }
    } else {
      out <- paste(
        out,
        green("  * Everything looks good", bold = TRUE),
        sep = "\n"
      )
    }
    cat(out, "\n")
  } else {
    # HTML ----
    htmltools::html_print(
      tohtml(x),
      background = css$background.color
    )
  }
  invisible(x)
} # print.CheckData
