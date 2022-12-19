# check_data2.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Check Data v2
#'
#' @param x data.frame, data.table or similar structure
#' @param name Character: Dataset name
#' @param recommend Logical: If TRUE, output recommendations at the end.
#' @param font.family Character: Font family to use.
#' @param color Character: Text color.
#' @param background.color Character: Background color.
#' @param class Character: CSS class to assign to div output.
#' @param verbose Logical: If TRUE, print output in HTML viewer.
#'
#' @author E.D. Gennatas
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
#' check_data2(x)
#' }
#'
check_data2 <- function(x,
                        name = NULL,
                        recommend = TRUE,
                        font.family = "'Inter', sans-serif",
                        color = "#e7e7e7",
                        background.color = "#121212",
                        # out_class = "checkData",
                        verbose = TRUE) {
    if (is.null(name)) name <- deparse(substitute(x))
    x <- as.data.table(x)
    n_rows <- NROW(x)
    n_cols <- NCOL(x)

    # Data Types ----
    classes <- sapply(x, \(v) class(v)[1])
    counts <- table(classes)

    ## Continuous ----
    # index_continuous <- which(sapply(x, \(i) is.double(i)))
    # n_continuous <- length(index_continuous)
    n_continuous <- max0(counts["numeric"])

    ## Integer ----
    # index_integer <- which(sapply(x, is.integer))
    # n_integer <- length(index_integer)
    n_integer <- max0(counts["integer"])

    ## Categorical ----
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

    ## Characters ----
    # index_character <- which(sapply(x, is.character))
    # n_character <- length(index_character)
    n_character <- max0(counts["character"])

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
    n_dups <- n_rows - uniqueN(x)

    ## NAs ----
    cols_anyna <- which(sapply(x, anyNA))
    n_cols_anyna <- length(cols_anyna)
    index_na <- which(is.na(x))
    n_na <- length(index_na)

    ## Get percent of NA values per feature and per case
    if (n_cols_anyna > 0) {
        na_feature_pct <- data.frame(
            Feature = names(cols_anyna),
            Pct_NA = sapply(seq_len(n_cols_anyna), \(i) {
                sum(is.na(x[[cols_anyna[1]]])) / n_cols
            })
        )

        index_incomplete <- which(!complete.cases(x))
        n_incomplete <- length(index_incomplete)
        na_case_pct <- data.frame(
            Case = index_incomplete,
            Pct_NA = sapply(seq_len(n_incomplete), \(i) {
                sum(is.na(x[index_incomplete[i], ])) / n_rows
            })
        )

        # Get types of features with NA
        classes_na <- table(classes[cols_anyna])
    } else {
        na_feature_pct <- na_case_pct <- rep(0, n_cols)
    }

    cd <- list(
        class = class(x)[1],
        n_rows = n_rows,
        n_cols = n_cols,
        n_continuous = n_continuous,
        n_integer = n_integer,
        n_factor = n_factor,
        n_ordered = n_ordered,
        n_constant = n_constant,
        n_dups = n_dups,
        n_cols_anyna = n_cols_anyna,
        n_na = n_na,
        na_feature_pct = na_feature_pct,
        na_case_pct = na_case_pct
    )
    class(cd) <- c("CheckData", "list")
    cd
} # rtemis::check_data2

html_highlight <- function(..., bold = TRUE) {
    if (bold) {
        span(..., style = "color: #16A0AC; font-weight: 700;")
    } else {
        span(..., style = "color: #16A0AC;")
    }
}

html_orange <- function(..., bold = TRUE) {
    if (bold) {
        span(..., style = "color: #FA6E1E; font-weight: 700;")
    } else {
        span(..., style = "color: #FA6E1E;")
    }
}

html_red <- function(..., bold = TRUE) {
    if (bold) {
        span(..., style = "color: #E61048; font-weight: 700;")
    } else {
        span(..., style = "color: #E61048;")
    }
}

html_success <- function(..., bold = TRUE) {
    if (bold) {
        span(..., style = "color: #32A03E; font-weight: 700;")
    } else {
        span(..., style = "color: #32A03E;")
    }
}


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

print.CheckData <- function(x, type = c("plaintext", "html"), ...) {
    type <- match.arg(type)

    n_rows <- x$n_rows
    n_cols <- x$n_cols
    n_continuous <- x$n_continuous
    n_integer <- x$n_integer
    n_factor <- x$n_factor
    n_ordered <- x$n_ordered
    n_constant <- x$n_constant
    n_dups <- x$n_dups
    n_cols_anyna <- x$n_cols_anyna
    n_na <- x$n_na
    na_feature_pct <- x$na_feature_pct
    na_case_pct <- x$na_case_pct

    if (type == "plaintext") {
        out <- paste0(
            " ", hilite(name),
            paste(
                ": A", x$class, "with",
                hilite(n_rows), ngettext(n_rows, "row", "rows"),
                "and", hilite(n_cols),
                ngettext(n_cols, "column", "columns")
            )
        )
        out <- paste(out,
            paste0(bold("\n  Data types"), "________________"),
            paste(
                "  *", bold(n_continuous), "continuous",
                ngettext(n_continuous, "feature", "features")
            ),
            paste(
                "  *", bold(n_integer), "integer",
                ngettext(n_integer, "feature", "features")
            ),
            sep = "\n"
        )
        isOrdered <- if (n_factor == 1) {
            paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
        } else if (n_factor > 1) {
            paste(", of which", bold(n_ordered), ngettext(n_ordered, "is", "are"), "ordered")
        } else {
            ""
        }
        out <- paste(out,
            paste0(
                "  * ", bold(n_factor), " categorical",
                ngettext(n_factor, " feature", " features"),
                isOrdered
            ),
            sep = "\n"
        )
        out <- paste(out,
            paste(
                "  *", bold(n_character), "character",
                ngettext(n_character, "feature", "features")
            ),
            sep = "\n"
        )
        out <- paste(out,
            paste(
                "  *", n_date, "date",
                ngettext(n_date, "feature", "features")
            ),
            sep = "\n"
        )
        out <- paste(out,
            paste0(bold("\n  Issues"), "____________________"),
            sep = "\n"
        )
        out <- paste(out,
            paste(
                "  *", n_constant, "constant",
                ngettext(n_constant, "feature", "features")
            ),
            sep = "\n"
        )
        out <- paste(out,
            paste(
                "  *", n_dups, "duplicate",
                ngettext(n_dups, "case", "cases")
            ),
            sep = "\n"
        )
        .col <- if (n_cols_anyna > 0) orange else I
        out <- paste(out,
            paste0(
                "  * ", bold(.col(n_cols_anyna)),
                ngettext(
                    n_cols_anyna, " feature includes",
                    " features include"
                ), " 'NA' values",
                ifelse(n_cols_anyna > 0, paste(
                    ";", bold(.col(n_na)), "'NA'",
                    ngettext(n_na, "value", "values"), "total"
                ), "")
            ),
            sep = "\n"
        )
        out <- paste(out,
            paste0(bold("\n  Recommendations"), "___________"),
            sep = "\n"
        )
        if (sum(n_character, n_constant, n_dups, n_cols_anyna) > 0) {

            if (n_constant > 0) {
                out <- paste(out,
                    bold(red(paste(
                        "  * Remove the constant",
                        ngettext(n_constant, "feature", "features")
                    ))),
                    sep = "\n"
                )
            }

            if (n_dups > 0) {
                out <- paste(out,
                    bold(orange(paste(
                        "  * Consider removing the duplicate",
                        ngettext(n_dups, "case", "cases")
                    ))),
                    sep = "\n"
                )
            }

            if (n_cols_anyna > 0) {
                out <- paste(out,
                    bold(orange(paste(
                        "  * Consider imputing missing values or use complete cases only"
                    ))),
                    sep = "\n"
                )
            }
            if (n_integer > 0) {
                out <- paste(out,
                    paste0(
                        "  * Check the",
                        ifelse(n_integer > 1, paste("", n_integer, ""), " "),
                        "integer",
                        ngettext(n_integer, " feature", " features"),
                        " and consider if",
                        ngettext(n_integer, " it", " they"), " should be converted to ",
                        ngettext(n_integer, "factor", "factors")
                    ),
                    sep = "\n"
                )
            }
        } else {
            out <- paste(out,
                green("  * Everything looks good", bold = TRUE),
                sep = "\n"
            )
        }

        cat(out, "\n")
    } else {
        # HTML out ----

        ## [] Data Types ----
        continuous <- HTML(paste(
            strong(n_continuous), "continuous",
            ngettext(n_continuous, "feature", "features")
        ))
        integer <- HTML(paste(
            strong(n_integer), "integer",
            ngettext(n_integer, "feature", "features")
        ))
        categorical <- HTML(paste0(
            strong(n_factor), " categorical",
            ngettext(n_factor, " feature", " features"),
            if (n_factor == 1) {
                paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
            } else if (n_factor > 1) {
                paste(
                    ", of which", strong(n_ordered),
                    ngettext(n_ordered, "is", "are"), "ordered"
                )
            }
        ))
        # .col <- if (n_character > 0) html_orange else strong
        .col <- strong
        characters <- HTML(paste(
            .col(n_character), "character",
            ngettext(n_character, "feature", "features")
        ))
        dates <- HTML(paste(
            strong(n_date), "date",
            ngettext(n_date, "feature", "features")
        ))

        ## [] Issues ----
        .col <- if (n_constant > 0) html_red else strong
        constants <- HTML(paste(
            .col(n_constant), "constant",
            ngettext(n_constant, "feature", "features")
        ))
        .col <- if (n_dups > 0) html_orange else strong
        duplicates <- HTML(paste(
            .col(n_dups), "duplicate",
            ngettext(n_dups, "case", "cases")
        ))

        .col <- if (n_cols_anyna > 0) html_orange else strong
        nas <- if (n_cols_anyna > 0) {
            HTML(paste(
                .col(n_na), "missing",
                ngettext(n_na, "value", "values"),
                "total in", .col(n_cols_anyna),
                ngettext(n_cols_anyna, "feature", "features"),
                tags$ul(
                    lapply(seq_along(classes_na), \(i) {
                        tags$li(HTML(paste(
                            .col(classes_na[i]),
                            tolower(names(classes_na)[i]),
                            ngettext(classes_na[i], "feature", "features")
                        )))
                    })
                )
            ))
        } else {
            HTML(paste(strong("0"), "missing values"))
        }

        ## [] Recs html ----
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
                    "Consider imputing missing values or using complete cases only"
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
        # HTML out ----
        out <- div(
            p(
                div(
                    html_highlight(name),
                    ": A", class(x)[1], "with",
                    html_highlight(n_rows),
                    ngettext(n_rows, "row", "rows"),
                    "and", html_highlight(n_cols),
                    ngettext(n_cols, "feature", "features"),
                    class = "checkdata-header"
                )
            ),
            p(
                span(strong("Data types"), class = "sidelined"),
                tags$ul(
                    tags$li(continuous),
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
                "font-family:'", font.family,
                "'; color:", color,
                "; background-color:", background.color, ";"
            )
        )
        htmltools::html_print(out,
            background = background.color
        )
    }
} # print.CheckData
