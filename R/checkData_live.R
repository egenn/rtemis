# checkData_live.R
# ::rtemis::
# 2022 E.D. Gennatas lambdamd.org

#' Check Data (HTML output)
#'
#' @param x data.frame, data.table or similar structure
#' @param name Character: Dataset name
#' @param recommend Logical: If TRUE, output recommendations at the end.
#' Default = TRUE
#' @param font.family Character: Font family to use. Default = "Fira Sans"
#' @param color Character: Text color. Default = "#e7e7e7"
#' @param background.color Character: Background color. Default = "transparent"
#' @param class Character: CSS class to assign to div output.
#' Default = "checkData"
#' @param verbose Logical: If TRUE, print output in HTML viewer.
#' Default = TRUE
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' ir <- iris
#' ir[12:15, 1] <- ir[103:107, 3] <- ir[58:61, 5] <- NA
#' checkData_live(ir)
#' }
#'
checkData_live <- function(x,
                           name = NULL,
                           recommend = TRUE,
                           font.family = "Fira Sans",
                           color = "#e7e7e7",
                           background.color = "#121212",
                           class = "checkData",
                           verbose = TRUE) {
    if (is.null(name)) name <- deparse(substitute(x))
    n.rows <- NROW(x)
    n.cols <- NCOL(x)

    # Data Types ----
    classes <- sapply(x, base::class)

    ## Continuous ----
    index.continuous <- which(sapply(x, function(i) is.numeric(i) & !is.integer(i)))
    n.continuous <- length(index.continuous)

    ## Integer ----
    index.integer <- which(sapply(x, is.integer))
    n.integer <- length(index.integer)

    ## Categorical ----
    index.factor <- which(sapply(x, is.factor))
    n.factor <- length(index.factor)
    index.ordered <- which(sapply(x, is.ordered))
    n.ordered <- length(index.ordered)
    index.gt2levels.nonordered <- which(sapply(x[, setdiff(index.factor, index.ordered), drop = FALSE], function(x) length(levels(x))) > 2)
    n.gt2levels.nonordered <- length(index.gt2levels.nonordered)

    ## Characters ----
    index.character <- which(sapply(x, is.character))
    n.character <- length(index.character)

    ## Dates ----
    index.date <- which(sapply(x, function(col) inherits(col, "Date")))
    n.date <- length(index.date)

    # Issues ----

    ## Constants ----
    index.constant <- which(sapply(x, is.constant))
    n.constant <- length(index.constant)

    ## Duplicates ----
    cindex.dups <- which(duplicated(x))
    n.dups <- length(cindex.dups)

    ## NAs ----
    cols.anyna <- which(sapply(x, anyNA))
    n.cols.anyna <- length(cols.anyna)
    index.na <- which(is.na(x))
    n.na <- length(index.na)

    ## Get percent of NA values per feature and per case
    if (n.cols.anyna > 0) {
        na.feature.pct <- data.frame(
            Feature = names(cols.anyna),
            Pct.NA = sapply(seq_len(n.cols.anyna), function(i) {
                sum(is.na(x[, cols.anyna[i]])) / length(x[, cols.anyna[i]])
            })
        )

        index.incomplete <- which(!complete.cases(x))
        n.incomplete <- length(index.incomplete)
        na.case.pct <- data.frame(
            Case = index.incomplete,
            Pct.NA = sapply(seq_len(n.incomplete), function(i) {
                sum(is.na(x[index.incomplete[i], ])) / length(x[index.incomplete[i], ])
            })
        )

        # Get types of features with NA
        classes.na <- table(classes[cols.anyna])

    } else {
        na.feature.pct <- na.case.pct <- rep(0, n.cols)
    }

    # HTML out ----

    ## [] Data Types ----
    continuous <- HTML(paste(
        strong(n.continuous), "continuous",
        ngettext(n.continuous, "feature", "features")
    ))
    integer <- HTML(paste(
        strong(n.integer), "integer",
        ngettext(n.integer, "feature", "features")
    ))
    categorical <- HTML(paste0(
        strong(n.factor), " categorical",
        ngettext(n.factor, " feature", " features"),
        if (n.factor == 1) {
            paste(", which", ngettext(n.ordered, "is", "is not"), "ordered")
        } else if (n.factor > 1) {
            paste(
                ", of which", strong(n.ordered),
                ngettext(n.ordered, "is", "are"), "ordered"
            )
        }
    ))
    # .col <- if (n.character > 0) html_orange else strong
    .col <- strong
    characters <- HTML(paste(
        .col(n.character), "character",
        ngettext(n.character, "feature", "features")
    ))
    dates <- HTML(paste(
        strong(n.date), "date",
        ngettext(n.date, "feature", "features")
    ))

    ## [] Issues ----
    .col <- if (n.constant > 0) html_red else strong
    constants <- HTML(paste(
        .col(n.constant), "constant",
        ngettext(n.constant, "feature", "features")
    ))
    .col <- if (n.dups > 0) html_red else strong
    duplicates <- HTML(paste(
        .col(n.dups), "duplicate",
        ngettext(n.dups, "case", "cases")
    ))


    .col <- if (n.cols.anyna > 0) html_orange else strong
    nas <- if (n.cols.anyna > 0) {
        HTML(paste(
            .col(n.na), "missing",
            ngettext(n.na, "value", "values"),
            "total in", .col(n.cols.anyna),
            ngettext(n.cols.anyna, "feature", "features"),
            tags$ul(
                lapply(seq_along(classes.na), \(i) {
                    tags$li(HTML(paste(
                        .col(classes.na[i]),
                        tolower(names(classes.na)[i]),
                        ngettext(classes.na[i], "feature", "features")
                    )))
                })
            )
        ))
    } else {
        HTML(paste(strong("0"), "missing values"))
    }

    ## [] Recs html ----
    rec_char <- NULL
    rec_constant <- if (n.constant > 0) {
        tags$li(HTML(paste(html_orange(
            "Remove the constant",
            ngettext(n.constant, "feature", "features")
        ))))
    } else {
        NULL
    }

    rec_dups <- if (n.dups > 0) {
        tags$li(HTML(paste(html_orange(
            "Remove the duplicate",
            ngettext(n.dups, "case", "cases")
        ))))
    } else {
        NULL
    }

    # rec_na <- if (n.cols.anyna > 0) {
    #     tags$li(HTML(paste(html_orange("Consider imputing missing values or use complete cases only"))))
    # } else {
    #     NULL
    # }

    rec_na <- if (n.cols.anyna > 0) {
        list(
            if (isTRUE(classes.na["factor"] > 0)) {
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

    recs <- if (sum(n.constant, n.dups, n.cols.anyna, n.gt2levels.nonordered) == 0) {
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
                html_highlight(n.rows),
                ngettext(n.rows, "row", "rows"),
                "and", html_highlight(n.cols),
                ngettext(n.cols, "feature", "features"),
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
        class = class,
        style = paste0(
            "font-family:'", font.family,
            "'; color:", color,
            "; background-color:", background.color, ";"
        )
    )

    if (verbose) {
        html_print(out,
            background = background.color
        )
    }

    invisible(list(
        out = out,
        n.rows = n.rows,
        n.cols = n.cols,
        n.continuous = n.continuous,
        n.integer = n.integer,
        n.factor = n.factor,
        n.ordered = n.ordered,
        n.constant = n.constant,
        n.cols.anyna = n.cols.anyna,
        n.na = n.na,
        na.feature.pct = na.feature.pct,
        na.case.pct = na.case.pct
    ))
} # rtemis::checkData_live

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
