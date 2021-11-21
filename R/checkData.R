# checkData.R
# ::rtemis::
# 2017-21 E.D. Gennatas lambdamd.org

#' Check Data
#'
#' Runs a series of simple checks on a dataset that may be important to perform ahead of
#' data analysis. This function makes no changes to data, but reports potential course of action
#' that can be taken using \link{preprocess}
#'
#' @param x Input dataset: will be converted to data.frame
#' @param name String (optional): Name of dataset. (This is helpful when applying \code{preprocess}
#' on a list of items by vectorization, e.g. using *ply commands, where the names of the list
#' elements will not be displayed correctly)
#' @param str Logical: If TRUE, show output of \code{str}. Default = FALSE
#' @param recommend Logical: If TRUE, print recommendations based on check. Default = TRUE
#' @param reportCases.thres Float (0, 1]: Report, by number, all cases missing greater or equal to
#' this fraction of features. Default = NULL
#' @param reportFeatures.thres Float (0, 1]: Report, by name, all features missing in greater or
#' equal to this fraction of cases. Default = NULL
#' @param toHTML Logical: If TRUE, convert output to HTML using the \code{fansi} package.
#' Default = FALSE
#' @param verbose Logical: If TRUE, print to console
#' @author E.D. Gennatas
#' @examples
#' checkData(iris)
#' @export

checkData <- function(x,
                      name = NULL,
                      str = FALSE,
                      recommend = TRUE,
                      reportCases.thres = NULL,
                      reportFeatures.thres = NULL,
                      toHTML = FALSE,
                      verbose = TRUE) {

  if (is.null(name)) name <- deparse(substitute(x))
  n.rows <- NROW(x)
  n.cols <- NCOL(x)
  out <- paste0(" ", rtHighlight$bold(name),
                paste(": A", class(x)[1], "with",
                      rtHighlight$bold(n.rows), ngettext(n.rows, "row", "rows"),
                      "and", rtHighlight$bold(n.cols), ngettext(n.cols, "feature", "features")))

  x <- as.data.frame(x)
  # This way avoids running line before all terminal colors are available
  orange <- crayon::make_style(orange = "orange")

  # Data Types ====

  ## Integers ====
  index.integer <- which(sapply(x, is.integer))
  n.integer <- length(index.integer)

  ## Floats ====
  index.continuous <- which(sapply(x, function(i) is.numeric(i) & !is.integer(i)))
  n.continuous <- length(index.continuous)

  ## Factors ====
  index.factor <- which(sapply(x, is.factor))
  n.factor <- length(index.factor)
  index.ordered <- which(sapply(x, is.ordered))
  n.ordered <- length(index.ordered)

  out <- paste(out,
               paste0(bold("\n  Data types"), "________________"),
               paste("  *", bold(n.continuous), "continuous", ngettext(n.continuous, "feature", "features")),
               paste("  *", bold(n.integer), "integer", ngettext(n.integer, "feature", "features")),
               sep = "\n")
  isOrdered <- if (n.factor == 1) {
    paste(", which", ngettext(n.ordered, "is", "is not"), "ordered")
  } else if (n.factor > 1) {
    paste(", of which", bold(n.ordered), ngettext(n.ordered, "is", "are"), "ordered")
  } else {
    ""
  }
  out <- paste(out,
               paste0("  * ", bold(n.factor), " categorical", ngettext(n.factor, " feature", " features"),
                      isOrdered),
               sep = "\n")
  index.gt2levels.nonordered <- which(sapply(x[, setdiff(index.factor, index.ordered), drop = FALSE], function(x) length(levels(x))) > 2)
  n.gt2levels.nonordered <- length(index.gt2levels.nonordered)
  if (n.gt2levels.nonordered > 0) {
    out <- paste(out,
                 paste("     -", bold(n.gt2levels.nonordered), "unordered categorical",
                       ngettext(n.gt2levels.nonordered, "feature has", "features have"),
                       "more than 2 levels"),
                 sep = "\n")
  }

  ## Characters ====
  index.character <- which(sapply(x, is.character))
  n.character <- length(index.character)
  .col <- if (n.character > 0) orange$bold else bold
  out <- paste(out,
               paste("  *", .col(n.character), "character", ngettext(n.character, "feature", "features")),
               sep = "\n")

  ## Dates ====
  index.date <- which(sapply(x, function(col) inherits(col, "Date")))
  n.date <- length(index.date)
  out <- paste(out,
               paste("  *", n.date, "date", ngettext(n.date, "feature", "features")),
               sep = "\n")

  # Issues ====

  ## Constants ====
  out <- paste(out,
               paste0(bold("\n  Issues") ,"____________________"),
               sep = "\n")
  index.constant <- which(sapply(x, is.constant))
  n.constant <- length(index.constant)
  .col <- if (n.constant > 0) red$bold else bold
  out <- paste(out,
               paste("  *", .col(n.constant), "constant", ngettext(n.constant, "feature", "features")),
               sep = "\n")

  ## Duplicates ====
  cindex.dups <- which(duplicated(x))
  n.dups <- length(cindex.dups)
  .col <- if (n.dups > 0) red$bold else bold
  out <- paste(out,
               paste("  *", .col(n.dups), "duplicated", ngettext(n.dups, "case", "cases")),
               sep = "\n")

  ## NAs ====
  cols.anyna <- which(sapply(x, anyNA))
  n.cols.anyna <- length(cols.anyna)
  index.na <- which(is.na(x))
  n.na <- length(index.na)

  ## Get percent of NA values per feature and per case
  if (n.cols.anyna > 0) {
    na.feature.pct <- data.frame(Feature = names(cols.anyna),
                                 Pct.NA = sapply(seq_len(n.cols.anyna), function(i)
                                   sum(is.na(x[, cols.anyna[i]])) / length(x[, cols.anyna[i]])))

    index.incomplete <- which(!complete.cases(x))
    n.incomplete <- length(index.incomplete)
    na.case.pct <- data.frame(Case = index.incomplete,
                              Pct.NA = sapply(seq_len(n.incomplete), function(i)
                                sum(is.na(x[index.incomplete[i], ])) / length(x[index.incomplete[i], ])))

  } else {
    na.feature.pct <- na.case.pct <- rep(0, n.cols)
  }
  .col <- if (n.cols.anyna > 0) orange$bold else bold
  out <- paste(out,
               paste0("  * ", .col(n.cols.anyna), ngettext(n.cols.anyna, " feature includes", " features include"), " 'NA' values",
                      ifelse(n.cols.anyna > 0, paste(";", .col(n.na), "'NA'", ngettext(n.na, "value", "values"), "total"), "")),
               sep = "\n")
  if (n.cols.anyna > 0) {
    if (!is.null(reportFeatures.thres)) {
      features.na.over.thres <- na.feature.pct[na.feature.pct$Pct.NA >= reportFeatures.thres, ]
      if (NROW(features.na.over.thres) > 0) {
        out <- paste(out,
                     paste0("     - ", .col(NROW(features.na.over.thres)), " features missing in >= ",
                            reportFeatures.thres*100, "% of cases:\n      *** ",
                            paste0(features.na.over.thres$Feature, ": ", ddSci(features.na.over.thres$Pct.NA),
                                   collapse = "\n      *** ")), sep = "\n")
      }
    } else {
      max.na.feature.name <- names(cols.anyna)[which.max(na.feature.pct$Pct.NA)]
      out <- paste(out,
                   paste0("     - Max percent missing in a feature is ", .col(ddSci(max(na.feature.pct$Pct.NA)*100)), .col("%"), " (",
                          bold(max.na.feature.name), ")"),
                   sep = "\n")
    }

    if (!is.null(reportCases.thres)) {
      cases.na.over.thres <- na.case.pct[na.case.pct$Pct.NA >= reportCases.thres, ]
      n.cases.na.over.thres <- NROW(cases.na.over.thres)
      if (n.cases.na.over.thres > 0 ) {
        out <- paste(out,
                     paste0("     - ", .col(n.cases.na.over.thres), ngettext(n.cases.na.over.thres, " case", " cases"),
                            " missing >= ", reportCases.thres*100, "% of features:\n      *** ",
                            paste0("#", cases.na.over.thres$Case, ": ", ddSci(cases.na.over.thres$Pct.NA),
                                   collapse = "\n      *** ")),
                     sep = "\n")
      }
    } else {
      max.na.case.number <- index.incomplete[which.max(na.case.pct$Pct.NA)]
      out <- paste(out,
                   paste0("     - Max percent missing in a case is ", .col(ddSci(max(na.case.pct$Pct.NA)*100)), .col("%"), " (case #",
                          bold(max.na.case.number), ")"), sep = "\n")
    }

  }

  # Recommendations ====
  if (recommend) {
    out <- paste(out,
                 paste0(bold("\n  Recommendations") ,"___________"), sep = "\n")
    if (sum(n.character, n.constant, n.dups, n.cols.anyna, n.gt2levels.nonordered) > 0) {
      if (n.character > 0) {
        out <- paste(out,
                     paste(orange$bold("  * Convert the character",
                                  ngettext(n.character, "feature", "features"), "to" ,
                                  ngettext(n.character, "a factor", "factors"))),
                     sep = "\n")
      }

      if (n.constant > 0) {
        out <- paste(out,
                     paste(orange$bold("  * Remove the constant", ngettext(n.constant, "feature", "features"))),
                     sep = "\n")
      }

      if (n.dups > 0) {
        out <- paste(out,
                     paste(orange$bold("  * Remove the duplicated", ngettext(n.dups, "case", "cases"))),
                     sep = "\n")
      }

      if (n.cols.anyna > 0) {
        out <- paste(out,
                     paste(orange$bold("  * Consider imputing missing values or use complete cases only")),
                     sep = "\n")
      }

      if (n.gt2levels.nonordered > 0) {
        out <- paste(out,
                     paste0("  * Check the", ifelse(n.gt2levels.nonordered > 1, paste("", n.gt2levels.nonordered, ""), " "),
                            "unordered categorical",
                            ifelse(n.gt2levels.nonordered > 1, " features", " feature"),
                            " with more than 2 levels and consider\n    if ordering would make sense"),
                     sep = "\n")
      }
      if (n.integer > 0) {
        out <- paste(out,
                     paste0("  * Check the", ifelse(n.integer > 1, paste("", n.integer, ""), " "),
                            "integer", ngettext(n.integer, " feature", " features"),
                            " and consider if", ngettext(n.integer, " it", " they"), " should be converted to ",
                            ngettext(n.integer, "factor", "factors")),
                     sep = "\n")
      }
    } else {
      out <- paste(out,
                   green("  * Everything looks good"),
                   sep = "\n")
    }

  }

  if (toHTML) {
    out <- gsub("\n", "<br>", out) |> fansi::sgr_to_html()
  }
  if (verbose) cat(out)

  # str() ====
  if (str & verbose) {
    cat(bold("\n\n  Feature structure", "_________\n"))
    str(x)
    cat("\n")
  }

  invisible(list(out = out,
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
                 na.case.pct = na.case.pct))

} # rtemis::checkData
