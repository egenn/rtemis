# checkData.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Check Data
#'
#' Performs a series of simple checks on a dataset that may be important to perform ahead of
#' data analysis. This function makes no changes to data, but reports potential course of action
#' that can be taken using \link{preprocess}
#'
#' @param x Input dataset; will be converted to data.frame
#' @param name String, optional: Name of dataset. (This is helpful when applying \code{preprocess}
#' on a list of items using by vectorization, e.g. using *ply commands, where the names of the list
#' elements will not be displayed correctly)
#' @param str Logical: If TRUE, show output of \code{str}
#' @param recommend Logical: If TRUE, print recommendations based on check. Default = TRUE
#' @param reportCases.thres Float (0, 1]: Report, by number, all cases missing greater or equal to
#' this fraction of features
#' @param reportFeatures.thres Float (0, 1]: Report, by name, all features missing in greater or
#' equal to this fraction of cases
#' @author Efstathios D. Gennatas
#' @export

checkData <- function(x,
                      name = NULL,
                      str = FALSE,
                      recommend = TRUE,
                      reportCases.thres = NULL,
                      reportFeatures.thres = NULL) {


  if (is.null(name)) name <- deparse(substitute(x))
  cat("\n  Dataset:", rtHighlight$bold(name), "\n")
  boxcat("Summary", pad = 2)
  x <- as.data.frame(x)

  # [ Report ] ====
  # Dimensions
  n.rows <- NROW(x)
  n.cols <- NCOL(x)
  cat(" ", bold(n.rows), ifelse(n.rows == 1, "case", "cases"),
      "with", bold(n.cols), ifelse(n.cols == 1, "feature:", "features:"), "\n")

  # [ Integers ] ====
  index.integer <- which(sapply(x, is.integer))
  n.integer <- length(index.integer)

  # [ Floats ] ====
  index.continuous <- which(sapply(x, function(i) is.numeric(i) & !is.integer(i)))
  n.continuous <- length(index.continuous)

  # [ Factors ] ====
  index.factor <- which(sapply(x, is.factor))
  n.factor <- length(index.factor)
  index.ordered <- which(sapply(x, is.ordered))
  n.ordered <- length(index.ordered)
  cat("  *", bold(n.continuous), "continuous", ifelse(n.continuous == 1, "feature", "features"), "\n")
  cat("  *", bold(n.integer), "integer", ifelse(n.integer == 1, "feature", "features"), "\n")
  isOrdered <- if (n.factor == 1) {
    paste(", which", ifelse(n.ordered == 1, "is", "is not"), "ordered")
  } else if (n.factor > 1) {
    paste(", of which", bold(n.ordered), ifelse(n.ordered == 1, "is", "are"), "ordered")
  } else {
    ""
  }
  cat("  * ", bold(n.factor), " categorical", ifelse(n.factor == 1, " feature", " features"),
      isOrdered, "\n", sep = "")
  # n.levels.nonordered <- sapply(x[, setdiff(index.factor, index.ordered)], function(x) length(levels(x)))
  index.gt2levels.nonordered <- which(sapply(x[, setdiff(index.factor, index.ordered), drop = FALSE], function(x) length(levels(x))) > 2)
  n.gt2levels.nonordered <- length(index.gt2levels.nonordered)
  if (n.gt2levels.nonordered > 0) {
    cat("    **", bold(n.gt2levels.nonordered), "unordered categorical",
        ifelse(n.gt2levels.nonordered == 1, "feature has", "features have"),
        "more than 2 levels\n")
  }

  # [ Constants ] ====
  index.constant <- which(sapply(x, is.constant))
  n.constant <- length(index.constant)
  cat("  *", bold(n.constant), "constant", ifelse(n.constant == 1, "feature", "features"), "\n")

  # [ NAs ] ====
  cols.anyna <- which(sapply(x, anyNA))
  n.cols.anyna <- length(cols.anyna)
  index.na <- which(is.na(x))
  n.na <- length(index.na)

  # Get percent of NA values per feature and per case
  if (n.cols.anyna > 0) {
    # na.feature.pct <- sapply(1:n.cols.anyna, function(i)
    #   sum(is.na(x[, cols.anyna[i]])) / length(x[, cols.anyna[i]]))
    na.feature.pct <- data.frame(Feature = names(cols.anyna),
                                 Pct.NA = sapply(1:n.cols.anyna, function(i)
                                   sum(is.na(x[, cols.anyna[i]])) / length(x[, cols.anyna[i]])))

    index.incomplete <- which(!complete.cases(x))
    n.incomplete <- length(index.incomplete)
    # na.case.pct <- sapply(1:length(index.incomplete), function(i)
    #   sum(is.na(x[index.incomplete[i], ])) / length(x[index.incomplete[i], ]))
    na.case.pct <- data.frame(Case = index.incomplete,
                              Pct.NA = sapply(1:n.incomplete, function(i)
                                sum(is.na(x[index.incomplete[i], ])) / length(x[index.incomplete[i], ])))

  }
  cat("  * ", bold(n.cols.anyna), ifelse(n.cols.anyna == 1, " feature includes", " features include"), " 'NA' values",
      ifelse(n.cols.anyna > 0, paste(";", bold(n.na), "'NA'", ifelse(n.na == 1, "value", "values"), "total"), ""),
      "\n", sep = "")
  if (n.cols.anyna > 0) {
    if (!is.null(reportFeatures.thres)) {
      features.na.over.thres <- na.feature.pct[na.feature.pct$Pct.NA >= reportFeatures.thres, ]
      if (NROW(features.na.over.thres) > 0) {
        cat("    ** ", bold(NROW(features.na.over.thres)), " features missing in >= ",
            reportFeatures.thres*100, "% of cases:\n      *** ",
            paste0(features.na.over.thres$Feature, ": ", ddSci(features.na.over.thres$Pct.NA),
                   collapse = "\n      *** "), "\n",
            sep = "")
      }
    } else {
      max.na.feature.name <- names(cols.anyna)[which.max(na.feature.pct$Pct.NA)]
      cat("    ** Max percent missing in a feature is ", bold(ddSci(max(na.feature.pct$Pct.NA)*100), "%"), " (",
          bold(max.na.feature.name), ")\n", sep = "")
    }

    if (!is.null(reportCases.thres)) {
      cases.na.over.thres <- na.case.pct[na.case.pct$Pct.NA >= reportCases.thres, ]
      n.cases.na.over.thres <- NROW(cases.na.over.thres)
      if (n.cases.na.over.thres > 0 ) {
        cat("    ** ", bold(n.cases.na.over.thres), ifelse(n.cases.na.over.thres > 1, " cases", " case"),
            " missing >= ", reportCases.thres*100, "% of features:\n      *** ",
            paste0("#", cases.na.over.thres$Case, ": ", ddSci(cases.na.over.thres$Pct.NA),
                   collapse = "\n      *** "), "\n",
            sep = "")
      }
    } else {
      max.na.case.number <- index.incomplete[which.max(na.case.pct$Pct.NA)]
      cat("    ** Max percent missing in a case is ", bold(ddSci(max(na.case.pct$Pct.NA)*100), "%"), " (case #",
          bold(max.na.case.number), ")\n", sep = "")
    }

  }

  # [ str() ] ====
  if (str) {
    # cat("\n  Feature info\n  -------------------------------\n")
    boxcat("Feature structure", pad = 2)
    str(x)
  }

  # [ Recomend ] ====
  if (recommend) {
    # cat("\n  Recommendations\n  -------------------------------\n")
    boxcat("Recommendations", pad = 2)
    if (n.constant > 0 | n.cols.anyna > 0 | n.gt2levels.nonordered > 0) {
      if (n.constant > 0) {
        cat(bold("  * Remove the constant", ifelse(n.constant == 1, "feature", "features"), "\n"))
      }

      if (n.cols.anyna > 0) {
        cat(bold("  * Consider imputing missing values or use complete cases only\n"))
      }

      if (n.gt2levels.nonordered > 0) {
        cat(bold("  * Check the", ifelse(n.gt2levels.nonordered > 1, paste("", n.gt2levels.nonordered, ""), " "),
            "unordered categorical",
            ifelse(n.gt2levels.nonordered > 1, " features", " feature"),
            " with more than 2 levels and consider\n    if ordering would make sense\n", sep = ""))
      }
      if (n.integer > 0) {
        cat(bold("  * Check the", ifelse(n.integer > 1, paste("", n.integer, ""), " "),
            "integer", ifelse(n.integer > 1, " features", " feature"),
            " and consider if", ifelse(n.integer > 1, " they", " it"), " should be converted to ",
            ifelse(n.integer > 1, "factors\n", "factor\n"), sep = ""))
      }
    } else {
      cat(bold("  * Everything looks good\n"))
    }

  }

  invisible(list(n.rows = n.rows,
                 n.cols = n.cols,
                 n.continuous = n.continuous,
                 n.integer = n.integer,
                 n.factor = n.factor,
                 n.ordered = n.ordered,
                 n.constant = n.constant,
                 n.cols.anyna = n.cols.anyna,
                 n.na = n.na))

} # rtemis::checkData
