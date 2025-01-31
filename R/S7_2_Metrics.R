# S7_Metrics.R
# ::rtemis::
# 2025 EDG rtemis.org

# Metrics ----
#' @title Metrics
#'
#' @description
#' Superclass for Metrics metrics.
#'
#' @author EDG
Metrics <- new_class(
  name = "Metrics",
  properties = list(
    sample = class_character,
    metrics = class_list | class_data.frame
  )
) # /rtemis::Metrics

# Make Metrics@metrics `$`-accessible
method(`$`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}

# Make Metrics@metrics `[[`-accessible
method(`[[`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}

# `$`-autocomplete Metrics@metrics
method(`.DollarNames`, Metrics) <- function(x, pattern = "") {
  all_names <- names(x@metrics)
  grep(pattern, all_names, value = TRUE)
}

# RegressionMetrics ----
#' @title RegressionMetrics
#'
#' @description
#' Metrics subclass for regression models.
#'
#' @author EDG
#' @export
RegressionMetrics <- new_class(
  name = "RegressionMetrics",
  parent = Metrics,
  # properties = list(
  #   MAE = class_numeric,
  #   MSE = class_numeric,
  #   RMSE = class_numeric,
  #   Rsq = class_numeric
  # ),
  constructor = function(MAE, MSE, RMSE, Rsq, sample = character()) {
    new_object(
      Metrics(
        sample = sample,
        metrics = data.frame(
          MAE = MAE,
          MSE = MSE,
          RMSE = RMSE,
          Rsq = Rsq
        )
      )
    )
  }
) # /rtemis::RegressionMetrics

# Print RegressionMetrics ----
method(print, RegressionMetrics) <- function(x) {
  if (!is.null(x@sample)) {
    objcat(paste(" ", x@sample, "Regression Metrics\n"))
  } else {
    objcat("  Regression Metrics\n")
  }
  printls(x@metrics)
  invisible(x)
} # /print.RegressionMetrics

# ClassificationMetrics ----
#' @title ClassificationMetrics
#'
#' @description
#' Metrics subclass for classification models.
#'
#' @author EDG
#' @export
ClassificationMetrics <- new_class(
  name = "ClassificationMetrics",
  parent = Metrics,
  constructor = function(Confusion_Matrix, Overall, Class, Positive_Class, sample = character()) {
    new_object(
      Metrics(
        sample = sample,
        metrics = list(
          Confusion_Matrix = Confusion_Matrix,
          Overall = Overall,
          Class = Class,
          Positive_Class = Positive_Class
        )
      )
    )
  }
) # /ClassificationMetrics

# Print ClassificationMetrics ----
method(print, ClassificationMetrics) <- function(x, decimal_places = 3) {
  if (!is.null(x@sample)) {
    objcat(paste(" ", x@sample, "Classification Metrics\n"))
  } else {
    objcat("  Classification Metrics\n")
  }
  tblpad <- 17 - max(nchar(colnames(x@metrics$Confusion_Matrix)), 9)
  printtable(x$Confusion_Matrix, pad = tblpad)
  printdf(x@metrics$Overall,
    transpose = TRUE,
    ddSci_dp = decimal_places,
    justify = "left",
    newline_pre = TRUE,
    newline = TRUE,
    spacing = 2,
    row_col = reset
  )
  if (is.na(x@metrics$Positive_Class)) {
    printdf(x@metrics$Class,
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2,
      row_col = reset
    )
  } else {
    cat("   Positive Class ", hilite(x@metrics$Positive_Class), "\n")
  }
  invisible(x)
} # /print.ClassificationMetrics
