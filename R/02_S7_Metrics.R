# S7_Metrics.R
# ::rtemis::
# 2025 EDG rtemis.org

# Metrics ----
#' @title Metrics
#'
#' @description
#' Superclass for Metrics metrics.
#'
#' @field sample Character: Sample name.
#' @field metrics List or data.frame: Metrics.
#'
#' @author EDG
#' @noRd
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

# `$`-autocomplete Metrics@metrics
method(`.DollarNames`, Metrics) <- function(x, pattern = "") {
  all_names <- names(x@metrics)
  grep(pattern, all_names, value = TRUE)
}

# Make Metrics@metrics `[[`-accessible
method(`[[`, Metrics) <- function(x, name) {
  x@metrics[[name]]
}

# RegressionMetrics ----
#' @title RegressionMetrics
#'
#' @description
#' Metrics subclass for regression models.
#'
#' @author EDG
#' @noRd
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
method(print, RegressionMetrics) <- function(x, ...) {
  if (!is.null(x@sample)) {
    objcat(paste(" ", x@sample, "Regression Metrics\n"))
  } else {
    objcat("  Regression Metrics\n")
  }
  printls(x@metrics, print_class = FALSE, print_df = TRUE)
  invisible(x)
} # /rtemis::print.RegressionMetrics

# ClassificationMetrics ----
#' @title ClassificationMetrics
#'
#' @description
#' Metrics subclass for classification models.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ClassificationMetrics <- new_class(
  name = "ClassificationMetrics",
  parent = Metrics,
  constructor = function(
    Confusion_Matrix,
    Overall,
    Class,
    Positive_Class,
    sample = character()
  ) {
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
) # /rtemis::ClassificationMetrics

# Print ClassificationMetrics ----
method(print, ClassificationMetrics) <- function(x, decimal_places = 3, ...) {
  if (!is.null(x@sample)) {
    objcat(paste(" ", x@sample, "Classification Metrics\n"))
  } else {
    objcat("  Classification Metrics\n")
  }
  tblpad <- 17 - max(nchar(colnames(x@metrics[["Confusion_Matrix"]])), 9)
  printtable(x[["Confusion_Matrix"]], pad = tblpad)
  printdf(
    x@metrics[["Overall"]],
    transpose = TRUE,
    ddSci_dp = decimal_places,
    justify = "left",
    newline_pre = TRUE,
    newline = TRUE,
    spacing = 2,
    row_col = reset
  )
  if (is.na(x@metrics[["Positive_Class"]])) {
    printdf(
      x@metrics[["Class"]],
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2,
      row_col = reset
    )
  } else {
    cat("   Positive Class ", hilite(x@metrics[["Positive_Class"]]), "\n")
  }
  invisible(x)
} # /rtemis::print.ClassificationMetrics


# MetricsRes ----
#' @title MetricsRes
#'
#' @description
#' Superclass for MetricsRes metrics.
#'
#' @field sample Character: Sample name.
#'
#' @author EDG
#' @noRd
MetricsRes <- new_class(
  name = "MetricsRes",
  properties = list(
    sample = class_character | NULL,
    res_metrics = class_list,
    mean_metrics = class_data.frame,
    sd_metrics = class_data.frame
  )
) # /rtemis::MetricsRes

# Print MetricsRes ----
#' Print MetricsRes
#'
#' @param x MetricsRes object.
#' @param decimal_places Integer: Number of decimal places.
#'
#' @author EDG
#' @noRd
print.MetricsRes <- function(x, decimal_places = 3L, ...) {
  type <- if (S7_inherits(x, RegressionMetricsRes)) {
    "Regression"
  } else {
    "Classification"
  }
  objcat(paste("  Resampled", type, x@sample, "Metrics"))
  cat(italic("  Showing mean (sd) across resamples.\n\n"))
  # Create list with mean_metrics (sd_metrics)
  out <- lapply(seq_along(x@mean_metrics), function(i) {
    paste0(
      ddSci(x@mean_metrics[[i]], decimal_places),
      thin(paste0(" (", ddSci(x@sd_metrics[[i]], decimal_places), ")"))
    )
  })
  names(out) <- names(x@mean_metrics)
  printls(out, print_class = FALSE, print_df = TRUE)
  invisible(x)
}
method(print, MetricsRes) <- function(x, decimal_places = 3, ...) {
  print.MetricsRes(x, decimal_places)
} # /rtemis::print.MetricsRes

#' @author EDG
#' @noRd
RegressionMetricsRes <- new_class(
  name = "RegressionMetricsRes",
  parent = MetricsRes,
  constructor = function(sample, res_metrics) {
    new_object(
      MetricsRes(
        sample = sample,
        res_metrics = res_metrics,
        mean_metrics = vec2df(
          colMeans(do.call(rbind, lapply(res_metrics, function(x) x@metrics)))
        ),
        sd_metrics = vec2df(
          sapply(do.call(rbind, lapply(res_metrics, function(x) x@metrics)), sd)
        )
      )
    )
  }
) # /rtemis::RegressionMetricsRes

#' @author EDG
#' @noRd
ClassificationMetricsRes <- new_class(
  name = "ClassificationMetricsRes",
  parent = MetricsRes,
  constructor = function(sample, res_metrics) {
    new_object(
      MetricsRes(
        sample = sample,
        res_metrics = res_metrics,
        mean_metrics = vec2df(
          colMeans(do.call(
            rbind,
            lapply(res_metrics, function(x) x@metrics[["Overall"]])
          ))
        ),
        sd_metrics = vec2df(
          sapply(
            do.call(
              rbind,
              lapply(res_metrics, function(x) x@metrics[["Overall"]])
            ),
            sd
          )
        )
      )
    )
  }
) # /rtemis::ClassificationMetricsRes
