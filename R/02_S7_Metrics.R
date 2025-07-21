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
method(print, RegressionMetrics) <- function(x, pad = 0L, ...) {
  if (!is.null(x@sample)) {
    objcat(paste(x@sample, "Regression Metrics"), pad = pad)
  } else {
    objcat("Regression Metrics", pad = pad)
  }
  printls(x@metrics, print_class = FALSE, print_df = TRUE, pad = pad + 2L)
  invisible(x)
} # /rtemis::print.RegressionMetrics

# Show RegressionMatrics ----
method(show, RegressionMetrics) <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  out <- if (!is.null(x@sample)) {
    obj_str(
      paste(x@sample, "Regression Metrics"),
      pad = pad,
      output_type = output_type
    )
  } else {
    obj_str("Regression Metrics", pad = pad, output_type = output_type)
  }
  out <- paste0(
    out,
    show_ls(
      x@metrics,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad + 2L,
      output_type = output_type
    )
  )
  out
} # /rtemis::show.RegressionMetrics


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
method(print, ClassificationMetrics) <- function(
  x,
  decimal_places = 3,
  pad = 0L,
  ...
) {
  if (!is.null(x@sample)) {
    objcat(paste(x@sample, "Classification Metrics"), pad = pad)
  } else {
    objcat("Classification Metrics", pad = pad)
  }
  tblpad <- 17 - max(nchar(colnames(x@metrics[["Confusion_Matrix"]])), 9) # + pad
  printtable(x[["Confusion_Matrix"]], pad = tblpad)
  printdf(
    x@metrics[["Overall"]],
    pad = pad,
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
      pad = pad,
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

# Show ClassificationMetrics ----
method(show, ClassificationMetrics) <- function(
  x,
  decimal_places = 3,
  pad = 0L,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  output_type <- match.arg(output_type)

  if (!is.null(x@sample)) {
    out <- obj_str(
      paste(x@sample, "Classification Metrics"),
      pad = pad,
      output_type = output_type
    )
  } else {
    out <- obj_str(
      "Classification Metrics",
      pad = pad,
      output_type = output_type
    )
  }
  tblpad <- 17 - max(nchar(colnames(x@metrics[["Confusion_Matrix"]])), 9) # + pad
  out <- paste0(
    out,
    show_table(x[["Confusion_Matrix"]], pad = tblpad, output_type = output_type)
  )
  out <- paste0(
    out,
    "\n",
    show_df(
      x@metrics[["Overall"]],
      pad = pad,
      transpose = TRUE,
      ddSci_dp = decimal_places,
      justify = "left",
      spacing = 2,
      output_type = output_type
    )
  )

  if (is.na(x@metrics[["Positive_Class"]])) {
    out <- paste(
      out,
      show_df(
        x@metrics[["Class"]],
        pad = pad,
        transpose = TRUE,
        ddSci_dp = decimal_places,
        justify = "left",
        spacing = 2,
        output_type = output_type
      )
    )
  } else {
    out <- paste(
      out,
      "\n   Positive Class ",
      bold(
        col256(
          x@metrics[["Positive_Class"]],
          col = hilite_col,
          output_type = output_type
        ),
        output_type = output_type
      ),
      "\n"
    )
  }
  out
} # /rtemis::show.ClassificationMetrics


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
print.MetricsRes <- function(x, decimal_places = 3L, pad = 0L, ...) {
  type <- if (S7_inherits(x, RegressionMetricsRes)) {
    "Regression"
  } else {
    "Classification"
  }
  objcat(paste("Resampled", type, x@sample, "Metrics"), pad = pad)
  cat(rep(" ", pad), sep = "")
  cat(italic("  Showing mean (sd) across resamples.\n"))
  # Create list with mean_metrics (sd_metrics)
  out <- lapply(seq_along(x@mean_metrics), function(i) {
    paste0(
      ddSci(x@mean_metrics[[i]], decimal_places),
      thin(paste0(" (", ddSci(x@sd_metrics[[i]], decimal_places), ")"))
    )
  })
  names(out) <- names(x@mean_metrics)
  printls(out, print_class = FALSE, print_df = TRUE, pad = pad + 2L)
  invisible(x)
} # /rtemis::print.MetricsRes

method(print, MetricsRes) <- function(x, decimal_places = 3L, pad = 0L, ...) {
  print.MetricsRes(x, decimal_places, pad = pad)
} # /rtemis::print.MetricsRes

# Show MetricsRes ----
method(show, MetricsRes) <- function(
  x,
  decimal_places = 3L,
  pad = 0L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  type <- if (S7_inherits(x, RegressionMetricsRes)) {
    "Regression"
  } else {
    "Classification"
  }
  out <- obj_str(
    paste("Resampled", type, x@sample, "Metrics"),
    pad = pad,
    output_type = output_type
  )
  out <- paste0(out, paste0(rep(" ", pad), collapse = ""))
  out <- paste0(out, italic("  Showing mean (sd) across resamples.\n"))
  # Create list with mean_metrics (sd_metrics)
  metricsl <- lapply(seq_along(x@mean_metrics), function(i) {
    paste0(
      ddSci(x@mean_metrics[[i]], decimal_places),
      thin(paste0(" (", ddSci(x@sd_metrics[[i]], decimal_places), ")"))
    )
  })
  names(metricsl) <- names(x@mean_metrics)
  out <- paste0(
    out,
    show_ls(
      metricsl,
      print_class = FALSE,
      print_df = TRUE,
      pad = pad + 2L,
      output_type = output_type
    )
  )
  out
} # /rtemis::show.MetricsRes

# RegressionMetricsRes ----

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
