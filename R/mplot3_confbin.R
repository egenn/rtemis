# mplot3_confbin
# ::rtemis::
# 2017-8 E.D. Gennatas rtemis.org
# TODO: Fix grid col

#' Plot extended confusion matrix for binary classification
#'
#' Plots an extended confusion matrix using [mplot3_img]
#'
#' @param object Either 1. a classification `rtMod`, b. a `caret::confusionMatrix` object, or c. a matrix /
#' data.frame / table
#' @param main Character: Plot title
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param mod.name Character: Name of the algorithm used to make predictions. If NULL, will look for
#' `object$mod.name`. Default = NULL
#' @param mar Numeric, vector, length 4: Overall margins
#' @param dim.lab Float: Height for labels
#' @param dim.in Float: Width and height for confusion matrix cells
#' @param dim.out Float: Height for metrics cells
#' @param font.in Integer: The `font` parameter for confusion matrix cells
#' @param font.out Integer: The `font` parameter for metrics cells
#' @param cex.in Float: The `cex` parameter for confusion matrix cells
#' @param cex.lab Float: The `cex` parameter for first line of label cells
#' @param cex.lab2 Float: The `cex` parameter for second line of label cells
#' @param cex.out Float: The `cex` parameter for metrics cells
#' @param col.text.out Color for metrics cells' text
#' @param col.bg.out Color for metrics cells' background
#' @param theme Character: "light", or "dark"
#' @param mid.color Color: The mid color for the confusion matrix. Default = "white" for theme = "light",
#' "black" for "dark"
#' @param hi.color.pos Color: The hi color for correct classification.
#' @param hi.color.neg Color: The hi color for missclassification
#' @param par.reset Logical: If TRUE, reset par before exit. Default = TRUE
#' @param pdf.width Float: PDF width, if `filename` is set
#' @param pdf.height Float: PDF height, if `filename` is set
#' @param ... Not used
#'
#' @param filename Character: If specified, save plot to this path. Default = NULL
#' @return List of metrics, invisibly
#' @author E.D. Gennatas
#' @export

mplot3_confbin <- function(
  object,
  # type = c("full", "reduced"),
  main = NULL,
  xlab = "True",
  ylab = "Estimated",
  mod.name = NULL,
  mar = c(4, 5, 4, 3),
  dim.lab = 1,
  dim.in = 4,
  dim.out = 2,
  font.in = 2,
  font.out = 2,
  cex.in = 1.2,
  cex.lab = 1.2,
  cex.lab2 = 1,
  cex.out = 1,
  col.text.out = "white",
  col.bg.out = "gray50",
  theme = "light",
  mid.color = NULL,
  hi.color.pos = "#18A3AC",
  hi.color.neg = "#716FB2",
  par.reset = TRUE,
  pdf.width = 8.7,
  pdf.height = 8.7,
  filename = NULL,
  ...
) {
  # [ Data ] ----
  if (inherits(object, "rtMod")) {
    tbl <- if (length(object$error.test) > 0)
      object$error.test$ConfusionMatrix else object$error.train$ConfusionMatrix
    if (is.null(mod.name)) mod.name <- object$mod.name
  } else if (inherits(object, "confusionMatrix")) {
    tbl <- object$table
  } else {
    tbl <- object
  }

  # File out ----
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  # colors ----
  if (is.null(mid.color)) {
    mid.color <- if (theme == "light") "white" else "black"
  }

  color.pos <- colorGrad(199, mid = mid.color, hi = hi.color.pos)
  color.neg <- colorGrad(199, mid = mid.color, hi = hi.color.neg)

  # metrics ----
  df <- as.data.frame(matrix(as.vector(tbl), nrow(tbl)))
  rownames(df) <- rownames(tbl)
  colnames(df) <- colnames(tbl)
  condition.positive <- sum(df[, 1])
  condition.negative <- sum(df[, 2])
  predicted.condition.positive <- sum(df[1, ])
  predicted.condition.negative <- sum(df[2, ])
  total <- condition.positive + condition.negative
  true.positive <- df[1, 1]
  false.negative <- df[2, 1]
  sensitivity <- true.positive / condition.positive
  false.negative.rate <- false.negative / condition.positive
  false.positive <- df[1, 2]
  true.negative <- df[2, 2]
  false.positive.rate <- false.positive / condition.negative
  specificity <- true.negative / condition.negative
  prevalence <- condition.positive / total
  precision <- true.positive / predicted.condition.positive
  false.omission.rate <- false.negative / predicted.condition.negative
  positive.likelihood.ratio <- sensitivity / false.positive.rate
  negative.likelihood.ratio <- false.negative.rate / specificity
  accuracy <- (true.positive + true.negative) / total
  false.discovery.rate <- false.positive / predicted.condition.positive
  negative.predictive.value <- true.negative / predicted.condition.negative
  f1 <- 1 / (.5 * (1 / sensitivity + 1 / precision))
  balanced.accuracy <- .5 * (sensitivity + specificity)

  lmat <- matrix(
    c(
      0,
      0,
      1,
      1,
      0,
      0,
      0,
      2:4,
      0,
      0,
      5:10,
      5,
      11:15,
      0,
      16:20,
      0,
      21:25
    ),
    nrow = 6
  )
  # 1: "predicted condition"
  # 2: "total =" total
  # 3: "predicted cond pos =" predicted.condition.positive
  # 4: "predicted cond neg = " predicted.condition.negative
  # 5: "true condition"
  # 6: condition.positive
  # 7: true.positive
  # 8: false.negative
  # 9: sensitivity
  # 10: false.negative.rate
  # 11: condition.negative
  # 12: false.positive
  # 13: true.negative
  # 14: false.positive.rate
  # 15: specificity
  # 16: prevalence
  # 17: pos pred value, aka precision
  # 18: false.omission.rate
  # 19: positive.likelihood.ratio
  # 20: negative.likelihood.ratio
  # 21: accuracy
  # 22: false.discovery.rate
  # 23: negative.predictive.value
  # 24: F1 score
  # 25: Balanced Accuracy

  # Par ----
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) {
    grDevices::pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  }

  # Plot ----
  par(mar = c(0, 0, 0, 0))
  layout(
    lmat,
    widths = c(dim.lab, dim.lab, dim.in, dim.in, dim.out, dim.out),
    heights = c(dim.lab, dim.lab, dim.in, dim.in, dim.out, dim.out),
    respect = TRUE
  )
  # 1
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, "Predicted condition", srt = 90, font = 2, cex = cex.lab)
  # 2
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("Total\n", total), srt = 45, font = 1, cex = cex.lab2)
  # 3
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(
    0,
    0,
    paste(
      "Predicted condition\npositive",
      predicted.condition.positive
    ),
    srt = 90,
    cex = cex.lab2
  )
  # 4
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(
    0,
    0,
    paste(
      "Predicted condition\nnegative",
      predicted.condition.negative
    ),
    srt = 90,
    cex = cex.lab2
  )
  # 5
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("True condition"), font = 2, cex = cex.lab)
  # 6
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("Condition positive\n", condition.positive), cex = cex.lab2)
  # 7 True positive
  col <- color.pos[100:199][round(sensitivity * 100)]
  # par(bg = col)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("True\npositive\n", true.positive),
    cex = cex.in,
    col = ifelse(sensitivity >= .5, "white", "black"),
    font = font.in
  )
  # 8 False negative
  col <- color.neg[100:199][round(false.negative.rate * 100)]
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False\nnegative\n", false.negative),
    cex = cex.in,
    col = ifelse(false.negative.rate >= .5, "white", "black"),
    font = font.in
  )
  # 9 Sensitivity
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Sensitivity\n", ddSci(sensitivity)),
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 10 false.negative.rate
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False negative\nrate\n", ddSci(false.negative.rate)),
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 11 condition.negative
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("Condition negative\n", condition.negative), cex = cex.lab2)
  # 12 false.positive
  col <- color.neg[100:199][round(false.positive.rate * 100)]
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False\npositive\n", false.positive),
    cex = cex.in,
    col = ifelse(false.positive.rate >= .5, "white", "black"),
    font = font.in
  )
  # 13 true.negative
  col <- color.pos[100:199][round(specificity * 100)]
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("True\nnegative\n", true.negative),
    cex = cex.in,
    col = ifelse(specificity >= .5, "white", "black"),
    font = font.in
  )
  # 14 false.positive.rate
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False pos rate\n", ddSci(false.positive.rate)),
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 15 Specificity
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Specificity\n", ddSci(specificity)),
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 16 Prevalence
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("Prevalence\n", ddSci(prevalence)), cex = cex.lab2)
  # 17 Precision
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Pos Pred Value\naka Precision\n", ddSci(precision)),
    srt = 90,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 18 False omission rate
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False omission\nrate\n", ddSci(false.omission.rate)),
    srt = 90,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 19 Positive likelihood ratio
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Positive\nlikelihood ratio\n", ddSci(positive.likelihood.ratio)),
    srt = 45,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 20 Negative likelihood ratio
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Negative\nlikelihood ratio\n", ddSci(negative.likelihood.ratio)),
    srt = 45,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 21 Accuracy
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, paste("Accuracy\n", ddSci(accuracy)), cex = cex.lab2)
  # 22 False discovery rate
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("False Discov Rate\n", ddSci(false.discovery.rate)),
    srt = 90,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 23 Negative predictive value
  # plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Neg pred value\n", ddSci(negative.predictive.value)),
    srt = 90,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 24 F1 Score
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("F1\n", ddSci(f1)),
    srt = 45,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )
  # 25 Balanced Accuracy
  plot(
    0,
    0,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    axes = FALSE,
    col = col.bg.out,
    cex = 50,
    pch = 15
  )
  text(
    0,
    0,
    paste("Balanced\nAccuracy\n", ddSci(balanced.accuracy)),
    srt = 45,
    col = col.text.out,
    cex = cex.out,
    font = font.out
  )

  if (!is.null(filename)) grDevices::dev.off()

  invisible(list(
    condition.positive = condition.positive,
    condition.negative = condition.negative,
    predicted.condition.positive = predicted.condition.positive,
    predicted.condition.negative = predicted.condition.negative,
    total = total,
    true.positive = true.positive,
    false.negative = false.negative,
    sensitivity = sensitivity,
    false.negative.rate = false.negative.rate,
    false.positive = false.positive,
    true.negative = true.negative,
    false.positive.rate = false.positive.rate,
    specificity = specificity,
    prevalence = prevalence,
    precision = precision,
    false.omission.rate = false.omission.rate,
    positive.likelihood.ratio = positive.likelihood.ratio,
    negative.likelihood.ratio = negative.likelihood.ratio,
    accuracy = accuracy,
    false.discovery.rate = false.discovery.rate,
    negative.predictive.value = negative.predictive.value,
    f1 = f1,
    balanced.accuracy = balanced.accuracy
  ))
} # rtemis::mplot3_conf
