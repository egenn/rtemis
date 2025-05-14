# mplot3_pr
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' `mplot3` Precision Recall curves
#'
#' Plot Precision Recall curve for a binary classifier
#'
#' @inheritParams mplot3_x
#' @param prob Vector, Float \[0, 1\]: Predicted probabilities (i.e. c(.1, .8, .2, .9))
#' @param labels Vector, Integer {0, 1}: True labels (i.e. c(0, 1, 0, 1))
#' @param f1 Logical: If TRUE, annotate the point of maximal F1 score.
#' @param main Character: Plot title.
#' @param col Color, vector: Colors to use for ROC curve(s)
#' @param cex Float: Character expansion factor.
#' @param lwd Float: Line width.
#' @param diagonal Logical: If TRUE, draw diagonal.
#' @param diagonal.lwd Float: Line width for diagonal.
#' @param diagonal.lty Integer: Line type for diagonal.
#' @param group.legend Logical
#' @param annotation.side Integer: Side of plot to place annotation.
#' @param annotation.col Color: Color of annotation.
#' @param annot.line Numeric: Line number for annotation.
#' @param annot.adj Numeric: Adjustment for annotation.
#' @param annot.font Integer: Font for annotation.
#' @param verbose Logical: If TRUE, print messages to console.
#'
#' @return List with Precision, Recall, and Threshold values, invisibly
#' @author E.D. Gennatas
#' @export

mplot3_pr <- function(
  prob,
  labels,
  f1 = FALSE,
  main = "",
  col = NULL,
  cex = 1.2,
  lwd = 2.5,
  diagonal = FALSE,
  hline.lty = 1,
  hline.lwd = 1,
  hline.col = "red",
  diagonal.lwd = 2.5,
  diagonal.lty = 3,
  group.legend = FALSE,
  annotation = TRUE,
  annotation.side = 3,
  annotation.col = col,
  annot.line = NULL,
  annot.adj = 1,
  annot.font = 1,
  mar = c(2.5, 3, 2.5, 1),
  theme = rtTheme,
  palette = rtPalette,
  par.reset = TRUE,
  verbose = TRUE,
  filename = NULL,
  pdf.width = 5,
  pdf.height = 5
) {
  # Dependencies ----
  dependency_check("PRROC")

  # Arguments ----
  # Output directory
  if (!is.null(filename)) {
    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename), recursive = TRUE)
    }
  }

  # Compatibility with rtlayout()
  if (exists("rtpar")) par.reset <- FALSE

  probl <- if (!is.list(prob)) list(prob) else prob
  labelsl <- if (!is.list(labels)) list(labels) else labels
  if (length(labelsl) < length(probl)) {
    if (verbose) msg2("Assuming same labels for each set of probabilities")
    labelsl <- rep(labelsl, length(probl) / length(labelsl))
  }

  # Theme ----
  # extraargs <- list(...)
  # if (is.character(theme)) {
  #     theme <- do.call(paste0("theme_", theme), extraargs)
  # } else {
  #     for (i in seq(extraargs)) {
  #         theme[[names(extraargs)[i]]] <- extraargs[[i]]
  #     }
  # }
  theme <- do.call(paste0("theme_", theme), list())
  theme$zerolines <- FALSE

  # PR ----
  pr <- lapply(
    seq_along(probl),
    \(i)
      PRROC::pr.curve(
        scores.class0 = probl[[i]],
        weights.class0 = 2 - as.numeric(labelsl[[i]]),
        curve = TRUE
      )
  )
  Recall <- lapply(pr, function(i) i$curve[, 1])
  Precision <- lapply(pr, function(i) i$curve[, 2])
  AUPRC <- lapply(pr, function(i) i$auc.integral)
  Threshold <- lapply(pr, function(i) i$curve[, 3])

  if (f1) {
    F1 <- lapply(seq_along(probl), function(i) f1(Recall[[i]], Precision[[i]]))
    F1.max.index <- lapply(seq_along(probl), function(i) which.max(F1[[i]]))
  }

  # Colors ----
  if (is.null(col)) col <- rtpalette(palette)

  # mplot3_xy ----
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) {
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  }
  # Get P / P + N
  freq <- table(labels)
  mplot3_xy(
    Recall,
    Precision,
    main = main,
    ylab = "Precision",
    xlab = "Recall",
    type = "l",
    line.alpha = 1,
    line.col = col,
    group.legend = group.legend,
    hline = freq[1] / sum(freq),
    hline.col = hline.col,
    hline.lty = hline.lty,
    hline.lwd = hline.lwd,
    xlim = c(0, 1),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i",
    cex = cex,
    order.on.x = FALSE,
    lwd = lwd,
    theme = theme,
    mar = mar,
    xpd = TRUE,
    par.reset = FALSE
  )
  if (f1) {
    for (i in seq_along(probl)) {
      points(
        x = Recall[[i]][F1.max.index[[i]]],
        y = Precision[[i]][F1.max.index[[i]]],
        col = col[[i]]
      )
      text(
        x = Recall[[i]][F1.max.index[[i]]] - .5,
        y = Precision[[i]][F1.max.index[[i]]] - .1,
        labels = paste0(
          "max F1 = ",
          ddSci(max(F1[[i]])),
          "\n(Thresh = ",
          ddSci(Threshold[[i]][F1.max.index[[i]]]),
          ")"
        ),
        col = col[[i]],
        pos = 4,
        xpd = TRUE,
        family = theme$font.family
      )
    }
  }

  # PR Annotation ----
  if (annotation) {
    auprc <- paste(names(probl), ddSci(unlist(AUPRC)), "  ")
    if (is.null(annot.line)) {
      annot.line <- seq(-length(probl), 0) - 1.7
      if (annotation.side == 3) annot.line <- rev(annot.line)
    }
    mtext(
      c("AUPRC   ", auprc),
      font = annot.font,
      side = annotation.side,
      line = annot.line,
      adj = annot.adj,
      cex = cex,
      col = c("gray50", unlist(col)[seq_along(probl)]),
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()

  invisible(list(Precision = Precision, Recall = Recall, Threshold = Threshold))
} # rtemis::mplot3_roc
