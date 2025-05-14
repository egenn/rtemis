# mplot3_roc.R
# ::rtemis::
# 2017 EDG rtemis.org

#' `mplot3` ROC curves
#'
#' Plot ROC curve for a binary classifier
#'
#' @inheritParams mplot3_x
#' @param prob Numeric vector or list of numeric vectors \[0, 1\]: Predicted
#' probabilities (e.g. c(.1, .8, .2, .9))
#' @param labels Integer vector or list of integer vectors {0, 1}: True labels
#' (e.g. c(0, 1, 0, 1))
#' @param method Character: "rt" or "pROC" will use [rtROC] and `pROC::roc`
#' respectively to get points of the ROC.
#' @param type Character: "TPR.FPR" or "Sens.Spec". Only changes the x and y labels.
#' True positive rate vs. False positive rate and Sensitivity vs. Specificity.
#' @param alpha Numeric: Alpha transparency for lines
#' @param balanced.accuracy Logical: If TRUE, annotate the point of maximal Balanced
#' Accuracy.
#' @param main Character: Plot title.
#' @param col Color, vector: Colors to use for ROC curve(s)
#' @param cex Float: Character expansion factor.
#' @param lwd Float: Line width.
#' @param diagonal Logical: If TRUE, draw diagonal.
#' @param diagonal.lwd Float: Line width for diagonal.
#' @param diagonal.lty Integer: Line type for diagonal.
#' @param diagonal.col Color: Color for diagonal.
#' @param annotation.col Color: Color for annotation.
#' @param annot.line Numeric: Line position for annotation.
#' @param annot.adj Numeric: Text adjustment for annotation.
#' @param annot.font Integer: Font for annotation.
#' @param group.legend Logical: If TRUE, print group legend
#' @param verbose Logical: If TRUE, print messages to console.
#'
#' @author E.D. Gennatas
#' @export

mplot3_roc <- function(
  prob,
  labels,
  method = c("pROC", "rt"),
  type = "TPR.FPR",
  balanced.accuracy = FALSE,
  main = "",
  col = NULL,
  alpha = 1,
  cex = 1.2,
  lwd = 2.5,
  diagonal = TRUE,
  diagonal.lwd = 1,
  diagonal.lty = 1,
  diagonal.col = "red",
  group.legend = FALSE,
  annotation = TRUE,
  annotation.col = col,
  annot.line = NULL,
  annot.adj = 1,
  annot.font = 1,
  pty = "s",
  mar = c(2.5, 3, 2, 1),
  theme = rtTheme,
  palette = rtPalette,
  verbose = TRUE,
  par.reset = TRUE,
  filename = NULL,
  pdf.width = 5,
  pdf.height = 5
) {
  # Arguments ----
  # Output directory
  if (!is.null(filename)) {
    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename), recursive = TRUE)
    }
  }
  method <- match.arg(method)
  # Compatibility with rtlayout()
  if (exists("rtpar")) par.reset <- FALSE

  # Theme ----
  theme <- do.call(paste0("theme_", theme), list())
  theme$zerolines <- FALSE

  # ROC ----
  probl <- if (!is.list(prob)) list(prob) else prob
  labelsl <- if (!is.list(labels)) list(labels) else labels
  # if (length(probl) != length(labels)) stop("Input prob and labels do not contain same number of sets")
  if (length(labelsl) < length(probl)) {
    if (verbose) msg2("Assuming same labels for each set of probabilities")
    labelsl <- rep(labelsl, length(probl) / length(labelsl))
  }

  if (method == "rt") {
    # '- method rt ----
    .roc <- lapply(
      seq(probl),
      \(l) rtROC(labelsl[[l]], probl[[l]], verbose = FALSE)
    )
    TPR <- Sensitivity <- lapply(seq(probl), \(l) .roc[[l]]$Sensitivity)
    Specificity <- lapply(seq(probl), \(l) .roc[[l]]$Specificity)
    FPR <- lapply(seq(probl), \(l) 1 - Specificity[[l]])
    AUC <- lapply(seq(probl), \(l) .roc[[l]]$AUC)
    names(Sensitivity) <- names(Specificity) <- names(TPR) <- names(
      FPR
    ) <- names(AUC) <- names(probl)
  } else if (method == "pROC") {
    # '- method pROC ----
    for (i in seq(labelsl)) {
      levels(labelsl[[i]]) <- c(1, 0)
    }
    dependency_check("pROC")
    .roc <- lapply(seq(probl), \(l) {
      pROC::roc(labelsl[[l]], probl[[l]], levels = c(0, 1), direction = "<")
    })
    TPR <- Sensitivity <- lapply(seq(probl), \(l) rev(.roc[[l]]$sensitivities))
    Specificity <- lapply(seq(probl), \(l) rev(.roc[[l]]$specificities))
    FPR <- lapply(seq(probl), \(l) 1 - Specificity[[l]])
    AUC <- lapply(seq(probl), \(l) .roc[[l]]$auc)
    names(Sensitivity) <- names(Specificity) <- names(TPR) <- names(
      FPR
    ) <- names(AUC) <- names(probl)
  }

  if (balanced.accuracy) {
    BA <- lapply(seq(probl), \(l) (Sensitivity[[l]] + Specificity[[l]]) / 2)
    BA.max.index <- lapply(seq(probl), \(l) which.max(BA[[l]]))
  }

  # Colors ----
  if (is.null(col)) col <- rtpalette(palette)

  # Plot ----
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  if (type == "Sens.Spec") {
    # '- type Sens.Spec ----
    mplot3_xy(
      Specificity,
      Sensitivity,
      main = main,
      xlab = "Specificity",
      ylab = "Sensitivity",
      line.col = col,
      line.alpha = alpha,
      group.legend = group.legend,
      diagonal.inv = diagonal,
      diagonal.lty = diagonal.lty,
      diagonal.lwd = diagonal.lwd,
      diagonal.col = diagonal.col,
      pty = pty,
      xlim = c(1, 0),
      xaxs = "i",
      yaxs = "i",
      cex = cex,
      type = "l",
      order.on.x = FALSE,
      lwd = lwd,
      theme = theme,
      zerolines = FALSE,
      mar = mar,
      xpd = TRUE,
      par.reset = FALSE
    )

    if (balanced.accuracy) {
      for (i in seq(probl)) {
        points(
          x = Specificity[[i]][BA.max.index[[i]]],
          y = Sensitivity[[i]][BA.max.index[[i]]],
          col = col[[i]]
        )
        text(
          x = Specificity[[i]][BA.max.index[[i]]] - .05,
          y = Sensitivity[[i]][BA.max.index[[i]]] - .05,
          labels = paste0(
            "max BA = ",
            ddSci(max(BA[[i]])),
            "\n(p = ",
            ddSci(.roc[[i]]$thresholds[BA.max.index[[i]]]),
            ")"
          ),
          col = col[[i]],
          pos = 4,
          family = theme$font.family
        )
      }
    }
  } else {
    # '- type TPR.FPR ----
    mplot3_xy(
      FPR,
      TPR,
      main = main,
      xlab = "False Positive Rate",
      ylab = "True Positive Rate",
      line.col = col,
      line.alpha = alpha,
      group.legend = group.legend,
      diagonal = diagonal,
      diagonal.lty = diagonal.lty,
      diagonal.lwd = diagonal.lwd,
      diagonal.col = diagonal.col,
      xlim = c(0, 1),
      xaxs = "i",
      yaxs = "i",
      cex = cex,
      type = "l",
      pty = pty,
      order.on.x = FALSE,
      lwd = lwd,
      theme = theme,
      zerolines = FALSE,
      mar = mar,
      xpd = TRUE,
      par.reset = FALSE
    )
    if (balanced.accuracy) {
      for (i in seq(probl)) {
        points(
          x = 1 - Specificity[[i]][BA.max.index[[i]]],
          y = Sensitivity[[i]][BA.max.index[[i]]],
          col = col[[i]]
        )
        text(
          x = 1 - Specificity[[i]][BA.max.index[[i]]] + .05,
          y = Sensitivity[[i]][BA.max.index[[i]]],
          labels = paste0(
            "max BA = ",
            ddSci(max(BA[[i]])),
            "\n(thresh = ",
            ddSci(.roc[[i]]$Thresholds[BA.max.index[[i]]]),
            ")"
          ),
          col = col[[i]],
          pos = 4,
          family = theme$font.family
        )
      }
    }
  } # /type TPR.FPR

  # AUC annotation ----
  if (annotation) {
    auc <- paste(names(probl), ddSci(unlist(AUC)), "  ")
    if (is.null(annot.line)) annot.line <- seq(-length(probl), 0) - 1.7
    mtext(
      c("AUROC   ", auc),
      font = annot.font,
      side = 1,
      line = annot.line,
      adj = annot.adj,
      cex = cex,
      col = c("gray50", unlist(col)[seq_along(probl)]),
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()

  # if (type == "Sens.Spec") {
  #     invisible(list(Sensitivity = Sensitivity, Specificity = Specificity))
  # } else {
  #     invisible(list(FPR = FPR, TPR = TPR))
  # }
  invisible(list(ROC = .roc, par.orig = par.orig))
} # rtemis::mplot3_roc
