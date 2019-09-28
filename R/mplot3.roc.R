# mplot3.roc
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' \code{mplot3} ROC curves
#'
#' Plot ROC curve for a binary classifier
#'
#' @param prob Vector, Float [0, 1]: Predicted probabilities (i.e. c(.1, .8, .2, .9))
#' @param labels Vector, Integer {0, 1}: True labels (i.e. c(0, 1, 0, 1))
#' @param method Character: "rt" or "pROC" will use \link{rtROC} and \code{pROC::roc} respectively
#' to get points of the ROC. Default = "rt"
#' @param type Character: "TPR.FPR" or "Sens.Spec". Only changes the x and y labels. True positive rate vs.
#' False positive rate and Sensitivity vs. Specificity. Default = "TPR.FPR"
#' @param balanced.accuracy Logical: If TRUE, annotate the point of maximal Balanced Accuracy. Default = FALSE
#' @param main Character: Plot title. Default = ""
#' @param col Color, vector: Colors to use for ROC curve(s)
#' @param cex Float: Character expansion factor. Default = 1.2
#' @param lwd Float: Line width. Default = 2.5
#' @param diagonal Logical: If TRUE, draw diagonal. Default = TRUE
#' @param diagonal.lwd Float: Line width for diagonal. Default = 2.5
#' @param diagonal.lty Integer: Line type for diagonal. Default = 1
#' @param group.legend Logical
#' @param ... Additional parameters to pass to \link{mplot3.xy}
#' @author Efstathios D. Gennatas
#' @export

mplot3.roc <- function(prob, labels,
                       method = c("rt", "pROC"),
                       type = "TPR.FPR",
                       balanced.accuracy = FALSE,
                       main = "",
                       col = ucsfPalette,
                       cex = 1.2,
                       lwd = 2.5,
                       diagonal = TRUE,
                       diagonal.lwd = 2.5,
                       diagonal.lty = 3,
                       group.legend = FALSE,
                       annotation = TRUE,
                       annotation.col = col,
                       annot.line = NULL,
                       annot.adj = 1,
                       annot.font = 1,
                       mar = c(2.5, 3, 2.5, 1),
                       theme = getOption("rt.theme", "lightgrid"),
                       par.reset = TRUE,
                       filename = NULL,
                       pdf.width = 5,
                       pdf.height = 5, ...) {

  # [ ARGUMENTS ] ====
  # Output directory
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)
  method <- match.arg(method)
  # Compatibility with rtlayout()
  if (exists("rtpar")) par.reset <- FALSE


  # [ ROC ] ====
  probl <- if (!is.list(prob)) list(prob) else prob
  labelsl <- if (!is.list(labels)) list(labels) else labels
  # if (length(probl) != length(labels)) stop("Input prob and labels do not contain same number of sets")
  if (length(labelsl) < length(probl)) {
    labelsl <- rep(labelsl, length(probl) / length(labelsl))
  }

  if (method == "rt") {
    # '- method rt ====
    .roc <- lapply(seq(probl), function(l) rtROC(labelsl[[l]], probl[[l]], verbose = FALSE))
    TPR <- Sensitivity <- lapply(seq(probl), function(l) .roc[[l]]$Sensitivity)
    Specificity <- lapply(seq(probl), function(l) .roc[[l]]$Specificity)
    FPR <- lapply(seq(probl), function(l) 1 - Specificity[[l]])
    AUC <- lapply(seq(probl), function(l) .roc[[l]]$AUC)
    names(Sensitivity) <- names(Specificity) <- names(TPR) <- names(FPR) <- names(AUC) <- names(probl)
  } else if (method == "pROC") {
    # '- method pROC ====
    for (i in seq(labelsl)) {
      levels(labelsl[[i]]) <- c(1, 0)
    }
    if (!depCheck("pROC", verbose = FALSE)) {
      cat("\n"); stop("Please install dependencies and try again")
    }
    .roc <- lapply(seq(probl), function(l) pROC::roc(labelsl[[l]], probl[[l]],
                                                     levels = c(0, 1), direction = "<"))
    TPR <- Sensitivity <- lapply(seq(probl), function(l) rev(.roc[[l]]$sensitivities))
    Specificity <- lapply(seq(probl), function(l) rev(.roc[[l]]$specificities))
    FPR <- lapply(seq(probl), function(l) 1 - Specificity[[l]])
    AUC <- lapply(seq(probl), function(l) .roc[[l]]$auc)
    names(Sensitivity) <- names(Specificity) <- names(TPR) <- names(FPR) <- names(AUC) <- names(probl)
  }

  if (balanced.accuracy) {
    BA <- lapply(seq(probl), function(l) (Sensitivity[[l]] + Specificity[[l]]) / 2)
    BA.max.index <- lapply(seq(probl), function(l) which.max(BA[[l]]))
  }

  # [ PLOT ] ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")
  if (type == "Sens.Spec") {
    mplot3.xy(Specificity, Sensitivity,
              main = main,
              xlab = "Specificity", ylab = "Sensitivity",
              line.alpha = 1, line.col = col, group.legend = group.legend,
              diagonal.inv = diagonal, diagonal.lty = diagonal.lty, diagonal.lwd = diagonal.lwd,
              xlim = c(1, 0), xaxs = "i", yaxs = "i", cex = cex,
              type = "l",
              order.on.x = FALSE,
              lwd = lwd, theme = theme, zero.lines = FALSE,
              mar = mar,
              xpd = TRUE, par.reset = FALSE, ...)
    # annotation = paste("AUC =", ddSci(AUC)))
    if (balanced.accuracy) {
      for (i in seq(probl)) {
        points(x = Specificity[[i]][BA.max.index[[i]]],
               y = Sensitivity[[i]][BA.max.index[[i]]],
               col = col[[i]])
        text(x = Specificity[[i]][BA.max.index[[i]]] - .05,
             y = Sensitivity[[i]][BA.max.index[[i]]],
             labels = paste0("max BA = ", ddSci(max(BA[[i]])), "\n(p = ",
                             ddSci(.roc[[i]]$thresholds[BA.max.index[[i]]]), ")"),
             col = col[[i]],
             pos = 4)
      }
    }
  } else {
    mplot3.xy(FPR, TPR,
              main = main,
              xlab = "False Positive Rate", ylab = "True Positive Rate",
              line.alpha = 1, line.col = col, group.legend = group.legend,
              diagonal = diagonal, diagonal.lty = diagonal.lty, diagonal.lwd = diagonal.lwd,
              xlim = c(0, 1), xaxs = "i", yaxs = "i", cex = cex,
              type = "l",
              order.on.x = FALSE,
              lwd = lwd, theme = theme, zero.lines = FALSE,
              mar = mar,
              xpd = TRUE, par.reset = FALSE, ...)
    # annotation = paste("AUC =", ddSci(AUC)), annotation.col = annotation.col)
    if (balanced.accuracy) {
      for (i in seq(probl)) {
        points(x = 1 - Specificity[[i]][BA.max.index[[i]]],
               y = Sensitivity[[i]][BA.max.index[[i]]],
               col = col[[i]])
        text(x = 1 - Specificity[[i]][BA.max.index[[i]]] + .05,
             y = Sensitivity[[i]][BA.max.index[[i]]],
             labels = paste0("max BA = ", ddSci(max(BA[[i]])), "\n(p = ",
                             ddSci(.roc[[i]]$thresholds[BA.max.index[[i]]]), ")"),
             col = col[[i]],
             pos = 4)
      }
    }
  }


  # [ AUC ANNOTATION ] ====
  if (annotation) {
    # auc <- ddSci(unlist(AUC))
    auc <- paste(names(probl), ddSci(unlist(AUC)), "  ")
    # mtext(text = auc, side = 1)
    # mtext(c("AUC", auc),
    #       side = 1,
    #       line = - 2 * length(probl),
    #       adj = .98,
    #       cex = cex,
    #       col = c("black", unlist(col)[1:length(probl)]),
    #       padj = seq(-1 * length(probl), -1 + 1.5 * length(probl), 1.5))
    if (is.null(annot.line)) annot.line <- seq(-length(probl), 0) - 1.7
    mtext(c("AUC   ", auc),
          font = annot.font,
          side = 1,
          line = annot.line,
          adj = annot.adj,
          cex = cex,
          col = c("gray50", unlist(col)[seq_along(probl)]))
  }

  # [ OUTRO ] ====
  if (!is.null(filename)) dev.off()

  if (type == "Sens.Spec") {
    invisible(list(Sensitivity = Sensitivity, Specificity = Specificity))
  } else {
    invisible(list(FPR = FPR, TPR = TPR))
  }

} # rtemis::mplot3.roc
