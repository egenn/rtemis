# mplot3.conf
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.lambdamd.org

#' Plot confusion matrix
#'
#' Plots confusion matrix and classification metrics
#'
#' @param object Either 1. a classification \code{rtMod}, b. a \code{caret::confusionMatrix} object, or c. a matrix /
#' data.frame / table
#' @param main Character: Plot title
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param plot.metrics Logical: If TRUE, draw classification metrics next to confusion matrix. Default = TRUE
#' @param mod.name Character: Name of the algorithm used to make predictions. If NULL, will look for
#' \code{object$mod.name}. Default = NULL
#' @param oma Float, vector, length 4: Outer margins. Default = c(0, 0, 0, 0)
#' @param dim.main Float: Height for title. Default = 1
#' @param dim.lab Float: Height for labels. Default = 1
#' @param dim.in Float: Height/Width for confusion matrix cells. Default = 4
#' @param dim.out Float: Height for metrics cells. Default = -1, which autoadjusts depending on number of output classes
#' @param font.in Integer: The \code{font} parameter for confusion matrix cells. Default = 2
#' @param font.out Integer: The \code{font} parameter for metrics cells. Default = 1
#' @param cex.in Float: The \code{cex} parameter for confusion matrix cells. Default = 1.8
#' @param cex.lab Float: The \code{cex} parameter for first line of label cells. Default = 1.8
#' @param cex.lab2 Float: The \code{cex} parameter for second line of label cells. Default = 1.8
#' @param cex.lab3 Float: The \code{cex} parameter for classification metrics. Default = 1.5
#' @param cex.out Float: The \code{cex} parameter for metrics cells. Default = 1.4
#' @param col.main Color for title. Default = "auto", determined by \code{theme}
#' @param col.lab Color for labels. Default = "auto", determined by \code{theme}
#' @param col.text.out Color for metrics cells' text. Default = "auto", determined by \code{theme}
#' @param col.bg Color for background. Default = "auto", determined by \code{theme}
#' @param col.bg.out1 Color for metrics cells' background (row1). Default = "auto", determined by \code{theme}
#' @param col.bg.out2 Color for metrics cells' background (row2). Default = "auto", determined by \code{theme}
#' @param col.text.hi Color for high confusion matrix values. Default = "auto", determined by \code{theme}
#' @param col.text.lo Color for low confusion matrix values. Default = "auto", determined by \code{theme}
#' @param theme Character: "light", or "dark". Set to \code{options("rt.theme")}, if set, otherwise "light"
#' @param mid.col Color: The mid color for the confusion matrix. Default = "auto", which results in "white" for
#' theme = "light", "black" for "dark"
#' @param hi.color.pos Color: The hi color for correct classification. Default = "#18A3AC" (teal)
#' @param hi.color.neg Color: The hi color for missclassification. Default = "#F48024" (orange)
#' @param par.reset Logical: If TRUE, reset par before exit. Default = TRUE
#' @param pdf.width Float: PDF width, if \code{filename} is set
#' @param pdf.height Float: PDF height, if \code{filename} is set
#' @param filename Character: If specified, save plot to this path. Default = NULL
#' @return List of metrics, invisibly
#' @author Efstathios D. Gennatas
#' @export

mplot3.conf <- function(object,
                        main = "auto",
                        xlab = "Reference",
                        ylab = "Estimated",
                        plot.metrics = TRUE,
                        mod.name = NULL,
                        oma = c(0, 0, 0, 0),
                        dim.main = 1,
                        dim.lab = 1,
                        dim.in = 4,
                        dim.out = -1,
                        font.in = 2,
                        font.out = 1,
                        cex.main = 1.8,
                        cex.in = 1.8,
                        cex.lab = 1.8,
                        cex.lab2 = 1.8,
                        cex.lab3 = 1.5,
                        cex.out = 1.4,
                        col.main = "auto",
                        col.lab = "auto",
                        col.text.out = "auto",
                        col.bg = "auto",
                        col.bg.out1 = "auto",
                        col.bg.out2 = "auto",
                        col.text.hi = "auto",
                        col.text.lo = "auto",
                        theme = getOption("rt.theme", "white"),
                        mid.col = "auto",
                        hi.color.pos = "#18A3AC",
                        hi.color.neg = "#F48024",
                        par.reset = TRUE,
                        pdf.width = 4.5,
                        pdf.height = 4.5,
                        filename = NULL, ...) {

  # [ DATA ] ====
  .test <- NULL
  if (inherits(object, "rtMod")) {
    .test <- length(object$error.test) > 0
    tbl <- if (.test) object$error.test$ConfusionMatrix else object$error.train$ConfusionMatrix
    if (is.null(mod.name)) mod.name <- object$mod.name
  } else if (class(object)[1]  == "classError") {
    tbl <- object$ConfusionMatrix
  } else if (class(object) == "confusionMatrix") {
    tbl <- object$table
  } else {
    tbl <- object
  }

  if (!is.null(mod.name) && !is.null(main) && main == "auto") {
    main <- paste(mod.name)
    if (!is.null(.test)) main <- if (.test) paste(main, "(Testing)") else paste(main, "(Training)")
  } else {
    if (!is.null(main) && main == "auto") main <- NULL
  }

  # [ THEME ] ====
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    # Override with extra arguments
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (col.bg == "auto") col.bg <- theme$bg
  mean.bg <- mean(col2rgb(col.bg))
  if (col.lab == "auto") {
    col.lab <- ifelse(mean.bg < 127, "gray75", "gray25")
  }
  if (mid.col == "auto") mid.col <- theme$bg
  if (col.text.hi == "auto") col.text.hi <- "white"
  if (col.text.lo == "auto") {
    col.text.lo <- ifelse(mean.bg < 127, "white", "gray50")
  }
  if (col.bg.out1 == "auto") {
    col.bg.out1 <- ifelse(mean.bg < 127, "gray15", "gray90")
  }
  if (col.bg.out2 == "auto") {
    col.bg.out2 <- ifelse(mean.bg < 127, "gray10", "gray95")
  }
  if (col.text.out == "auto") {
    col.text.out <- ifelse(mean.bg < 127, "gray70", "gray30")
  }

  # File out ====
  if (!is.null(filename)) if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)

  color.pos <- colorRampPalette(c(mid.col, hi.color.pos))(100)
  color.neg <- colorRampPalette(c(mid.col, hi.color.neg))(100)
  class.labels <- colnames(tbl)
  n.classes <-  length(class.labels)
  if (dim.out == -1) dim.out <- if (n.classes == 2) 1.2 else 1

  # metrics ====
  class.totals <- colSums(tbl)
  predicted.totals <- rowSums(tbl)
  total <- sum(tbl)
  hits <- diag(tbl)
  misses <- class.totals - hits
  class.sensitivity <- hits/class.totals
  condition.negative <- total - class.totals
  true.negative <- total - predicted.totals - (class.totals - hits)
  class.specificity <- true.negative / condition.negative
  class.balancedAccuracy <- .5*(class.sensitivity + class.specificity)
  # PPV = true positive / predicted condition positive
  class.ppv <- hits/predicted.totals
  # NPV  = true negative / predicted condition negative
  class.npv <- true.negative/(total - predicted.totals)

  # Lmat ====
  lmat <- matrix(0, 2 + n.classes, 2 + n.classes)
  # xlab
  lmat[, 1] <- c(0, 0, rep(1, n.classes))
  # ylab
  lmat[1, ] <- c(0, 0, rep(2, n.classes))
  # Predicted labs
  lmat[, 2] <- c(0, 0, 3:(3 + n.classes - 1))
  cid <- 3 + n.classes
  # True labs
  lmat[2, ] <- c(0, 0, cid:(cid + n.classes - 1)) # True labs
  cid <- cid + n.classes
  # Confusion matrix ====
  for (i in seq(n.classes)) {
    lmat[3:(2 + n.classes), 2 + i] <- cid:(cid + n.classes - 1)
    cid <- cid + n.classes
  }
  # '- Plot metrics ====
  if (plot.metrics) {
    lmat <- cbind(rbind(lmat, 0), 0)
    if (n.classes == 2) {
      # ''- 2 classes ====
      # Sens+Spec row
      lmat[5, 2 + seq(2)] <- cid:(cid + 1)
      cid <- cid + 2
      # PPV+NPV column
      lmat[2 + seq(2), 5] <- cid:(cid + 1)
      cid <- cid + 2
    } else {
      # ''- >2 classes ====
      lmat <- cbind(rbind(lmat, 0), 0)
      # Sens, Spec labels
      lmat[3 + n.classes, 1:2] <- cid
      lmat[4 + n.classes, 1:2] <- cid + 1
      cid <- cid + 2
      # metrics row
      lmat[3 + n.classes, 2 + seq(n.classes)] <- cid:(cid + n.classes - 1)
      cid <- cid + n.classes
      lmat[4 + n.classes, 2 + seq(n.classes)] <- cid:(cid + n.classes - 1)
      cid <- cid + n.classes
      # PPV, NPV labels
      lmat[1:2, 3 + n.classes] <- cid
      lmat[1:2, 4 + n.classes] <- cid + 1
      cid <- cid + 2
      # metrics col
      lmat[2 + seq(n.classes), 3 + n.classes] <- cid:(cid + n.classes - 1)
      cid <- cid + n.classes
      lmat[2 + seq(n.classes), 4 + n.classes] <- cid:(cid + n.classes - 1)
      cid <- cid + n.classes
      # # bottom right
      # lmat[(3 + n.classes):(4 + n.classes), (3 + n.classes):(4 + n.classes)] <- cid
    }
  }
  # ''- Main ====
  if (!is.null(main)) {
    lmat <- rbind(0, lmat)
    # lmat[1, 3:ncol(lmat)] <- cid
    lmat[1, 3:NCOL(lmat)] <- cid
  }

  # Par ====
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (!is.null(filename)) grDevices::pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")

  # Plot ====
  par(mar = c(0, 0, 0, 0), bg = col.bg, oma = oma)

  widths <- c(dim.lab, dim.lab, rep(dim.in, n.classes), dim.out, dim.out)
  heights <- c(dim.lab, dim.lab, rep(dim.in, n.classes), dim.out, dim.out)
  if (!is.null(main)) {
    heights <- c(dim.main, dim.lab, dim.lab, rep(dim.in, n.classes), dim.out, dim.out)
  }
  layout(lmat, widths = widths, heights = heights, respect = TRUE)

  # 1 True lab
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, ylab, srt = 90, font = 2, cex = cex.lab, col = col.lab)
  # 2 Estimated lab
  plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  text(0, 0, xlab, font = 2, cex = cex.lab, col = col.lab)
  # Predicted labels
  for (i in seq(n.classes)) {
    plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
    # text(0, 0, paste(class.labels[i], "=", predicted.totals[i]), srt = 90, cex = cex.lab2)
    text(0, 0, class.labels[i], srt = 90, cex = cex.lab2, col = col.lab)
  }
  # True labels
  for (i in seq(n.classes)) {
    plot(NULL, NULL, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
    # text(0, 0, paste(class.labels[i], "=", class.totals[i]), cex = cex.lab2)
    text(0, 0, class.labels[i], cex = cex.lab2, col = col.lab)
  }

  # '- Confusion matrix ====
  for (j in seq(n.classes)) {
    for (i in seq(n.classes)) {
      frac <- tbl[i, j]/class.totals[j]
      if (i == j) {
        col <- color.pos[round(frac*100)]
      } else {
        col <- color.neg[round(frac*100)]
      }
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col, cex = 50, pch = 15)
      text(0, 0, paste(tbl[i, j]), cex = cex.in,
           col = ifelse(frac >= .5, col.text.hi, col.text.lo), font = font.in)
    }
  }

  # '- Metrics ====
  if (plot.metrics) {
    if (n.classes == 2) {
      # Sensitivity
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
      text(0, 0, paste("Sensitivity\n", ddSci(class.sensitivity[1])),
           col = col.text.out, cex = cex.out, font = font.out)
      # Specificity
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
      text(0, 0, paste("Specificity\n", ddSci(class.specificity[1])),
           col = col.text.out, cex = cex.out, font = font.out)
      # PPV
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
      text(0, 0, paste("PPV\n", ddSci(class.ppv[1])), col = col.text.out, cex = cex.out,
           srt = 90, font = font.out)
      # NPV
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
      text(0, 0, paste("NPV\n", ddSci(class.npv[1])), col = col.text.out, cex = cex.out,
           srt = 90, font = font.out)
    } else {
      # Sens, Spec labels
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg, cex = 50, pch = 15)
      text(0, 0, "Sens.", cex = cex.lab3, col = col.text.out)
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg, cex = 50, pch = 15)
      text(0, 0, "Spec.", cex = cex.lab3, col = col.text.out)
      # Sensitivity values
      for (i in seq(n.classes)) {
        plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
        text(0, 0, ddSci(class.sensitivity[i]), col = col.text.out, cex = cex.out, font = font.out)
      }
      # Specificity values
      for (i in seq(n.classes)) {
        plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out2, cex = 50, pch = 15)
        text(0, 0, ddSci(class.specificity[i]), col = col.text.out, cex = cex.out, font = font.out)
      }
      # PPV, NPV labels
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg, cex = 50, pch = 15)
      text(0, 0, "PPV", cex = cex.lab3, srt = 90, col = col.text.out)
      plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg, cex = 50, pch = 15)
      text(0, 0, "NPV", cex = cex.lab3, srt = 90, col = col.text.out)
      # PPV
      for (i in seq(n.classes)) {
        plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out1, cex = 50, pch = 15)
        text(0, 0, ddSci(class.ppv[i]), col = col.text.out, srt = 90, cex = cex.out, font = font.out)
      }
      # NPV
      for (i in seq(n.classes)) {
        plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out2, cex = 50, pch = 15)
        text(0, 0, ddSci(class.npv[i]), col = col.text.out, srt = 90, cex = cex.out, font = font.out)
      }
    }
  }

  # Balanced Accuracy
  # plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out2, cex = 50, pch = 15)
  # text(0, 0, paste("Balanced\nAccuracy\n=", ddSci(balanced.accuracy)), srt = 45,
  #      col = col.text.out, cex = cex.out, font = font.out)

  # '- Main ====
  if (exists("autolabel", envir = rtenv)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg, cex = 50, pch = 15, xaxs = 'i')
    text(-1, 0, main, font = 2,
         cex = cex.main,
         col = theme$main.col, adj = 0, xpd = TRUE)
  }

  if (!is.null(filename)) grDevices::dev.off()

  # Return ====
  invisible(list(confusion.matrix = tbl,
                 n.classes = n.classes,
                 class.totals = class.totals,
                 predicted.totals = predicted.totals,
                 total = total,
                 hits = hits,
                 misses = misses,
                 class.sensitivity = class.sensitivity,
                 condition.negative = condition.negative,
                 true.negative = true.negative,
                 class.specificity = class.specificity,
                 class.balancedAccuracy = class.balancedAccuracy,
                 class.ppv = class.ppv,
                 class.npv = class.npv,
                 lmat = lmat,
                 main = main,
                 heights = heights,
                 widths = widths))
} # rtemis::mplot3.conf
