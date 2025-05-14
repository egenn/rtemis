# mplot3_conf
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Plot confusion matrix
#'
#' Plots confusion matrix and classification metrics
#'
#' This function uses its multiple cex args instead of the theme's `cex` parameter
#'
#' @param object Either a classification `rtMod`, or a table/matrix/data.frame confusion matrix
#' where rows are the reference classes and columns are the predicted classes.
#' @param main Character: Plot title.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param plot.metrics Logical: If TRUE, draw classification metrics next to confusion
#' matrix.
#' @param mod.name Character: Name of the algorithm used to make predictions. If NULL,
#'  will look for `object$mod.name`.
#' @param oma Numeric, vector, length 4: Outer margins.
#' @param dim.main Numeric: Height for title.
#' @param dim.lab Numeric: Height for labels.
#' @param dim.in Numeric: Height/Width for confusion matrix cells.
#' @param dim.out Numeric: Height for metrics cells. Default = -1, which autoadjusts
#' depending on number of output classes.
#' @param font.in Integer: The `font` parameter for confusion matrix cells.
#' @param font.out Integer: The `font` parameter for metrics cells.
#' @param cex.main Numeric: The `cex` parameter for the main title.
#' @param cex.in Numeric: The `cex` parameter for confusion matrix cells.
#' @param cex.lab Numeric: The `cex` parameter for first line of label cells.
#' @param cex.lab2 Numeric: The `cex` parameter for second line of label cells.
#' @param cex.lab3 Numeric: The `cex` parameter for classification metrics.
#' @param cex.out Numeric: The `cex` parameter for metrics cells.
#' @param col.main Color for title. Default = "auto", determined by `theme`.
#' @param col.lab Color for labels. Default = "auto", determined by `theme`.
#' @param col.text.out Color for metrics cells' text. Default = "auto",
#' determined by `theme`.
#' @param col.bg Color for background. Default = "auto", determined by
#' `theme`.
#' @param col.bg.out1 Color for metrics cells' background (row1).
#' Default = "auto", determined by `theme`.
#' @param col.bg.out2 Color for metrics cells' background (row2).
#' Default = "auto", determined by `theme`.
#' @param col.text.hi Color for high confusion matrix values. Default = "auto",
#' determined by `theme`.
#' @param col.text.lo Color for low confusion matrix values. Default = "auto",
#' determined by `theme`.
#' @param show.ba Logical: If TRUE, show Balanced Accuracy at bottom right corner.
#' @param theme Character: "light", or "dark". Set to
#' `options("rt.theme")`, if set, otherwise "light"
#' @param mid.col Color: The mid color for the confusion matrix.
#' Default = "auto", determined by `theme`.
#' @param hi.color.pos Color: The hi color for correct classification.
#' @param hi.color.neg Color: The hi color for missclassification.
#' @param autolabel Character vector to be used to generate autolabels when using
#' [rtlayout] with `autolabel = TRUE`.
#' @param par.reset Logical: If TRUE, reset par before exit.
#' @param pdf.width Numeric: PDF width, if `filename` is set.
#' @param pdf.height Numeric: PDF height, if `filename` is set.
#' @param filename Character: If specified, save plot to this path.
#' @param ... Additional arguments passed to `theme`.
#'
#' @return List of metrics, invisibly
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' true <- c("alpha", "alpha", "alpha", "alpha", "beta", "beta", "beta", "beta")
#' predicted <- c("alpha", "alpha", "alpha", "beta", "beta", "alpha", "alpha", "beta")
#' mplot3_conf(table(predicted, true))
#' }

mplot3_conf <- function(
  object,
  main = "auto",
  xlab = "Reference",
  ylab = "Predicted",
  plot.metrics = TRUE,
  mod.name = NULL,
  oma = c(0, 0, 0, 0),
  dim.main = NULL,
  dim.lab = 1,
  dim.in = 4,
  dim.out = -1,
  font.in = 2,
  font.out = 1,
  cex.main = 1.2,
  cex.in = 1.2,
  cex.lab = 1.2,
  cex.lab2 = 1.2,
  cex.lab3 = 1,
  cex.out = 1,
  col.main = "auto",
  col.lab = "auto",
  col.text.out = "auto",
  col.bg = "auto",
  col.bg.out1 = "auto",
  col.bg.out2 = "auto",
  col.text.hi = "auto",
  col.text.lo = "auto",
  show.ba = TRUE,
  theme = getOption("rt.theme", "white"),
  mid.col = "auto",
  hi.color.pos = "#18A3AC",
  hi.color.neg = "#C23A70", # "#F48024"
  autolabel = letters,
  par.reset = TRUE,
  pdf.width = 7,
  pdf.height = 7,
  filename = NULL,
  ...
) {
  # Data ----
  .test <- NULL
  if (inherits(object, "rtMod")) {
    .test <- length(object$error.test) > 0
    tbl <- if (.test) object$error.test$ConfusionMatrix else
      object$error.train$ConfusionMatrix
    if (is.null(mod.name)) mod.name <- object$mod.name
  } else if (inherits(object, "class_error")) {
    tbl <- object$ConfusionMatrix
  } else if (inherits(object, "confusionMatrix")) {
    tbl <- object$table
  } else {
    tbl <- object
  }
  n.classes <- ncol(tbl)

  if (!is.null(mod.name) && !is.null(main) && main == "auto") {
    main <- paste(mod.name)
    if (!is.null(.test))
      main <- if (.test) paste(main, "(Testing)") else paste(main, "(Training)")
  } else {
    if (!is.null(main) && main == "auto") main <- NULL
  }

  # Theme ----
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
  # consider multiplying all custom cex vals with theme$cex

  # File out ----
  if (!is.null(filename))
    if (!dir.exists(dirname(filename)))
      dir.create(dirname(filename), recursive = TRUE)

  color.pos <- colorRampPalette(c(mid.col, hi.color.pos))(100)
  color.neg <- colorRampPalette(c(mid.col, hi.color.neg))(100)
  class.labels <- colnames(tbl)
  # n.classes <-  length(class.labels)
  if (dim.out == -1) dim.out <- if (n.classes == 2) 1.2 else 1

  # metrics ----
  class.totals <- rowSums(tbl)
  predicted.totals <- colSums(tbl)
  total <- sum(tbl)
  hits <- diag(tbl)
  misses <- class.totals - hits
  class.sensitivity <- hits / class.totals
  condition.negative <- total - class.totals
  true.negative <- total - predicted.totals - (class.totals - hits)
  class.specificity <- true.negative / condition.negative
  class.balancedAccuracy <- .5 * (class.sensitivity + class.specificity)
  # PPV = true positive / predicted condition positive
  class.ppv <- hits / predicted.totals
  # NPV  = true negative / predicted condition negative
  class.npv <- true.negative / (total - predicted.totals)

  if (!is.null(filename))
    grDevices::pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )

  # Par ----
  if (!is.null(rtenv$rtpar)) {
    par(mar = c(0, 0, 0, 0), bg = col.bg, pty = "s")
  } else {
    par.orig <- par(no.readonly = TRUE)
    if (par.reset) on.exit(suppressWarnings(par(par.orig)))
    # par(mar = c(0, 0, 0, 0), bg = col.bg, oma = oma)
    par(mar = c(0, 0, 0, 0), bg = col.bg, pty = "s", oma = oma)
  }
  par(family = theme$font.family)

  # Plot ----
  if (!is.null(main)) {
    if (is.null(dim.main)) dim.main <- length(strsplit(main, "\\\n")[[1]])
  } else {
    dim.main <- 0
  }
  widths <- c(
    rep(dim.lab, 2),
    rep(dim.in, n.classes),
    rep(dim.out, ifelse(n.classes > 2, 2, 1))
  )
  heights <- rev(c(
    rep(dim.lab, ifelse(is.null(main), 2, 3)),
    rep(dim.in, n.classes),
    rep(dim.out, ifelse(n.classes > 2, 2, 1))
  ))

  leftpad <- 2 * dim.lab
  bottompad <- ifelse(n.classes > 2, 2, 1) * dim.out
  width <- leftpad + n.classes * dim.in + ifelse(n.classes == 2, 1, 2) * dim.out
  height <- bottompad + 2 * dim.lab + n.classes * dim.in + dim.main

  plot(
    NULL,
    NULL,
    xlim = c(0, width),
    ylim = c(0, height),
    axes = FALSE,
    ann = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # "Predicted"
  # y: middle of confusion matrix
  text(
    x = widths[1] / 2,
    y = bottompad + n.classes / 2 * dim.in,
    labels = ylab,
    col = col.lab,
    cex = cex.lab,
    srt = 90,
    adj = c(.5, .5),
    font = 2
  )

  # Predicted classes
  text(
    x = widths[1] + widths[2] / 2,
    y = bottompad + dim.in / 2 + c(seq(n.classes) - 1) * dim.in,
    labels = rev(class.labels),
    adj = c(.5, .5),
    srt = 90,
    cex = cex.lab2,
    col = col.lab
  )

  # "Reference"
  text(
    x = leftpad + n.classes / 2 * dim.in,
    y = bottompad + n.classes * dim.in + 1.5 * dim.lab,
    labels = xlab,
    col = col.lab,
    cex = cex.lab,
    adj = c(.5, .5),
    font = 2
  )
  # Reference classes
  text(
    x = sum(widths[1:2]) + dim.in / 2 + c(seq(n.classes) - 1) * dim.in,
    bottompad + n.classes * dim.in + .5 * dim.lab,
    labels = class.labels,
    adj = c(.5, .5),
    cex = cex.lab2,
    col = col.lab
  )

  # '- Confusion matrix ----
  for (i in seq_len(n.classes)) {
    for (j in seq_len(n.classes)) {
      frac <- tbl[j, i] / class.totals[i]
      if (i == j) {
        col <- color.pos[round(frac * 100)]
      } else {
        col <- color.neg[round(frac * 100)]
      }
      # {xi, yj}: top-left starting point
      xi <- leftpad + (i - 1) * dim.in
      yj <- bottompad + (n.classes + 1 - j) * dim.in
      polygon(
        x = c(xi, rep(xi + dim.in, 2), xi),
        y = c(rep(yj, 2), rep((yj - dim.in), 2)),
        col = col,
        border = NA
      )
      # N in each cell
      text(
        x = xi + .5 * dim.in,
        y = yj - .5 * dim.in,
        labels = tbl[j, i],
        cex = cex.in,
        font = font.in,
        col = ifelse(frac >= .5, col.text.hi, col.text.lo)
      )
    }
  }

  # '- Metrics ----
  if (plot.metrics) {
    if (n.classes == 2) {
      # 2 classes
      # PPV & NPV
      polygon(
        x = c(leftpad, rep(leftpad + dim.in, 2), leftpad),
        y = c(rep(dim.out, 2), rep(0, 2)),
        border = NA,
        col = col.bg.out1
      )
      polygon(
        x = c(leftpad + dim.in, rep(leftpad + 2 * dim.in, 2), leftpad + dim.in),
        y = c(rep(dim.out, 2), rep(0, 2)),
        border = NA,
        col = col.bg.out2
      )
      text(
        x = leftpad + c(.5, 1.5) * dim.in,
        y = .5 * dim.out,
        labels = c(
          paste("PPV\n", ddSci(class.ppv[1])),
          paste("NPV\n", ddSci(class.npv[1]))
        ),
        col = col.text.out,
        cex = cex.out,
        font = font.out
      )

      # Sensitivity & Specificity
      xright <- leftpad + 2 * dim.in
      polygon(
        x = c(xright, rep(xright + dim.out, 2), xright),
        y = c(rep(dim.out + 2 * dim.in, 2), rep(dim.out + dim.in, 2)),
        border = NA,
        col = col.bg.out1
      )
      polygon(
        x = c(xright, rep(xright + dim.out, 2), xright),
        y = c(rep(dim.out + dim.in, 2), rep(dim.out, 2)),
        border = NA,
        col = col.bg.out2
      )
      text(
        x = xright + .5 * dim.out,
        y = dim.out + c(.5, 1.5) * dim.in,
        labels = c(
          paste("Specificity\n", ddSci(class.specificity[1])),
          paste("Sensitivity\n", ddSci(class.sensitivity[1]))
        ),
        col = col.text.out,
        cex = cex.out,
        srt = 90,
        font = font.out
      )
    } else {
      # 3+ classes
      # "PPV", "NPV"
      text(
        x = dim.lab,
        y = c(1.5, .5) * dim.out,
        labels = c("PPV", "NPV"),
        adj = c(.5, .5),
        col = col.text.out,
        cex = cex.lab3
      )
      polygon(
        x = c(leftpad, rep(leftpad + n.classes * dim.in, 2), leftpad),
        y = c(rep(2 * dim.out, 2), rep(dim.out, 2)),
        col = col.bg.out1,
        border = NA
      )
      polygon(
        x = c(leftpad, rep(leftpad + n.classes * dim.in, 2), leftpad),
        y = c(rep(dim.out, 2), rep(0, 2)),
        col = col.bg.out2,
        border = NA
      )
      # Sensitivity values
      text(
        x = leftpad + c(seq(n.classes) - .5) * dim.in,
        y = 1.5 * dim.out,
        labels = ddSci(class.ppv),
        col = col.text.out,
        cex = cex.out,
        font = font.out
      )
      # Specificity values
      text(
        x = leftpad + c(seq(n.classes) - .5) * dim.in,
        y = .5 * dim.out,
        labels = ddSci(class.npv),
        col = col.text.out,
        cex = cex.out,
        font = font.out
      )

      # "Sens.", "Spec."
      text(
        x = leftpad + n.classes * dim.in + c(.5, 1.5) * dim.out,
        y = bottompad + n.classes * dim.in + dim.lab,
        labels = c("Sens.", "Spec."),
        adj = c(.5, .5),
        xpd = TRUE,
        col = col.text.out,
        cex = cex.lab3,
        srt = 90
      )
      xstart <- leftpad + n.classes * dim.in
      ystart <- bottompad + n.classes * dim.in
      polygon(
        x = c(xstart, rep(xstart + dim.out, 2), xstart),
        y = c(rep(ystart, 2), rep(ystart - n.classes * dim.in, 2)),
        col = col.bg.out1,
        border = NA
      )
      # PPV values
      text(
        x = xstart + .5 * dim.out,
        y = rev(bottompad + c(seq_len(n.classes) - .5) * dim.in),
        labels = ddSci(class.sensitivity),
        srt = 90,
        adj = c(.5, .5),
        col = col.text.out,
        cex = cex.out,
        font = font.out
      )
      # NPV values
      xstart <- leftpad + n.classes * dim.in + dim.out
      polygon(
        x = c(xstart, rep(xstart + dim.out, 2), xstart),
        y = c(rep(ystart, 2), rep(ystart - n.classes * dim.in, 2)),
        col = col.bg.out2,
        border = NA
      )
      text(
        x = xstart + .5 * dim.out,
        y = bottompad + c(seq_len(n.classes) - .5) * dim.in,
        labels = ddSci(rev(class.specificity)),
        srt = 90,
        adj = c(.5, .5),
        col = col.text.out,
        cex = cex.out,
        font = font.out
      )
    }
  }

  # Balanced Accuracy
  if (show.ba) {
    text(
      x = width - .5 * bottompad,
      y = .5 * bottompad,
      labels = paste0("BA\n", ddSci(mean(class.sensitivity))),
      # srt = 45,
      adj = c(.5, .5),
      col = col.text.out,
      cex = cex.out,
      font = font.out
    )
  }
  # plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, col = col.bg.out2, cex = 50, pch = 15)
  # text(0, 0, paste("Balanced\nAccuracy\n=", ddSci(balanced.accuracy)), srt = 45,
  #      col = col.text.out, cex = cex.out, font = font.out)

  # '- Main ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  if (length(main) > 0) {
    text(
      x = leftpad,
      y = height - .5 * dim.main,
      labels = main,
      font = theme$main.font,
      cex = cex.main,
      col = theme$main.col,
      adj = 0,
      xpd = TRUE
    )
  }

  if (!is.null(filename)) grDevices::dev.off()

  # Return ----
  invisible(list(
    confusion.matrix = tbl,
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
    main = main,
    heights = heights,
    widths = widths
  ))
} # rtemis::mplot3_conf
