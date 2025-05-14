# htest.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Basic Bivariate Hypothesis Testing and Plotting
#'
#' @param y Float, vector: Outcome of interest
#' @param group Factor: Groups to compare
#' @param x Float, vector: Second outcome for correlation tests
#' @param yname Character: y variable name
#' @param groupname Character: group variable name
#' @param xname Character: x variable name
#' @param test Character: Test to use; one of:
#' \itemize{
#' \item Continuous outcome by group: "t.test", "wilcox.test", "aov", "kruskal.test"
#' \item Categorical outcome by group: "chisq.test", "fisher.test", "cor.test"
#' \item Two continuous variables: "pearson", "kendall", "spearman"
#' }
#' @param print.plot Logical: If TRUE, print plot. Default = TRUE
#' @param plot.args List of arguments to pass to plotting function
#' @param theme Character: Run `themes()` for available themes
# @param plot.engine Character: "mplot3" or "dplot3" for static and interactive plots, respectively
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @param ... Additional arguments to pass to test call
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # t.test, wilcoxon
#' y <- c(rnorm(200, 2, 1.2), rnorm(300, 2.5, 1.4))
#' group <- c(rep(1, 200), rep(2, 300))
#'
#' ht_ttest <- htest(y, group, test = "t.test")
#' ht_wilcoxon <- htest(y, group, test = "wilcox.test")
#'
#' # aov, kruskal
#' y <- c(rnorm(200, 2, 1.2), rnorm(300, 2.5, 1.4), rnorm(100, 2.3, 1.1))
#' group <- c(rep(1, 200), rep(2, 300), rep(3, 100))
#'
#' ht_aov <- htest(y, group, test = "aov")
#' ht_kruskal <- htest(y, group, test = "kruskal.test")
#'
#' # chisq, fisher
#' y <- c(sample(c(1, 2), 100, T, c(.7, .3)), sample(c(1, 2), 100, T, c(.35, .65)))
#' group <- c(rep(1, 100), rep(2, 100))
#' ht_chisq <- htest(y, group, test = "chisq")
#' ht_fisher <- htest(y, group, test = "fisher")
#'
#' # cor.test
#' x <- rnorm(300)
#' y <- x * .3 + rnorm(300)
#' ht_pearson <- htest(x = x, y = y, test = "pearson")
#' ht_kendall <- htest(x = x, y = y, test = "kendall")
#' ht_kendall <- htest(x = x, y = y, test = "spearman")
#' }
#'
htest <- function(
  y,
  group = NULL,
  x = NULL,
  yname = NULL,
  groupname = NULL,
  xname = NULL,
  test = c(
    "t.test",
    "wilcox.test",
    "aov",
    "kruskal.test", # continuous by group
    "chisq.test",
    "fisher.test",
    "cor.test", # categorical by group
    "pearson",
    "kendall",
    "spearman",
    "ks"
  ), # continuous vs. continuous
  print.plot = TRUE,
  plot.args = list(),
  theme = rtTheme,
  verbose = TRUE,
  ...
) {
  # Arguments ----
  .y <- deparse(substitute(y))
  if (is.null(yname)) yname <- .y
  if (is.null(x)) {
    .group <- deparse(substitute(group))
    if (is.null(groupname)) groupname <- .group
    if (!is.factor(group)) group <- factor(group)
    ngroups <- length(levels(group))
    if (ngroups == 1) stop("Need at least two groups")
    if (ngroups > 10)
      stop("Are you sure you want to compare ", ngroups, " groups? I'm not.")
  } else {
    .x <- deparse(substitute(x))
    if (is.null(xname)) xname <- .x
    ngroups <- 0
  }

  if (length(test) > 1) {
    if (!is.null(x)) {
      test <- "spearman"
    } else if (is.factor(y)) {
      test <- "chisq.test"
    } else {
      test <- if (ngroups == 2) "wilcox.test" else "kruskal.test"
    }
  } else {
    test <- match.arg(test)
  }

  # Test ----
  if (verbose) {
    testname <- switch(
      test,
      pearson = "Correlation test (Pearson)",
      kendall = "Correlation test (Kendall)",
      spearman = "Correlation test (Spearman)",
      ks = "Kolmogorov-Smirnoff test",
      t.test = "T-test",
      wilcox.test = "Wilcoxon Test",
      aov = "Analysis of variance",
      kruskal.test = "Kruskal-Wallis Test",
      chisq.test = "Chi-square Test",
      fisher.test = "Fisher's Exact Test"
    )
    cat(gray("   Test:"), cyan(testname, TRUE), "\n")
    cat(gray("Formula: "))
    if (test %in% c("pearson", "kendall", "spearman")) {
      cat(cyan(paste(xname, "~", yname), TRUE), "\n")
    } else if (test == "ks") {
      cat(cyan(paste(xname, "and", yname), TRUE), "\n")
    } else {
      cat(cyan(paste(yname, "~", groupname), TRUE), "\n")
    }
  }

  if (is.null(x)) {
    .formula <- as.formula(paste(.y, "~", .group))
    .formulatoo <- paste(yname, "~", groupname)
    if (test == "chisq.test") {
      .t <- chisq.test(x = y, y = group, ...)
    } else if (test == "fisher.test") {
      .t <- fisher.test(x = y, y = group, ...)
    } else {
      # t.test, wilcox.test, aov, kruskal.test
      dat <- data.frame(y, group)
      # Use .y and .group, because yname, groupname could include characters
      # incompatible with formula interface
      colnames(dat) <- c(.y, .group)
      .t <- do.call(
        test,
        list(formula = .formula, data = dat),
        ...
      )
    }
  } else if (test == "ks") {
    .t <- ks.test(x, y, ...)
    .formulatoo <- paste(xname, "and", yname)
  } else {
    .formula <- as.formula(paste(.x, "~", .y))
    .formulatoo <- paste(xname, "~", yname)
    .t <- cor.test(x, y, method = test, ...)
  }

  if (verbose) {
    if (test == "aov") {
      print(summary(.t))
    } else {
      print(.t)
    }
  }

  # Pval ----
  p.value <- if (test == "aov") {
    summary(.t)[[1]][1, 5]
  } else {
    .t$p.value
  }

  if (verbose) {
    cat(gray("p-value: "))
    cat(cyan(ddSci(p.value, 4), TRUE), "\n")
  }

  out <- list(
    x = x,
    y = y,
    group = group,
    formula = .formulatoo,
    testname = test,
    htest = .t,
    p.value = p.value,
    xname = xname,
    groupname = groupname,
    yname = yname
  )
  class(out) <- c("rtTest", "list")

  if (print.plot) {
    do.call(plot, args = c(list(x = out, theme = theme), plot.args))
  }

  invisible(out)
} # rtemis::h.test

#' Plot `rtTest` object
#'
#' @param x `rtTest` object
#' @param main Character: Main title
# @param plot.engine Character: "mplot3" or "dplot3" for static and interactive plots, respectively
#' @param theme Character: Run `themes()` for available themes
#' @author E.D. Gennatas
#' @export

plot.rtTest <- function(
  x,
  main = NULL,
  mar = NULL,
  # plot.engine = "mplot3",
  uni.type = c("density", "histogram", "hd"),
  boxplot.xlab = FALSE,
  theme = rtTheme,
  par.reset = TRUE,
  ...
) {
  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }

  # Main ----
  if (is.null(main)) {
    main <- if (x$testname %in% c("pearson", "kendall", "spearman")) {
      paste(x$testname, "cor.test p-value =", ddSci(x$p.value))
    } else {
      paste(x$testname, "p-value =", ddSci(x$p.value))
    }
  }

  # Plot ----
  if (exists("rtpar", envir = rtenv)) par.reset <- FALSE
  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))
  if (x$test %in% c("t.test", "wilcox.test", "aov", "kruskal.test")) {
    mplot3_box(
      split(x$y, x$group),
      oma = c(0, 0, 2, 0),
      theme = theme,
      xlab = if (boxplot.xlab) x$groupname else NULL,
      ylab = x$yname,
      mar = mar,
      par.reset = FALSE
    )
  } else if (x$test == "ks") {
    dat <- list(x = x$x, y = x$y)
    names(dat) <- c(x$xname, x$yname)
    mplot3_x(
      dat,
      type = match.arg(uni.type),
      density.avg.line = TRUE,
      oma = c(0, 0, 2, 0),
      mar = mar,
      par.reset = FALSE
    )
  } else if (x$test %in% c("chisq.test", "fisher.test")) {
    mplot3_mosaic(
      table(x$y, x$group),
      oma = c(0, 0, 2, 0),
      theme = theme,
      mar = mar,
      par.reset = FALSE
    )
  } else {
    mplot3_xy(
      x$x,
      x$y,
      fit = "lm",
      se.fit = TRUE,
      # main = main,
      oma = c(0, 0, 2, 0),
      fit.legend = FALSE,
      theme = theme,
      xlab = x$xname,
      ylab = x$yname,
      mar = mar,
      par.reset = FALSE
    )
  }

  # Title and sub ----
  mtext(
    x$formula,
    side = 3,
    line = 1,
    adj = 0,
    font = 2,
    col = theme$fg,
    xpd = TRUE
  )
  mtext(main, side = 3, line = 0, adj = 0, font = 1, col = theme$fg, xpd = TRUE)
} # rtemis::plot.rtTest
