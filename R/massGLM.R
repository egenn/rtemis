# massGLM.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Mass-univariate GLM Analysis
#'
#' Run a mass-univariate analysis with either:
#' a) single outome (y) and multiple predictors (x), one at a time, with optional a common set of
#' covariates in each model - "massx"
#' b) multiple different outcomes (y) with a fixed set of predictors (x) - "massy"
#' Therefore the term mass-univariate refers to looking at one variable of interest (with
#' potential covariates of no interest) at a time
#'
#' @param x Matrix / data frame of features
#' @param y Matrix / data frame of outcomes
#' @param type Character: "massx" or "massy". Default = NULL,
#' where if (NCOL(x) > NCOL(y)) "massx" else "massy"
#' @param xnames Character vector: names of \code{x} feature(s)
#' @param ynames Character vector: names of \code{y} feature(s)
#' @param save.mods Logical: If TRUE, save models. Default = TRUE
# @param p.adjust.method Character: p-value adjustment method. See \code{p.adjust}.
# Default = "holm"
#' @param print.plot Logical: If TRUE, print plot. Default = FALSE (best to choose which p-values
#' you want to plot directly)
#' @param trace Integer: If > 0, print more verbose output to console. Default = 0
#' @param verbose Logical: If TRUE, print messages during run
#' @param n.cores Integer: Number of cores to use
#' @author E.D. Gennatas
#' @export

massGLM <- function(x, y,
                    type = NULL,
                    xnames = NULL,
                    ynames = NULL,
                    save.mods = TRUE,
                    # p.adjust.method = "holm",
                    print.plot = FALSE,
                    verbose = TRUE,
                    trace = 0,
                    n.cores = rtCores) {

  # Intro ====
  start.time <- intro(verbose = verbose)

  # Data ====
  if (is.null(type)) type <- if (NCOL(x) > NCOL(y)) "massx" else "massy"
  if (trace > 0) msg0('massGLM type is "', type, '"')
  if (type == "massx") {
    if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
    nmods <- NCOL(x)
  } else {
    if (is.null(colnames(y))) colnames(y) <- paste0("Outcome_", seq(NCOL(y)))
    nmods <- NCOL(y)
  }
  if (trace > 0) msg('Will train', nmods, 'models')

  if (is.null(xnames)) {
    xnames <- if (is.null(colnames(x))) deparse(substitute(x)) else colnames(x)
  }
  if (trace > 0) msg("Feature names:", paste(xnames, collapse = ", "))
  if (is.null(ynames)) {
    ynames <- if (is.null(colnames(y))) deparse(substitute(y)) else colnames(y)
  }
  if (trace > 0) msg("Outcome names:", paste(ynames, collapse = ", "))

  dat <- data.frame(x, y)
  colnames(dat) <- c(xnames, ynames)

  # mod1: Loop function ====
  mod1 <- function(index, dat, type) {
    if (type == "massx") {
      .formula <- as.formula(paste(ynames, "~", xnames[index]))
      .family <- if (is.factor(dat[[ynames]])) "binomial" else "gaussian"
      glm(.formula, family = .family, data = dat)
    } else {
      .formula <- as.formula(paste(ynames[index], "~", paste(xnames, collapse = " + ")))
      .family <- if (is.factor(dat[[ynames[index]]])) "binomial" else "gaussian"
      glm(.formula, family = .family, data = dat)
    }
  }

  # Models ====
  if (verbose) msg("Training mass-univariate models...")
  if (verbose) {
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  mods <- pbapply::pblapply(seq_len(nmods), mod1,
                            dat = dat,
                            type = type,
                            cl = n.cores)
  names(mods) <- if (type == "massx") xnames else ynames

  # Outro ====
  out <- list(mods = if (save.mods) mods else NULL,
              summary = glm2table(mods),
              xnames = xnames,
              ynames = ynames,
              type = type)
  class(out) <- c("massGLM", "list")
  if (print.plot) print(plot(out))
  outro(start.time, verbose = verbose)
  out

} # rtemis::massUni


#' \code{print}\link{massGLM} object
#'
#' @method print massGLM
#' @param x \link{massGLM} object
#' @author E.D. Gennatas
#' @export

print.massGLM <- function(x, ...) {
  nx <- length(x$xnames)
  ny <- length(x$ynames)
  .text <- paste("Mass-univariate GLM analysis with", nx,
                 ngettext(nx, "predictor", "predictors"),
                 "and", ny, ngettext(ny, "outcome", "outcomes"))
  cat(.text)
  invisible(.text)
}

#' \code{massGLM} object summary
#'
#' @param object An object created by \link{massGLM}
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

summary.massGLM <- function(object, ...) {
  print(object$summary, row.names = FALSE, class = FALSE)
}

#' Plot \code{massGLM} object
#'
#' Plots a \code{massGLM} object using \link{dplot3.bar}
#'
#' @method plot massGLM
#' @param x \code{massGLM} object
#' @param what Character: "adjusted" or "raw" p-values to plot
#'
#' @author E.D. Gennatas
#' @export

plot.massGLM <- function(x,
                         predictor = NULL,
                         main = NULL,
                         what = c("adjusted", "raw", "coef"),
                         p.adjust.method = "holm",
                         p.transform = function(x) 1 - x,
                         pval.hline = c(.001, .05),
                         hline.col = "#FE4AA3",
                         hline.dash = "dash",
                         ylim = NULL,
                         ylab = NULL,
                         theme = getOption("rt.theme", "lightgrid"),
                         displayModeBar = FALSE, ...) {

  if (x$type == "massy") {
    if (is.null(predictor)) predictor <- x$xnames[1]
    what <- match.arg(what)

    if (what == "adjusted" & p.adjust.method != "none") {
      if (is.null(main)) main <- "p-values"
      pval_idi <- grep(paste("p_value", predictor), names(x$summary))[1]
      pval_name <- gsub("p_value", "", names(x$summary)[pval_idi])
      if (is.null(ylab)) ylab <- paste(print_transform(deparse(p.transform)[2]), "adjusted", pval_name, "p-value")
      dplot3.bar(p.transform(p.adjust(x$summary[[pval_idi]],
                                method = p.adjust.method)),
                 group.names = if (x$type == "massy") x$ynames else x$xnames,
                 main = main,
                 # ylim = c(0, 1),
                 ylim = ylim,
                 legend = FALSE,
                 ylab = ylab,
                 hline = p.transform(pval.hline),
                 hline.col = hline.col,
                 hline.dash = hline.dash,
                 theme = theme,
                 displayModeBar = displayModeBar, ...)
    } else if (what == "raw" | (what == "adjusted" & p.adjust.method == "none")) {
      if (is.null(main)) main <- "p-values"
      pval_idi <- grep(paste("p_value", predictor), names(x$summary))[1]
      pval_name <- gsub("p_value", "", names(x$summary)[pval_idi])
      if (is.null(ylab)) ylab <- paste(print_transform(deparse(p.transform)[2]), "raw", pval_name, "p-value")
      dplot3.bar(p.transform(x$summary[[pval_idi]]),
                 group.names = if (x$type == "massy") x$ynames else x$xnames,
                 main = main,
                 ylim = ylim,
                 legend = FALSE,
                 ylab = ylab,
                 hline = p.transform(pval.hline),
                 hline.col = hline.col,
                 hline.dash = hline.dash,
                 theme = theme,
                 displayModeBar = displayModeBar, ...)
    } else {
      if (is.null(main)) main <- "Coefficients"
      coef_idi <- grep(paste("Coefficient", predictor), names(x$summary))[1]
      coef_name <- gsub("Coefficient", "", names(x$summary)[coef_idi])
      dplot3.bar(x$summary[[coef_idi]],
                 group.names = if (x$type == "massy") x$ynames else x$xnames,
                 main = main,
                 legend = FALSE,
                 ylab = paste(coef_name, "Coefficients"),
                 theme = theme,
                 displayModeBar = displayModeBar, ...)
    }
  } else {
    cat('"massx" support not yet implemented')
  }

} # rtemis::plot.massGLM

print_transform <- function(x) gsub("[x()]", "", x)
