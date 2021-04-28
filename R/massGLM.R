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
#' @param xnames Character vector: names of \code{x} feature(s)
#' @param ynames Character vector: names of \code{y} feature(s)
#' @param verbose Logical: If TRUE, print messages during run
#' @param n.cores Integer: Number of cores to use
#' @param ... Arguments to be passed to \code{mod}
#' @author E.D. Gennatas
#' @export

massGLM <- function(x, y,
                    xnames = NULL,
                    ynames = NULL,
                    save.mods = FALSE,
                    p.adjust.method = "holm",
                    print.plot = FALSE,
                    verbose = TRUE,
                    n.cores = rtCores) {

  # [ Intro ] ====
  start.time <- intro(verbose = verbose)

  # [ Arguments ] ====
  learner <- modSelect(mod)

  # [ Data ] ====
  # if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
  # ynames <- colnames(y)
  type <- if (NCOL(x) > NCOL(y)) "massx" else "massy"
  if (type == "massx") {
    if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq(NCOL(x)))
    nmods <- NCOL(x)
  } else {
    if (is.null(colnames(y))) colnames(y) <- paste0("Outcome_", seq(NCOL(y)))
    nmods <- NCOL(y)
  }

  if (is.null(xnames)) {
    xnames <- if (type == "massx") colnames(x) else "x"
  }
  if (is.null(ynames)) {
    ynames <- if (type == "massy") colnames(y) else "y"
  }

  dat <- data.frame(x, y)
  colnames(dat) <- c(xnames, ynames)

  # mod1: Loop function ====
  mod1 <- function(index, dat, type) {
    if (type == "massx") {
      .formula <- as.formula(paste(ynames, "~", xnames[index]))
      glm(.formula, data = dat)
    } else {
      .formula <- as.formula(paste(ynames[index], "~", xnames))
      glm(.formula, data = dat)
    }
  }

  # Models ====
  if (verbose) msg("Training mass-univariate models...")
  if (verbose) {
    pbapply::pboptions(type = "timer")
  } else {
    pbapply::pboptions(type = "none")
  }
  mods <- pbapply::pblapply(seq_len(nmods), mod1, dat = dat, type = type,
                            cl = n.cores)
  names(mods) <- if (type == "massx") xnames else ynames

  # Coefficients & p-values ====
  .coefs_pvals <- sapply(mods, function(i) {
    .coef <- coef(summary(i))
    c(.coef[2, 1], .coef[2, 4])
  })
  coefs_pvals <- data.frame(Variable = colnames(.coefs_pvals),
                            Coefficient = .coefs_pvals[1, ],
                            pvalue = .coefs_pvals[2, ])

  # p.val adjust ====
  coefs_pvals$Adjusted_pvalue <- p.adjust(coefs_pvals[["pvalue"]],
                                          method = p.adjust.method)

  # [ Outro ] ====
  out <- list(mods = if (save.mods) mods else NULL,
              coefs_pvals = coefs_pvals,
              xnames = xnames,
              ynames = ynames,
              type = type)
  class(out) <- c("massGLM", "list")
  if (print.plot) print(plot(out))
  outro(start.time, verbose = verbose)
  out

} # rtemis::massUni

print.massGLM <- function(x, ...) {
  cat("Mass-univariate GLM analysis with", length(x$mods))
  if (type == "massx") {
    cat(" predictors and a single outcome")
  } else {
    cat(" outcomes and a single predictor")
  }
}

plot.massGLM <- function(x,
                         hline = .05,
                         hline.col = "#FE4AA3",
                         hline.dash = "dash", ...) {
  dplot3.bar(1 - x$coefs_pvals$pvalue,
             group.names = x$coefs_pvals$Variable,
             legend = F,
             hline = 1 - hline,
             hline.col = hline.col,
             hline.dash = hline.dash, ...)
}
