# massGLM.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Mass-univariate GLM Analysis
#'
#' Run a mass-univariate analysis with either:
#' a) single outome (y) and multiple predictors (x), one at a time, with optional a common set of
#' covariates in each model - "massx"
#' b) multiple different outcomes (y) with a fixed set of predictors (x) - "massy"
#' Therefore, the term mass-univariate refers to looking at one variable of interest (with
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
#' @param n.cores Integer: Number of cores to use. Default = 1 (Do not change)
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # Common usage is "reversed":
#' # x: outcome of interest as first column, optional covariates of no interest
#' # in the other columns
#' # y: features whose influence on x you want to study
#' set.seed(2022)
#' features <- rnormmat(500, 40)
#' outcome <- features[, 3] - features[, 5] + features[, 14] + rnorm(500)
#' massmod <- massGLM(outcome, features)
#' plot(massmod)
#' plot(massmod, what = "coef")
#' plot(massmod, what = "volcano")
#' }
#'
massGLM <- function(x, y,
            type = NULL,
            xnames = NULL,
            ynames = NULL,
            save.mods = TRUE,
            print.plot = FALSE,
            verbose = TRUE,
            trace = 0,
            n.cores = 1) {

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
    if (verbose) msg("Will train", nmods, "models")

    if (is.null(xnames)) {
        xnames <- if (is.null(colnames(x))) paste(deparse(substitute(x)), seq_len(NCOL(x)), sep = "_") else colnames(x)
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
    if (verbose) msg("Training mass-GLM models...")
    if (verbose) {
        pbapply::pboptions(type = "timer")
    } else {
        pbapply::pboptions(type = "none")
    }
    mods <- pbapply::pblapply(seq_len(nmods), mod1,
        dat = dat,
        type = type,
        cl = n.cores
    )
    names(mods) <- if (type == "massx") xnames else ynames

    # Outro ====
    out <- list(
        mods = if (save.mods) mods else NULL,
        summary = glm2table(mods),
        xnames = xnames,
        coefnames = names(coef(mods[[1]])),
        ynames = ynames,
        type = type
    )
    class(out) <- c("massGLM", "list")
    if (print.plot) print(plot(out))
    outro(start.time, verbose = verbose)
    out
} # rtemis::massGLM


#' \code{print}\link{massGLM} object
#'
#' @method print massGLM
#' @param x \link{massGLM} object
#' @author E.D. Gennatas
#' @export

print.massGLM <- function(x, ...) {
    nx <- length(x$xnames)
    ny <- length(x$ynames)
    .text <- paste(
        "Mass-univariate GLM analysis with", nx,
        ngettext(nx, "predictor", "predictors"),
        "and", ny, ngettext(ny, "outcome", "outcomes")
    )
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
#' Plots a \code{massGLM} object using \link{dplot3_bar}
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
            what = c("coefs", "pvals", "volcano"),
            p.adjust.method = "none",
            p.transform = function(x) -log10(x),
            pval.hline = c(.05, .001),
            hline.col = "#ffffff",
            hline.dash = "dash",
            hline.annotate = as.character(pval.hline),
            ylim = NULL,
            ylab = NULL,
            col.neg = "#43A4AC",
            col.pos = "#FA9860",
            col.ns = "#7f7f7f",
            theme = getOption("rt.theme"),
            volcano.annotate = TRUE,
            volcano.annotate.n = 7,
            volcano.p.transform = "-log10",
            margin = NULL,
            displayModeBar = FALSE,
            trace = 0, ...) {
    what <- match.arg(what)
    if (x$type == "massy") {
        if (is.null(predictor)) predictor <- x$coefnames[2]
        what <- match.arg(what)
        if (what == "pvals") {
            # p-values ====
            if (is.null(main)) main <- "p-values"
            .idi <- grep(paste("p_value", predictor), names(x$summary))[1]
            .name <- gsub("p_value ", "", names(x$summary)[.idi])
            .pvals <- p.adjust(x$summary[[.idi]], method = p.adjust.method)
            .coefname <- getnames(x$summary, paste("Coefficient", .name))
            .cols <- rep(col.ns, length(x$summary[[.coefname]]))
            .cols[x$summary[[.coefname]] < 0 & .pvals < .05] <- col.neg
            .cols[x$summary[[.coefname]] > 0 & .pvals < .05] <- col.pos

            if (is.null(ylab)) {
                ylab <- paste(
                    print_transform(deparse(p.transform)[2]),
                    what, .name, "p-value"
                )
            }
            dplot3_bar(p.transform(.pvals),
                group.names = if (x$type == "massy") x$ynames else x$xnames,
                main = main,
                # ylim = c(0, 1),
                ylim = ylim,
                legend = FALSE,
                ylab = ylab,
                col = .cols,
                hline = p.transform(pval.hline),
                hline.col = hline.col,
                hline.dash = hline.dash,
                hline.annotate = hline.annotate,
                theme = theme,
                margin = margin,
                displayModeBar = displayModeBar, ...
            )
        } else if (what == "coefs") {
            # Coefficients ====
            if (is.null(main)) main <- "Coefficients"
            pvals.idi <- grep(paste("p_value", predictor), names(x$summary))[1]
            coef.idi <- grep(paste("Coefficient", predictor), names(x$summary))[1]
            .name <- gsub("Coefficient ", "", names(x$summary)[coef.idi])
            .pvals <- p.adjust(x$summary[[pvals.idi]], method = p.adjust.method)
            .coefname <- getnames(x$summary, paste("Coefficient", .name))
            .cols <- rep(col.ns, length(x$summary[[.coefname]]))
            .cols[x$summary[[.coefname]] < 0 & .pvals < .05] <- col.neg
            .cols[x$summary[[.coefname]] > 0 & .pvals < .05] <- col.pos

            dplot3_bar(x$summary[[coef.idi]],
                group.names = if (x$type == "massy") x$ynames else x$xnames,
                main = main,
                legend = FALSE,
                ylab = paste(.name, "Coefficients"),
                col = .cols,
                theme = theme,
                margin = margin,
                displayModeBar = displayModeBar, ...
            )
        } else {
            # Volcano ====
            # coef_idi <- grep(paste("Coefficient", predictor), names(x$summary))[1]
            coef_idi <- which(names(x$summary) == paste("Coefficient", predictor))
            # coef_name <- gsub("Coefficient ", "", names(x$summary)[coef_idi])
            # pval_idi <- grep(paste("p_value", predictor), names(x$summary))[1]
            pval_idi <- which(names(x$summary) == paste("p_value", predictor))

            dplot3_volcano(
                x = x$summary[[coef_idi]],
                pvals = x$summary[[pval_idi]],
                x.thresh = 0,
                label.lo = "Neg",
                label.hi = "Pos",
                xnames = x$ynames,
                xlab = paste(predictor, "Coefficient"),
                p.adjust.method = p.adjust.method,
                p.transform = volcano.p.transform,
                annotate = volcano.annotate,
                annotate.n = volcano.annotate.n,
                theme = theme,
                displayModeBar = displayModeBar,
                verbose = trace > 0
            )
        }
    } else {
        cat('"massx" support not yet implemented')
    }
} # rtemis::plot.massGLM

print_transform <- function(x) gsub("[x()]", "", x)
