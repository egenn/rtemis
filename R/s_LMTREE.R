# s_LMTREE.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Linear Tree [C, R]
#'
#' Train a LMTREE for regression or classification using \code{rpart}
#'
#' @inheritParams s_CART
#' @param offset Numeric vector of a priori known offsets
#'
#' @return Object of class \link{rtMod}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @export

s_LMTREE <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     x.name = NULL, y.name = NULL,
                     weights = NULL,
                     offset = NULL,
                     ipw = TRUE,
                     ipw.type = 2,
                     upsample = FALSE,
                     downsample = FALSE,
                     resample.seed = NULL,
                     na.action = na.exclude,
                     print.plot = TRUE,
                     plot.fitted = NULL,
                     plot.predicted = NULL,
                     plot.theme = rtTheme,
                     question = NULL,
                     verbose = TRUE,
                     outdir = NULL,
                     save.mod = ifelse(!is.null(outdir), TRUE, FALSE)) {
    # Intro ----
    if (missing(x)) {
        print(args(s_LMTREE))
        return(invisible(9))
    }
    if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
    logFile <- if (!is.null(outdir)) {
        paste0(outdir, sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
    } else {
        NULL
    }
    start.time <- intro(verbose = verbose, logFile = logFile)
    mod.name <- "LMTREE"

    # Dependencies ----
    dependency_check("partykit")

    # Arguments ----
    if (is.null(y) & NCOL(x) < 2) {
        print(args(s_LMTREE))
        stop("y is missing")
    }
    if (is.null(x.name)) x.name <- getName(x, "x")
    if (is.null(y.name)) y.name <- getName(y, "y")
    if (!verbose) print.plot <- FALSE
    verbose <- verbose | !is.null(logFile)
    if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name, "/")
    if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")


    # Data ----
    dt <- dataPrepare(x, y, x.test, y.test,
        ipw = ipw,
        ipw.type = ipw.type,
        upsample = upsample,
        downsample = downsample,
        resample.seed = resample.seed,
        verbose = verbose
    )
    x <- dt$x
    y <- dt$y
    x.test <- dt$x.test
    y.test <- dt$y.test
    xnames <- dt$xnames
    type <- dt$type
    .weights <- if (is.null(weights) & ipw) dt$weights else weights
    # x0 <- if (upsample | downsample) dt$x0 else x # x0, y0 are passed to gridSearchLearn
    # y0 <- if (upsample | downsample) dt$y0 else y
    if (verbose) dataSummary(x, y, x.test, y.test, type)
    if (type != "Survival") df.train <- data.frame(y = y, x)

    if (print.plot) {
        if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
        if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
    } else {
        plot.fitted <- plot.predicted <- FALSE
    }

    # Formula ----
    features <- paste(xnames, collapse = " + ")
    .formula <- as.formula(paste0(y.name, " ~ ", features))

    # lmtree ----
    if (verbose) msg("Training LMTREE...", newline.pre = TRUE)
    mod <- partykit::lmtree(
        formula = .formula,
        data = df.train,
        na.action = na.action,
        weights = .weights,
        offset = offset
    )

    # Fitted ----
    fitted.prob <- NULL
    fitted <- predict(mod, x, type = "response")

    # attr(fitted, "names") <- NULL
    error.train <- modError(y, fitted, fitted.prob)
    if (verbose) errorSummary(error.train, mod.name)

    # Predicted ----
    predicted.prob <- predicted <- error.test <- NULL
    if (!is.null(x.test)) {
        predicted <- predict(mod, x.test, type = "vector")
        predicted.prob <- NULL
        if (!is.null(y.test)) {
            error.test <- modError(y.test, predicted, predicted.prob)
            if (verbose) errorSummary(error.test, mod.name)
        } else {
            error.test <- NULL
        }
    }

    # Outro ----
    rt <- rtModSet(
        rtclass = "rtMod",
        mod = mod,
        mod.name = mod.name,
        type = type,
        # call = .call,
        y.train = y,
        y.test = y.test,
        x.name = x.name,
        y.name = y.name,
        xnames = xnames,
        fitted = fitted,
        fitted.prob = fitted.prob,
        se.fit = NULL,
        error.train = error.train,
        predicted = predicted,
        predicted.prob = predicted.prob,
        se.prediction = NULL,
        error.test = error.test,
        varimp = varimp,
        question = question,
        extra = NULL
    )

    rtMod.out(
        rt,
        print.plot,
        plot.fitted,
        plot.predicted,
        y.test,
        mod.name,
        outdir,
        save.mod,
        verbose,
        plot.theme
    )

    outro(start.time,
        verbose = verbose,
        sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
    )
    rt
} # rtemis::s_LMTREE
