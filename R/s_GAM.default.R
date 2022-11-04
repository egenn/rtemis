# s_GAM.default.R
# ::rtemis::
# 2015 E.D. Gennatas www.lambdamd.org

#' Generalized Additive Model (GAM) [C, R]
#'
#' Trains a GAM using \code{mgcv::gam} and validates it.
#' Input will be used to create a formula of the form:
#' \deqn{y = s(x_{1}, k) + s(x_{2}, k) + ... + s(x_{n}, k)}
#'
#' @inheritParams s_GLM
#' @param k Integer. Number of bases for smoothing spline
#' @param ... Additional arguments to be passed to \code{mgcv::gam}
#'
#' @return \link{rtMod}
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s_GAM.default <- function(x, y = NULL,
                          x.test = NULL, y.test = NULL,
                          x.name = NULL, y.name = NULL,
                          data = NULL,
                          data.test = NULL,
                          k = 6,
                          family = NULL,
                          weights = NULL,
                          ipw = TRUE,
                          ipw.type = 2,
                          upsample = FALSE,
                          downsample = FALSE,
                          resample.seed = NULL,
                          method = "REML",
                          select = FALSE,
                          removeMissingLevels = TRUE,
                          spline.index = NULL,
                          verbose = TRUE,
                          trace = 0,
                          print.plot = TRUE,
                          plot.fitted = NULL,
                          plot.predicted = NULL,
                          plot.theme = rtTheme,
                          na.action = na.exclude,
                          question = NULL,
                          n.cores = rtCores,
                          outdir = NULL,
                          save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {
    # Intro ----
    if (missing(x)) {
        print(args(s_GAM))
        return(invisible(9))
    }
    if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
    logFile <- if (!is.null(outdir)) {
        paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
    } else {
        NULL
    }
    start.time <- intro(verbose = verbose, logFile = logFile)
    mod.name <- "GAM"

    # Dependencies ----
    dependency_check("mgcv")

    # Arguments ----
    if (missing(x)) {
        print(args(s_GAM))
        stop("x is missing")
    }
    if (is.null(y) & NCOL(x) < 2) {
        print(args(s_GAM))
        stop("y is missing")
    }
    if (is.null(x.name)) x.name <- getName(x, "x")
    if (is.null(y.name)) y.name <- getName(y, "y")

    # Data ----
    dt <- dataPrepare(x, y,
        x.test, y.test,
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
    if (is.null(weights) && type == "Classification" && ipw) weights <- dt$weights
    if (verbose) dataSummary(x, y, x.test, y.test, type = type)
    if (print.plot) {
        if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
        if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
    } else {
        plot.fitted <- plot.predicted <- FALSE
    }

    df.train <- data.frame(y = y, x)
    if (is.null(family)) {
        if (type == "Regression") {
            family <- gaussian()
        } else {
            K <- length(levels(y)) - 1
            family <- if (length(levels(y)) == 2) binomial() else mgcv::multinom(K = K)
        }
    }

    # Formula ----
    if (is.null(spline.index)) spline.index <- which(sapply(x, is.numeric))
    lin.index <- which(!(seq_len(NCOL(x)) %in% spline.index))

    ### Predictors
    spline.features <- paste0("s(", colnames(x)[spline.index], ", k = ", k, ")",
        collapse = " + "
    )
    lin.features <- if (length(lin.index) > 0) {
        paste0(colnames(x)[lin.index], collapse = " + ")
    } else {
        NULL
    }
    features <- if (is.null(lin.features)) {
        spline.features
    } else {
        paste(spline.features, lin.features, sep = " + ")
    }

    ### Data frame
    df.test <- NULL
    if (!is.null(x.test)) {
        # Remove missing levels ----
        if (removeMissingLevels) {
            index.factor <- which(sapply(x, is.factor))
            if (length(index.factor) > 0) {
                levels.training <- lapply(x[, index.factor], function(x) levels(droplevels(x)))
                levels.testing <- lapply(x.test[, index.factor], levels)
                # Get index of levels present in test set and not in training
                index.missing <- lapply(seq_along(levels.training), function(i) levels.testing[[i]] %in% levels.training[[i]])
                # Set levels present in testing and missing in training to NA
                which.missing <- sapply(index.missing, all)
                if (any(!which.missing)) {
                    if (verbose) msg("Levels present in testing and not in training replaced with NA")
                    for (i in which(!which.missing)) {
                        missing.level <- levels.testing[[i]][!index.missing[[i]]]
                        index.extralevel <- x.test[, index.factor][, i] == missing.level
                        x.test[, index.factor][, i][index.extralevel] <- NA
                    }
                }
            }
        }

        df.test <- as.data.frame(x.test)
        colnames(df.test) <- colnames(df.train)[-1]
    }

    ### Formula
    if (family$family != "multinom") {
        .formula <- as.formula(paste0(y.name, " ~ ", features))
    } else {
        # mgcv::multinom() needs a list of K- 1 formulas
        .formula <- lapply(
            c(
                list(paste0(y.name, " ~ ", features)),
                as.list((paste0(rep(paste0("~ ", features), K - 1))))
            ),
            as.formula
        )
        df.train$y <- as.integer(df.train$y) - 1
    }

    if (!verbose) print.plot <- FALSE
    verbose <- verbose | !is.null(logFile)
    if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

    # GAM ----
    if (verbose) msg("Training GAM...", newline.pre = TRUE)
    args <- c(
        list(
            formula = .formula,
            family = family,
            data = df.train,
            weights = weights,
            select = select,
            method = method,
            na.action = na.action
        ),
        list(...)
    )
    mod <- do.call(mgcv::gam, args)
    if (trace > 0) print(summary(mod))

    # Fitted ----
    if (type == "Regression") {
        fitted <- predict(mod, df.train, se.fit = TRUE)
        se.fit <- as.numeric(fitted$se.fit)
        fitted <- as.numeric(fitted$fit)
    } else {
        se.fit <- NULL
        fitted <- predict(mod, df.train, type = "response")
        if (family$family == "binomial") {
            fitted <- factor(levels(y)[ifelse(fitted >= .5, 1, 0) + 1])
        } else {
            fitted <- factor(levels(y)[apply(fitted, 1, which.max)])
        }
    }

    if (type == "Classification") fitted <- factor(fitted, levels = levels(y))
    error.train <- modError(y, fitted)
    if (verbose) errorSummary(error.train, mod.name)

    # Predicted ----
    predicted <- se.prediction <- error.test <- NULL
    if (!is.null(df.test)) {
        if (type == "Regression") {
            predicted <- predict(mod, data.frame(df.test), se.fit = TRUE)
            se.prediction <- predicted$se.fit
            predicted <- predicted <- as.numeric(predicted$fit)
        } else {
            predicted <- predict(mod, df.test, type = "response")
            if (family$family == "binomial") {
                predicted <- factor(levels(y)[ifelse(predicted >= .5, 1, 0) + 1])
            } else {
                predicted <- factor(levels(y)[apply(predicted, 1, which.max)])
            }
            se.prediction <- NULL
        }

        if (type == "Classification") predicted <- factor(predicted, levels = levels(y))
        if (!is.null(y.test)) {
            error.test <- modError(y.test, predicted)
            if (verbose) errorSummary(error.test, mod.name)
        }
    }

    # Outro ----
    rt <- rtModSet(
        rtclass = "rtMod",
        mod = mod,
        mod.name = mod.name,
        type = type,
        y.train = y,
        y.test = y.test,
        x.name = x.name,
        y.name = y.name,
        xnames = xnames,
        fitted = fitted,
        se.fit = se.fit,
        error.train = error.train,
        predicted = predicted,
        se.prediction = se.prediction,
        error.test = error.test,
        parameters = list(
            formula = .formula,
            family = family,
            k = k,
            ipw = ipw,
            ipw.type = ipw.type,
            upsample = upsample,
            downsample = downsample,
            resample.seed = resample.seed
        ),
        question = question
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
} # rtemis::s_GAM
