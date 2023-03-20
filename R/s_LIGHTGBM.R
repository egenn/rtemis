# s_LIGHTGBM.R
# ::rtemis::
# 2023 E.D. Gennatas www.lambdamd.org
# https://lightgbm.readthedocs.io/en/latest/R/index.html
# https://lightgbm.readthedocs.io/en/latest/Parameters.html
# https://lightgbm.readthedocs.io/en/latest/R/articles/basic_walkthrough.html

#' LightGBM Classification and Regression [C, R]
#'
#' Tune hyperparameters using grid search and resampling,
#' train a final model, and validate it
#'
#' [gS]: indicates parameter will be autotuned by grid search if multiple 
#' values are passed.
#' For categorical variables, convert to integer and indicate to lgb they are categorical,
#' so that they are not treated as numeric.
#' 
#' @inheritParams s_GLM
#' @param booster Character: "gbtree", "gblinear": Booster to use.
#' @param max_nrounds Integer: Maximum number of rounds to run. Can be set to a high number 
#' as early stopping will limit nrounds by monitoring inner CV error
#' @param force_nrounds Integer: Number of rounds to run if not estimating optimal number by CV
#' @param early_stopping_rounds Integer: Training on resamples of \code{x} (tuning) will
#' stop if performance does not improve for this many rounds
#' @param objective (Default = NULL)
#' @param nthread Integer: Number of threads for lightgbm using OpenMP. Only parallelize 
#' resamples using \code{n.cores} or the lightgbm execution using this setting.
#'
#' @return \link{rtMod} object
#' @author E.D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @family Tree-based methods
#' @export

s_LIGHTGBM <- function(x, y = NULL,
                      x.test = NULL, y.test = NULL,
                      x.name = NULL, y.name = NULL,
                      weights = NULL,
                      ipw = TRUE,
                      ipw.type = 2,
                      upsample = FALSE,
                      downsample = FALSE,
                      resample.seed = NULL,
                      boosting = "gbdt",
                      objective = NULL,
                      max_nrounds = 1000L,
                      force_nrounds = NULL,
                      early_stopping_rounds = 10L,
                      nrounds_default = 100L,
                      max_depth = 3L,
                      learning_rate = .01,
                      bagging_fraction = .8,
                      data_sample_strategy = "bagging",
                      resampler = "strat.sub",
                      n.resamples = 10,
                      train.p = 0.75,
                      strat.n.bins = 4,
                      stratify.var = NULL,
                      target.length = NULL,
                      seed = NULL,
                      plot.res = TRUE,
                      save.res = FALSE,
                      .gs = FALSE,
                      grid.resample.rtset = rtset.resample("kfold", 5),
                      grid.search.type = "exhaustive",
                      metric = NULL,
                      maximize = NULL,
                      importance = TRUE,
                      print.plot = FALSE,
                      plot.fitted = NULL,
                      plot.predicted = NULL,
                      plot.theme = rtTheme,
                      question = NULL,
                      rtclass = NULL,
                      save.dump = FALSE,
                      verbose = TRUE,
                      grid.verbose = FALSE,
                      lightgbm_verbose = 0,
                      trace = 0,
                      save.gridrun = FALSE,
                      n.cores = 1,
                      n_threads = rtCores,
                      force_col_wise = FALSE,
                      force_row_wise = FALSE,
                      parallel.type = c("psock", "fork"),
                      outdir = NULL,
                      save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

    # Intro ----
    if (missing(x)) {
        print(args(s_LIGHTGBM))
        return(invisible(9))
    }
    if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
    logFile <- if (!is.null(outdir)) {
        paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
    } else {
        NULL
    }
    start.time <- intro(verbose = verbose, logFile = logFile)
    mod.name <- "LIGHTGBM"

    # Dependencies ----
    dependency_check("lightgbm")

    # Arguments ----
    nrounds <- max_nrounds
    #   if (save.res.mod) save.res <- TRUE
    if (is.null(x.name)) x.name <- getName(x, "x")
    if (is.null(y.name)) y.name <- getName(y, "y")
    if (!verbose) print.plot <- FALSE
    verbose <- verbose | !is.null(logFile)
    if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
    if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

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
    .weights <- if (is.null(weights) && ipw) dt$weights else weights
    if (any(sapply(x, is.factor))) {
        factor_index <- names(x)[which(sapply(x, is.factor))]
        # x <- preprocess(x, oneHot = TRUE)
        # if (!is.null(x.test)) x.test <- preprocess(x.test, oneHot = TRUE)
        x <- preprocess(x, factor2integer = TRUE, factor2integer_startat0 = TRUE)
        if (!is.null(x.test)) {
            x.test <- preprocess(x.test,
                factor2integer = TRUE,
                factor2integer_startat0 = TRUE
            )
        }
    } else {
        factor_index <- NULL
    }
    x0 <- if (upsample || downsample) dt$x0 else x
    y0 <- if (upsample || downsample) dt$y0 else y
    if (verbose) dataSummary(x, y, x.test, y.test, type)
    if (print.plot) {
        if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
        if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
    } else {
        plot.fitted <- plot.predicted <- FALSE
    }
    if (type == "Classification") y.num <- as.numeric(y) - 1
    nclass <- ifelse(type == "Classification", length(levels(y)), 0)
    if (is.null(objective)) {
        if (type == "Regression") {
            objective <- "regression"
        } else {
            # objective <- ifelse(nclass == 2, "binary:logistic", "multi:softmax")
            objective <- "xentropy"
        }
    }
    dat.train <- lightgbm::lgb.Dataset(
        data = as.matrix(x),
        params = list(missing = missing),
        # categorical_feature = factor_index,
        label = if (type == "Classification") as.numeric(y) - 1 else y,
        weight = .weights
    )

    if (!is.null(x.test)) {
        dat.test <- lightgbm::lgb.Dataset(
            data = as.matrix(x.test),
            params = list(missing = missing),
            # categorical_feature = factor_index,
            label = if (type == "Classification") as.numeric(y.test) - 1 else y.test
        )
    }

    # Grid Search ----
    if (is.null(metric)) {
        if (type == "Classification") {
            metric <- "Balanced Accuracy"
            if (is.null(maximize)) maximize <- TRUE
        } else if (type == "Regression") {
            metric <- "MSE"
            if (is.null(maximize)) maximize <- FALSE
        }
    }

    if (is.null(maximize)) {
        maximize <- if (type == "Classification") TRUE else FALSE
    }

    gc <- gridCheck(
        max_depth, learning_rate, bagging_fraction
    )
    
    tuned <- FALSE
    if (!.gs && (gc || is.null(force_nrounds))) {
        grid.params <- 
            list(
                max_depth = max_depth,
                learning_rate = learning_rate,
                bagging_fraction = bagging_fraction
            )
        gs <- gridSearchLearn(
            x = x0, y = y0,
            mod = mod.name,
            resample.rtset = grid.resample.rtset,
            grid.params = grid.params,
            fixed.params = list(
                max_nrounds = max_nrounds,
                early_stopping_rounds = early_stopping_rounds,
                objective = objective,
                ipw = ipw,
                ipw.type = ipw.type,
                upsample = upsample,
                resample.seed = resample.seed,
                .gs = TRUE
            ),
            search.type = grid.search.type,
            weights = weights,
            metric = metric,
            maximize = maximize,
            save.mod = save.gridrun,
            verbose = verbose,
            grid.verbose = grid.verbose,
            n.cores = n.cores
        )

        nrounds <- gs$best.tune$nrounds
        if (nrounds <= 0) {
            warning(
                "Could not get CV estimate for best N rounds. Defaulting to",
                nrounds_default
            )
            nrounds <- nrounds_default
        }
        max_depth <- gs$best.tune$max_depth
        learning_rate <- gs$best.tune$learning_rate
        bagging_fraction <- gs$best.tune$bagging_fraction
        tuned <- TRUE

        # Now ready to train final full model
        .gs <- FALSE
    } else {
        gs <- NULL
    }
    if (!is.null(force_nrounds)) nrounds <- force_nrounds
    parameters <- list(
        objective = objective,
        nrounds = nrounds,
        max_depth = max_depth,
        learning_rate = learning_rate,
        bagging_fraction = bagging_fraction,
        num_threads = n_threads,
        force_col_wise = force_col_wise,
        force_row_wise = force_row_wise
    )

    # LightGBM ----
    if (verbose) {
        if (tuned) {
            msg20("Training LightGBM ", type, " with tuned hyperparameters...", newline.pre = TRUE)
        } else {
            msg20("Training LightGBM ", type, "...", newline.pre = TRUE)
        }
    } 
    # watchlist <- if (.gs) {
    #     list(train = xg.dat.train, valid = xg.dat.test)
    # } else {
    #     NULL
    # }
    if (trace > 0) printls(parameters)
    if (.gs) {
        valids <- dat.test
    }
    mod <- lightgbm::lgb.train(
        parameters,
        data = dat.train,
        nrounds = nrounds,
        verbose = lightgbm_verbose,
        valids = if (.gs) list(train = dat.train, valid = dat.test) else NULL,
        early_stopping_rounds = if (.gs) early_stopping_rounds else NULL, ...
    )

    # Fitted ----
    fitted <- predict(mod, as.matrix(x))
    fitted.prob <- NULL
    if (type == "Classification") {
        if (nclass == 2) {
            fitted.prob <- 1 - fitted
            fitted <- factor(ifelse(fitted.prob >= .5, 1, 0),
                levels = c(1, 0),
                labels = levels(y)
            )
        } else {
            fitted <- factor(fitted,
                levels = seq(nclass) - 1,
                labels = levels(y)
            )
        }
    }

    error.train <- modError(y, fitted, fitted.prob)
    if (verbose) errorSummary(error.train, mod.name)

    # Predicted ----
    predicted.prob <- predicted <- error.test <- NULL
    if (!is.null(x.test)) {
        predicted <- predict(mod, as.matrix(x.test))
        if (type == "Classification") {
            if (nclass == 2) {
                predicted.prob <- 1 - predicted
                predicted <- factor(ifelse(predicted.prob >= .5, 1, 0),
                    levels = c(1, 0),
                    labels = levels(y)
                )
            } else {
                predicted <- factor(predicted,
                    levels = seq(nclass) - 1,
                    labels = levels(y)
                )
            }
        }
        if (!is.null(y.test)) {
            error.test <- modError(y.test, predicted, predicted.prob)
            if (verbose) errorSummary(error.test, mod.name)
        }
    }

    # Relative Influence / Variable Importance ----
    varimp <- NULL
    # This may take a while
    if (importance) {
        if (verbose) msg2("Estimating variable importance...")
        .lgbvarimp <- lightgbm::lgb.importance(model = mod, percentage = TRUE)
        varimp <- .lgbvarimp$Gain
        names(varimp) <- .lgbvarimp$Feature
    }

    # Outro ----
    rt <- rtModSet(
        rtclass = "rtMod",
        mod = mod,
        mod.name = mod.name,
        type = type,
        gridsearch = gs,
        parameters = parameters,
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
} # rtemis::s_LIGHTGBM
