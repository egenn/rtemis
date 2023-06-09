# resLearn_future.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org
# new version of resLearn using future

#' \pkg{rtemis} internal: Resample Learn
#'
#' Train an \pkg{rtemis} learner on a set of resamples
#'
#' Input: features (x) and outcome (y)
#' Procedure: [resample], train learners
#' Output: trained learners
#' This is used internally by [train] and for bagging, when the `bag.resampler`
#' argument is set in a learner.
#' @param x features - training set
#' @param y outcome - training set
#' @param mod Character: \pkg{rtemis} model. See `learnSelect` gives available models
#' @param resample.rtset List: output of [rtset] (or a list of same structure)
#' @param params List of named elements, each is a single value
#' @param verbose Logical: If TRUE, print messages to screen
#' @param res.verbose Logical: Will be passed to each `mod`'s `verbose` argument
#' @param save.mods Logical: If TRUE, save all models, otherwise discard after training.
#' Use with [train] when training a large number of resamples. Default = TRUE
#' @param outdir Character: Path to save output. Default = NULL
#' @param n.workers Integer: Number of cores to use.
#'
#' @author E.D. Gennatas
#' @export

resLearn_future <- function(x, y, mod,
                            resample.rtset = rtset.cv.resample(),
                            weights = NULL,
                            params = list(),
                            mtry = NULL,
                            .preprocess = NULL,
                            verbose = TRUE,
                            res.verbose = FALSE,
                            trace = 0,
                            save.mods = TRUE,
                            outdir = NULL,
                            n.workers = 1,
                            parallel.type = "nobodycares") {

    # Dependencies ----
    dependency_check("future.apply")
    
    # Intro ----
    start.time <- intro(
        verbose = trace > 0,
        message = "Starting resLearn...",
        newline.pre = TRUE
    )
    future::plan(rtPlan)
    # rtemis_init(n.workers, context = "Outer resampling")

    # Arguments ----
    if (missing(x) || missing(y)) {
        print(args(resLearn))
        stop("Input missing")
    }
    mod.name <- toupper(mod)
    if (!is.null(outdir)) {
        outdir <- normalizePath(outdir, mustWork = FALSE)
        if (!dir.exists(outdir)) {
            dir.create(outdir,
                showWarnings = TRUE,
                recursive = TRUE,
                mode = "0777"
            )
        }
    }

    # Resamples ----
    learner <- learnSelect(mod)
    res <- resample(y, rtset = resample.rtset, verbose = trace > 0)
    resampler <- attr(res, "type") # for res.group and res.index
    n.workers <- min(n.workers, resample.rtset$n.resamples)

    # Parallel ----
    if (n.workers == 1) {
        future::plan(list("sequential", rtPlan), workers = n.workers)
        if (verbose) {
            msg2(magenta("Outer resampling plan set to",
                bold("sequential"))
            )
        }
    } else {
        future::plan(list(rtPlan, "sequential"), workers = n.workers)
        if (verbose) {
            msg2(magenta("Outer resampling: Future plan set to", bold(rtPlan),
                "with", bold(n.workers), "workers")
            )
        }
    }

    # learner1 ----
    p <- progressr::progressor(along = res)
    learner1 <- function(index, learner,
                         x, y,
                         weights = NULL,
                         mtry,
                         res,
                         params,
                         .preprocess,
                         verbose,
                         outdir,
                         save.mods,
                         nres) {
        if (verbose) msg2("Running resample #", index, sep = "")
        res1 <- res[[index]]
        if (is.null(mtry)) {
            feat.index <- seq_len(NCOL(x))
        } else {
            feat.index <- sample(seq(NCOL), mtry, replace = FALSE)
        }
        x.train1 <- x[res1, feat.index, drop = FALSE]
        y.train1 <- y[res1]
        x.test1 <- x[-res1, feat.index, drop = FALSE]
        y.test1 <- y[-res1]

        if (!is.null(.preprocess)) {
            # This allows imputing training and testing sets separately
            preproc.params <- c(
                list(x = x.train1),
                .preprocess,
                verbose = verbose
            )
            x.train1 <- do.call(preprocess, preproc.params)
            preproc.params <- c(
                list(x = x.test1),
                .preprocess,
                verbose = verbose
            )
            x.test1 <- do.call(preprocess, preproc.params)
        }

        if (!is.null(weights)) weights <- weights[res1]

        if (!is.null(outdir)) {
            outdir1 <- paste0(outdir, "/", mod.name, ".resLearn.", index)
        } else {
            outdir1 <- NULL
        }

        if (!is.null(weights)) params$weights <- weights

        args <- list(
            x = x.train1, y = y.train1,
            x.test = x.test1, y.test = y.test1,
            print.plot = FALSE,
            verbose = res.verbose,
            outdir = outdir1
        )
        ## Get id.strat for resample
        if (!is.null(attr(res, "id.strat")) && !is.null(params$grid.resample.rtset)) {
            params$grid.resample.rtset$id.strat <- attr(res, "id.strat")[res1]
        }
        args <- c(args, params)

        mod1 <- do.call(learner, args = args)
        if (!save.mods) mod1$mod <- NA
        p(sprintf("Outer resample: %i/%i...", index, nres))
        list(
            mod1 = mod1,
            res.id = index,
            params = params
        )
    }

    # Res run ----
    if (trace > 0) {
        desc <- switch(resampler,
            kfold = "independent folds",
            strat.sub = "stratified subsamples",
            strat.boot = "stratified bootstraps",
            bootstrap = "bootstrap resamples",
            loocv = "independent folds (LOOCV)",
            "custom resamples"
        )
        msg20("Training ", learnSelect(mod, desc = TRUE), " on ",
            length(res), " ", desc, "...",
            newline.pre = FALSE
        )
    }

    res.run <- future.apply::future_lapply(
        seq_along(res), learner1,
        learner,
        x, y,
        weights,
        mtry,
        res,
        params,
        .preprocess,
        verbose = res.verbose,
        outdir = outdir,
        save.mods = save.mods,
        future.seed = TRUE,
        length(res)
    )
    
    names(res.run) <- paste0(toupper(mod), seq(res))
    if (res.verbose) cat("\n")

    # Outro ----
    outro(start.time, verbose = trace > 0)
    list(res = res, mods = res.run)

} # rtemis::resLearn_future
