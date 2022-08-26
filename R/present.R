# present.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Present elevate models
#' 
#' @param ... rtModCV objects created with \link{elevate}
#' 
#' @author E.D. Gennatas
#' @export 

present <- function(...,
                    which.repeat = 1,
                    metric = NULL,
                    main = NULL,
                    htest = "wilcox.test",
                    htest.annotate.y = -.15,
                    margin = list(b = 65, l = 100, t = 60, r = 18, pad = 0)) {
    mods <- list(...)

    # Check types ----
    types <- sapply(mods, \(m) m$type)
    if (length(unique(types)) > 1) {
        stop("All models must be of the same type")
    }
    type <- mods[[1]]$type
    if (is.null(metric)) {
        metric <- if (type == "Classification") {
            "Balanced Accuracy"
        } else {
            "R sq"
        }
    }

    # Get error ----
    mod.names <- sapply(mods, \(m) m$mod.name)
    train.error <- lapply(mods, \(m) m$error.train.res[[which.repeat]][[metric]])
    test.error <- lapply(mods, \(m) m$error.test.res[[which.repeat]][[metric]])
    names(train.error) <- names(test.error) <- mod.names

    # Plot ----
    plot_train <- dplot3_box(
        train.error,
        main = main,
        ylab = paste("Training", metric),
        htest = htest,
        htest.annotate = FALSE,
        margin = margin
    )

    plot_test <- dplot3_box(
        test.error,
        main = main,
        ylab = paste("Testing", metric),
        htest = htest,
        htest.annotate.y = htest.annotate.y,
        margin = margin
    )

    plotly::subplot(
        plot_train, plot_test,
        nrows = 2,
        shareX = TRUE,
        titleY = TRUE
    )
    
} # rtemis::present
