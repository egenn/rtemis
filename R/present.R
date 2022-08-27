# present.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Present elevate models
#' 
#' Plot training and testing performance boxplots of multiple \link{rtModCV}
#' objects created by \link{elevate} using \link{dplot3_box}
#' 
#' @param ... rtModCV objects created with \link{elevate}
#' @param which.repeat Integer: which \code{rtModCV} repeat to plot
#' @param metric Character: which metric to plot
#' @param main Character: title
#' @param htest Character: "none", "t.test", or "wilcox.test"
#' @param htest.annotate.y Numeric: y-axis coordinate for htest annotation
#' @param margin Named list with margins and padding
#' 
#' @author E.D. Gennatas
#' @export 

present <- function(...,
                    which.repeat = 1,
                    metric = NULL,
                    plot.train = TRUE,
                    plot.test = TRUE,
                    main = NULL,
                    htest = "wilcox.test",
                    htest.annotate.y = NULL,
                    margin = list(b = 65, l = 100, t = 60, r = 18, pad = 0)) {

    mods <- list(...)
    if (is.null(htest.annotate.y)) {
        htest.annotate.y <- if ((plot.train & plot.test)) {
            -.105
        } else {
            -.05
        }
    }

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
    if (plot.train) {
        plot_train <- dplot3_box(
            train.error,
            main = main,
            ylab = paste("Training", metric),
            htest = htest,
            htest.annotate = htest.annotate.y,
            margin = margin
        )
    }
    
    if (plot.test) {
        plot_test <- dplot3_box(
            test.error,
            ylab = paste("Testing", metric),
            htest = htest,
            htest.annotate.y = htest.annotate.y,
            margin = margin
        )
    }
    
    if (plot.train & plot.test) {
        plotly::subplot(
            plot_train, plot_test,
            nrows = 2,
            shareX = TRUE,
            titleY = TRUE
        )
    } else if (plot.test) {
        plot_test
    } else {
        plot_train
    }
    
    
} # rtemis::present
