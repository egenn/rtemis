# s.XGBLIN.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.lambdamd.org

#' XGBoost with linear booster (wrapper for \link{s.XGB})
#'
#' Train an XGBoost learner with linear boosters
#'
#' [gS] denotes vector will trigger grid search
#' Warning: Using \code{nthread > 1} may lower accuracy - test on your data
#' @inheritParams s.XGB
#' @param ... Additional arguments to be passed to \link{s.XGB}
#' @author Efstathios D. Gennatas
#' @seealso \link{elevate} for external cross-validation
#' @family Supervised Learning
#' @export

s.XGBLIN <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     weights = NULL,
                     lambda = NULL,
                     lambda.bias = 0,
                     alpha = 0,
                     early_stopping_rounds = 50,
                     force.nrounds = NULL,
                     resampler = 'strat.sub',
                     n.resamples = 10,
                     train.p = 0.75,
                     strat.n.bins = 4,
                     stratify.var = NULL,
                     target.length = NULL,
                     seed = NULL,
                     importance = TRUE,
                     n.cores = 1,
                     nthread = detectCores(),
                     parallel.type = c("psock", "fork"),
                     print.plot = TRUE,
                     outdir = NULL,
                     verbose = TRUE,
                     xgb.verbose = FALSE, ...) {

  parallel.type <- match.arg(parallel.type)

  s.XGB(x, y, x.test, y.test, booster = "gblinear",
        weights = weights,
        lambda = lambda,
        lambda.bias = lambda.bias,
        alpha = alpha,
        force.nrounds = force.nrounds,
        resampler = resampler,
        n.resamples = n.resamples,
        train.p = train.p,
        strat.n.bins = strat.n.bins,
        stratify.var = stratify.var,
        target.length = target.length,
        seed = seed,
        importance = importance,
        n.cores = n.cores,
        nthread = nthread,
        parallel.type = parallel.type,
        print.plot = print.plot,
        outdir = outdir,
        verbose = verbose,
        xgb.verbose = xgb.verbose, ...)

} # rtemis::s.XGBLIN
