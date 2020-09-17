# rtset.R
# ::rtemis::
# 2016-8 Efstathios D. Gennatas egenn.github.io

#' \pkg{rtemis} default-setting functions
#'
#' These functions output lists of default settings for different \pkg{rtemis} functions.
#' This removes the need of passing named lists of arguments, and provides autocompletion,
#' making it easier to setup functions without having to refer to the manual.
#'
#' @name rtset
#' @author Efstathios D. Gennatas
NULL


#' \code{rtset.resample}: Set \link{resample} settings
#'
#'
#' @inheritParams resample
#' @export
rtset.resample <- function(resampler = "kfold",
                           n.resamples = 10,
                           stratify.var = NULL,
                           train.p = .75,
                           strat.n.bins = 4,
                           target.length = NULL,
                           seed = NULL) {


  list(resampler = resampler,
       n.resamples = n.resamples,
       stratify.var = stratify.var,
       train.p = train.p,
       strat.n.bins = strat.n.bins,
       target.length = target.length,
       seed = seed)

} # rtemis::rtset.resample


#' \code{rtset.grid.resample}: \link{resample} defaults for \link{gridSearchLearn}
#'
#' @inheritParams resample
#' @export
rtset.grid.resample <- function(resampler = "strat.boot", n.resamples = 10,
                                stratify.var = NULL, train.p = .75, strat.n.bins = 4,
                                target.length = NULL, verbose = TRUE) {
  list(resampler = resampler,
       n.resamples = n.resamples,
       stratify.var = stratify.var,
       train.p = train.p,
       strat.n.bins = strat.n.bins,
       target.length = target.length,
       verbose = verbose)

} # rtemis::rtset.grid.resample


#' \code{rtset.bag.resample}: \link{resample} defaults for \code{rtMod} bagging
#'
#' @inheritParams resample
#' @export
rtset.bag.resample <- function(resampler = "strat.sub", n.resamples = 10,
                               stratify.var = NULL, train.p = .75, strat.n.bins = 4,
                               target.length = NULL, verbose = TRUE) {

  list(resampler = resampler,
       n.resamples = n.resamples,
       stratify.var = stratify.var,
       train.p = train.p,
       strat.n.bins = strat.n.bins,
       target.length = target.length,
       verbose = verbose)

} # rtemis::rtset.bag.resample


#' \code{rtset.meta.resampler}: \link{resample} defaults for meta model training
#'
#' @inheritParams resample
#' @export
rtset.meta.resample <- function(resampler = "strat.sub", n.resamples = 4,
                                stratify.var = NULL, train.p = .75, strat.n.bins = 4,
                                target.length = NULL, verbose = TRUE) {

  list(resampler = resampler,
       n.resamples = n.resamples,
       stratify.var = stratify.var,
       train.p = train.p,
       strat.n.bins = strat.n.bins,
       target.length = target.length,
       verbose = verbose)

} # rtemis::rtset.meta.resample


#' \code{rtset.cv.resample}: \link{resample} defaults for cross-validation
#'
#' @inheritParams resample
#' @export
rtset.cv.resample <- function(resampler = "kfold", n.resamples = 10,
                              stratify.var = NULL, train.p = .75, strat.n.bins = 4,
                              target.length = NULL, verbose = TRUE) {

  list(resampler = resampler,
       n.resamples = n.resamples,
       stratify.var = stratify.var,
       train.p = train.p,
       strat.n.bins = strat.n.bins,
       target.length = target.length,
       verbose = verbose)

} # rtemis::rtset.cv.resample


# #' rtset
# #' @author Efstathios D. Gennatas
# #' @export
# rtset <- function(fn) {
#
#   .args <- as.list(formals(fn))
#   return(.args)
#
# }


#' \code{rtset.cluster}: Set up parallel processing using forking (Linux, macOS) or PSOCK cluster (macOS, Linux, Windows)
#' Defaults to type = "fork", which would call \code{mclapply}
#' Some functions fail to work correctly with \code{mclapply}, like \code{nnet::multinom}, in those cases
#' we use PSOCK cluster
#'
#' @param type Character: "fork", "psock"
#' @param hosts Vector of strings: For type = "psock": Host names on which to run (macOS, Linux, Windows)
#' @param n.cores Integer: Number of cores to use on \code{localhost} for type = "fork" (macOS, Linux only)
#' @param ... \code{rtset.cluster}: Additional argument to be passed to \code{parallel::makePSOCKcluster}
#' @return List with parameters
#' @export

rtset.cluster <- function(type = "fork",
                          hosts = NULL,
                          n.cores = rtCores, ...) {

  c(list(type = type, hosts = hosts, n.cores = n.cores), list(...))

} # rtemis::rtset.cluster


#' \code{rtset.color}: Set parameters for \link{colorGrad}
#'
#' @inheritParams colorGrad
#' @export

rtset.color <- function(n = 101, colors = NULL,
                        space = "rgb",
                        lo = "#01256E",
                        lomid = NULL,
                        mid = "white",
                        midhi = NULL,
                        hi = "#95001A",
                        colorbar = FALSE,
                        cb.mar = c(1, 1, 1, 1), ...) {

  c(list(n = n, colors = colors, space = space,
         lo = lo, lomid = lomid, mid = mid, midhi = midhi, hi = hi,
         colobar = colorbar, cb.mar = cb.mar), list(...))

} # rtemis::rtset.color


#' \code{rtset.preprocess}: Set \link{preprocess} parameters for \link{elevate}'s \code{.preprocess} argument
#'
#' @param decom Character: Name of decomposer to use. Default = "ICA"
#' @param k Integer: Number of dimensions to project to. Default = 2
#' @export

rtset.preprocess <- function(completeCases = FALSE,
                             removeCases.thres = NULL,
                             removeFeatures.thres = NULL,
                             impute = FALSE,
                             impute.type = "missRanger",
                             impute.missRanger.params = list(pmm.k = 0, maxiter = 10),
                             impute.missForest.params = list(maxiter = 10, ntree = 500, parallelize = "no"),
                             impute.rfImpute.params = list(niter = 10, ntree = 500),
                             impute.discrete = getMode,
                             impute.numeric = mean,
                             integer2factor = FALSE,
                             integer2numeric = FALSE,
                             logical2factor = FALSE,
                             logical2numeric = FALSE,
                             numeric2factor = FALSE,
                             numeric2factor.levels = NULL,
                             character2factor = FALSE,
                             nonzeroFactors = FALSE,
                             scale = FALSE,
                             center = FALSE,
                             removeConstants = TRUE,
                             oneHot = FALSE,
                             exclude = NULL) {

  list(completeCases = completeCases,
       removeCases.thres = removeCases.thres,
       removeFeatures.thres = removeFeatures.thres,
       impute = impute,
       impute.type = impute.type,
       impute.missRanger.params = impute.missRanger.params,
       impute.missForest.params = impute.missForest.params,
       impute.rfImpute.params = impute.rfImpute.params,
       impute.discrete = impute.discrete,
       impute.numeric = impute.numeric,
       integer2factor = integer2factor,
       integer2numeric = integer2numeric,
       logical2factor = logical2factor,
       logical2numeric = logical2numeric,
       numeric2factor = numeric2factor,
       numeric2factor.levels = numeric2factor.levels,
       character2factor = character2factor,
       nonzeroFactors = nonzeroFactors,
       scale = scale,
       center = center,
       removeConstants = removeConstants,
       oneHot = oneHot,
       exclude = exclude)

} # rtemis::rtset.preprocess


#' \code{rtset.decompose}: Set decomposition parameters for \code{elevate}'s \code{.decompose} argument
#'
#' @param decom Character: Name of decomposer to use. Default = "ICA"
#' @param k Integer: Number of dimensions to project to. Default = 2
#' @export

rtset.decompose <- function(decom = "ICA",
                            k = 2, ...) {

  c(list(decom = decom, k = k), list(...))

} # rtemis::rtset.decompose

#' \code{rtset.earlystop}: Set parameters for \link{earlystop}
#'
#' @inheritParams earlystop
#' @export

rtset.earlystop <- function(window = 150,
                            window_decrease_pct_min = 0.01,
                            total_decrease_pct_max = NULL) {

  list(window = window,
       window_decrease_pct_min = window_decrease_pct_min,
       total_decrease_pct_max = total_decrease_pct_max)

} # rtemis::rtset.earlystop


#' \code{rtset.LIHAD}: Set parameters for \link{s.LIHAD}
#'
#' @inheritParams s.LIHAD
#' @export

rtset.LIHAD <- function(max.depth = 2,
                         learning.rate = 1,
                         lincoef.params = rtset.lincoef("glmnet"),
                         alpha = 0,
                         lambda = .1,
                         minobsinnode = 2,
                         minobsinnode.lin = 20, ...) {

  c(list(max.depth = max.depth,
         learning.rate = learning.rate,
         lincoef.params = lincoef.params,
         alpha = alpha,
         lambda = lambda,
         minobsinnode = minobsinnode,
         minobsinnode.lin = minobsinnode.lin),
    list(...))

} # rtemis::rtset.ADDT


#' \code{rtset.GBM}: Set parameters for \link{s.GBM}
#'
#' @inheritParams s.GBM
#' @export

rtset.GBM <- function(interaction.depth = 2,
                      shrinkage = .001,
                      max.trees = 5000,
                      min.trees = 100,
                      bag.fraction = .9,
                      n.minobsinnode = 5,
                      grid.resample.rtset = rtset.resample("kfold", 5),
                      ipw = TRUE,
                      upsample = FALSE,
                      downsample = FALSE,
                      resample.seed = NULL, ...) {

  c(list(interaction.depth = interaction.depth,
         shrinkage = shrinkage,
         max.trees = max.trees,
         min.trees = min.trees,
         bag.fraction = bag.fraction,
         n.minobsinnode = n.minobsinnode,
         grid.resample.rtset = grid.resample.rtset,
         ipw = ipw,
         upsample = upsample,
         downsample = downsample,
         resample.seed = resample.seed),
    list(...))

} # rtemis::rtset.GBM


#' \code{rtset.RANGER}: Set parameters for \link{s.RANGER}
#'
#' @inheritParams s.RANGER
#' @export

rtset.RANGER <- function(n.trees = 1000,
                         min.node.size = 1,
                         mtry = NULL,
                         grid.resample.rtset = rtset.resample("kfold", 5),
                         ipw = TRUE,
                         upsample = FALSE,
                         downsample = FALSE,
                         resample.seed = NULL, ...) {

  c(list(n.trees = n.trees,
         min.node.size = min.node.size,
         mtry = mtry,
         grid.resample.rtset = grid.resample.rtset,
         ipw = ipw,
         upsample = upsample,
         downsample = downsample,
         resample.seed = resample.seed),
    list(...))

} # rtemis::rtset.RANGER


#' \code{rtset.DN}: Set parameters for \link{s.DN}
#'
#' @inheritParams s.DN
#' @export

rtset.DN <- function(hidden = 1,
                     activation = NULL,
                     learning.rate = .8,
                     momentum = .5,
                     learningrate_scale = 1,
                     output = NULL,
                     numepochs = 100,
                     batchsize = NULL,
                     hidden_dropout = 0,
                     visible_dropout = 0, ...) {

  c(list(hidden = hidden,
         activation = activation,
         learning.rate = learning.rate,
         momentum = momentum,
         learningrate_scale = learningrate_scale,
         output = output,
         numepochs = numepochs,
         batchsize = batchsize,
         hidden_dropout = hidden_dropout,
         visible_dropout = visible_dropout),
    list(...))

} # rtemis::rtset.DN


# \code{rtset.MXN}: Set parameters for \link{s.MXN}
#
# @inheritParams s.MXN
# @export
#
# rtset.MXN <- function(n.hidden.nodes = NULL,
#                       output = NULL,
#                       activation = 'relu',
#                       ctx = mxnet::mx.cpu(),
#                       optimizer = "sgd",
#                       initializer = mxnet::mx.init.Xavier(),
#                       batch.size = NULL,
#                       momentum = .9,
#                       max.epochs = 2000,
#                       min.epochs = 25,
#                       early.stop = "train",
#                       early.stop.n.steps = NULL,
#                       early.stop.relativeVariance.threshold = NULL,
#                       learning.rate = NULL,
#                       dropout = 0,
#                       dropout.before = 1,
#                       dropout.after = 0,
#                       eval.metric = NULL,
#                       arg.params = NULL,
#                       mx.seed = NULL) {
#
#   list(n.hidden.nodes = n.hidden.nodes,
#        output = output,
#        activation = activation,
#        ctx = ctx,
#        optimizer = optimizer,
#        initializer = initializer,
#        batch.size = batch.size,
#        momentum = momentum,
#        max.epochs = max.epochs,
#        min.epochs = min.epochs,
#        early.stop = early.stop,
#        early.stop.n.steps = early.stop.n.steps,
#        early.stop.relativeVariance.threshold = early.stop.relativeVariance.threshold,
#        learning.rate = learning.rate,
#        dropout = dropout,
#        dropout.before = dropout.before,
#        dropout.after = dropout.after,
#        eval.metric = eval.metric,
#        arg.params = arg.params,
#        mx.seed = mx.seed)
#
# } # rtemis::rtset.MXN


#' \code{rtset.lincoef}: Set parameters for \link{lincoef}
#'
#' @inheritParams lincoef
#' @export

rtset.lincoef <- function(method = c("glmnet",
                                     "cv.glmnet",
                                     "lm.ridge",
                                     "allSubsets",
                                     "forwardStepwise",
                                     "backwardStepwise",
                                     "glm",
                                     "sgd",
                                     "solve"),
                          alpha = 0,
                          lambda = .01,
                          lambda.seq = NULL,
                          cv.glmnet.nfolds = 5,
                          which.cv.glmnet.lambda = c("lambda.min", "lambda.1se"),
                          nbest = 1,
                          nvmax = 8,
                          sgd.model = "glm",
                          sgd.model.control = list(lambda1 = 0,
                                                   lambda2 = 0),
                          sgd.control = list(method = "ai-sgd")) {

  list(method = match.arg(method),
       alpha = alpha,
       lambda = lambda,
       lambda.seq = lambda.seq,
       cv.glmnet.nfolds = cv.glmnet.nfolds,
       which.cv.glmnet.lambda = which.cv.glmnet.lambda,
       nbest = nbest,
       nvmax = nvmax,
       sgd.model = sgd.model,
       sgd.model.control = sgd.model.control,
       sgd.control = sgd.control)

} # rtemis::rtset.lincoef


#' \code{rtset.MARS}: Set parameters for \link{s.MARS}
#'
#' @inheritParams s.MARS
#' @export

rtset.MARS <- function(hidden = 1,
                     activation = NULL,
                     learning.rate = .8,
                     momentum = .5,
                     learningrate_scale = 1,
                     output = NULL,
                     numepochs = 100,
                     batchsize = NULL,
                     hidden_dropout = 0,
                     visible_dropout = 0, ...) {

  c(list(hidden = hidden,
         activation = activation,
         learning.rate = learning.rate,
         momentum = momentum,
         learningrate_scale = learningrate_scale,
         output = output,
         numepochs = numepochs,
         batchsize = batchsize,
         hidden_dropout = hidden_dropout,
         visible_dropout = visible_dropout),
    list(...))

} # rtemis::rtset.DN
