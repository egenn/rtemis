# setup.R
# ::rtemis::
# 2016-8 E.D. Gennatas rtemis.org

#' \pkg{rtemis} default-setting functions
#'
#' These functions output lists of default settings for different \pkg{rtemis} functions.
#' This removes the need of passing named lists of arguments, and provides autocompletion,
#' making it easier to setup functions without having to refer to the manual.
#'
#' @name rtset
#' @author E.D. Gennatas
NULL

#' Set [resample] settings
#'
#' @inheritParams resample
#' @export

setup.resample <- function(
  resampler = c("strat.sub", "strat.boot", "kfold", "bootstrap", "loocv"),
  n.resamples = 10,
  stratify.var = NULL,
  train.p = .8,
  strat.n.bins = 4,
  target.length = NULL,
  id.strat = NULL,
  seed = NULL
) {
  list(
    resampler = match.arg(resampler),
    n.resamples = n.resamples,
    stratify.var = stratify.var,
    train.p = train.p,
    strat.n.bins = strat.n.bins,
    target.length = target.length,
    id.strat = id.strat,
    seed = seed
  )
} # rtemis::setup.resample


#' Set [resample] parameters for `gridSearchLearn`
#'
#' @inheritParams resample
#' @export

setup.grid.resample <- function(
  resampler = "kfold",
  n.resamples = 5,
  stratify.var = NULL,
  train.p = .75,
  strat.n.bins = 4,
  target.length = NULL,
  verbosity = 1
) {
  list(
    resampler = resampler,
    n.resamples = n.resamples,
    stratify.var = stratify.var,
    train.p = train.p,
    strat.n.bins = strat.n.bins,
    target.length = target.length,
    verbosity = verbosity
  )
} # rtemis::setup.grid.resample


#' Set [resample] parameters for `rtMod` bagging
#'
#' @inheritParams resample
#' @export

setup.bag.resample <- function(
  resampler = "strat.sub",
  n.resamples = 10,
  stratify.var = NULL,
  train.p = .75,
  strat.n.bins = 4,
  target.length = NULL,
  verbosity = 1
) {
  list(
    resampler = resampler,
    n.resamples = n.resamples,
    stratify.var = stratify.var,
    train.p = train.p,
    strat.n.bins = strat.n.bins,
    target.length = target.length,
    verbosity = verbosity
  )
} # rtemis::setup.bag.resample


#' Set [resample] parameters for meta model training
#'
#' @inheritParams resample
#' @export

setup.meta.resample <- function(
  resampler = "strat.sub",
  n.resamples = 4,
  stratify.var = NULL,
  train.p = .75,
  strat.n.bins = 4,
  target.length = NULL,
  verbosity = TRUE
) {
  list(
    resampler = resampler,
    n.resamples = n.resamples,
    stratify.var = stratify.var,
    train.p = train.p,
    strat.n.bins = strat.n.bins,
    target.length = target.length,
    verbosity = verbosity
  )
} # rtemis::setup.meta.resample


#' `setup.cv.resample`: [resample] defaults for cross-validation
#'
#' @inheritParams resample
#' @export

setup.cv.resample <- function(
  resampler = "strat.sub",
  n.resamples = 10,
  stratify.var = NULL,
  train.p = .8,
  strat.n.bins = 4,
  target.length = NULL,
  id.strat = NULL,
  verbosity = 1
) {
  list(
    resampler = resampler,
    n.resamples = n.resamples,
    stratify.var = stratify.var,
    train.p = train.p,
    strat.n.bins = strat.n.bins,
    target.length = target.length,
    id.strat = id.strat,
    verbosity = verbosity
  )
} # rtemis::setup.cv.resample


# #' rtset
# #' @author E.D. Gennatas
# #' @export
# rtset <- function(fn) {
#
#   .args <- as.list(formals(fn))
#   return(.args)
#
# }

# #' \code{setup.cluster}: Set up parallel processing using forking (Linux, macOS) or PSOCK cluster (macOS, Linux, Windows)
# #' Defaults to type = "fork", which would call \code{mclapply}
# #' Some functions fail to work correctly with \code{mclapply}, like \code{nnet::multinom}, in those cases
# #' we use PSOCK cluster
# #'
# #' @param type Character: "fork", "psock"
# #' @param hosts Vector of strings: For type = "psock": Host names on which to run (macOS, Linux, Windows)
# #' @param n.cores Integer: Number of cores to use on \code{localhost} for type = "fork" (macOS, Linux only)
# #' @param ... \code{setup.cluster}: Additional argument to be passed to \code{parallel::makePSOCKcluster}
# #' @return List with parameters
# #' @export

# setup.cluster <- function(type = "fork",
#                           hosts = NULL,
#                           n.cores = rtCores, ...) {

#   c(list(type = type, hosts = hosts, n.cores = n.cores), list(...))

# } # rtemis::setup.cluster

#' Set [colorGrad] parameters
#'
#' @inheritParams colorGrad
#' @param ... Additional arguments
#'
#' @export

setup.color <- function(
  n = 101,
  colors = NULL,
  space = "rgb",
  lo = "#01256E",
  lomid = NULL,
  mid = "white",
  midhi = NULL,
  hi = "#95001A",
  colorbar = FALSE,
  cb.mar = c(1, 1, 1, 1),
  ...
) {
  c(
    list(
      n = n,
      colors = colors,
      space = space,
      lo = lo,
      lomid = lomid,
      mid = mid,
      midhi = midhi,
      hi = hi,
      colobar = colorbar,
      cb.mar = cb.mar
    ),
    list(...)
  )
} # rtemis::setup.color


#' Set [preprocess] parameters for [train_cv] `.preprocess` argument
#'
#' @inheritParams preprocess
#' @export

setup.preprocess <- function(
  completeCases = FALSE,
  removeCases.thres = NULL,
  removeFeatures.thres = NULL,
  impute = FALSE,
  impute.type = "missRanger",
  impute.missRanger.params = list(pmm.k = 0, maxiter = 10),
  impute.discrete = get_mode,
  impute.numeric = mean,
  integer2factor = FALSE,
  integer2numeric = FALSE,
  logical2factor = FALSE,
  logical2numeric = FALSE,
  numeric2factor = FALSE,
  numeric2factor.levels = NULL,
  numeric.cut.n = 0,
  numeric.cut.labels = FALSE,
  numeric.quant.n = 0,
  character2factor = FALSE,
  scale = FALSE,
  center = FALSE,
  removeConstants = TRUE,
  oneHot = FALSE,
  exclude = NULL
) {
  list(
    completeCases = completeCases,
    removeCases.thres = removeCases.thres,
    removeFeatures.thres = removeFeatures.thres,
    impute = impute,
    impute.type = impute.type,
    impute.missRanger.params = impute.missRanger.params,
    impute.discrete = impute.discrete,
    impute.numeric = impute.numeric,
    integer2factor = integer2factor,
    integer2numeric = integer2numeric,
    logical2factor = logical2factor,
    logical2numeric = logical2numeric,
    numeric2factor = numeric2factor,
    numeric2factor.levels = numeric2factor.levels,
    character2factor = character2factor,
    scale = scale,
    center = center,
    removeConstants = removeConstants,
    oneHot = oneHot,
    exclude = exclude
  )
} # rtemis::setup.preprocess


#' Set decomposition parameters for [train_cv] `.decompose` argument
#'
#' @param decom Character: Name of decomposer to use.
#' @param k Integer: Number of dimensions to project to.
#' @param ... Additional arguments to be passed to decomposer
#'
#' @export

setup.decompose <- function(
  decom = "ICA",
  k = 2,
  ...
) {
  c(list(decom = decom, k = k), list(...))
} # rtemis::setup.decompose

#' Set [earlystop] parameters
#'
#' @inheritParams earlystop
#' @export

setup.earlystop <- function(
  window = 150,
  window_decrease_pct_min = 0.01,
  total_decrease_pct_max = NULL
) {
  list(
    window = window,
    window_decrease_pct_min = window_decrease_pct_min,
    total_decrease_pct_max = total_decrease_pct_max
  )
} # rtemis::setup.earlystop


#' Set [s_LIHAD] parameters
#'
#' @inheritParams s_LIHAD
#' @param ... Additional arguments
#'
#' @export

setup.LIHAD <- function(
  max.depth = 2,
  learning.rate = 1,
  lincoef.params = setup.lincoef("glmnet"),
  alpha = 0,
  lambda = .1,
  minobsinnode = 2,
  minobsinnode.lin = 20,
  ...
) {
  c(
    list(
      max.depth = max.depth,
      learning.rate = learning.rate,
      lincoef.params = lincoef.params,
      alpha = alpha,
      lambda = lambda,
      minobsinnode = minobsinnode,
      minobsinnode.lin = minobsinnode.lin
    ),
    list(...)
  )
} # rtemis::setup.ADDT


#' Set [s_GBM] parameters
#'
#' @inheritParams s_GBM
#' @param ... Additional arguments
#' @export

setup.GBM <- function(
  interaction.depth = 2,
  shrinkage = .001,
  max.trees = 5000,
  min.trees = 100,
  bag.fraction = .9,
  n.minobsinnode = 5,
  grid.resample.params = setup.resample("kfold", 5),
  ifw = TRUE,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  ...
) {
  c(
    list(
      interaction.depth = interaction.depth,
      shrinkage = shrinkage,
      max.trees = max.trees,
      min.trees = min.trees,
      bag.fraction = bag.fraction,
      n.minobsinnode = n.minobsinnode,
      grid.resample.params = grid.resample.params,
      ifw = ifw,
      upsample = upsample,
      downsample = downsample,
      resample.seed = resample.seed
    ),
    list(...)
  )
} # rtemis::setup.GBM


#' Set [s_Ranger] parameters
#'
#' @inheritParams s_Ranger
#' @export

setup.Ranger <- function(
  n.trees = 1000,
  min.node.size = 1,
  mtry = NULL,
  grid.resample.params = setup.resample("kfold", 5),
  ifw = TRUE,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  ...
) {
  c(
    list(
      n.trees = n.trees,
      min.node.size = min.node.size,
      mtry = mtry,
      grid.resample.params = grid.resample.params,
      ifw = ifw,
      upsample = upsample,
      downsample = downsample,
      resample.seed = resample.seed
    ),
    list(...)
  )
} # rtemis::setup.Ranger


# \code{setup.MXN}: Set parameters for \link{s_MXN}
#
# @inheritParams s_MXN
# @export
#
# setup.MXN <- function(n.hidden.nodes = NULL,
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
# } # rtemis::setup.MXN

#' Set [lincoef] parameters
#'
#' @inheritParams lincoef
#' @export

setup.lincoef <- function(
  method = c(
    "glmnet",
    "cv.glmnet",
    "lm.ridge",
    "allSubsets",
    "forwardStepwise",
    "backwardStepwise",
    "glm",
    "sgd",
    "solve"
  ),
  alpha = 0,
  lambda = .01,
  lambda.seq = NULL,
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = c("lambda.min", "lambda.1se"),
  nbest = 1,
  nvmax = 8,
  sgd.model = "glm",
  sgd.model.control = list(
    lambda1 = 0,
    lambda2 = 0
  ),
  sgd.control = list(method = "ai-sgd")
) {
  list(
    method = match.arg(method),
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda,
    nbest = nbest,
    nvmax = nvmax,
    sgd.model = sgd.model,
    sgd.model.control = sgd.model.control,
    sgd.control = sgd.control
  )
} # rtemis::setup.lincoef


#' Set [s_MARS] parameters
#'
#' @inheritParams s_MARS
#' @export

setup.MARS <- function(
  hidden = 1,
  activation = NULL,
  learning.rate = .8,
  momentum = .5,
  learningrate_scale = 1,
  output = NULL,
  numepochs = 100,
  batchsize = NULL,
  hidden_dropout = 0,
  visible_dropout = 0,
  ...
) {
  c(
    list(
      hidden = hidden,
      activation = activation,
      learning.rate = learning.rate,
      momentum = momentum,
      learningrate_scale = learningrate_scale,
      output = output,
      numepochs = numepochs,
      batchsize = batchsize,
      hidden_dropout = hidden_dropout,
      visible_dropout = visible_dropout
    ),
    list(...)
  )
} # rtemis::setup.DN


#' Set [s_LightRuleFit] parameters
#'
#' Sets parameters for the GBM and GLMNET (LASSO) steps of [s_LightRuleFit]
#'
#' @inheritParams s_LightGBM
#' @inheritParams s_GLMNET
#' @inheritParams s_LightRuleFit
#' @param lightgbm.ifw Logical: Passed to [s_LightGBM]'s `ifw` argument
#' @param glmnet.ifw Logical: Passed to [s_GLMNET]'s `ifw` argument
#'
#' @author ED Gennatas
#' @export

setup.LightRuleFit <- function(
  n_trees = 200,
  num_leaves = 32L,
  max_depth = 3,
  learning_rate = .1,
  subsample = .666,
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  objective = NULL,
  extra.lgbm.params = NULL,
  lightgbm.ifw = TRUE,
  lightgbm.resample.params = setup.resample("kfold", 5),
  glmnet.ifw = TRUE,
  importance = FALSE,
  alpha = 1,
  lambda = NULL,
  glmnet.resample.params = setup.resample("kfold", 5)
) {
  list(
    lgbm.params = c(
      list(
        n_trees = n_trees,
        num_leaves = num_leaves,
        max_depth = max_depth,
        learning_rate = learning_rate,
        subsample = subsample,
        subsample_freq = subsample_freq,
        lambda_l1 = lambda_l1,
        lambda_l2 = lambda_l2,
        objective = objective,
        ifw = lightgbm.ifw,
        importance = importance,
        grid.resample.params = lightgbm.resample.params
      ),
      extra.lgbm.params
    ),
    glmnet.params = list(
      ifw = glmnet.ifw,
      alpha = alpha,
      lambda = lambda,
      grid.resample.params = glmnet.resample.params
    )
  )
} # rtemis::rteset.LightRuleFit
