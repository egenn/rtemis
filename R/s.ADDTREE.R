# s.ADDTREE.R
# ::rtemis::
# 2017 E.D. Gennatas lambdamd.org
# TODO: check if factor outcome with string levels works & with addtree_path_to_rules

#' Additive Tree: Tree-Structured Boosting [C]
#'
#' Train an Additive Tree model
#'
#' This function is for binary classification. The outcome must be a factor with two levels, the first level
#' is the 'positive' class. Ensure there are no missing values in the data and that variables are either numeric
#' (including integers) or factors. Use \link{preprocess} as needed to impute and convert characters to factors.
#'
#' Factor levels should not contain the "/" character (it is used to separate conditions
#' in the addtree object)
#'
#' [gS] Indicates that more than one value can be supplied, which will result in grid search using
#' internal resampling
#' lambda = gamma/(1 - gamma)
#' @inheritParams s.GLM
#' @param update Character: "exponential" or "polynomial". Type of weight update. Default = "exponential"
#' @param min.update Float: Minimum update for gradient step
#' @param min.hessian [gS] Float: Minimum second derivative to continue splitting. Default = .001
#' @param min.membership Integer: Minimum number of cases in a node. Default = 1
#' @param steps.past.min.membership Integer: N steps to make past \code{min.membership} - For testing. Default = 0
#' @param gamma [gS] Float: acceleration factor = lambda/(1 + lambda). Default = .8
#' @param max.depth [gS] Integer: maximum depth of the tree. Default = 30
#' @param learning.rate [gS] learning rate for the Newton Raphson step that updates the function values
#' of the node
#' @param imetrics Logical: If TRUE, save interpretability metrics, i.e. N total nodes in tree and depth, in output. Default = TRUE
#' @param rpart.params List: \code{rpart} parameters, passed to \code{rpart::rpart("parms")}
#' @param match.rules Logical: If TRUE, match cases to rules to get statistics per node, i.e. what
#' percent of cases match each rule. If available, these are used by \link{dplot3.addtree} when plotting. Default = TRUE
#' @return Object of class \link{rtMod}
#' @author E.D. Gennatas
#' @family Supervised Learning
#' @family Tree-based methods
#' @family Interpretable models
#' @references
#' Jose Marcio Luna, Efstathios D Gennatas, Lyle H Ungar, Eric Eaton, Eric S Diffenderfer, Shane T Jensen,
#' Charles B Simone, Jerome H Friedman, Timothy D Solberg, Gilmer Valdes
#' Building more accurate decision trees with the additive tree
#' Proc Natl Acad Sci U S A. 2019 Oct 1;116(40):19887-19893. doi: 10.1073/pnas.1816748116
#' @export

s.ADDTREE <- function(x, y = NULL,
                      x.test = NULL, y.test = NULL,
                      x.name = NULL, y.name = NULL,
                      weights = NULL,
                      update = c("exponential", "polynomial"),
                      min.update = ifelse(update == "polynomial", .035, 1000),
                      min.hessian = .001,
                      min.membership = 1,
                      steps.past.min.membership = 0,
                      gamma = .8,
                      max.depth = 30,
                      learning.rate = .1,
                      ipw = TRUE,
                      ipw.type = 2,
                      upsample = FALSE,
                      downsample = FALSE,
                      resample.seed = NULL,
                      imetrics = TRUE,
                      grid.resample.rtset = rtset.resample("kfold", 5),
                      metric = "Balanced Accuracy",
                      maximize = TRUE,
                      rpart.params = NULL,
                      match.rules = TRUE,
                      print.plot = TRUE,
                      plot.fitted = NULL,
                      plot.predicted = NULL,
                      plot.theme = getOption("rt.fit.theme", "lightgrid"),
                      question = NULL,
                      rtclass = NULL,
                      verbose = TRUE,
                      prune.verbose = FALSE,
                      trace = 1,
                      grid.verbose = TRUE,
                      outdir = NULL,
                      save.rpart = FALSE,
                      save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
                      n.cores = rtCores, ...) {

  # Intro ====
  if (missing(x)) {
    print(args(s.ADDTREE))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "ADDTREE"

  # Dependencies ====
  if (!depCheck("rpart", "data.tree", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # Arguments ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.ADDTREE))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  update <- match.arg(update)
  # if (update == "exponential") {
  #   if (!(0 <= min(gamma) & max(gamma) <= 1)) stop("gamma must be between 0 and 1")
  # }
  if (!verbose) prune.verbose <- FALSE

  # Data ====
  dt <- dataPrepare(x, y,
                    x.test, y.test,
                    ipw = ipw,
                    ipw.type = ipw.type,
                    upsample = upsample,
                    downsample = downsample,
                    resample.seed = resample.seed,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  .weights <- if (is.null(weights) & ipw) dt$weights else weights
  x0 <- if (upsample | downsample) dt$x0 else x
  y0 <- if (upsample | downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (dt$type != "Classification") stop("Only binary classification is currently supported")
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Grid Search ====
  if (gridCheck(gamma, max.depth, learning.rate)) {
    gs <- gridSearchLearn(x0, y0,
                          mod.name,
                          resample.rtset = grid.resample.rtset,
                          grid.params = list(gamma = gamma,
                                             max.depth = max.depth,
                                             learning.rate = learning.rate,
                                             min.hessian = min.hessian),
                          fixed.params = list(catPredictors = NULL,
                                              ipw = ipw,
                                              ipw.type = ipw.type,
                                              upsample = upsample,
                                              resample.seed = resample.seed),
                          weights = weights,
                          metric = metric,
                          maximize = maximize,
                          verbose = verbose,
                          grid.verbose = grid.verbose,
                          n.cores = n.cores)
    gamma <- gs$best.tune$gamma
    max.depth <- gs$best.tune$max.depth
    learning.rate <- gs$best.tune$learning.rate
    min.hessian <- gs$best.tune$min.hessian
  } else {
    gs <- NULL
  }
  parameters <- list(gamma = gamma,
                     max.depth = max.depth,
                     learning.rate = learning.rate,
                     min.hessian = min.hessian,
                     ipw = ipw,
                     ipw.type = ipw.type,
                     upsample = upsample,
                     resample.seed = resample.seed)

  # addtree ====
  if (verbose) msg("Training ADDTREE...", newline.pre = TRUE)
  mod <- addtree(x, y,
                 catPredictors = NULL,
                 depthLimit = max.depth,
                 learningRate = learning.rate,
                 gamma = gamma,
                 update = update,
                 min.update = min.update,
                 min.hessian = min.hessian,
                 min.membership = min.membership,
                 steps.past.min.membership = steps.past.min.membership,
                 weights = .weights,
                 rpart.params = rpart.params,
                 save.rpart = save.rpart,
                 verbose = verbose,
                 trace = trace)

  # Fitted ====
  fitted <- predict(mod, x)
  error.train <- try(modError(y, fitted))
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ====
  if (!is.null(x.test)) {
    predicted <- predict(mod, x.test)
    if (!is.null(y.test)) {
      error.test <- try(modError(y.test, predicted))
      if (verbose) errorSummary(error.test, mod.name)
    } else {
      error.test <- NULL
    }
  } else {
    predicted <- error.test <- NULL
  }

  # Outro ====
  extra <- list(gridSearch = gs)
  rt <- rtModSet(rtclass = rtclass,
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 parameters = parameters,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 question = question,
                 extra = extra)

  # data.tree ====
  if (verbose) msg("Traversing tree by preorder...")
  rt$mod$frame <- preorderTree.addtree(rt, x)
  if (verbose) msg("Converting paths to rules...")
  rt$mod$frame$Rule <- addtree_path_to_rules(rt$mod$frame$Path)
  if (verbose) msg("Converting to data.tree object...")
  rt$mod$addtree <- data.tree::as.Node(rt$mod$frame, pathName = "Path")

  # Prune ====
  prune <- prune.empty.leaves <- remove.bad.parents <- TRUE
  if (prune) {
    if (verbose) msg("Pruning tree...")
    rt$mod$addtree.pruned <- prune.addtree(rt$mod$addtree,
                                           prune.empty.leaves = prune.empty.leaves,
                                           remove.bad.parents = remove.bad.parents,
                                           verbose = prune.verbose)
    rt$mod$rules <- data.tree::Get(data.tree::Traverse(rt$mod$addtree.pruned,
                                                       filterFun = function(node) node$isLeaf), "Rule")
    names(rt$mod$rules) <- NULL
  }

  if (match.rules) {
    rules <- data.tree::Get(data.tree::Traverse(rt$mod$addtree.pruned), "Rule")
    xt <- data.table::as.data.table(cbind(x, y))
    n.match <- n.pos <- vector("integer", length(rules))
    n.match[1] <- NROW(x)
    n.pos[1] <- table(y)[1]
    for (i in 2:length(rules)) {
      match <- xt[eval(parse(text = rules[i]))]
      n.match[i] <- NROW(match)
      n.pos[i] <- table(match$y)[1]
    }
    rt$extra$node.stats <- data.frame(n.match = n.match,
                                      pct.total = n.match / NROW(x),
                                      pct.pos = n.pos / n.match)
    rt$mod$addtree.pruned$Set(n.match = rt$extra$node.stats$n.match)
    rt$mod$addtree.pruned$Set(pct.total = rt$extra$node.stats$pct.total)
    rt$mod$addtree.pruned$Set(pct.pos = rt$extra$node.stats$pct.pos)
  }

  # imetrics ====
  rt$extra$imetrics <- list(n.nodes = rt$mod$addtree.pruned$totalCount - 1,
                            depth = rt$mod$addtree.pruned$height - 1)

  rtMod.out(rt,
            print.plot,
            plot.fitted,
            plot.predicted,
            y.test,
            mod.name,
            outdir,
            save.mod,
            verbose,
            plot.theme)

  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s.ADDTREE
