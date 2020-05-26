# s.RULEFEAT.R
# ::rtemis::
# 2017-8 Efstathios D. Gennatas egenn.github.io
# TODO: Add option to include raw features as well as rules

#' ruleFeat [C, R]
#'
#' Train a gradient boosting model, extract rules,
#' and fit using LASSO
#'
#' Based on "Predictive Learning via Rule Ensembles"
#' by Friedman and Popescu
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#'
#' @inheritParams s.GBM
#' @param gbm.params Named list: Parameters for \link{s.GBM}
#' @param meta.alpha Float [0, 1]: \code{alpha} for \link{s.GLMNET}, Default = 1
#' @param meta.lambda Float: \code{lambda} for \link{s.GLMNET}. Default = NULL (will be determined automatically
#' by crossvalidation)
#' @param meta.extra.params Named list: Parameters for \link{s.GLMNET} for the feature
#' selection step
#' @param cases.by.rules Matrix of cases by rules from a previoue rulefeat run. If provided,
#' the GBM step is skipped. Default = NULL
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @references Friedman JH, Popescu BE, "Predictive Learning via Rule Ensembles",
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#' @export

s.RULEFEAT <- function(x, y = NULL,
                       x.test = NULL, y.test = NULL,
                       n.trees = 100,
                       gbm.params = list(bag.fraction = .5,
                                         shrinkage = .001,
                                         interaction.depth = 5,
                                         ipw = TRUE),
                       meta.alpha = 1,
                       meta.lambda = NULL,
                       meta.extra.params = list(ipw = TRUE),
                       cases.by.rules = NULL,
                       x.name = NULL,
                       y.name = NULL,
                       question = NULL,
                       verbose = TRUE,
                       n.cores = rtCores,
                       which.gbm = c("gbm", "gbm3"),
                       print.plot = TRUE,
                       plot.fitted = NULL,
                       plot.predicted = NULL,
                       plot.theme = getOption("rt.fit.theme", "lightgrid"),
                       outdir = NULL,
                       save.mod = if (!is.null(outdir)) TRUE else FALSE) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.RULEFEAT))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "RULEFEAT"
  which.gbm <- match.arg(which.gbm)
  .gbm <- ifelse(which.gbm == "gbm", "s.GBM", "s.GBM3")

  # [ DEPENDENCIES ] ====
  if (!depCheck(which.gbm, "glmnet", "gsubfn", "inTrees", "data.table", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(s.RULEFEAT))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  verbose <- verbose | !is.null(logFile)

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test,
                    verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  if (is.null(cases.by.rules)) {
    # [ Gradient Boosting ] ====
    gbm.args <- c(list(x = x, y = y,
                       force.n.trees = n.trees,
                       verbose = verbose,
                       print.plot = FALSE,
                       n.cores = n.cores),
                  gbm.params)
    if (verbose) msg("Running Gradient Boosting...")
    mod.gbm <- do.call(.gbm, gbm.args)

    # [ Get Rules ] ====
    if (verbose) msg("Collecting Gradient Boosting Rules (Trees)...")
    gbm.list <- rt.GBM2List(mod.gbm$mod, X = x, which.gbm = which.gbm)
    gbm.rules <- inTrees::extractRules(gbm.list, X = x, ntree = n.trees,
                                       maxdepth = gbm.params$interaction.depth)
    if (verbose) msg("Extracted", length(gbm.rules), "rules...")
    gbm.rules <- unique(gbm.rules)
    n.rules.total <- length(gbm.rules)
    if (verbose) msg("...and kept", n.rules.total, "unique rules")
    gbm.rules.names <- inTrees::presentRules(gbm.rules, colN = names(x))
    colnames(gbm.rules.names) <- "Rule"

    # [ Match Cases by Rules ] ====
    cases.by.rules <- matchCasesByRules(x, gbm.rules.names, verbose = verbose)
  } else {
    mod.gbm <- gbm.rules <- gbm.rules.names <- NA
  }

  # [ META: Select Rules ] ====
  if (verbose) msg("Running LASSO on GBM rules...")
  glmnet.select.args <- c(list(x = cases.by.rules, y = y,
                               alpha = meta.alpha,
                               lambda = meta.lambda,
                               verbose = verbose,
                               print.plot = FALSE,
                               n.cores = n.cores),
                          meta.extra.params)
  mod.glmnet.select <- do.call(s.GLMNET, glmnet.select.args)
  rule.coefs <- data.matrix(coef(mod.glmnet.select$mod))
  intercept.coef <- rule.coefs[1, , drop = FALSE]
  colnames(intercept.coef) <- "Coefficient"
  rule.coefs <- data.frame(Rule = gbm.rules.names, Coefficient = rule.coefs[-1, 1])
  nonzero.index <- which(abs(rule.coefs$Coefficient) > 0)
  n.nonzero.rules <- length(nonzero.index)
  rules.selected <- gbm.rules.names[nonzero.index]
  cases.by.rules.selected <- cases.by.rules[, nonzero.index]
  Ncases.by.rules <- matrixStats::colSums2(cases.by.rules.selected)

  if (!is.null(outdir)) {
    rules.selected.file <- paste0(outdir, "rules.selected.csv")
    write.csv(rules.selected, rules.selected.file, row.names = TRUE)
    if (file.exists(rules.selected.file)) {
      if (verbose) msg("Selected rules written to", rules.selected.file)
    }
  }

  # [ EMPIRICAL RISK ] ====
  dat <- as.data.table(cbind(x, outcome = y))
  empirical.risk <- vector("numeric", length(rules.selected))
  for (i in seq(rules.selected)) {
    match <- dat[eval(parse(text = rules.selected[i]))]
    freq <- table(match$outcome)
    empirical.risk[i] <- freq[1]/sum(freq)
  }

  # [ WRITE CSV ] ====
  rules.selected.formatted <- formatRules(rules.selected, decimal.places = 2)
  rules.selected.coef.er <- data.frame(Rule_ID = seq(rules.selected.formatted),
                                       Rule = rules.selected.formatted,
                                       N_Cases = Ncases.by.rules,
                                       Coefficient = rule.coefs$Coefficient[nonzero.index],
                                       Empirical_Risk = empirical.risk)
  if (!is.null(outdir)) {
    write.csv(rules.selected.coef.er,
              paste0(outdir, "Rules.selected_Coefs_Empirical.Risk.csv"),
              row.names = FALSE)
  }

  # [ ruleFeat object ] ====
  rulefeat.obj <- list(mod.gbm = mod.gbm,
                       gbm.rules = gbm.rules,
                       gbm.rules.names = gbm.rules.names,
                       mod.glmnet.select = mod.glmnet.select,
                       rules.selected = rules.selected,
                       rules.selected.formatted = rules.selected.formatted,
                       empirical.risk = empirical.risk,
                       rules.selected.coef.er = rules.selected.coef.er,
                       rules.index = nonzero.index,
                       cases.by.rules = cases.by.rules,
                       cases.by.rules.selected = cases.by.rules.selected,
                       rule.coefs = rule.coefs,
                       x = x,
                       y = y,
                       metrics = data.frame(N.Rules.Total = n.rules.total,
                                            N.Nonzero.Rules = n.nonzero.rules))
  class(rulefeat.obj) <- c("ruleFeat", "list")

  # [ FITTED ] ====
  fitted <- mod.glmnet.select$fitted
  if (type == "Classification") {
    fitted.prob <- mod.glmnet.select$fitted.prob
  } else {
    fitted.prob <- NULL
  }

  error.train <- modError(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(rulefeat.obj, x.test, verbose = verbose)
    if (type == "Classification") {
      predicted.prob <- predicted$prob
      predicted <- predicted$estimate
    }
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, predicted.prob, verbose)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  rt <- rtModSet(rtclass = "rtMod",
                 mod = rulefeat.obj,
                 mod.name = mod.name,
                 type = type,
                 call = call,
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
                 parameters = list(gbm.params = gbm.params,
                                   meta.alpha = meta.alpha,
                                   meta.lambda = meta.lambda,
                                   meta.extra.params = meta.extra.params),
                 question = question)

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

} # rtemis::s.RULEFEAT


# predict.ruleFeat
# ::rtemis::

#' \code{predict} method for \code{ruleFeat} object
#'
#' @param object \code{ruleFeat} object
#' @param newdata Feature matrix / data.frame: will be converted to \code{data.table}
#' @param verbose Logical: If TRUE, print messages during execution. Default = TRUE
#' @param ... Ignored
#' @return Vector of estimated values
#' @export

predict.ruleFeat <- function(object, newdata = NULL,
                             verbose = TRUE, ...) {

  # [ RULES ] ====
  # Get rules.selected from object
  rules <- object$gbm.rules.names

  # [ MATCH ] ====
  # Match newdata to rules: create features for predict
  if (!is.null(newdata)) {
    if (verbose) msg("Matching newdata to rules...")
    cases.by.rules <- matchCasesByRules(newdata, rules, verbose = verbose)
  } else {
    cases.by.rules <- object$cases.by.rules.selected
  }

  # [ PREDICT ] ====
  if (object$mod.gbm$type == "Classification") {

    prob <- predict(object$mod.glmnet.select$mod,
                    newx = data.matrix(cases.by.rules),
                    type = "response")[, 1]

    yhat <- factor(predict(object$mod.glmnet.select$mod,
                           newx = data.matrix(cases.by.rules),
                           type = "class"),
                   levels = levels(object$y))
  } else {
    prob <- NULL
    yhat <- as.numeric(predict(object$mod.glmnet.select$mod,
                               newx = data.matrix(cases.by.rules)))
  }

  if (is.null(prob)) {
    return(yhat)
  } else {
    return(list(prob = prob, estimate = yhat))
  }

} # rtemis::predict.ruleFeat


rt.GBM2List <- function(gbm1, X, which.gbm = "gbm") {

  treeList <- if (which.gbm == "gbm") list(ntree = gbm1$n.trees) else list(ntree = gbm1$params$num_trees)
  treeList$list <- vector("list", treeList$ntree)
  for (i in seq(treeList$ntree)) {
    if (which.gbm == "gbm") {
      treeList$list[[i]] <- gbm::pretty.gbm.tree(gbm1, i.tree = i)
    } else {
      treeList$list[[i]] <- gbm3::pretty_gbm_tree(gbm1, tree_index = i)
    }
  }
  v2int <- function(v) {
    sum((-v + 1)/2 * 2^seq(0, (length(v) - 1), 1))
  }
  splitBin <- sapply(gbm1$c.splits, v2int)
  return(inTrees::formatGBM(treeList, splitBin, X))
} # rtemis::rt.GBM2List
