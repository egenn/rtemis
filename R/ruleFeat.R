# ruleFeat
# ::rtemis::
# 2017-8 Efstathios D. Gennatas egenn.github.io
# allow running gbm multiple times with depth 1, 3, 5
# x 150, 150, 200

#' ruleFeat [C]
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
#' @param glmnet.select.params Named list: Parameters for \link{s.GLMNET} for the feature
#' selection step
#' @param glmnet.final.params Named list: Parameters for \link{s.GLMNET} for the final model
#' building
#' @return \link{rtMod} object
#' @author Efstathios D. Gennatas
#' @references Friedman JH, Popescu BE, "Predictive Learning via Rule Ensembles",
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#' @export

ruleFeat <- function(x, y = NULL,
                     x.test = NULL, y.test = NULL,
                     n.trees = 500,
                     gbm.params = list(bag.fraction = .5,
                                       shrinkage = .001,
                                       interaction.depth = 5,
                                       ipw = TRUE),
                     glmnet.select.params = list(alpha = 1,
                                                 ipw = TRUE),
                     cases.by.rules = NULL,
                     x.name = NULL,
                     y.name = NULL,
                     question = NULL,
                     verbose = TRUE,
                     n.cores = 1,
                     outdir = NULL,
                     save.mod = if (!is.null(outdir)) TRUE else FALSE) {
  
  # [ INTRO ] ====
  if (missing(x)) {
    print(args(ruleFeat))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  call <- NULL
  mod.name <- "ruleFeat"
  
  # [ DEPENDENCIES ] ====
  if (!depCheck("gbm", "glmnet", "inTrees", "data.table", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }
  
  # [ ARGUMENTS ] ====
  if (is.null(y) & NCOL(x) < 2) {
    print(args(ruleFeat))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  verbose <- verbose | !is.null(logFile)
  
  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test, verbose = FALSE)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  if (type != "Classification") stop("Only classification supported at this point")
  
  if (is.null(cases.by.rules)) {
    # [ Gradient Boosting ] ====
    gbm.args <- c(list(x = x, y = y,
                       force.n.trees = n.trees,
                       verbose = verbose,
                       print.plot = FALSE),
                  gbm.params)
    if (verbose) msg("Running Gradient Boosting...")
    mod.gbm <- do.call(s.GBM3, gbm.args)
    
    # [ Get Rules ] ====
    if (verbose) msg("Collecting Gradient Boosting Rules (Trees)...")
    gbm.list <- rt.GBM2List(mod.gbm$mod, X = x)
    gbm.rules <- inTrees::extractRules(gbm.list, X = x, ntree = n.trees)
    if (verbose) msg("Extracted", length(gbm.rules), "rules...")
    gbm.rules <- unique(gbm.rules)
    n.rules.total <- length(gbm.rules)
    if (verbose) msg("...and kept", n.rules.total, "unique rules")
    gbm.rules.names <- inTrees::presentRules(gbm.rules, colN = names(x))
    colnames(gbm.rules.names) <- "Rule"
    
    # [ Match Cases by Rules ] ====
    cases.by.rules <- matchCasesByRules(x, gbm.rules.names)
  } else {
    mod.gbm <- gbm.rules <- gbm.rules.names <- NA
  }
  
  # [ LASSO: Select Rules ] ====
  if (verbose) msg("Running LASSO on GBM rules...")
  glmnet.select.args <- c(list(x = cases.by.rules, y = y,
                               verbose = verbose,
                               print.plot = FALSE),
                          glmnet.select.params)
  mod.glmnet.select <- do.call(s.GLMNET, glmnet.select.args)
  rule.coefs <- data.matrix(coef(mod.glmnet.select$mod))
  intercept.coef <- rule.coefs[1, , drop = FALSE]
  colnames(intercept.coef) <- "Coefficient"
  rule.coefs <- data.frame(Rule = gbm.rules.names, Coefficient = rule.coefs[-1, 1])
  nonzero.index <- which(abs(rule.coefs$Coefficient) > 0)
  n.nonzero.rules <- length(nonzero.index)
  rules.selected <- gbm.rules.names[nonzero.index]
  rules.selected.file <- paste0(outdir, "rules.selected.csv")
  if (!is.null(outdir)) {
    write.csv(rules.selected, rules.selected.file, row.names = FALSE)
    if (file.exists(paste0(outdir, "rules.selected.csv"))) {
      if (verbose) msg("Selected rules written to", rules.selected.file)
    }
  }
  
  cases.by.rules.selected <- cases.by.rules[, nonzero.index]
  
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
  rules.selected.coef.er <- data.frame(Rule = rules.selected.formatted,
                                       Coefficient = rule.coefs$Coefficient[nonzero.index],
                                       Empirical.Risk = empirical.risk)
  write.csv(rules.selected.coef.er, paste0(outdir, "Rules.selected_Coefs_Empirical.Risk.csv"))
  
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
  # TODO: fitted.prob
  fitted <- mod.glmnet.select$fitted
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)
  
  # [ PREDICTED ] ====
  # TODO: predicted.prob
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(rulefeat.obj, x.test, verbose = verbose)
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted, verbose)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }
  
  # [ OUTRO ] ====
  extra <- list(ruleFeat = rulefeat.obj)
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
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 question = question,
                 extra = extra)
  
  if (save.mod) rtSave(rt, outdir, file.prefix = "ruleFeat", verbose = verbose)
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
    cases.by.rules <- matchCasesByRules(newdata, rules)
  } else {
    cases.by.rules <- object$cases.by.rules.selected
  }
  
  # [ PREDICT ] ====
  yhat <- factor(predict(object$mod.glmnet.select$mod,
                         newx = data.matrix(cases.by.rules),
                         type = "class"),
                 levels = levels(object$y))
  yhat
  
} # rtemis::predict.ruleFeat


rt.GBM2List <- function (gbm1, X) {
  treeList <- list(ntree = gbm1$params$num_trees)
  treeList$list <- vector("list", gbm1$params$num_trees)
  for (i in seq(treeList$ntree)) {
    treeList$list[[i]] <- gbm3::pretty_gbm_tree(gbm1, tree_index = i)
  }
  v2int <- function(v) {
    sum((-v + 1)/2 * 2^seq(0, (length(v) - 1), 1))
  }
  splitBin = sapply(gbm1$c.splits, v2int)
  return(inTrees::formatGBM(treeList, splitBin, X))
}
