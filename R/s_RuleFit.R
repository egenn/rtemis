# s_RuleFit.R
# ::rtemis::
# 2017-23 E.D. Gennatas rtemis.org

#' Rulefit \[C, R\]
#'
#' Train a gradient boosting model, extract rules,
#' and fit using LASSO
#'
#' Based on "Predictive Learning via Rule Ensembles"
#' by Friedman and Popescu
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#'
#' @inheritParams s_GBM
#' @param gbm.params List of named lists: A list, each element of which is a named list
#' of parameters for [s_GBM]. i.e. If you want to train a single GBM model, this could
#' be:
#' `gbm.params = list(
#'                    list(
#'                      n.trees = 300,
#'                      bag.fraction = 1,
#'                      shrinkage = .1,
#'                      interaction.depth = 3,
#'                      ifw = TRUE
#'                    )
#'                  )`
#' if you wanted to train 2 GBM models, this could be:
#' `gbm.params = list(
#'                    list(
#'                      n.trees = 300,
#'                      bag.fraction = 1,
#'                      shrinkage = .1,
#'                      interaction.depth = 3,
#'                      ifw = TRUE
#'                   ),
#'                    list(
#'                         n.trees = 500,
#'                         bag.fraction = 1,
#'                         shrinkage = .1,
#'                         interaction.depth = 3,
#'                         ifw = TRUE
#'                         )
#'                    )`
#' @param meta.alpha Float \[0, 1\]: `alpha` for [s_GLMNET]
#' @param meta.lambda Float: `lambda` for [s_GLMNET]. Default = NULL (will be
#' determined automatically by crossvalidation)
#' @param meta.extra.params Named list: Parameters for [s_GLMNET] for the
#' feature selection step
#' @param cases.by.rules Matrix of cases by rules from a previoue rulefit run.
#' If provided, the GBM step is skipped. Default = NULL
#' @param n.cores Integer: Number of cores to use
# @param which.gbm Character: "gbm" or "gbm3"
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @references Friedman JH, Popescu BE, "Predictive Learning via Rule Ensembles",
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#' @export

s_RuleFit <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  gbm.params = list(
    list(
      n.trees = 300,
      bag.fraction = 1,
      shrinkage = .1,
      interaction.depth = 3,
      ifw = TRUE
    )
  ),
  meta.alpha = 1,
  meta.lambda = NULL,
  meta.extra.params = list(ifw = TRUE),
  cases.by.rules = NULL,
  x.name = NULL,
  y.name = NULL,
  n.cores = rtCores,
  #   which.gbm = c("gbm", "gbm3"),
  question = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  outdir = NULL,
  save.mod = if (!is.null(outdir)) TRUE else FALSE,
  verbose = TRUE
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_RuleFit))
    return(invisible(9))
  }
  which.gbm <- "gbm"
  if (!is.null(outdir)) {
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  }
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "RuleFit"
  # which.gbm <- match.arg(which.gbm)
  # .gbm <- ifelse(which.gbm == "gbm", "s_GBM", "s_GBM3")
  .gbm <- "s_GBM"

  # Dependencies ----
  dependency_check(which.gbm, "glmnet", "gsubfn", "inTrees", "data.table")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_RuleFit))
    stop("y is missing")
  }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  verbose <- verbose | !is.null(logFile)

  # Data ----
  dt <- prepare_data(x, y, x.test, y.test, verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  if (is.null(cases.by.rules)) {
    # Gradient Boosting ----
    # gbm.args <- c(
    #   list(
    #     x = x, y = y,
    #     force.n.trees = n.trees,
    #     verbose = verbose,
    #     print.plot = FALSE,
    #     n.cores = n.cores,
    #     relInf = FALSE
    #   ),
    #   gbm.params
    # )
    n_gbms <- length(gbm.params)
    # if (verbose) msg2("Running Gradient Boosting...")
    if (verbose) {
      msg20("Running ", singorplu(n_gbms, "Gradient Boosting Model"), "...")
    }

    # mod.gbm <- do.call(.gbm, gbm.args)
    # Allow running multiple GBM models for rule extraction
    gbm.args <- lapply(seq_along(gbm.params), function(i) {
      c(
        list(
          x = x,
          y = y,
          force.n.trees = gbm.params[[i]]$n.trees,
          verbose = verbose,
          print.plot = FALSE,
          n.cores = n.cores,
          relInf = FALSE
        ),
        gbm.params[[i]]
      )
    })
    mod.gbm <- lapply(seq_along(gbm.params), function(i) {
      do.call(.gbm, gbm.args[[i]])
    })
    # Get Rules ----
    if (verbose) msg2("Collecting Gradient Boosting Rules (Trees)...")
    gbm.list <- lapply(seq_along(mod.gbm), function(i) {
      rt.GBM2List(mod.gbm[[i]]$mod, x, which.gbm)
    })

    # gbm.rules <- inTrees::extractRules(gbm.list,
    #   X = x, ntree = n.trees,
    #   maxdepth = gbm.params$interaction.depth
    # )
    # Can also extract rules of different depth by chaning maxdepth here
    gbm.rules <- do.call(
      rbind,
      lapply(seq_along(mod.gbm), function(i) {
        inTrees::extractRules(
          gbm.list[[i]],
          X = x,
          ntree = gbm.params[[i]]$n.trees,
          maxdepth = gbm.params[[i]]$interaction.depth
        )
      })
    )
    if (verbose) msg2("Extracted", nrow(gbm.rules), "rules...")
    gbm.rules <- unique(gbm.rules)
    n.rules.total <- length(gbm.rules)
    if (verbose) msg2("...and kept", n.rules.total, "unique rules")
    gbm.rules.names <- inTrees::presentRules(gbm.rules, colN = names(x))
    colnames(gbm.rules.names) <- "Rule"

    # Match Cases by Rules ----
    cases.by.rules <- matchCasesByRules(x, gbm.rules.names, verbose = verbose)
  } else {
    mod.gbm <- gbm.rules <- gbm.rules.names <- NA
  }

  # Meta: Select Rules ----
  if (verbose) msg2("Running LASSO on GBM rules...")
  glmnet.select.args <- c(
    list(
      x = cases.by.rules,
      y = y,
      alpha = meta.alpha,
      lambda = meta.lambda,
      verbose = verbose,
      print.plot = FALSE,
      n.cores = n.cores
    ),
    meta.extra.params
  )
  mod.glmnet.select <- do.call(s_GLMNET, glmnet.select.args)
  rule.coefs <- data.matrix(coef(mod.glmnet.select$mod))
  intercept.coef <- rule.coefs[1, , drop = FALSE]
  colnames(intercept.coef) <- "Coefficient"
  rule.coefs <- data.frame(
    Rule = gbm.rules.names,
    Coefficient = rule.coefs[-1, 1]
  )
  nonzero.index <- which(abs(rule.coefs$Coefficient) > 0)
  n.nonzero.rules <- length(nonzero.index)
  rules.selected <- gbm.rules.names[nonzero.index]
  cases.by.rules.selected <- cases.by.rules[, nonzero.index]
  Ncases.by.rules <- matrixStats::colSums2(cases.by.rules.selected)

  if (!is.null(outdir)) {
    rules.selected.file <- paste0(outdir, "rules.selected.csv")
    write.csv(rules.selected, rules.selected.file, row.names = TRUE)
    if (file.exists(rules.selected.file)) {
      if (verbose) msg2("Selected rules written to", rules.selected.file)
    }
  }

  # Empirical risk ----
  dat <- as.data.table(cbind(x, outcome = y))
  empirical.risk <- vector("numeric", length(rules.selected))
  for (i in seq_along(rules.selected)) {
    match <- dat[eval(parse(text = rules.selected[i]))]
    freq <- table(match$outcome)
    empirical.risk[i] <- freq[1] / sum(freq)
  }

  # Write CSV ----
  rules.selected.formatted <- formatRules(rules.selected, decimal.places = 2)
  rules.selected.coef.er <- data.frame(
    Rule_ID = seq(rules.selected.formatted),
    Rule = rules.selected.formatted,
    N_Cases = Ncases.by.rules,
    Coefficient = rule.coefs$Coefficient[nonzero.index],
    Empirical_Risk = empirical.risk
  )
  if (!is.null(outdir)) {
    write.csv(
      rules.selected.coef.er,
      paste0(outdir, "Rules.selected_Coefs_Empirical.Risk.csv"),
      row.names = FALSE
    )
  }

  # rulefit object ----
  rulefit.obj <- list(
    mod.gbm = mod.gbm,
    gbm.rules = gbm.rules,
    gbm.rules.names = gbm.rules.names,
    mod.glmnet.select = mod.glmnet.select,
    rules.selected = rules.selected,
    rules.selected.formatted = rules.selected.formatted,
    empirical.risk = empirical.risk,
    rules.selected.coef.er = rules.selected.coef.er,
    rules.index = nonzero.index,
    # cases.by.rules = cases.by.rules,
    # cases.by.rules.selected = cases.by.rules.selected,
    rule.coefs = rule.coefs,
    y_levels = if (type == "Classification") levels(y) else NULL,
    metrics = data.frame(
      N.Rules.Total = n.rules.total,
      N.Nonzero.Rules = n.nonzero.rules
    )
  )
  class(rulefit.obj) <- c("rulefit", "list")

  # Fitted ----
  fitted <- mod.glmnet.select$fitted
  if (type == "Classification") {
    fitted.prob <- mod.glmnet.select$fitted.prob
  } else {
    fitted.prob <- NULL
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(rulefit.obj, x.test, verbose = verbose)
    if (type == "Classification") {
      predicted.prob <- predicted$prob
      predicted <- predicted$estimate
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = rulefit.obj,
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
    parameters = list(
      gbm.params = gbm.params,
      meta.alpha = meta.alpha,
      meta.lambda = meta.lambda,
      meta.extra.params = meta.extra.params
    ),
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_RuleFit


# predict.rulefit
# ::rtemis::

#' `predict` method for `rulefit` object
#'
#' @param object `rulefit` object
#' @param newdata Feature matrix / data.frame: will be converted to `data.table`
#' @param verbose Logical: If TRUE, print messages during execution. Default = TRUE
#' @param ... Ignored
#' @return Vector of estimated values
#' @export

predict.rulefit <- function(object, newdata = NULL, verbose = TRUE, ...) {
  # Rules ----
  # Get all rules, some have 0 coefficients
  rules <- object$gbm.rules.names

  # Match ----
  # Match newdata to rules: create features for predict
  # if (!is.null(newdata)) {
  #   if (verbose) msg2("Matching newdata to rules...")
  #   cases.by.rules <- matchCasesByRules(newdata, rules, verbose = verbose)
  # } else {
  #   cases.by.rules <- object$cases.by.rules.selected
  # }
  if (verbose) msg2("Matching cases to rules...")
  cases.by.rules <- matchCasesByRules(newdata, rules, verbose = verbose)

  # Predict ----
  if (object$mod.gbm[[1]]$type == "Classification") {
    prob <- predict(
      object$mod.glmnet.select$mod,
      newx = data.matrix(cases.by.rules),
      type = "response"
    )[, 1]

    yhat <- factor(
      predict(
        object$mod.glmnet.select$mod,
        newx = data.matrix(cases.by.rules),
        type = "class"
      ),
      levels = object$y_levels
    )
  } else {
    prob <- NULL
    yhat <- as.numeric(predict(
      object$mod.glmnet.select$mod,
      newx = data.matrix(cases.by.rules)
    ))
  }

  if (is.null(prob)) {
    return(yhat)
  } else {
    return(list(prob = prob, estimate = yhat))
  }
} # rtemis::predict.rulefit


rt.GBM2List <- function(gbm1, X, which.gbm = "gbm") {
  treeList <- if (which.gbm == "gbm") list(ntree = gbm1$n.trees) else
    list(ntree = gbm1$params$num_trees)
  treeList$list <- vector("list", treeList$ntree)
  for (i in seq(treeList$ntree)) {
    if (which.gbm == "gbm") {
      treeList$list[[i]] <- gbm::pretty.gbm.tree(gbm1, i.tree = i)
    } else {
      # treeList$list[[i]] <- gbm3::pretty_gbm_tree(gbm1, tree_index = i)
    }
  }
  v2int <- function(v) {
    sum((-v + 1) / 2 * 2^seq(0, (length(v) - 1), 1))
  }
  splitBin <- sapply(gbm1$c.splits, v2int)
  return(inTrees::formatGBM(treeList, splitBin, X))
} # rtemis::rt.GBM2List
