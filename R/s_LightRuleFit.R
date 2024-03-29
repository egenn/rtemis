# s_LightRuleFit.R
# ::rtemis::
# 2023 E.D. Gennatas www.lambdamd.org

#' RuleFit with LightGBM (C, R)
#'
#' Train a LightGBM gradient boosting model, extract rules,
#' and fit using LASSO
#'
#' Based on "Predictive Learning via Rule Ensembles"
#' by Friedman and Popescu
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#'
#' @inheritParams s_LightGBM
#' @param params Training parameters for GBM and LASSO steps, set using
#' [setup.LightRuleFit].
#' @param lgbm.mod rtMod object created by [s_LightGBM]. If provided, the gradient
#' boosting step is skipped.
#' @param empirical_risk Logical: If TRUE, calculate empirical risk
#' @param cases_by_rules Matrix of cases by rules from a previoue rulefit run.
#' If provided, the GBM step is skipped.
#' @param save_cases_by_rules Logical: If TRUE, save cases_by_rules to object
#' @param n.cores Integer: Number of cores to use
#' @param trace Integer: Verbosity level
#'
#' @return `rtMod` object
#' @author E.D. Gennatas
#' @references Friedman JH, Popescu BE, "Predictive Learning via Rule Ensembles",
#' http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf
#' @export

s_LightRuleFit <- function(x, y = NULL,
                           x.test = NULL, y.test = NULL,
                           params = setup.LightRuleFit(),
                           lgbm.mod = NULL,
                           empirical_risk = TRUE,
                           cases_by_rules = NULL,
                           save_cases_by_rules = FALSE,
                           x.name = NULL,
                           y.name = NULL,
                           n.cores = rtCores,
                           question = NULL,
                           print.plot = FALSE,
                           plot.fitted = NULL,
                           plot.predicted = NULL,
                           plot.theme = rtTheme,
                           outdir = NULL,
                           save.mod = if (!is.null(outdir)) TRUE else FALSE,
                           verbose = TRUE,
                           trace = 0) {
  # Intro ----
  if (missing(x)) {
    print(args(s_LightRuleFit))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "LightRuleFit"

  # Dependencies ----
  dependency_check("lightgbm", "glmnet", "gsubfn", "data.table")

  # Arguments ----
  if (is.null(y) && NCOL(x) < 2) {
    print(args(s_LightRuleFit))
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
  # if (any(sapply(x, is.factor))) {
  #   factor_index <- names(x)[which(sapply(x, is.factor))]
  #   xp <- preprocess(x,
  #     factor2integer = TRUE,
  #     factor2integer_startat0 = TRUE,
  #     verbose = trace > 0
  #   )
  #   # if (!is.null(x.test)) {
  #   #     x.test <- preprocess(x.test,
  #   #         factor2integer = TRUE,
  #   #         factor2integer_startat0 = TRUE
  #   #     )
  #   # }
  # } else {
  #   factor_index <- NULL
  #   xp <- x
  # }
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  nclasses <- if (type == "Classification") length(levels(y)) else -1

  if (is.null(cases_by_rules)) {
    if (is.null(lgbm.mod)) {
      # No LightGBM model provided
      # LightGBM ----
      lgbm_param <- params$lgbm.params
      lgbm_param$n_trees <- NULL
      lgbm_args <- c(
        list(
          x = x, y = y,
          force_nrounds = params$lgbm.params$n_trees,
          verbose = verbose,
          print.plot = FALSE
        ),
        lgbm_param
      )
      if (verbose) msg2("Running LightGBM...")
      mod_lgbm <- do.call("s_LightGBM", lgbm_args)
    } else {
      # LightGBM model provided
      if (verbose) msg2("Using provided LightGBM model...")
      mod_lgbm <- lgbm.mod
    }

    # Get Rules ----
    if (verbose) msg2start("Extracting LightGBM rules...")
    lgbm_rules <- lgb2rules(
      mod_lgbm$mod,
      n_iter = NULL,
      xnames = names(x),
      factor_levels = dt_get_factor_levels(copy(x))
    )
    if (verbose) msg2done()
    if (verbose) msg2("Extracted", hilite(length(lgbm_rules)), "rules.")
    n_rules_total <- length(lgbm_rules)
    # Match Cases by Rules ----
    cases_by_rules <- matchCasesByRules(x, lgbm_rules, verbose = verbose)
  } else {
    mod_lgbm <- lgbm_rules <- NA
  }

  # Meta: Select Rules ----
  if (verbose) msg2("Running LASSO on GBM rules...")
  glmnet_select_args <- c(
    list(
      x = cases_by_rules, y = y,
      verbose = verbose,
      print.plot = FALSE,
      n.cores = n.cores
    ),
    params$glmnet.params
  )
  mod_glmnet_select <- do.call(s_GLMNET, glmnet_select_args)
  rule_coefs <- data.matrix(coef(mod_glmnet_select$mod))
  intercept_coef <- rule_coefs[1, , drop = FALSE]
  colnames(intercept_coef) <- "Coefficient"
  rule_coefs <- data.frame(Rule = lgbm_rules, Coefficient = rule_coefs[-1, 1])
  nonzero_index <- which(abs(rule_coefs$Coefficient) > 0)
  rules_selected <- lgbm_rules[nonzero_index]
  cases_by_rules_selected <- cases_by_rules[, nonzero_index]
  Ncases_by_rules <- matrixStats::colSums2(cases_by_rules_selected)

  if (!is.null(outdir)) {
    rules_selected_file <- paste0(outdir, "rules_selected.csv")
    write.csv(rules_selected, rules_selected_file, row.names = TRUE)
    if (file.exists(rules_selected_file)) {
      if (verbose) msg2("Selected rules written to", rules_selected_file)
    }
  }

  # Empirical risk ----
  dat <- as.data.table(cbind(x, outcome = y))
  if (empirical_risk && type == "Classification" && nclasses == 2) {
    empirical_risk <- vector("numeric", length(rules_selected))
    for (i in seq_along(rules_selected)) {
      match <- dat[eval(parse(text = rules_selected[i]))]
      freq <- table(match$outcome)
      empirical_risk[i] <- freq[rtenv$binclasspos] / sum(freq)
    }
  } else {
    empirical_risk <- NULL
  }

  # Write CSV ----
  rules_selected_formatted <- gsub(
    "  ", " ",
    formatLightRules(rules_selected, decimal.places = 2)
  )
  # appease R CMD check
  Coefficient <- NULL
  rules_selected_formatted_coef <- data.table(
    Rule_ID = seq(rules_selected_formatted),
    Rule = rules_selected_formatted,
    N_Cases = Ncases_by_rules,
    Coefficient = rule_coefs$Coefficient[nonzero_index]
  )
  if (type == "Classification" && nclasses == 2) {
    # appease R CMD check
    Empirical_Risk <- NULL
    rules_selected_formatted_coef[, Empirical_Risk := empirical_risk]
  }
  setorder(rules_selected_formatted_coef, -Coefficient)
  if (!is.null(outdir)) {
    outname <- if (type == "Classification" && nclasses == 2) {
      "rules_selected_formatted_coef_empiricalRisk.csv"
    } else {
      "rules_selected_formatted_coef.csv"
    }
    write.csv(
      rules_selected_formatted_coef,
      paste0(outdir, outname),
      row.names = FALSE
    )
  }

  # LightRuleFit object ----
  LightRuleFit_obj <- list(
    mod_lgbm = mod_lgbm,
    lgbm_rules = lgbm_rules,
    mod_glmnet_select = mod_glmnet_select,
    rules_selected = rules_selected,
    rules_selected_formatted = rules_selected_formatted,
    rules_selected_formatted_coef = rules_selected_formatted_coef,
    rules_index = nonzero_index,
    rule_coefs = rule_coefs,
    y_levels = if (type == "Classification") levels(y) else NULL,
    metrics = data.frame(
      n_rules_total = n_rules_total,
      n_nonzero_rules = length(nonzero_index)
    )
  )
  if (save_cases_by_rules) {
    LightRuleFit_obj$cases_by_rules_selected <- cases_by_rules_selected
  }
  class(LightRuleFit_obj) <- c("LightRuleFit", "list")

  # Fitted ----
  fitted <- mod_glmnet_select$fitted
  if (type == "Classification") {
    fitted.prob <- mod_glmnet_select$fitted.prob
  } else {
    fitted.prob <- NULL
  }

  error.train <- mod_error(y, fitted, fitted.prob)
  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(LightRuleFit_obj, x.test, verbose = verbose)
    if (type == "Classification") {
      predicted.prob <- predicted$predicted.prob
      predicted <- predicted$predicted
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted, predicted.prob, verbose)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  rt <- rtModSet(
    mod = LightRuleFit_obj,
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
    parameters = params,
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

  outro(start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_LightRuleFit


# predict.LightRuleFit
# ::rtemis::

#' `predict` method for `LightRuleFit` object
#'
#' @param object `LightRuleFit` object
#' @param newdata Feature matrix / data.frame: will be converted to `data.table`
#' @param return.cases.by.rules Logical: If TRUE, return cases by rules matrix
#' @param verbose Logical: If TRUE, print messages during execution. Default = TRUE
#' @param ... Ignored
#' @return Vector of estimated values
#' @export

predict.LightRuleFit <- function(object,
                                 newdata = NULL,
                                 return.cases.by.rules = FALSE,
                                 verbose = TRUE, ...) {
  # Rules ----
  # Get all rules, some have 0 coefficients
  rules <- object$lgbm_rules

  # Match ----
  # Match newdata to rules: create features for predict
  if (!is.null(newdata)) {
    cases_by_rules <- matchCasesByRules(newdata, rules, verbose = verbose)
  } else {
    if (verbose) msg2("Using stored cases_by_rules_selected")
    cases_by_rules <- if (!is.null(object$cases_by_rules_selected)) {
      object$cases_by_rules_selected
    } else {
      matchCasesByRules(newdata, rules, verbose = verbose)
    }
  }

  # Predict ----
  datm <- data.matrix(cases_by_rules)
  if (object$mod_lgbm$type == "Classification") {
    prob <- predict(object$mod_glmnet_select$mod,
      newx = datm,
      type = "response"
    )[, 1]

    yhat <- factor(
      predict(object$mod_glmnet_select$mod,
        newx = datm,
        type = "class"
      ),
      levels = object$y_levels
    )
  } else {
    prob <- NULL
    yhat <- as.numeric(predict(object$mod_glmnet_select$mod,
      newx = datm
    ))
  }
  if (return.cases.by.rules) {
    if (is.null(prob)) {
      return(list(
        predicted = yhat,
        cases.by.rules = cases_by_rules
      ))
    } else {
      return(list(
        predicted.prob = prob,
        predicted = yhat,
        cases.by.rules = cases_by_rules
      ))
    }
  } else {
    if (is.null(prob)) {
      return(yhat)
    } else {
      return(list(
        predicted.prob = prob,
        predicted = yhat
      ))
    }
  }
} # rtemis::predict.LightRuleFit
