# distillTreeRules.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Distill rules from trained RF and GBM learners
#'
#' Extract rules from RF or GBM model, prune, and remove unnecessary rules using `inTrees`
#'
#' Models must be trained with [s_RF] or [s_GBM]
#'
#' @param mod A trained RF or GBM model
#' @param x The training set features
#' @param y The training set outcomes. If NULL, assumed to be last column of `x`
#' @param n.trees Integer: Number of trees to extract
#' @param maxdepth Integer: Max depth to consider
#' @param maxDecay Float: See `inTree=es::pruneRule`
#' @param typeDecay Integer: See `inTreees::pruneRule`
#' @param verbose Logical: If TRUE, print messages to output
#' @author E.D. Gennatas
#' @export

distillTreeRules <- function(
  mod,
  x,
  y = NULL,
  n.trees = NULL,
  maxdepth = 100,
  maxDecay = 0.05,
  typeDecay = 2,
  verbose = TRUE
) {
  # Dependencies ----
  dependency_check("inTrees")

  # Input ----
  mod.name <- mod$mod.name
  mod <- mod$mod
  if (class(mod)[1] == "rtMod") mod <- mod$mod
  if (is.null(y) && NCOL(x) > 1) {
    y <- x[, ncol(x)]
    x <- x[, (seq_len(ncol(x))) - 1]
  }

  if (is.null(n.trees)) {
    n.trees <- if (mod.name == "RF") mod$ntree else mod$n.trees
  }

  if (is.null(colnames(x))) colnames(x) <- paste0("Feature", seq_len(NCOL(x)))

  if (verbose)
    msg2("Working on", mod.name, "model; looking at", n.trees, "trees")

  # Get Rules ----
  if (mod.name == "RF") {
    trees <- inTrees::RF2List(mod)
  } else {
    trees <- inTrees::GBM2List(mod, X = x)
  }
  if (verbose) msg2("Extracting rules from model...")
  rules <- inTrees::extractRules(
    treeList = trees,
    X = x,
    ntree = n.trees,
    maxdepth = maxdepth,
    random = FALSE
  )
  rules <- unique(rules)
  if (verbose) msg2("Extracting rule metrics...")
  ruleMetrics <- inTrees::getRuleMetric(ruleExec = rules, X = x, target = y)
  if (verbose) msg2("Pruning rules...")
  rules.pruned <- inTrees::pruneRule(
    rules = ruleMetrics,
    X = x,
    target = y,
    maxDecay = maxDecay,
    typeDecay = typeDecay
  )
  if (verbose) msg2("Adding variable names to rules...")
  rules.names <- inTrees::presentRules(rules = rules.pruned, colN = colnames(x))
  if (verbose) msg2("Building simplified learner...")
  rules.distilled <- inTrees::buildLearner(
    ruleMetric = rules.pruned,
    X = x,
    target = y
  )
  if (verbose) msg2("Adding variable names to simplified rules")
  rules.distilled.names <- inTrees::presentRules(
    rules = rules.distilled,
    colN = colnames(x)
  )

  list(
    trees = trees,
    rules = rules,
    ruleMetrics = ruleMetrics,
    rules.pruned = rules.pruned,
    rules.names = rules.names,
    rules.distilled = rules.distilled,
    rules.distilled.names = rules.distilled.names
  )
} # rtemis::distillTreeRules
