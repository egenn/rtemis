# select_learn.R
# ::rtemis::
# 2016-23 E.D. Gennatas rtemis.org

# Name + CRS ----
rtAlgs <- data.frame(rbind(
  c("TREELINED", "Lined Tree Learner", FALSE, TRUE, FALSE),
  c("RGB", "Representational Gradient Boosting", TRUE, TRUE, TRUE),
  c("AADDT", "Asymmetric Additive Tree", TRUE, TRUE, FALSE),
  c("CSL", "Conditional SuperLearner", TRUE, TRUE, FALSE),
  c("HAL", "Highly Adaptive LASSO", TRUE, TRUE, TRUE),
  c("LINAD", "Linear Additive Tree", TRUE, TRUE, FALSE),
  c("LINOA", "Linear Optimized Additive Tree", TRUE, TRUE, FALSE),
  c("LITEBOOSTTV", "Lite Boosted Learner", FALSE, TRUE, FALSE),
  c("AdaBoost", "Adaptive Boosting", TRUE, FALSE, FALSE),
  c("AddTree", "Additive Tree", TRUE, FALSE, FALSE),
  c("ADDTBOOSTTV", "Boosting of Additive Trees TV", FALSE, TRUE, FALSE),
  c("BAG", "Bagged Learner", TRUE, TRUE, FALSE),
  c("BOOST", "Boosted Learner", TRUE, TRUE, FALSE),
  c("BART", "Bayesian Additive Regression Trees", TRUE, TRUE, FALSE),
  c("BayesGLM", "Bayesian Generalized Linear Model", TRUE, TRUE, FALSE),
  c("BOOST", "Boosted rtemis Model", FALSE, TRUE, FALSE),
  c("BRUTO", "BRUTO Additive Model", FALSE, TRUE, FALSE),
  c("CART", "Classification and Regression Trees", TRUE, TRUE, TRUE),
  c("CARTLITE", "CART Lite", FALSE, TRUE, FALSE),
  c("CARTLITEBOOST", "Boosted CART Lite", FALSE, TRUE, FALSE),
  c("CARTLITEBOOSTTV", "Boosted CART Lite TV", FALSE, TRUE, FALSE),
  c("CTree", "Conditional Inference Trees", TRUE, TRUE, FALSE),
  c("C50", "C5.0 Decision Tree", TRUE, FALSE, FALSE),
  # c("DN", "deepnet Neural Network", TRUE, TRUE, FALSE),
  # c("ET", "Extra Trees", TRUE, TRUE, FALSE), # removed from CRAN
  c(
    "EVTree",
    "Evolutionary Learning of Globally Optimal Trees",
    TRUE,
    TRUE,
    FALSE
  ),
  c("GAM", "Generalized Additive Model", TRUE, TRUE, FALSE),
  # c("GAMSEL", "Regularized Generalized Additive Model", TRUE, TRUE, FALSE),
  # c("GAMSELX", "GAMSEL Interaction Model", FALSE, TRUE, FALSE),
  # c("GAMSELX2", "GAMSELX TOO", FALSE, TRUE, FALSE),
  c("GBM", "Gradient Boosting Machine", TRUE, TRUE, TRUE),
  # c("GBM3", "Gradient Boosting Machine", TRUE, TRUE, TRUE), # GitHub, fails to compile
  c("GLM", "Generalized Linear Model", TRUE, TRUE, FALSE),
  c("GLMLITE", "Lite GLM", FALSE, TRUE, FALSE),
  c("GLMLITEBOOST", "Boosted GLM Lite", FALSE, TRUE, FALSE),
  c("GLMLITEBOOSTTV", "Boosted GLM Lite TV", FALSE, TRUE, FALSE),
  c("GLMNET", "Elastic Net", TRUE, TRUE, TRUE),
  c("GLMTree", "Generalized Linear Model Tree", TRUE, TRUE, FALSE),
  c("GLS", "Generalized Least Squares", FALSE, TRUE, FALSE),
  c("H2ODL", "H2O Deep Learning", TRUE, TRUE, FALSE),
  c("H2OGBM", "H2O Gradient Boosting Machine", TRUE, TRUE, FALSE),
  c("H2ORF", "H2O Random Forest", TRUE, TRUE, FALSE),
  c("Isotonic", "Isotonic Regression", FALSE, TRUE, FALSE),
  c("LIHADBoost", "Boosting of Additive Trees", FALSE, TRUE, FALSE),
  c("LIHAD", "Linear Hard Additive Tree", FALSE, TRUE, FALSE),
  # c("KernelKNN", "Kernel k-Nearest Neighbor", TRUE, TRUE, FALSE),
  c("KNN", "k-Nearest Neighbor", TRUE, TRUE, FALSE),
  c("LDA", "Linear Discriminant Analysis", TRUE, FALSE, FALSE),
  c("LightCART", "LightGBM", TRUE, TRUE, FALSE),
  c("LightGBM", "LightGBM", TRUE, TRUE, FALSE),
  c("LightRF", "LightGBM Random Forest", TRUE, TRUE, FALSE),
  c("LightRuleFit", "LightGBM RuleFit", TRUE, TRUE, FALSE),
  c("LM", "Ordinary Least Squares Regression", FALSE, TRUE, FALSE),
  c("LOESS", "Local Polynomial Regression", FALSE, TRUE, FALSE),
  c("LOGISTIC", "Logistic Regression", TRUE, FALSE, FALSE),
  c("MARS", "Multivariate Adaptive Regression Splines", FALSE, TRUE, FALSE),
  # c("MLGBM", "Spark MLlib Gradient Boosting", TRUE, TRUE, FALSE),
  # c("MLMLP", "Spark MLlib Multilayer Perceptron", TRUE, FALSE, FALSE),
  c("MLRF", "Spark MLlib Random Forest", TRUE, TRUE, FALSE),
  c("MULTINOM", "Multinomial Logistic Regression", TRUE, FALSE, FALSE),
  c("MXN", "MXNET Neural Network", TRUE, TRUE, FALSE),
  c("NBayes", "Naive Bayes", TRUE, FALSE, FALSE),
  c("NLA", "Nonlinear Activation Unit Regression", FALSE, TRUE, FALSE),
  c("NLS", "Nonlinear Least Squares", FALSE, TRUE, FALSE),
  c("NW", "Nadaraya-Watson Kernel Regression", FALSE, TRUE, FALSE),
  c("POLY", "Polynomial Regression", FALSE, TRUE, FALSE),
  c(
    "PolyMARS",
    "Multivariate Adaptive Polynomial Spline Regression",
    FALSE,
    TRUE,
    FALSE
  ),
  c("POWER", "Power function using NLS", FALSE, TRUE, FALSE),
  c("PPR", "Projection Pursuit Regression", FALSE, TRUE, FALSE),
  # c("PPTree", "Projection Pursuit Tree", TRUE, FALSE, FALSE),
  c("QDA", "Quadratic Discriminant Analysis", TRUE, FALSE, FALSE),
  c("QRNN", "Quantile Neural Network Regression", FALSE, TRUE, FALSE),
  c("Ranger", "Ranger Random Forest", TRUE, TRUE, FALSE),
  c("RF", "Random Forest", TRUE, TRUE, FALSE),
  # c("RRF", "Regularized Random Forest", TRUE, TRUE, FALSE),
  c("RFSRC", "Random Forest SRC", TRUE, TRUE, TRUE),
  c("RLM", "Robust Linear Model", FALSE, TRUE, FALSE),
  c("RuleFit", "RuleFit", TRUE, TRUE, FALSE),
  c("SDA", "Sparse Linear Discriminant Analysis", TRUE, FALSE, FALSE),
  c("SGD", "Stochastic Gradient Descent", FALSE, TRUE, FALSE),
  c("SPLS", "Sparse Partial Least Squares", FALSE, TRUE, FALSE),
  c("SVM", "Support Vector Machine", TRUE, TRUE, FALSE),
  c("TFN", "TensorFlow Neural Network", TRUE, TRUE, FALSE),
  c("TLS", "Total Least Squares", FALSE, TRUE, FALSE),
  c("XGBoost", "XGBoost", TRUE, TRUE, FALSE),
  c("XGBLIN", "XGBoost with Linear Models", FALSE, TRUE, FALSE),
  c("XRF", "XGBoost Random Forest", TRUE, TRUE, FALSE)
))
colnames(rtAlgs) <- c("rtemis name", "Description", "Class", "Reg", "Surv")

#' Select \pkg{rtemis} Learner
#'
#' Accepts learner name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param alg Character: Model name. Case insensitive. e.g. "XGB" for xgboost
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param name Logical: If TRUE, return canonical name of algorithm `alg`
#' @param desc Logical: If TRUE, return full name / description of algorithm `alg`
#'
#' @return function or name of function (see param `fn`) or short algorithm name
#' (`name = TRUE`) or full algorithm name (`desc = TRUE`)
#' @author E.D. Gennatas
#' @export

select_learn <- function(alg, fn = FALSE, name = FALSE, desc = FALSE) {
  if (missing(alg)) {
    cat(hilite(
      "\n  rtemis supports the following algorithms for training learners:\n\n"
    ))
    # Exclude first so many
    print(rtAlgs[-seq_len(4), ], quote = FALSE, row.names = FALSE)
    return(invisible(rtAlgs))
  }

  # check:
  # These are for print.* functions iirc
  if (strtrim(alg, 6) == "Bagged" && desc) {
    return(paste("Bagged", select_learn(substr(alg, 7, 100), desc = TRUE)))
  }
  if (strtrim(alg, 7) == "Boosted" && desc) {
    return(paste("Boosted", select_learn(substr(alg, 8, 100), desc = TRUE)))
  }

  algname <- rtAlgs[, 1][tolower(alg) == tolower(rtAlgs[, 1])]
  if (length(algname) == 0) {
    stop(alg, ": Incorrect algorithm specified")
  }

  if (name) {
    return(algname)
  }

  if (desc) {
    return(as.character(rtAlgs$Description[rtAlgs[, 1] == algname]))
  }

  # fn ----
  if (algname == "BAG") {
    if (fn) getFromNamespace("bag", "rtemis") else "bag"
  } else if (algname == "BOOST") {
    if (fn) getFromNamespace("boost", "rtemis") else "boost"
  } else {
    s_algname <- paste0("s_", algname)
    if (fn) getFromNamespace(s_algname, "rtemis") else s_algname
  }
} # rtemis::select_learn

# wip
gsc <- c(
  "AddTree",
  "CART",
  "DN",
  "GBM",
  "GBM0",
  "GBM3",
  "GLMNET",
  "GLMTree",
  "H2OGBM",
  "LIHAD",
  "LINAD",
  "LINOA",
  "MARS",
  "PolyMARS",
  "PPR",
  "Ranger",
  "RF",
  "SPLS",
  "SVM",
  "XGBoost",
  "LightGBM",
  "LightRF"
)


RT_tunable <- data.table(
  Algorithm = c("CART", "GBM", "GLMNET", "Ranger"),
  `Tunable Hyperparameters` = list(
    c("maxdepth", "minbucket", "minsplit", "cp", "mtry"),
    c("interaction.depth", "ntree", "shrinkage", "n.minobsinnode"),
    c("alpha", "lambda"),
    c("mtry")
  )
)

glmnet_params <- list(
  alpha = "Float (0, 1): Elastic net mixing parameter.",
  lambda = "Float: Regularization parameter."
)

svm_params <- list(
  C = "Float: Penalty parameter C of the error term.",
  kernel = "String: Specifies the kernel type to be used in the algorithm.",
  degree = "Integer: Degree of the polynomial kernel function.",
  gamma = "Float: Kernel coefficient for 'rbf', 'poly' and 'sigmoid'.",
  coef0 = "Float: Independent term in kernel function.",
  shrinking = "Boolean: Whether to use the shrinking heuristic.",
  probability = "Boolean: Whether to enable probability estimates.",
  tol = "Float: Tolerance for stopping criterion.",
  cache_size = "Float: Specify the size of the kernel cache (in MB).",
  class_weight = "String: Set the parameter C of class i to class_weight[i]*C for SVC.",
  verbose = "Boolean: Enable verbose output.",
  max_iter = "Integer: Hard limit on iterations within solver, or -1 for no limit."
)

cart_params <- list(
  cp = "Numeric: Complexity parameter.",
  maxdepth = "Integer: Maximum depth of tree.",
  minbucket = "Integer: Minimum number of observations in a terminal node.",
  minsplit = "Integer: Minimum number of observations in a node to split.",
  prune.cp = "Numeric: Complexity for cost-complexity pruning after tree is built"
)

ranger_params <- list(
  mtry = "Integer: Number of variables to randomly sample as candidates at each split.",
  min.node.size = "Integer: Minimum node size."
)

gbm_params <- list(
  interaction.depth = "Integer: Interaction depth.",
  shrinkage = "Float: Shrinkage (learning rate).",
  n.minobsinnode = "Integer: Minimum number of observation allowed in node.",
  bag.fraction = "Float (0, 1): Fraction of cases to use to train each tree."
)

xgboost_params <- list(
  eta = "Float (0, 1): Learning rate.",
  gamma = "Float: Minimum loss reduction required to make a further partition on a leaf node of the tree.",
  max_depth = "Integer: Maximum depth of each tree.",
  min_child_weight = "Integer: Minimum sum of instance weight (hessian) needed in a child.",
  subsample = "Float (0, 1): Subsample ratio of the training instances.",
  colsample_bytree = "Float (0, 1): Subsample ratio of columns when constructing each tree.",
  colsample_bylevel = "Float (0, 1): Subsample ratio of columns for each split, in each level.",
  lambda = "Float: L2 regularization term on weights.",
  alpha = "Float: L1 regularization term on weights."
)

lightgbm_params <- list(
  num_leaves = "Integer: Maximum tree leaves for base learners.",
  max_depth = "Integer: Maximum tree depth for base learners, -1 means no limit.",
  learning_rate = "Float: Boosting learning rate.",
  bagging_fraction = "Float: Subsample ratio of the training set.",
  lambda_l1 = "Float: L1 regularization.",
  lambda_l2 = "Float: L2 regularization."
)

alg_params <- list(
  GLMNET = glmnet_params,
  SVM = svm_params,
  CART = cart_params,
  Ranger = ranger_params,
  GBM = gbm_params,
  XGBoost = xgboost_params,
  LightGBM = lightgbm_params
)

#' Print tunable hyperparameters for a supervised learning algorithm
#'
#' @param alg Character string: Algorithm name.
#'
#' @author EDG
#' @return Prints tunable hyperparameters for the specified algorithm.
#' @export
tunable <- function(
  alg = c(
    "glmnet",
    "svm",
    "cart",
    "ranger",
    "gbm",
    "xgboost",
    "lightgbm"
  )
) {
  algname <- rtAlgs[, 1][tolower(alg) == tolower(rtAlgs[, 1])]
  msg2(hilite(select_learn(alg, desc = TRUE)), "tunable hyperparameters:")
  printls(alg_params[[algname]])
}
