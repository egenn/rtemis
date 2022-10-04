# modSelect.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Select \pkg{rtemis} Learner
#'
#' Accepts learner name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param mod Character: Model name. Case insensitive. e.g. "XGB" for xgboost
#' @param fn Logical: If TRUE, return function, otherwise name of function. Defaults to FALSE
#' @param desc Logical: If TRUE, return full name / description of algorithm \code{mod}
#' @return function or name of function (see param \code{fn}) or full name of algorithm (\code{desc})
#' @author E.D. Gennatas
#' @export

modSelect <- function(mod,
                      fn = FALSE,
                      desc = FALSE) {

  # Name + CRS ----
  rtMods <- data.frame(rbind(
    c("TREELINED", "Lined Tree Learner", FALSE, TRUE, FALSE), 
    c("RGB", "Representational Gradient Boosting", TRUE, TRUE, TRUE), 
    c("AADDT", "Asymmetric Additive Tree", TRUE, TRUE, FALSE), 
    c("CSL", "Conditional SuperLearner", TRUE, TRUE, FALSE), 
    c("LINAD", "Linear Additive Tree", TRUE, TRUE, FALSE),
    c("LINOA", "Linear Optimized Additive Tree", TRUE, TRUE, FALSE),
    c("LITEBOOSTTV", "Lite Boosted Learner", FALSE, TRUE, FALSE),
    c("ADABOOST", "Adaptive Boosting", TRUE, FALSE, FALSE),
    c("ADDTREE", "Additive Tree", TRUE, FALSE, FALSE),
    c("ADDTBOOSTTV", "Boosting of Additive Trees TV", FALSE, TRUE, FALSE),
    c("BAG", "Bagged Learner", TRUE, TRUE, FALSE),
    c("BOOST", "Boosted Learner", TRUE, TRUE, FALSE),
    c("BART", "Bayesian Additive Regression Trees", TRUE, TRUE, FALSE),
    c("BAYESGLM", "Bayesian Generalized Linear Model", TRUE, TRUE, FALSE),
    c("BOOST", "Boosted rtemis Model", FALSE, TRUE, FALSE),
    c("BRUTO", "BRUTO Additive Model", FALSE, TRUE, FALSE),
    c("CART", "Classification and Regression Trees", TRUE, TRUE, TRUE),
    c("CARTLITE", "CART Lite", FALSE, TRUE, FALSE),
    c("CARTLITEBOOST", "Boosted CART Lite", FALSE, TRUE, FALSE),
    c("CARTLITEBOOSTTV", "Boosted CART Lite TV", FALSE, TRUE, FALSE),
    c("CTREE", "Conditional Inference Trees", TRUE, TRUE, FALSE),
    c("C50", "C5.0 Decision Tree", TRUE, FALSE, FALSE),
    c("DN", "deepnet Neural Network", TRUE, TRUE, FALSE),
    c("ET", "Extra Trees", TRUE, TRUE, FALSE),
    c("EVTREE", "Evolutionary Learning of Globally Optimal Trees", TRUE, TRUE, FALSE),
    c("GAM", "Generalized Additive Model", TRUE, TRUE, FALSE),
    c("GAMSEL", "Regularized Generalized Additive Model", TRUE, TRUE, FALSE),
    c("GAMSELX", "GAMSEL Interaction Model", FALSE, TRUE, FALSE),
    c("GAMSELX2", "GAMSELX TOO", FALSE, TRUE, FALSE),
    c("GBM", "Gradient Boosting Machine", TRUE, TRUE, TRUE),
    c("GBM3", "Gradient Boosting Machine", TRUE, TRUE, TRUE),
    c("GLM", "Generalized Linear Model", TRUE, TRUE, FALSE),
    c("GLMLITE", "Lite GLM", FALSE, TRUE, FALSE),
    c("GLMLITEBOOST", "Boosted GLM Lite", FALSE, TRUE, FALSE),
    c("GLMLITEBOOSTTV", "Boosted GLM Lite TV", FALSE, TRUE, FALSE),
    c("GLMNET", "Elastic Net", TRUE, TRUE, TRUE),
    c("GLMTREE", "Generalized Linear Model Tree", TRUE, TRUE, FALSE),
    c("GLS", "Generalized Least Squares", FALSE, TRUE, FALSE),
    c("H2ODL", "H2O Deep Learning", TRUE, TRUE, FALSE),
    c("H2OGBM", "H2O Gradient Boosting Machine", TRUE, TRUE, FALSE),
    c("H2ORF", "H2O Random Forest", TRUE, TRUE, FALSE),
    c("LIHADBOOST", "Boosting of Additive Trees", FALSE, TRUE, FALSE),
    c("LIHAD", "Linear Hard Additive Tree", FALSE, TRUE, FALSE),
    c("KNN", "k-Nearest Neighbor", TRUE, TRUE, FALSE),
    c("LDA", "Linear Discriminant Analysis", TRUE, FALSE, FALSE),
    c("LGB", "Light GBM", TRUE, TRUE, FALSE),
    c("LM", "Ordinary Least Squares Regression", FALSE, TRUE, FALSE),
    c("LOESS", "Local Polynomial Regression", FALSE, TRUE, FALSE),
    c("LOGISTIC", "Logistic Regression", TRUE, FALSE, FALSE),
    c("MARS", "Multivariate Adaptive Regression Splines", FALSE, TRUE, FALSE),
    # c("MLGBM", "Spark MLlib Gradient Boosting", TRUE, TRUE, FALSE),
    # c("MLMLP", "Spark MLlib Multilayer Perceptron", TRUE, FALSE, FALSE),
    c("MLRF", "Spark MLlib Random Forest", TRUE, TRUE, FALSE),
    c("MULTINOM", "Multinomial Logistic Regression", TRUE, FALSE, FALSE),
    c("MXN", "MXNET Neural Network", TRUE, TRUE, FALSE),
    c("NBAYES", "Naive Bayes", TRUE, FALSE, FALSE),
    c("NLA", "Nonlinear Activation Unit Regression", FALSE, TRUE, FALSE),
    c("NLS", "Nonlinear Least Squares", FALSE, TRUE, FALSE),
    c("NW", "Nadaraya-Watson Kernel Regression", FALSE, TRUE, FALSE),
    c("POLY", "Polynomial Regression", FALSE, TRUE, FALSE),
    c("POLYMARS", "Multivariate Adaptive Polynomial Spline Regression", FALSE, TRUE, FALSE),
    c("POWER", "Power function using NLS", FALSE, TRUE, FALSE),
    c("PPR", "Projection Pursuit Regression", FALSE, TRUE, FALSE),
    c("PPTREE", "Projection Pursuit Tree", TRUE, FALSE, FALSE),
    c("QDA", "Quadratic Discriminant Analysis", TRUE, FALSE, FALSE),
    c("QRNN", "Quantile Neural Network Regression", FALSE, TRUE, FALSE),
    c("RANGER", "Random Forest (ranger)", TRUE, TRUE, FALSE),
    c("RF", "Random Forest", TRUE, TRUE, FALSE),
    # c("RRF", "Regularized Random Forest", TRUE, TRUE, FALSE),
    c("RFSRC", "Random Forest SRC", TRUE, TRUE, TRUE),
    c("RLM", "Robust Linear Model", FALSE, TRUE, FALSE),
    c("RULEFIT", "RuleFit Ensemble Model", TRUE, TRUE, FALSE),
    c("SDA", "Sparse Linear Discriminant Analysis", TRUE, FALSE, FALSE),
    c("SGD", "Stochastic Gradient Descent", FALSE, TRUE, FALSE),
    c("SPLS", "Sparse Partial Least Squares", FALSE, TRUE, FALSE),
    c("SVM", "Support Vector Machine", TRUE, TRUE, FALSE),
    c("TFN", "TensorFlow Neural Network", TRUE, TRUE, FALSE),
    c("TLS", "Total Least Squares", FALSE, TRUE, FALSE),
    c("XGB", "XGBoost", TRUE, TRUE, FALSE),
    c("XGBOOST", "XGBoost", TRUE, TRUE, FALSE),
    c("XGBLIN", "XGBoost with Linear Models", FALSE, TRUE, FALSE)
  ))
  colnames(rtMods) <- c("rtemis name", "Description", "Class", "Reg", "Surv")

  # if (missing(mod) & !listAliases) {
  if (missing(mod)) {
    cat(hilite("\n  rtemis supports the following algorithms for training learners:\n\n"))
    # Exclude first so many
    print(rtMods[-seq(6), ], quote = FALSE, row.names = FALSE)
    return(invisible(rtMods))
  }

  # check:
  # These are for print. functions iirc
  if (strtrim(mod, 6) == "Bagged" && desc) {
    return(paste("Bagged", modSelect(substr(mod, 7, 100), desc = TRUE)))
  }
  if (strtrim(mod, 7) == "Boosted" && desc) {
    return(paste("Boosted", modSelect(substr(mod, 8, 100), desc = TRUE)))
  }

  name.vec <- toupper(rtMods[, 1])
  name <- name.vec[pmatch(toupper(mod), name.vec)]
  if (is.na(name)) {
    print(rtMods[, 1:2], quote = FALSE)
    stop(mod, ": Incorrect model specified")
  }

  if (desc) return(as.character(rtMods$Description[rtMods[, 1] == name]))

  # fn ----
  if (name == "BAG") {
    learner <- if (fn) getFromNamespace("bag", "rtemis") else "bag"
  } else if (name == "BOOST") {
    learner <- if (fn) getFromNamespace("boost", "rtemis") else "boost"
  } else {
    s.name <- paste0("s_", name)
    learner <- if (fn) getFromNamespace(s.name, "rtemis") else s.name
  }

  learner

} # rtemis::modSelect

gsc <- c(
  "ADDTREE", "CART", "DN", "GBM", "GBM0", "GBM3", "GLMNET", "GLMTREE",
  "H2OGBM", "LIHAD", "LINAD", "LINOA", "MARS", "POLYMARS", "PPR", "RANGER",
  "RF", "SPLS", "SVM", "XGBOOST"
)
