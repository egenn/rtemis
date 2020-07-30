# modSelect.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

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
#' @author Efstathios D. Gennatas
#' @export

modSelect <- function(mod,
                      fn = FALSE,
                      desc = FALSE) {

  # Name + CRS ====
  rtMods <- data.frame(rbind(
    c("TREELINED", "Lined Tree Learner", F, T, F), # Yep -
    c("RGB", "Representational Gradient Boosting", T, T, T), # that's
    c("AADDT", "Asymmetric Additive Tree", T, T, F), # right,
    c("CSL", "Conditional SuperLearner", T, T, F), # bud
    c("SHYTREE", "Stepwise Hybrid Tree", T, T, F), # !
    c("SHYOPTREE", "Stepwise Hybrid Optimized Tree", T, T, F), # !
    c("LITEBOOSTTV", "Lite Boosted Learner", F, T, F),
    c("ADABOOST", "Adaptive Boosting", T, F, F),
    c("ADDTREE", "Additive Tree", T, F, F),
    c("ADDTBOOSTTV", "Boosting of Additive Trees TV", F, T, F),
    c("BAG", "Bagged Learner", T, T, F),
    c("BOOST", "Boosted Learner", T, T, F),
    c("BART", "Bayesian Additive Regression Trees", T, T, F),
    c("BAYESGLM", "Bayesian Generalized Linear Model", T, T, F),
    c("BOOST", "Boosted rtemis Model", F, T, F),
    c("BRUTO", "BRUTO Additive Model", F, T, F),
    c("CART", "Classification and Regression Trees", T, T, T),
    c("CARTLITE", "CART Lite", F, T, F),
    c("CARTLITEBOOST", "Boosted CART Lite", F, T, F),
    c("CARTLITEBOOSTTV", "Boosted CART Lite TV", F, T, F),
    c("CTREE", "Conditional Inference Trees", T, T, F),
    c("C50", "C5.0 Decision Tree", T, F, F),
    c("DN", "deepnet Neural Network", T, T, F),
    c("ET", "Extra Trees", T, T, F),
    c("EVTREE", "Evolutionary Learning of Globally Optimal Trees", T, T, F),
    c("GAM", "Generalized Additive Model", T, T, F),
    c("GAMSEL", "Regularized Generalized Additive Model", T, T, F),
    c("GAMSELX", "GAMSEL Interaction Model", F, T, F),
    c("GAMSELX2", "GAMSELX TOO", F, T, F),
    c("GBM", "Gradient Boosting Machine", T, T, T),
    c("GBM3", "Gradient Boosting Machine", T, T, T),
    c("GLM", "Generalized Linear Model", T, T, F),
    c("GLMLITE", "Lite GLM", F, T, F),
    c("GLMLITEBOOST", "Boosted GLM Lite", F, T, F),
    c("GLMLITEBOOSTTV", "Boosted GLM Lite TV", F, T, F),
    c("GLMNET", "Elastic Net", T, T, T),
    c("GLS", "Generalized Least Squares", F, T, F),
    c("H2ODL", "H2O Deep Learning", T, T, F),
    c("H2OGBM", "H2O Gradient Boosting Machine", T, T, F),
    c("H2ORF", "H2O Random Forest", T, T, F),
    c("HYTBOOST", "Boosting of Additive Trees", F, T, F),
    c("HYTREE", "Hybrid Additive Tree", F, T, F),
    c("KNN", "k-Nearest Neighbor", T, T, F),
    c("LDA", "Linear Discriminant Analysis", T, F, F),
    c("LGB", "Light GBM", T, T, F),
    c("LM", "Ordinary Least Squares Regression", F, T, F),
    c("LOESS", "Local Polynomial Regression", F, T, F),
    c("LOGISTIC", "Logistic Regression", T, F, F),
    c("MARS", "Multivariate Adaptive Regression Splines", F, T, F),
    # c("MLGBM", "Spark MLlib Gradient Boosting", T, T, F),
    # c("MLMLP", "Spark MLlib Multilayer Perceptron", T, F, F),
    c("MLRF", "Spark MLlib Random Forest", T, T, F),
    c("MULTINOM", "Multinomial Logistic Regression", T, F, F),
    c("MXN", "MXNET Neural Network", T, T, F),
    c("NBAYES", "Naive Bayes", T, F, F),
    c("NLA", "Nonlinear Activation Unit Regression", F, T, F),
    c("NLS", "Nonlinear Least Squares", F, T, F),
    c("NW", "Nadaraya-Watson Kernel Regression", F, T, F),
    c("POLY", "Polynomial Regression", F, T, F),
    c("POLYMARS", "Multivariate Adaptive Polynomial Spline Regression", F, T, F),
    c("POWER", "Power function by NLS", F, T, F),
    c("PPR", "Projection Pursuit Regression", F, T, F),
    c("PPTREE", "Projection Pursuit Tree", T, F, F),
    c("QDA", "Quadratic Discriminant Analysis", T, F, F),
    c("QRNN", "Quantile Neural Network Regression", F, T, F),
    c("RANGER", "Random Forest (ranger)", T, T, F),
    c("RF", "Random Forest", T, T, F),
    # c("RRF", "Regularized Random Forest", T, T, F),
    c("RFSRC", "Random Forest SRC", T, T, T),
    c("RLM", "Robust Linear Model", F, T, F),
    c("RULEFEAT", "RuleFeat Ensemble Model", T, T, F),
    c("SGD", "Stochastic Gradient Descent", F, T, F),
    c("SPLS", "Sparse Partial Least Squares", F, T, F),
    c("SVM", "Support Vector Machine", T, T, F),
    c("TFN", "TensorFlow Neural Network", T, T, F),
    c("TLS", "Total Least Squares", F, T, F),
    c("XGB", "XGBoost", T, T, F),
    c("XGBLIN", "XGBoost with Linear Models", F, T, F)
  ))
  colnames(rtMods) <- c("rtemis name", "Description", "Class", "Reg", "Surv")

  # if (missing(mod) & !listAliases) {
  if (missing(mod)) {
    cat(rtHighlight$bold("\n  rtemis supports the following algorithms for training learners:\n\n"))
    # Exclude first so many
    print(rtMods[-seq(6), ], quote = FALSE, row.names = FALSE)
    return(invisible(rtMods))
  }

  # check:
  # These are for print. functions iirc
  if (strtrim(mod, 6) == "Bagged" & desc) {
    return(paste("Bagged", modSelect(substr(mod, 7, 100), desc = TRUE)))
  }
  if (strtrim(mod, 7) == "Boosted" & desc) {
    return(paste("Boosted", modSelect(substr(mod, 8, 100), desc = TRUE)))
  }

  name.vec <- toupper(rtMods[, 1])
  name <- name.vec[pmatch(toupper(mod), name.vec)]
  if (is.na(name)) {
    print(rtMods[, 1:2], quote = FALSE)
    stop(mod, ": Incorrect model specified")
  }

  if (desc) return(as.character(rtMods$Description[rtMods[, 1] == name]))

  # fn ====
  if (name == "BAG") {
    learner <- if (fn) getFromNamespace("bag", "rtemis") else "bag"
  } else if (name == "BOOST") {
    learner <- if (fn) getFromNamespace("boost", "rtemis") else "boost"
  } else {
    s.name <- paste0("s.", name)
    learner <- if (fn) getFromNamespace(s.name, "rtemis") else s.name
  }

  learner

} # rtemis::modSelect
