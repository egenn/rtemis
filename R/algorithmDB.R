# algorithmDB.R
# ::rtemis::
# 2025 EDG rtemis.org

# Supervised Learning ----
supervised_algorithms <- data.frame(rbind(
  # c("TREELINED", "Lined Tree Learner", FALSE, TRUE, FALSE),
  # c("RGB", "Representational Gradient Boosting", TRUE, TRUE, TRUE),
  # c("AADDT", "Asymmetric Additive Tree", TRUE, TRUE, FALSE),
  # c("CSL", "Conditional SuperLearner", TRUE, TRUE, FALSE),
  # c("HAL", "Highly Adaptive LASSO", TRUE, TRUE, TRUE),
  # c("LINAD", "Linear Additive Tree", TRUE, TRUE, FALSE),
  # c("LINOA", "Linear Optimized Additive Tree", TRUE, TRUE, FALSE),
  # c("LITEBOOSTTV", "Lite Boosted Learner", FALSE, TRUE, FALSE),
  # c("AdaBoost", "Adaptive Boosting", TRUE, FALSE, FALSE),
  # c("AddTree", "Additive Tree", TRUE, FALSE, FALSE),
  # c("ADDTBOOSTTV", "Boosting of Additive Trees TV", FALSE, TRUE, FALSE),
  # c("BAG", "Bagged Learner", TRUE, TRUE, FALSE),
  # c("BOOST", "Boosted Learner", TRUE, TRUE, FALSE),
  # c("BART", "Bayesian Additive Regression Trees", TRUE, TRUE, FALSE),
  # c("BayesGLM", "Bayesian Generalized Linear Model", TRUE, TRUE, FALSE),
  # c("BOOST", "Boosted rtemis Model", FALSE, TRUE, FALSE),
  # c("BRUTO", "BRUTO Additive Model", FALSE, TRUE, FALSE),
  c("CART", "Classification and Regression Trees", TRUE, TRUE, TRUE),
  # c("CARTLITE", "CART Lite", FALSE, TRUE, FALSE),
  # c("CARTLITEBOOST", "Boosted CART Lite", FALSE, TRUE, FALSE),
  # c("CARTLITEBOOSTTV", "Boosted CART Lite TV", FALSE, TRUE, FALSE),
  # c("CTree", "Conditional Inference Trees", TRUE, TRUE, FALSE),
  # c("C50", "C5.0 Decision Tree", TRUE, FALSE, FALSE),
  # c("DN", "deepnet Neural Network", TRUE, TRUE, FALSE),
  # c("ET", "Extra Trees", TRUE, TRUE, FALSE), # removed from CRAN
  # c("EVTree", "Evolutionary Learning of Globally Optimal Trees", TRUE, TRUE, FALSE),
  c("GAM", "Generalized Additive Model", TRUE, TRUE, FALSE),
  # c("GAMSEL", "Regularized Generalized Additive Model", TRUE, TRUE, FALSE),
  # c("GAMSELX", "GAMSEL Interaction Model", FALSE, TRUE, FALSE),
  # c("GAMSELX2", "GAMSELX TOO", FALSE, TRUE, FALSE),
  # c("GBM", "Gradient Boosting", TRUE, TRUE, TRUE),
  # c("GBM3", "Gradient Boosting Machine", TRUE, TRUE, TRUE), # GitHub, fails to compile
  c("GLM", "Generalized Linear Model", TRUE, TRUE, FALSE),
  # c("GLMLITE", "Lite GLM", FALSE, TRUE, FALSE),
  # c("GLMLITEBOOST", "Boosted GLM Lite", FALSE, TRUE, FALSE),
  # c("GLMLITEBOOSTTV", "Boosted GLM Lite TV", FALSE, TRUE, FALSE),
  c("GLMNET", "Elastic Net", TRUE, TRUE, TRUE),
  # c("GLMTree", "Generalized Linear Model Tree", TRUE, TRUE, FALSE),
  # c("GLS", "Generalized Least Squares", FALSE, TRUE, FALSE),
  # c("H2ODL", "H2O Deep Learning", TRUE, TRUE, FALSE),
  # c("H2OGBM", "H2O Gradient Boosting Machine", TRUE, TRUE, FALSE),
  # c("H2ORF", "H2O Random Forest", TRUE, TRUE, FALSE),
  c("Isotonic", "Isotonic Regression", FALSE, TRUE, FALSE),
  # c("LIHADBoost", "Boosting of Additive Trees", FALSE, TRUE, FALSE),
  # c("LIHAD", "Linear Hard Additive Tree", FALSE, TRUE, FALSE),
  # c("KernelKNN", "Kernel k-Nearest Neighbor", TRUE, TRUE, FALSE),
  # c("KNN", "k-Nearest Neighbor", TRUE, TRUE, FALSE),
  # c("LDA", "Linear Discriminant Analysis", TRUE, FALSE, FALSE),
  c("LightCART", "Decision Tree", TRUE, TRUE, FALSE),
  c("LightGBM", "Gradient Boosting", TRUE, TRUE, FALSE),
  c("LightRF", "LightGBM Random Forest", TRUE, TRUE, FALSE),
  c("LightRuleFit", "LightGBM RuleFit", TRUE, TRUE, FALSE)
  # c("LM", "Ordinary Least Squares Regression", FALSE, TRUE, FALSE),
  # c("LOESS", "Local Polynomial Regression", FALSE, TRUE, FALSE),
  # c("LOGISTIC", "Logistic Regression", TRUE, FALSE, FALSE),
  # c("MARS", "Multivariate Adaptive Regression Splines", FALSE, TRUE, FALSE),
  # c("MLGBM", "Spark MLlib Gradient Boosting", TRUE, TRUE, FALSE),
  # c("MLMLP", "Spark MLlib Multilayer Perceptron", TRUE, FALSE, FALSE),
  # c("MLRF", "Spark MLlib Random Forest", TRUE, TRUE, FALSE),
  # c("MULTINOM", "Multinomial Logistic Regression", TRUE, FALSE, FALSE),
  # c("MXN", "MXNET Neural Network", TRUE, TRUE, FALSE),
  # c("NBayes", "Naive Bayes", TRUE, FALSE, FALSE),
  # c("NLA", "Nonlinear Activation Unit Regression", FALSE, TRUE, FALSE),
  # c("NLS", "Nonlinear Least Squares", FALSE, TRUE, FALSE),
  # c("NW", "Nadaraya-Watson Kernel Regression", FALSE, TRUE, FALSE),
  # c("POLY", "Polynomial Regression", FALSE, TRUE, FALSE),
  # c("PolyMARS", "Multivariate Adaptive Polynomial Spline Regression", FALSE, TRUE, FALSE),
  # c("POWER", "Power function using NLS", FALSE, TRUE, FALSE),
  # c("PPR", "Projection Pursuit Regression", FALSE, TRUE, FALSE),
  # c("PPTree", "Projection Pursuit Tree", TRUE, FALSE, FALSE),
  # c("QDA", "Quadratic Discriminant Analysis", TRUE, FALSE, FALSE),
  # c("QRNN", "Quantile Neural Network Regression", FALSE, TRUE, FALSE),
  # c("Ranger", "Ranger Random Forest", TRUE, TRUE, FALSE),
  # c("RF", "Random Forest", TRUE, TRUE, FALSE),
  # c("RRF", "Regularized Random Forest", TRUE, TRUE, FALSE),
  # c("RFSRC", "Random Forest SRC", TRUE, TRUE, TRUE),
  # c("RLM", "Robust Linear Model", FALSE, TRUE, FALSE),
  # c("RuleFit", "RuleFit", TRUE, TRUE, FALSE),
  # c("SDA", "Sparse Linear Discriminant Analysis", TRUE, FALSE, FALSE),
  # c("SGD", "Stochastic Gradient Descent", FALSE, TRUE, FALSE),
  # c("SPLS", "Sparse Partial Least Squares", FALSE, TRUE, FALSE),
  # c("SVM", "Support Vector Machine", TRUE, TRUE, FALSE),
  # c("TFN", "TensorFlow Neural Network", TRUE, TRUE, FALSE),
  # c("TLS", "Total Least Squares", FALSE, TRUE, FALSE),
  # c("XGBoost", "XGBoost", TRUE, TRUE, FALSE),
  # c("XGBLIN", "XGBoost with Linear Models", FALSE, TRUE, FALSE),
  # c("XRF", "XGBoost Random Forest", TRUE, TRUE, FALSE)
))
colnames(supervised_algorithms) <- c("Name", "Description", "Class", "Reg", "Surv")

get_alg_name <- function(algorithm) {
  algname <- supervised_algorithms[, 1][tolower(algorithm) == tolower(supervised_algorithms[, 1])]
  if (length(algname) == 0) {
    stop(algorithm, ": Incorrect algorithm specified")
  }
  algname
}

get_alg_setup <- function(algorithm) {
  paste0("setup_", get_alg_name(algorithm))
}

get_alg_desc <- function(algorithm) {
  algdesc <- supervised_algorithms[, 2][tolower(algorithm) == tolower(supervised_algorithms[, 1])]
  if (length(algdesc) == 0) {
    stop(algorithm, ": Incorrect algorithm specified")
  }
  algdesc
}

get_train_fn <- function(algorithm) {
  paste0("train_", get_alg_name(algorithm))
} # rtemis::get_train_fn

get_default_hyperparameters <- function(algorithm) {
  do.call(paste0("setup_", get_alg_name(algorithm)), list())
}

get_predict_fn <- function(algorithm) {
  paste0("predict_", get_alg_name(algorithm))
}

get_se_fn <- function(algorithm) {
  paste0("se_", get_alg_name(algorithm))
}

# get_predict_prob_fn <- function(algorithm) {
#   paste0("predict_prob_", get_alg_name(algorithm))
# }

get_varimp_fn <- function(algorithm) {
  paste0("varimp_", get_alg_name(algorithm))
}

# Clustering ----
clust_algorithms <- data.frame(rbind(
  c("CMeans", "Fuzzy C-means Clustering"),
  # c("DBSCAN", "Density-based spatial clustering of applications with noise"),
  # c("EMC", "Expectation Maximization Clustering"),
  c("HardCL", "Hard Competitive Learning"),
  # c("HOPACH", "Hierarchical Ordered Partitioning And Collapsing Hybrid"),
  # c("H2OKMeans", "H2O K-Means Clustering"),
  c("KMeans", "K-Means Clustering"),
  # c("MeanShift", "Mean Shift Clustering"),
  c("NeuralGas", "Neural Gas Clustering")
  # c("PAM", "Partitioning Around Medoids"),
  # c("PAMK", "Partitioning Around Medoids with k estimation"),
  # c("SPEC", "Spectral Clustering")
))

get_clust_name <- function(algorithm) {
  clustname <- clust_algorithms[, 1][tolower(algorithm) == tolower(clust_algorithms[, 1])]
  if (length(clustname) == 0) {
    stop(algorithm, ": Incorrect clustering algorithm specified")
  }
  clustname
} # /rtemis::get_clust_name

get_clust_desc <- function(algorithm) {
  clustdesc <- clust_algorithms[, 2][tolower(algorithm) == tolower(clust_algorithms[, 1])]
  if (length(clustdesc) == 0) {
    stop(algorithm, ": Incorrect clustering algorithm specified")
  }
  clustdesc
} # /rtemis::get_clust_desc

get_clust_fn <- function(algorithm) {
  paste0("cluster_", get_clust_name(algorithm))
} # /rtemis::get_clust_fn

get_default_clusterparams <- function(algorithm) {
  do.call(paste0("setup_", get_clust_name(algorithm)), list())
}

get_clustpredict_fn <- function(algorithm) {
  paste0("clustpredict_", get_clust_name(algorithm))
}

get_clust_setup_fn <- function(algorithm) {
  paste0("setup_", get_clust_name(algorithm))
} # /rtemis::get_clust_setup_fn


# Decomposition ----
decom_algorithms <- data.frame(rbind(
  # c("H2OAE", "H2O Autoencoder"),
  # c("H2OGLRM", "H2O Generalized Low-Rank Model"),
  c("ICA", "Independent Component Analysis"),
  # c("Isomap", "Isomap"),
  # c("KPCA", "Kernel Principal Component Analysis"),
  # c("LLE", "Locally Linear Embedding"),
  # c("MDS", "Multidimensional Scaling"),
  # c("NMF", "Non-negative Matrix Factorization"),
  c("PCA", "Principal Component Analysis"),
  # c("SPCA", "Sparse Principal Component Analysis"),
  # c("SVD", "Singular Value Decomposition"),
  c("TSNE", "t-distributed Stochastic Neighbor Embedding"),
  c("UMAP", "Uniform Manifold Approximation and Projection")
))

get_decom_name <- function(algorithm) {
  decomname <- decom_algorithms[, 1][tolower(algorithm) == tolower(decom_algorithms[, 1])]
  if (length(decomname) == 0) {
    stop(algorithm, ": Incorrect decomposition algorithm specified")
  }
  decomname
} # /rtemis::get_decom_name

get_decom_desc <- function(algorithm) {
  decomdesc <- decom_algorithms[, 2][tolower(algorithm) == tolower(decom_algorithms[, 1])]
  if (length(decomdesc) == 0) {
    stop(algorithm, ": Incorrect decomposition algorithm specified")
  }
  decomdesc
} # /rtemis::get_decom_desc

get_decom_fn <- function(algorithm) {
  paste0("decom_", get_decom_name(algorithm))
} # /rtemis::get_decom_fn

get_default_decomparams <- function(algorithm) {
  do.call(paste0("setup_", get_decom_name(algorithm)), list())
} # /rtemis::get_default_decomparams

get_decom_setup_fn <- function(algorithm) {
  paste0("setup_", get_decom_name(algorithm))
} # /rtemis::get_decom_setup_fn

get_decom_predict_fn <- function(algorithm) {
  paste0("predict_", get_decom_name(algorithm))
} # /rtemis::get_decom_predict_fn


#' Available Algorithms
#'
#' Print available algorithms for supervised learning, clustering, and decomposition.
#'
#' @rdname available_algorithms
#' @aliases available_algorithms
#' @export
available_supervised <- function() {
  algs <- structure(supervised_algorithms[, 2], names = supervised_algorithms[, 1], class = "list")
  printls(algs, print_class = FALSE)
}

#' @rdname available_algorithms
available_clustering <- function() {
  algs <- structure(clust_algorithms[, 2], names = clust_algorithms[, 1], class = "list")
  printls(algs, print_class = FALSE)
}

#' @rdname available_algorithms
available_decomposition <- function() {
  algs <- structure(decom_algorithms[, 2], names = decom_algorithms[, 1], class = "list")
  printls(algs, print_class = FALSE)
}
