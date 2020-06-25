# supervisedLearning_tests.R
# ::rtemis::
# 2018-9 Efstathios D. Gennatas MBBS AICSM PhD egenn.github.io

library(rtemis)

# Regression Data ====
x <- rnormmat(50, 2, seed = 2018)
w <- rnorm(2)
y <- c(x %*% w + rnorm(50))
x[10, 1] <- NA
x <- preprocess(x, impute = TRUE, impute.type = "meanMode")
res <- resample(y, seed = 2019)
dat <- data.frame(x, y)
dat.train <- dat[res$Subsample_1, ]
dat.test <- dat[-res$Subsample_1, ]
x <- dat.train[, -3]
y <- dat.train[, 3]
x.test <- dat.test[, -3]
y.test <- dat.test[, 3]

# Classification Data ====
# iris2 <- iris[51:150, ]
iris2 <- iris
iris2$Species[iris2$Species  == "versicolor"] <- "setosa"
iris2$Species <- factor(iris2$Species)
res <- resample(iris2, seed = 2019)
iris2.train <- iris2[res$Subsample_1, ]
iris2.test <- iris2[-res$Subsample_1, ]
checkData(iris2, str = TRUE)

# Test different ways of data input (dataPrepare) ====
mod <- s.GLM(dat.train)
mod <- s.GLM(dat.train, dat.test)
mod <- s.GLM(x, y, x.test, y.test)

# Test for tibble and data.table inputs ====
# Commented out because tibble is not in Suggests and R CMD check gives warning
# if (requireNamespace("tibble", quietly = TRUE)) {
#   tb.train <- tibble::as_tibble(dat.train)
#   tb.test <- tibble::as_tibble(dat.test)
#   mod <- s.GLM(tb.train, tb.test)
# }

if (requireNamespace("data.table", quietly = TRUE)) {
  dt.train <- data.table::as.data.table(dat.train)
  dt.test <- data.table::as.data.table(dat.test)
  mod <- s.GLM(dt.train, dt.test)
}

# Classification & Regression Models ====
if (requireNamespace("ada", quietly = TRUE)) {
  mod <- s.ADABOOST(iris2.train, iris2.test, iter = 5)
}

if (requireNamespace("rpart", quietly = TRUE) && requireNamespace("glmnet", quietly = TRUE)) {
  mod <- s.HYTBOOST(dat.train, dat.test, max.iter = 4)
  mod <- s.HYTREE(dat.train, dat.test, max.depth = 3)
  mod <- s.ADDTREE(iris2.train, iris2.test, max.depth = 3)
  mod <- cartLinBoostTV(x, y, max.iter = 4)
  mod <- cartLite(x, y)
  mod <- cartLinBoostTV(x, y, max.iter = 4)
}

if (requireNamespace("bartMachine", quietly = TRUE)) {
  mod <- s.BART(dat.train, dat.test, n.trees = 3, n.burnin = 2, n.iter = 2)
  mod <- s.BART(iris2.train, iris2.test, n.trees = 3, n.burnin = 2, n.iter = 2)
}

if (requireNamespace("arm", quietly = TRUE)) {
  mod <- s.BAYESGLM(dat.train, dat.test)
  mod <- s.BAYESGLM(iris2.train, iris2.test)
}

if (requireNamespace("mda", quietly = TRUE)) {
  mod <- s.BRUTO(dat.train, dat.test)
}

if (requireNamespace("C50", quietly = TRUE)) {
  mod <- s.C50(iris2.train, iris2.test)
}

if (requireNamespace("rpart", quietly = TRUE)) {
  mod <- s.CART(dat.train, dat.test)
  mod <- s.CART(iris2.train, iris2.test)
}

if (requireNamespace("partykit", quietly = TRUE)) {
  mod <- s.CTREE(dat.train, dat.test)
  mod <- s.CTREE(iris2.train, iris2.test)
}

if (requireNamespace("MASS", quietly = TRUE)) {
  mod <- s.LDA(iris2.train, iris2.test)
  mod <- s.QDA(iris2.train, iris2.test)
}

if (requireNamespace("deepnet", quietly = TRUE)) {
  mod <- s.DN(dat.train, dat.test)
}

if (requireNamespace("extraTrees", quietly = TRUE)) {
  mod <- s.ET(dat.train, dat.test)
}

if (requireNamespace("evtree", quietly = TRUE)) {
  mod <- s.EVTREE(dat.train, dat.test)
}

mod <- s.GAM(dat.train, dat.test)
mod <- s.GAM(iris2.train, iris2.test)

if (requireNamespace("gamsel", quietly = TRUE)) {
  mod <- s.GAMSEL(dat.train, dat.test)
  mod <- s.GAMSEL(iris2.train, iris2.test)
}

if (requireNamespace("gbm", quietly = TRUE)) {
  mod <- s.GBM(dat.train, dat.test, force.n.trees = 10)
  mod <- s.GBM(iris2.train, iris2.test, force.n.trees = 10)
}

# Development has moved back to CRAN gbm
# if (requireNamespace("gbm3", quietly = TRUE)) {
#   mod <- s.GBM3(dat.train, dat.test, force.n.trees = 10)
#   mod <- s.GBM3(iris2.train, iris2.test, force.n.trees = 10)
# }

mod <- s.GLM(dat.train, dat.test)
mod <- s.GLM(iris2.train, iris2.test)

if (requireNamespace("glmnet", quietly = TRUE)) {
  mod <- s.GLMNET(dat.train, dat.test, alpha = 0, lambda = 0)
  mod <- glmLite(x, y)
  mod <- glmLiteBoostTV(x, y, max.iter = 4)
}

if (requireNamespace("nlme", quietly = TRUE)) {
  mod <- s.GLS(dat.train, dat.test)
}

if (requireNamespace("h2o", quietly = TRUE)) {
  mod <- s.H2OGBM(dat.train, dat.test, force.n.trees = 10)
  mod <- s.H2OGBM(iris2.train, iris2.test, force.n.trees = 10)
  mod <- s.H2ORF(dat.train, dat.test, n.trees = 20)
  mod <- s.H2ORF(iris2.train, iris2.test)
}

# if (requireNamespace("iRF", quietly = TRUE)) {
#   mod <- s.IRF(dat.train, dat.test)
# }

mod <- s.LOESS(dat.train, dat.test)

if (requireNamespace("earth", quietly = TRUE)) {
  mod <- s.MARS(dat.train, dat.test)
}

# fails under covr
# if (requireNamespace("sparklyr", quietly = TRUE)) {
#   mod <- s.MLRF(dat.train, dat.test, n.trees = 10, feature.subset.strategy = "all")
#   mod <- s.MLRF(iris2.train, iris2.test, n.trees = 10, feature.subset.strategy = "all")
# }

# if (requireNamespace("mxnet", quietly = TRUE)) {
#   mod <- s.MXN(dat.train, dat.test, n.hidden.nodes = 2, max.epochs = 10)
#   mod <- s.MXN(iris2.train, iris2.test, max.epochs = 10)
# }

mod <- s.NLA(dat.train, dat.test)

if (requireNamespace("glmnet", quietly = TRUE)) {
  mod <- s.NLS(dat.train, dat.test)
}

if (requireNamespace("np", quietly = TRUE)) {
  mod <- s.NW(dat.train, dat.test)
}

if (requireNamespace("polspline", quietly = TRUE)) {
  mod <- s.POLYMARS(dat.train, dat.test)
  mod <- s.POLYMARS(iris2.train, iris2.test)
}

mod <- s.PPR(dat.train, dat.test)

if (requireNamespace("R.utils", quietly = TRUE)) {
  mod <- learn(dat.train, dat.test)
}

if (requireNamespace("PPtree", quietly = TRUE)) {
  mod <- s.PPTREE(iris2.train, iris2.test)
}

if (requireNamespace("ranger", quietly = TRUE)) {
  mod <- s.RANGER(dat.train, dat.test)
  mod <- s.RANGER(iris2.train, iris2.test)
}

if (requireNamespace("randomForest", quietly = TRUE)) {
  mod <- s.RF(dat.train, dat.test)
  mod <- s.RF(iris2.train, iris2.test)
}

if (requireNamespace("randomForestSRC", quietly = TRUE)) {
  mod <- s.RFSRC(dat.train, dat.test)
  mod <- s.RFSRC(iris2.train, iris2.test)
}

if (requireNamespace("sgd", quietly = TRUE)) {
  mod <- s.SGD(dat.train, dat.test)
  mod <- s.SGD(iris2.train, iris2.test)
}

if (requireNamespace("spls", quietly = TRUE)) {
  mod <- s.SPLS(dat.train, dat.test)
  mod <- s.SPLS(iris2.train, iris2.test)
}

if (requireNamespace("e1071", quietly = TRUE)) {
  mod <- s.NBAYES(iris2.train, iris2.test)
  mod <- s.SVM(dat.train, dat.test, cost = 1)
  mod <- s.SVM(iris2.train, iris2.test, cost = 1)

}

mod <- s.TLS(dat.train, dat.test)

if (requireNamespace("xgboost", quietly = TRUE)) {
  mod <- s.XGB(dat.train, dat.test, nrounds = 10)
  mod <- s.XGB(iris2.train, iris2.test, nrounds = 10)
}

# Bagging & Boosting ====
if (requireNamespace("rpart", quietly = TRUE) & requireNamespace("pbapply", quietly = TRUE)) {
  mod <- bag(dat.train, dat.test, k = 4)
  mod <- bag(iris2.train, iris2.test, k = 10)
  mod <- boost(dat.train, dat.test, max.iter = 4)
}

# rtModLog ====
logger <- rtModLogger$new()
logger$add(mod)

# distillTreeRules ====
if (requireNamespace("randomForest", quietly = TRUE)) {
  mod <- s.RF(iris, n.trees = 3)
  rules <- distillTreeRules(mod, iris)
  formatRules(rules$rules)
}

# gp ====
if (requireNamespace("tgp", quietly = TRUE)) {
  mod <- gp(x, y)
}

# Ranger IPW ====
if (requireNamespace("mlbench", quietly = TRUE)) {
  data(Sonar, package = "mlbench")
  sonar.train <- Sonar[-res$Subsample_1, ]
  sonar.test <- Sonar[-res$Subsample_1, ]
  mod.rf.ipw <- s.RANGER(sonar.train, sonar.test, ipw = F, upsample = F, ipw.case.weights = F, ipw.class.weights = F)
  mod.rf.ipw <- s.RANGER(sonar.train, sonar.test, ipw = T, upsample = F, ipw.case.weights = T, ipw.class.weights = F)
  mod.rf.ipw <- s.RANGER(sonar.train, sonar.test, ipw = T, upsample = F, ipw.case.weights = F, ipw.class.weights = T)
}
