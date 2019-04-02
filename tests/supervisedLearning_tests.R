# supervisedLearning_tests.R
# ::rtemis::
# 2018-9 Efstathios D. Gennatas MBBS AICSM PhD egenn.github.io

library(rtemis)

# Regression Data ====
x <- rnormmat(50, 2, seed = 2018)
w <- rnorm(2)
y <- x %*% w + rnorm(50)
res <- resample(y)
dat <- data.frame(x, y)
x <- dat.train <- dat[res$Resample01, ]
x.test <- dat.test <- dat[-res$Resample01, ]
y <- y.test <- NULL

# Regression Models ====
# mod <- s.AADDT(dat.train, dat.test, force.max.leaves = 2)
mod <- s.ADDT(dat.train, dat.test, max.depth = 3)
mod <- s.BART(dat.train, dat.test, n.trees = 3, n.burnin = 2, n.iter = 2)
mod <- s.BAYESGLM(dat.train, dat.test)
mod <- s.BRUTO(dat.train, dat.test)
mod <- s.CART(dat.train, dat.test)
mod <- s.CTREE(dat.train, dat.test)
mod <- s.DN(dat.train, dat.test)
mod <- s.ET(dat.train, dat.test)
mod <- s.EVTREE(dat.train, dat.test)
mod <- s.GAM(dat.train, dat.test)
mod <- s.GAMSEL(dat.train, dat.test)
mod <- s.GBM(dat.train, dat.test, max.trees = 100)
mod <- s.GLM(dat.train, dat.test)
mod <- s.GLMNET(dat.train, dat.test, alpha = 0, lambda = 0)
mod <- s.GLM(dat.train, dat.test)
mod <- s.GLS(dat.train, dat.test)
mod <- s.H2OGBM(dat.train, dat.test, force.n.trees = 10)
mod <- s.H2ORF(dat.train, dat.test, n.trees = 20)
mod <- s.IRF(dat.train, dat.test)
mod <- s.LOESS(dat.train, dat.test)
mod <- s.MARS(dat.train, dat.test)
# mod <- s.MLRF(dat.train, dat.test, n.trees = 20, feature.subset.strategy = "all") # omit to save time
mod <- s.MXN(dat.train, dat.test, n.hidden.nodes = 2, max.epochs = 10)
mod <- s.NW(dat.train, dat.test)
mod <- s.RANGER(dat.train, dat.test)
mod <- s.RF(dat.train, dat.test)
mod <- s.RFSRC(dat.train, dat.test)
mod <- s.SGD(dat.train, dat.test)
mod <- s.SPLS(dat.train, dat.test)
mod <- s.SVM(dat.train, dat.test, cost = 1)
mod <- s.TLS(dat.train, dat.test)
mod <- s.XGB(dat.train, dat.test, nrounds = 10)

# Bagged Regression ====
mod <- bag(dat.train, dat.test, k = 10)

# Boosted Regression ====
mod <- boost(dat.train, dat.test, max.iter = 10)

# Classification Data ====
iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
res <- resample(iris2)
x <- iris2.train <- iris2[res$Resample01, ]
x.test <- iris2.test <- iris2[-res$Resample01, ]
y <- y.test <- NULL

# Classification Models ====
mod <- s.ADABOOST(iris2.train, iris2.test, iter = 5)
mod <- s.BART(iris2.train, x.test = iris2.test, n.trees = 3, n.burnin = 2, n.iter = 2)
mod <- s.BAYESGLM(iris2.train, iris2.test)
mod <- s.C50(iris2.train, x.test = iris2.test)
mod <- s.CART(iris2.train, x.test = iris2.test)
mod <- s.CTREE(iris2.train, x.test = iris2.test)
mod <- s.DA(iris2.train, x.test = iris2.test)
mod <- s.GAM(iris2.train, iris2.test)
mod <- s.GAMSEL(iris2.train, iris2.test)
# mod <- s.MLRF(iris2.train, x.test = iris2.test, feature.subset.strategy = "all")
mod <- s.MXN(iris2.train, x.test = iris2.test, max.epochs = 1000)
mod <- s.NBAYES(iris2.train, x.test = iris2.test)
mod <- s.POLYMARS(iris2.train, x.test = iris2.test)
mod <- s.PPTREE(iris2.train, x.test = iris2.test)
mod <- s.QDA(iris2.train, x.test = iris2.test)
mod <- s.RANGER(iris2.train, x.test = iris2.test)
mod <- s.RF(iris2.train, x.test = iris2.test)
mod <- s.RFSRC(iris2.train, x.test = iris2.test)
mod <- s.SGD(iris2.train, x.test = iris2.test)
mod <- s.SPLS(iris2.train, x.test = iris2.test)
mod <- s.SVM(iris2.train, x.test = iris2.test, cost = 1)
mod <- s.XGB(iris2.train, x.test = iris2.test, nrounds = 10)

# Bagged Classification ====
mod <- bag(iris2.train, iris2.test, k = 10)
