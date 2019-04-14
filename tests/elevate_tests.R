# elevate_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

library(rtemis)

# Regression
x <- rnormmat(100, 5)
w <- rnorm(5)
y <- x %*% w + rnorm(100)
dat <- data.frame(x, y)
.test <- requireNamespace("rpart", quietly = TRUE) & requireNamespace("plyr", quietly = TRUE) &
  requireNamespace("pbapply", quietly = TRUE)
if (.test) {
  elr <- elevate(dat, n.resamples = 3, mod = 'cart')
}

# Classification
iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
.test <- .test & requireNamespace("pROC", quietly = TRUE)
if (.test) {
  elc <- elevate(iris2, n.resamples = 3, mod = 'cart')
}
