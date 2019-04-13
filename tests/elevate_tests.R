# elevate_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

if (requireNamespace("pROC", quietly = TRUE)) {

library(rtemis)

# Regression
x <- rnormmat(100, 5)
w <- rnorm(5)
y <- x %*% w + rnorm(100)
dat <- data.frame(x, y)
elr <- elevate(dat, n.resamples = 3)

# Classification
iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
elc <- elevate(iris2, n.resamples = 3)

} # if (requireNamespace(...))
