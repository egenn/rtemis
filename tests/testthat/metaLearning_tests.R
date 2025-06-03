# metaLearning_tests.R
# ::rtemis::
# 2019- EDG rtemis.org

options(rt.font = "sans")
library(rtemisalpha)

# Regression Data ----
x <- rnormmat(200, 2, seed = 2018)
y <- .8 * x[, 1] + x[, 2]^2 + rnorm(200)
dat <- data.frame(x, y)
res <- resample(dat, seed = 2019)
dat.train <- dat[res$Subsample_1, ]
dat.test <- dat[-res$Subsample_1, ]

# metaMod ----
if (requireNamespace("rpart", quietly = TRUE)) {
  meta_glm.cart <- metaMod(
    dat.train,
    dat.test,
    base.mods = c("cart", "glm"),
    meta.mod = "gam"
  )
}
