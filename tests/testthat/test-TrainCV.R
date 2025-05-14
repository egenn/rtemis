# test-TrainCV.R
# ::rtemis::
# EDG rtemis.org

options(rt.font = "sans")
# Data ----
# '- Regression ----
x <- rnormmat(200, 5)
y <- x[, 3] + x[, 5] + rnorm(200)
dat <- data.frame(x, y)

# '- Classification ----
iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)

# Test ----
test_that("train CART Regression succeeds", {
  mod_r <- train_cv(
    dat,
    alg = "cart",
    outer.resampling = setup.resample(n.resamples = 3)
  )
  expect_identical(class(mod_r)[1], "rtModCV")
})

test_that("train CART Classification succeeds", {
  mod_c <- train_cv(
    iris2,
    alg = "cart",
    outer.resampling = setup.resample(n.resamples = 3)
  )
  expect_identical(class(mod_c)[1], "rtModCVClass")
})

test_that("train Ranger Regression succeeds", {
  mod_r <- train_cv(
    dat,
    alg = "ranger",
    outer.resampling = setup.resample(n.resamples = 3)
  )
  expect_identical(class(mod_r)[1], "rtModCV")
})

test_that("train Ranger Classification succeeds", {
  mod_c <- train_cv(
    iris2,
    alg = "ranger",
    outer.resampling = setup.resample(n.resamples = 3)
  )
  expect_identical(class(mod_c)[1], "rtModCVClass")
})
