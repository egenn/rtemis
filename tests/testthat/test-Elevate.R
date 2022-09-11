# Data ----

options(rt.font = "sans")

# '- Regression ----
x <- rnormmat(200, 5)
y <- x[, 3] + x[, 5] + rnorm(200)
dat <- data.frame(x, y)

# '- Classification ----
iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)

# Test ----
test_that("elevate CART Regression succeeds", {
  mod_r <- elevate(dat,
    mod = "cart",
    outer.resampling = rtset.resample(n.resamples = 3)
  )
  expect_identical(class(mod_r)[1], "rtModCV")
})

test_that("elevate CART Classification succeeds", {
  mod_c <- elevate(iris2,
    mod = "cart",
    outer.resampling = rtset.resample(n.resamples = 3)
  )
  expect_identical(class(mod_c)[1], "rtModCVClass")
})

test_that("elevate RANGER Regression succeeds", {
  mod_r <- elevate(dat,
    mod = "ranger",
    outer.resampling = rtset.resample(n.resamples = 3)
  )
  expect_identical(class(mod_r)[1], "rtModCV")
})

test_that("elevate RANGER Classification succeeds", {
  mod_c <- elevate(iris2,
    mod = "ranger",
    outer.resampling = rtset.resample(n.resamples = 3)
  )
  expect_identical(class(mod_c)[1], "rtModCVClass")
})
