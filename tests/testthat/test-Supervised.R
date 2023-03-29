# Supervised Learning Tests ----
options(rt.font = "sans")

# Data ----
# '- Regression ----
x <- rnormmat(200, 5)
y <- x[, 3] + x[, 5] + rnorm(200)
datr <- data.frame(x, y)
resr <- resample(datr)
datr_train <- datr[resr$Subsample_1, ]
datr_test <- datr[-resr$Subsample_1, ]

# '- Classification ----
# '-- binary ----
datc2 <- iris[51:150, ]
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Subsample_1, ]
datc2_test <- datc2[-resc2$Subsample_1, ]

# '-- 3-class ----
resc3 <- resample(iris)
datc3_train <- iris[resc3$Subsample_1, ]
datc3_test <- iris[-resc3$Subsample_1, ]

# Tests ----

## GLMNET ----
test_that("GLMNET Regression succeeds", {
  mod_r <- s_GLMNET(datr_train, datr_test, alpha = .5, lambda = .01)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("GLMNET Binary Classification succeeds", {
  mod_c <- s_GLMNET(datc2_train, datc2_test, alpha = .5, lambda = .01)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("GLMNET 3-class Classification succeeds", {
  mod_c <- s_GLMNET(datc3_train, datc3_test, alpha = .5, lambda = .01)
  expect_identical(class(mod_c)[1], "rtModClass")
})

## CART ----
test_that("CART Regression succeeds", {
  mod_r <- s_CART(datr_train, datr_test)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("CART Binary Classification succeeds", {
  mod_c <- s_CART(datc2_train, datc2_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("CART 3-class Classification succeeds", {
  mod_c <- s_CART(datc3_train, datc3_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})


## RANGER ----
test_that("RANGER Regression succeeds", {
  mod_r <- s_RANGER(datr_train, datr_test)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("RANGER Binary Classification succeeds", {
  mod_c <- s_RANGER(datc2_train, datc2_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("RANGER 3-class Classification succeeds", {
  mod_c <- s_RANGER(datc3_train, datc3_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})

## LIGHTRF ----
test_that("LIGHTRF Regression succeeds", {
  mod_r <- s_LIGHTRF(datr_train, datr_test, force_nrounds = 20)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("LIGHTRF Binary Classification succeeds", {
  mod_c <- s_LIGHTRF(datc2_train, datc2_test, force_nrounds = 20)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("LIGHTRF 3-class Classification succeeds", {
  mod_c <- s_LIGHTRF(datc3_train, datc3_test, force_nrounds = 20)
  expect_identical(class(mod_c)[1], "rtModClass")
})

## LIGHTGBM ----
test_that("LIGHTGBM Regression succeeds", {
  mod_r <- s_LIGHTGBM(datr_train, datr_test, force_nrounds = 20)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("LIGHTGBM Binary Classification succeeds", {
  mod_c <- s_LIGHTGBM(datc2_train, datc2_test, force_nrounds = 20)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("LIGHTGBM 3-class Classification succeeds", {
  mod_c <- s_LIGHTGBM(datc3_train, datc3_test, force_nrounds = 20)
  expect_identical(class(mod_c)[1], "rtModClass")
})