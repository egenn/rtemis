# test-Supervised.R
# ::rtemis::
# EDG rtemis.org

# Supervised Learning Tests ----
options(rt.font = "sans")

# Data ----
## Regression ----
x <- rnormmat(200, 5)
y <- x[, 3] + x[, 5] + rnorm(200)
datr <- data.frame(x, y)
resr <- resample(datr)
datr_train <- datr[resr$Subsample_1, ]
datr_test <- datr[-resr$Subsample_1, ]

## Classification ----
### binary ----
datc2 <- iris[51:150, ]
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Subsample_1, ]
datc2_test <- datc2[-resc2$Subsample_1, ]

### 3-class ----
resc3 <- resample(iris)
datc3_train <- iris[resc3$Subsample_1, ]
datc3_test <- iris[-resc3$Subsample_1, ]

# Tests ----

## GLMNET ----
test_that("GLMNET Regression succeeds", {
  mod_r <- s_GLMNET(datr_train, datr_test, alpha = .5, lambda = .01)
  expect_s3_class(mod_r, "rtMod")
})

test_that("GLMNET Binary Classification succeeds", {
  mod_c <- s_GLMNET(datc2_train, datc2_test, alpha = .5, lambda = .01)
  expect_s3_class(mod_c, "rtModClass")
})

test_that("GLMNET 3-class Classification succeeds", {
  mod_c <- s_GLMNET(datc3_train, datc3_test, alpha = .5, lambda = .01)
  expect_s3_class(mod_c, "rtModClass")
})

## GAM ----
test_that("GAM Regression succeeds", {
  mod_r <- s_GAM(datr_train, datr_test)
  expect_s3_class(mod_r, "rtMod")
})

test_that("GAM Binary Classification succeeds", {
  # Suppress warning for Newton step
  # "Fitting terminated with step failure - check results carefully"
  suppressWarnings(mod_c <- s_GAM(datc2_train, datc2_test))
  expect_s3_class(mod_c, "rtModClass")
})

## CART ----
test_that("CART Regression succeeds", {
  mod_r <- s_CART(datr_train, datr_test)
  expect_s3_class(mod_r, "rtMod")
})

test_that("CART Binary Classification succeeds", {
  mod_c <- s_CART(datc2_train, datc2_test)
  expect_s3_class(mod_c, "rtModClass")
})

test_that("CART 3-class Classification succeeds", {
  mod_c <- s_CART(datc3_train, datc3_test)
  expect_s3_class(mod_c, "rtModClass")
})

## Ranger ----
test_that("Ranger Regression succeeds", {
  mod_r <- s_Ranger(datr_train, datr_test)
  expect_s3_class(mod_r, "rtMod")
})

test_that("Ranger Binary Classification succeeds", {
  mod_c <- s_Ranger(datc2_train, datc2_test)
  expect_s3_class(mod_c, "rtModClass")
})

test_that("Ranger 3-class Classification succeeds", {
  mod_c <- s_Ranger(datc3_train, datc3_test)
  expect_s3_class(mod_c, "rtModClass")
})

## LightRF ----
test_that("LightRF Regression succeeds", {
  mod_r <- s_LightRF(datr_train, datr_test, nrounds = 20)
  expect_s3_class(mod_r, "rtMod")
})

test_that("LightRF Binary Classification succeeds", {
  mod_c <- s_LightRF(datc2_train, datc2_test, nrounds = 20)
  expect_s3_class(mod_c, "rtModClass")
})

test_that("LightRF 3-class Classification succeeds", {
  mod_c <- s_LightRF(datc3_train, datc3_test, nrounds = 20)
  expect_s3_class(mod_c, "rtModClass")
})

## LightGBM ----
test_that("LightGBM Regression succeeds", {
  mod_r <- s_LightGBM(datr_train, datr_test, force_nrounds = 20)
  expect_s3_class(mod_r, "rtMod")
})

test_that("LightGBM Binary Classification succeeds", {
  mod_c <- s_LightGBM(datc2_train, datc2_test, force_nrounds = 20)
  expect_s3_class(mod_c, "rtModClass")
})

test_that("LightGBM 3-class Classification succeeds", {
  mod_c <- s_LightGBM(datc3_train, datc3_test, force_nrounds = 20)
  expect_s3_class(mod_c, "rtModClass")
})

## LightRuleFit ----
test_that("LightRuleFit Regression succeeds", {
  mod_r <- s_LightRuleFit(datr_train, datr_test, n_trees = 20)
  expect_s3_class(mod_r, "rtMod")
})

test_that("LightRuleFit Binary Classification succeeds", {
  mod_c <- s_LightRuleFit(datc2_train, datc2_test, n_trees = 20)
  expect_s3_class(mod_c, "rtModClass")
})

## Isotonic Regression ----
test_that("Isotonic Regression succeeds", {
  mod_r <- s_Isotonic(datr_train[, 5:6], datr_test[, 5:6])
  expect_s3_class(mod_r, "rtMod")
})

test_that("Isotonic Binary Classification succeeds", {
  mod_c <- s_Isotonic(datc2_train[, 4:5], datc2_test[, 4:5])
  expect_s3_class(mod_c, "rtModClass")
})
