# test-Supervised.R
# ::rtemis::
# EDG rtemis.org

# Data ----
## Regression ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.frame(x, g, y)
resr <- resample(datr)
datr_train <- datr[resr$Fold_1, ]
datr_test <- datr[-resr$Fold_1, ]

## Classification ----
### binary ----
datc2 <- iris[51:150, ]
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Fold_1, ]
datc2_test <- datc2[-resc2$Fold_1, ]

### 3-class ----
resc3 <- resample(iris)
datc3_train <- iris[resc3$Fold_1, ]
datc3_test <- iris[-resc3$Fold_1, ]

# CART ----
hyperparameters <- setup_CART(prune.cp = c(.001, .01, .1))
hyperparameters
hyperparameters <- setup_CART(prune.cp = c(.001, .01, .1), minsplit = c(2L, 10L))
hyperparameters
hyperparameters <- setup_CART(prune.cp = c(.001, .01, .1), minsplit = c(2L, 10L), minbucket = c(1L, 10L))
hyperparameters
hyperparameters <- setup_CART(maxdepth = c(1L, 3L, 10L))
hyperparameters

# Regression ----

## train_CART() ----
test_that("train_CART() succeeds", {
  mod_r_rpart <- train_CART(x = datr_train, dat_testing = datr_test)
  expect_s3_class(mod_r_rpart, "rpart")
})

## CART Regression ----
# model$method # "anova"
test_that("train() Regression succeeds", {
  mod_r_cart <- train(
    datr_train,
    dat_testing = datr_test,
    algorithm = "cart"
  )
  expect_s7_class(mod_r_cart, Regression)
})

## CART Regression + grid search ----
tuner_parameters <- setup_GridSearch()
tuner_parameters
test_that("train() Regression with grid_search() succeeds", {
  mod_r_cart_tuned <- train(
    datr_train,
    dat_testing = datr_test,
    hyperparameters = setup_CART(
      maxdepth = c(1, 2, 10),
      minbucket = c(1L, 4L)
    )
  )
  expect_s7_class(mod_r_cart_tuned, Regression)
})

## CART Regression CV ----
hyperparameters <- setup_CART()
crossvalidation_parameters <- setup_Resampler(n_resamples = 10L, type = "KFold")
crossvalidation_parameters
future::plan()
test_that("train() Regression with crossvalidation succeeds", {
  mod_r_cart_cv <- train(
    x = datr,
    crossvalidation_parameters = crossvalidation_parameters
  )
  expect_s7_class(mod_r_cart_cv, RegressionCV)
})

## GLMNET ----
hyperparameters <- setup_GLMNET()
hyperparameters
hyperparameters <- setup_GLMNET(alpha = c(0, 0.5, 1))
hyperparameters
get_params_need_tuning(hyperparameters)

## train_GLMNET ----
test_that("train_GLMNET() succeeds", {
  mod_r_glmnet <- train_GLMNET(
    x = datr_train, dat_testing = datr_test,
    hyperparameters = setup_GLMNET(lambda = 0.05)
  )
  expect_s3_class(mod_r_glmnet, "glmnet")
})

## GLMNET Regression ----
test_that("train() GLMNET Regression with fixed lambda succeeds", {
  mod_r_glmnet <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(lambda = 0.01)
  )
  expect_s7_class(mod_r_glmnet, Regression)
})

test_that("train() GLMNET Regression with auto-lambda grid search succeeds", {
  mod_r_glmnet <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET()
  )
  expect_s7_class(mod_r_glmnet, Regression)
})

test_that("train() GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  mod_r_glmnet <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(alpha = c(0, 0.5, 1))
  )
  expect_s7_class(mod_r_glmnet, Regression)
})

test_that("train() CV-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  mod_r_glmnet <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(alpha = c(0, 0.5, 1)),
    crossvalidation_parameters = setup_Resampler(n_resamples = 5L, type = "KFold")
  )
  expect_s7_class(mod_r_glmnet, RegressionCV)
})

## LightRF Regression ----
test_that("train() LightRF Regression succeeds", {
  mod_r_lightrf <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "lightrf"
  )
  expect_s7_class(mod_r_lightrf, Regression)
})

test_that("train() LightRF Regression with l1, l2 succeeds", {
  mod_r_lightgbm <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "lightrf",
    hyperparameters = setup_LightRF(
      lambda_l1 = .1,
      lambda_l2 = .1
    )
  )
  expect_s7_class(mod_r_lightgbm, Regression)
})

test_that("train() LightRF Regression with l1 tuning succeeds", {
  mod_r_lightgbm <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "lightrf",
    hyperparameters = setup_LightRF(
      lambda_l1 = c(0, .1, 1)
    )
  )
  expect_s7_class(mod_r_lightgbm, Regression)
})


# Binary Classification ----

## CART Classification ----
# model <- train_CART(dat_training = datc2_train, dat_testing = datc2_test)
# model$method #"class"
test_that("train() CART Classification succeeds", {
  mod_c_cart <- train(
    x = datc2_train,
    dat_testing = datc2_test,
    algorithm = "cart"
  )
  expect_s7_class(mod_c_cart, Classification)
})

## CART Classification + grid search ----
test_that("train() Classification with grid_search() succeeds", {
  mod_c_cart_tuned <- train(
    x = datc2_train,
    dat_testing = datc2_test,
    hyperparameters = setup_CART(
      maxdepth = c(1L, 2L, 5L),
      minbucket = c(1L, 4L)
    )
  )
  expect_s7_class(mod_c_cart_tuned, Classification)
})

## GLMNET Binary Classification ----
test_that("train() GLMNET Classification with fixed lambda succeeds", {
  mod_c_glmnet <- train(
    x = datc2_train,
    dat_testing = datc2_test,
    algorithm = "glmnet",
    hyperparameters = setup_GLMNET(lambda = 0.01)
  )
  expect_s7_class(mod_c_glmnet, Classification)
})

## LightRF Classification ----
test_that("train() LightRF Classification succeeds", {
  mod_c_lightrf <- train(
    x = datc2_train,
    dat_testing = datc2_test,
    algorithm = "lightrf"
  )
  expect_s7_class(mod_c_lightrf, Classification)
})

## LightRF CV Classification ----
test_that("train() LightRF Classification with crossvalidation succeeds", {
  mod_c_lightrf_cv <- train(
    x = datc2,
    algorithm = "lightrf",
    crossvalidation_parameters = setup_Resampler(n_resamples = 5L, type = "KFold")
  )
  expect_s7_class(mod_c_lightrf_cv, ClassificationCV)
})

# Multiclass Classification ----

# train() GLMNET Multiclass Classification ----
# test_that("train() GLMNET Multiclass Classification with fixed lambda succeeds", {
#   mod_c_glmnet <- train(
#     x = datc3_train,
#     dat_testing = datc3_test,
#     algorithm = "glmnet",
#     hyperparameters = setup_GLMNET(lambda = 0.01)
#   )
#   expect_s7_class(mod_c_glmnet, Classification)
# })
