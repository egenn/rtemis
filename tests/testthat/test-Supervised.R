# test-Supervised.R
# ::rtemis::
# EDG rtemis.org

# Setup progressr ----
progressr::handlers(global = TRUE)
progressr::handlers("cli")

# Data ----
## Regression Data ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.frame(x, g, y)
resr <- resample(datr)
datr_train <- datr[resr$Fold_1, ]
datr_test <- datr[-resr$Fold_1, ]

## Classification Data ----
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

# Regression ----

## train_CART() ----
test_that("train_CART() succeeds", {
  mod_r_rpart <- train_CART(x = datr_train)
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
hyperparameters <- setup_CART(
  maxdepth = c(1, 2, 10),
  minbucket = c(1L, 4L)
)

# Test that tuned == 0----
test_that("tuned is set correctly", {
  expect_identical(hyperparameters@tuned, 0L)
})

mod_r_cart_tuned <- train(
  datr_train,
  dat_testing = datr_test,
  hyperparameters = hyperparameters
)
test_that("train() Regression with grid_search() succeeds", {
  expect_s7_class(mod_r_cart_tuned, Regression)
})

# Test that tuned == 1----
test_that("tuned is set correctly", {
  expect_identical(mod_r_cart_tuned@hyperparameters@tuned, 1L)
})

## CV CART Regression ----
hyperparameters <- setup_CART()
crossvalidation_parameters <- setup_Resampler(n_resamples = 10L, type = "KFold")
crossvalidation_parameters
future::plan()
cvmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(),
  crossvalidation_parameters = setup_Resampler()
)
test_that("train() Regression with crossvalidation succeeds", { 
  expect_s7_class(cvmod_r_cart, RegressionCV)
})

## CV CART Regression + tuning ----
crossvalidation_parameters <- setup_Resampler(n_resamples = 10L, type = "KFold")
crossvalidation_parameters
future::plan()
cvtmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(maxdepth = c(1, 2, 10)),
  crossvalidation_parameters = setup_Resampler()
)
test_that("train() Regression with crossvalidation succeeds", {
  expect_s7_class(cvtmod_r_cart, RegressionCV)
})

cvmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(prune.cp = c(.001, .01)),
  crossvalidation_parameters = setup_Resampler()
)
test_that("train() Regression with crossvalidation succeeds", {
  expect_s7_class(cvmod_r_cart, RegressionCV)
})

## GLMNET ----
hyperparameters <- setup_GLMNET()
hyperparameters
hyperparameters <- setup_GLMNET(alpha = c(0, 0.5, 1))
hyperparameters
get_params_need_tuning(hyperparameters)

## train_GLMNET ----
mod_r_glmnet <- train_GLMNET(
  x = datr_train,
  hyperparameters = setup_GLMNET(lambda = 0.05)
)
test_that("train_GLMNET() succeeds", {
  expect_s3_class(mod_r_glmnet, "glmnet")
})

## GLMNET Regression ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(lambda = 0.01)
)
test_that("train() GLMNET Regression with fixed lambda succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## GLMNET Regression + auto-lambda grid search ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET()
)
test_that("train() GLMNET Regression with auto-lambda grid search succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## GLMNET Regression + auto-lambda + alpha grid search ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(alpha = c(0, 0.5, 1))
)
test_that("train() GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## CV GLMNET Regression + auto-lambda + alpha grid search ----
cvmod_r_glmnet <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(alpha = c(0, 0.5, 1)),
  crossvalidation_parameters = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() CV-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  expect_s7_class(cvmod_r_glmnet, RegressionCV)
})

## LightRF Regression ----
mod_r_lightrf <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "lightrf"
)
test_that("train() LightRF Regression succeeds", { 
  expect_s7_class(mod_r_lightrf, Regression)
})

mod_r_lightrf <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    lambda_l1 = .1,
    lambda_l2 = .1
  )
)
test_that("train() LightRF Regression with l1, l2 succeeds", {
  expect_s7_class(mod_r_lightrf, Regression)
})

## LightRF Regression + grid search ----
tmod_r_lightgbm <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    lambda_l1 = c(0, .1, 1)
  )
)
test_that("train() LightRF Regression with l1 tuning succeeds", {
  expect_s7_class(tmod_r_lightgbm, Regression)
})

## LightGBM Regression ----
mod_r_lightgbm <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "lightgbm",
  hyperparameters = setup_LightGBM(
    force_nrounds = 100L
  )
)
test_that("train() LightGBM Regression succeeds", {
  expect_s7_class(mod_r_lightgbm, Regression)
})

## LightGBM Regression + autotune nrounds ----
tmod_r_lightgbm <- train(
  x = datr_train,
  dat_testing = datr_test,
  hyperparameters = setup_LightGBM()
)
test_that("train() LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(tmod_r_lightgbm, Regression)
})

## CV LightGBM Regression + autotune nrounds grid search ----
test_that("train() LightGBM Regression with autotune nrounds succeeds", {
  cvmod_r_lightgbm <- train(
    x = datr_train,
    dat_testing = datr_test,
    algorithm = "lightgbm",
    hyperparameters = setup_LightGBM(),
    crossvalidation_parameters = setup_Resampler(n_resamples = 5L, type = "KFold")
  )
  expect_s7_class(mod_r_lightgbm, RegressionCV)
})

## LightRuleFit Regression ----
mod_r_lightrlft <- train(
  x = datr_train,
  dat_testing = datr_test,
  algorithm = "lightrulefit",
  hyperparameters = setup_LightRuleFit()
)

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
