# test-Supervised.R
# ::rtemis::
# EDG rtemis.org

# Setup ----
# progressr::handlers(global = TRUE)
# progressr::handlers("cli")
# devtools::load_all()
# library(testthat)
library(data.table)

# Data ----
## Regression Data ----
n <- 400
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.table(x, g, y)
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
# data(Sonar, package = "mlbench")
# datc2 <- Sonar
# resc2 <- resample(datc2)
# datc2_train <- datc2[resc2$Fold_1, ]
# datc2_test <- datc2[-resc2$Fold_1, ]

### Synthetic binary data where positive class is 10% of the data ----
# set.seed(2025)
# n <- 500
# datc2 <- data.frame(
#   x1 = rnorm(n),
#   x2 = rnorm(n),
#   x3 = rnorm(n),
#   g = factor(sample(c("A", "B"), n, replace = TRUE, prob = c(.1, .9)))
# )
# # Binary outcome dependent on x2 and g, with levels "neg" and "pos", where "pos" is 10% of the data
# datc2$y <- factor(ifelse(datc2$x2 > 0 & datc2$g == "A", "pos", "neg"))
# resc2 <- resample(datc2)
# datc2_train <- datc2[resc2$Fold_1, ]
# datc2_test <- datc2[-resc2$Fold_1, ]

### 3-class ----
# resc3 <- resample(iris)
# datc3_train <- iris[resc3$Fold_1, ]
# datc3_test <- iris[-resc3$Fold_1, ]

# Utils ----
test_that("class_imbalance() works", {
  expect_type(class_imbalance(outcome(datc2)), "double")
})

# Regression ----
## GLM Regression ----
mod_r_glm <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glm"
)
test_that("train() GLM Regression succeeds", {
  expect_s7_class(mod_r_glm, Regression)
})
test_that("train() GLM standard errors are available", {
  expect_type(mod_r_glm@se_training, "double")
  expect_type(mod_r_glm@se_test, "double")
})

## GLM Regression predict ----
predicted <- predict(mod_r_glm, features(datr_test))
test_that("predict() GLM Regression succeeds", {
  expect_identical(mod_r_glm@predicted_test, predicted)
  expect_null(dim(predicted))
})

## Throw error when missing data is passed to GLM ----
datr_train_na <- datr_train
datr_train_na[10:2, 1] <- NA
test_that("train() GLM Regression with missing data throws error", {
  expect_error(
    train(x = datr_train_na, dat_test = datr_test, algorithm = "glm")
  )
})

## Res GLM Regression ----
resmod_r_glm <- train(
  x = datr,
  algorithm = "glm",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() Res GLM Regression succeeds", {
  expect_s7_class(resmod_r_glm, RegressionRes)
})

## GLMNET ----
hyperparameters <- setup_GLMNET()
hyperparameters
hyperparameters <- setup_GLMNET(alpha = c(0, 0.5, 1))
hyperparameters
get_params_need_tuning(hyperparameters)

## GLMNET Regression ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(lambda = 0.01)
)
test_that("train() GLMNET Regression with fixed lambda succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## GLMNET Regression predict ----
predicted <- predict(mod_r_glmnet, features(datr_test))
test_that("predict() GLMNET Regression succeeds", {
  expect_identical(mod_r_glmnet@predicted_test, predicted)
  expect_null(dim(predicted))
})

## GLMNET Regression + auto-lambda grid search ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET()
)
test_that("train() GLMNET Regression with auto-lambda grid search succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## GLMNET Regression + auto-lambda + alpha grid search ----
mod_r_glmnet <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(alpha = c(0, 1))
)
test_that("train() GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  expect_s7_class(mod_r_glmnet, Regression)
})

## Res GLMNET Regression + auto-lambda + alpha grid search ----
resmod_r_glmnet <- train(
  x = datr_train,
  algorithm = "glmnet",
  hyperparameters = setup_GLMNET(alpha = c(0.5, 1)),
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() Res-GLMNET Regression with auto-lambda + alpha grid search succeeds", {
  expect_s7_class(resmod_r_glmnet, RegressionRes)
})

## GAM Regression ----
hyperparameters <- setup_GAM()
hyperparameters
mod_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "gam"
)
test_that("train() GAM Regression with spline + parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

mod_r_gam <- train(
  x = datr_train[, -6],
  dat_test = datr_test[, -6],
  algorithm = "gam"
)
test_that("train() GAM Regression with only spline terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

mod_r_gam <- train(
  x = datr_train[, 6:7],
  dat_test = datr_test[, 6:7],
  algorithm = "gam"
)
test_that("train() GAM Regression with only parametric terms succeeds.", {
  expect_s7_class(mod_r_gam, Regression)
})

## GAM Regression + grid search ----
tmod_r_gam <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "gam",
  hyperparameters = setup_GAM(k = c(3, 5, 7)),
)
test_that("train() GAM Regression with grid_search() succeeds", {
  expect_s7_class(tmod_r_gam, Regression)
})

## predict GAM ----
predicted <- predict(tmod_r_gam, datr_test)
test_that("predict() GAM Regression succeeds", {
  expect_identical(tmod_r_gam@predicted_test, predicted)
})

## Res GAM Regression ----
resmod_r_gam <- train(
  x = datr,
  algorithm = "gam",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)

## Test ... args to train ---
# mod_r_gam_aa <- train(
#   x = datr_train,
#   dat_test = datr_test,
#   algorithm = "gam",
#   k = 7L
# )
# test_that("train() GAM Regression with ... args succeeds", {
#   expect_s7_class(mod_r_gam_aa, Regression)
#   expect_identical(mod_r_gam_aa@hyperparameters$k, 7L)
# })

## SVM Regression ----
mod_r_svmr <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "svm",
  hyperparameters = setup_RadialSVM()
)
test_that("train() SVM Regression succeeds", {
  expect_s7_class(mod_r_svmr, Regression)
})

## SVM Regression + tuning ----
tmod_r_svmr <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "svm",
  hyperparameters = setup_RadialSVM(cost = c(1, 10, 100))
)
test_that("train() SVM Regression with tuning succeeds", {
  expect_s7_class(tmod_r_svmr, Regression)
})

## Res SVM Regression ----
resmod_r_svmr <- train(
  x = datr,
  algorithm = "svm",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() Res SVM Regression succeeds", {
  expect_s7_class(resmod_r_svmr, RegressionRes)
})

## Res SVM Regression + tuning ----
restmod_r_svmr <- train(
  x = datr,
  algorithm = "svm",
  hyperparameters = setup_RadialSVM(cost = c(1, 10)),
  outer_resampling = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res SVM Regression with tuning succeeds", {
  expect_s7_class(restmod_r_svmr, RegressionRes)
})

## CART Regression ----
mod_r_cart <- train(
  datr_train,
  dat_test = datr_test,
  algorithm = "cart"
)
test_that("train() Regression succeeds", {
  expect_s7_class(mod_r_cart, Regression)
})

## CART Regression + grid search ----
tuner_parameters <- setup_GridSearch()
tuner_parameters
hyperparameters <- setup_CART(
  maxdepth = c(1, 2, 10),
  minbucket = c(1L, 4L)
)

## Test that tuned == 0----
test_that("tuned field is set correctly", {
  expect_identical(hyperparameters@tuned, 0L)
})

tmod_r_cart <- train(
  datr_train,
  dat_test = datr_test,
  hyperparameters = hyperparameters
)
test_that("train() Regression with grid_search() succeeds", {
  expect_s7_class(tmod_r_cart, Regression)
})

## Test that tuned == 1----
test_that("tuned is set correctly", {
  expect_identical(tmod_r_cart@hyperparameters@tuned, 1L)
})

## Res CART Regression ----
resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(),
  outer_resampling = setup_Resampler(3L)
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmod_r_cart, RegressionRes)
})

## Res CART Regression + tuning ----
restmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(maxdepth = c(1, 2)),
  outer_resampling = setup_Resampler(3L)
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(restmod_r_cart, RegressionRes)
})

resmod_r_cart <- train(
  x = datr,
  hyperparameters = setup_CART(prune_cp = c(.001, .01)),
  outer_resampling = setup_Resampler(3L)
)
test_that("train() RegressionRes succeeds", {
  expect_s7_class(resmod_r_cart, RegressionRes)
})

## LightCART Regression ----
mod_r_lightcart <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightcart"
)
test_that("train() LightCART Regression succeeds", {
  expect_s7_class(mod_r_lightcart, Regression)
})

mod_r_lightcartlin <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightcart",
  hyperparameters = setup_LightCART(
    linear_tree = TRUE
  )
)
test_that("train() LightCART Regression with linear_tree succeeds", {
  expect_s7_class(mod_r_lightcartlin, Regression)
  expect_identical(
    mod_r_lightcartlin@hyperparameters$linear_tree,
    mod_r_lightcartlin@model$params$linear_tree
  )
})

## LightRF Regression ----
mod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(num_threads = 8L)
)
test_that("train() LightRF Regression succeeds", {
  expect_s7_class(mod_r_lightrf, Regression)
})

mod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
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
tmod_r_lightrf <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightrf",
  hyperparameters = setup_LightRF(
    lambda_l1 = c(0, .1)
  ),
  parallel_type = "none"
)
test_that("train() LightRF Regression with l1 tuning succeeds", {
  expect_s7_class(tmod_r_lightrf, Regression)
})

## LightGBM Regression ----
mod_r_lightgbm <- train(
  x = datr_train,
  dat_test = datr_test,
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
  dat_test = datr_test,
  hyperparameters = setup_LightGBM()
)
test_that("train() LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(tmod_r_lightgbm, Regression)
})

## Res LightGBM Regression + autotune nrounds grid search ----
restmod_r_lightgbm <- train(
  x = datr_train,
  hyperparameters = setup_LightGBM(max_nrounds = 50L),
  outer_resampling = setup_Resampler(n_resamples = 3L, type = "KFold")
)
test_that("train() Res LightGBM Regression with autotune nrounds succeeds", {
  expect_s7_class(restmod_r_lightgbm, RegressionRes)
})

## LightRuleFit Regression ----
mod_r_lightrlft_l1l2 <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "lightrulefit",
  hyperparameters = setup_LightRuleFit(
    nrounds = 50L,
    lambda_l1 = 10,
    lambda_l2 = 10
  )
)

test_that("train() LightRuleFit Regression with l1, l2 params passed", {
  expect_s7_class(mod_r_lightrlft_l1l2, Regression)
  expect_identical(
    mod_r_lightrlft_l1l2@model@model_lightgbm@model$params$lambda_l1,
    10
  )
  expect_identical(
    mod_r_lightrlft_l1l2@model@model_lightgbm@model$params$lambda_l2,
    10
  )
})

# mod_r_lightrlft_reg <- train(
#   x = datr_train,
#   dat_test = datr_test,
#   algorithm = "lightrulefit",
#   hyperparameters = setup_LightRuleFit(num_leaves = 2^2, lambda_l1 = 100)
# )

# TabNet Regression ----
## TabNet Regression ----
# Test if lantern is installed
if (torch::torch_is_installed()) {
  mod_r_tabnet <- train(
    x = datr_train,
    dat_test = datr_test,
    algorithm = "tabnet",
    hyperparameters = setup_TabNet(epochs = 3L, learn_rate = .01)
  )
  test_that("train() TabNet Regression succeeds", {
    expect_s7_class(mod_r_tabnet, Regression)
  })
}

# Binary Classification ----
## GLM Classification ----
mod_c_glm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "glm"
)
test_that("train() GLM Classification succeeds", {
  expect_s7_class(mod_c_glm, Classification)
})

## GLM Classification IFW ----
mod_c_glm_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "glm",
  hyperparameters = setup_GLM(ifw = TRUE)
)
test_that("train() GLM Classification with IFW succeeds", {
  expect_s7_class(mod_c_glm_ifw, Classification)
})

## GLM ClassificationRes ----
resmod_c_glm <- train(
  x = datc2,
  algorithm = "glm",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() GLM ClassificationRes succeeds", {
  expect_s7_class(resmod_c_glm, ClassificationRes)
})

## GAM Classification ----
mod_c_gam <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "gam"
)
test_that("train() GAM Classification succeeds", {
  expect_s7_class(mod_c_gam, Classification)
})

## GAM Classification IFW ----
mod_c_gam_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "gam",
  hyperparameters = setup_GAM(ifw = TRUE)
)
test_that("train() GAM Classification with IFW succeeds", {
  expect_s7_class(mod_c_gam_ifw, Classification)
})

## CART Classification ----
# model <- train_CART(dat_training = datc2_train, dat_test = datc2_test)
# model$method #"class"
mod_c_cart <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "cart"
)
test_that("train() CART Classification succeeds", {
  expect_s7_class(mod_c_cart, Classification)
})

## Res CART Classification ----
resmod_c_cart <- train(
  x = datc2,
  algorithm = "cart",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() CART ClassificationRes succeeds", {
  expect_s7_class(resmod_c_cart, ClassificationRes)
})

## CART Classification + IFW ----
mod_c_cart_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "cart",
  hyperparameters = setup_CART(
    ifw = TRUE
  )
)
test_that("train() CART Classification with IFW succeeds", {
  expect_s7_class(mod_c_cart_ifw, Classification)
})

## CART Classification + grid search ----
mod_c_cart_tuned <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_CART(
    maxdepth = c(1L, 3L)
  )
)
test_that("train() Classification with grid_search() succeeds", {
  expect_s7_class(mod_c_cart_tuned, Classification)
})

## GLMNET Binary Classification ----
mod_c_glmnet <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLMNET(ifw = FALSE)
)
test_that("train() GLMNET Binary Classification succeeds", {
  expect_s7_class(mod_c_glmnet, Classification)
})

## GLMNET Binary Classification IFW ----
mod_c_glmnet_ifw <- train(
  x = datc2_train,
  dat_test = datc2_test,
  hyperparameters = setup_GLMNET(ifw = TRUE, lambda = .001)
)
test_that("train() GLMNET Binary Classification with IFW & fixed lambda succeeds", {
  expect_s7_class(mod_c_glmnet_ifw, Classification)
})

## LightCART Classification ----
mod_c_lightcart <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightcart"
)
test_that("train() LightCART Classification succeeds", {
  expect_s7_class(mod_c_lightcart, Classification)
})

## LightRF Classification ----
mod_c_lightrf <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightrf"
)
test_that("train() LightRF Classification succeeds", {
  expect_s7_class(mod_c_lightrf, Classification)
})

## LightRF Res Classification ----
resmod_c_lightrf <- train(
  x = datc2,
  algorithm = "lightrf",
  outer_resampling = setup_Resampler(n_resamples = 5L, type = "KFold")
)
test_that("train() LightRF ClassificationRes succeeds", {
  expect_s7_class(resmod_c_lightrf, ClassificationRes)
})

## LightGBM Binary Classification ----
mod_c_lightgbm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightgbm",
  # hyperparameters = setup_LightGBM(
  #   force_nrounds = 100L
  # ),
  tuner_parameters = setup_GridSearch(
    resampler_parameters = setup_Resampler(
      n_resamples = 3L,
      type = "KFold"
    )
  )
)
test_that("train() LightGBM Classification succeeds", {
  expect_s7_class(mod_c_lightgbm, Classification)
})

## LightRuleFit Binary Classification ----
mod_c_lightrlft <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "lightrulefit"
)
test_that("train() LightRuleFit Classification succeeds", {
  expect_s7_class(mod_c_lightrlft, Classification)
})

# Isotonic Regression ----
x <- rnorm(50)
y <- x^5 + rnorm(50)
dat <- data.table(x, y)
mod_iso <- train(dat, algorithm = "Isotonic")
test_that("train() Isotonic Regression succeeds", {
  expect_s7_class(mod_iso, Regression)
})

# Isotonic Classification ----
set.seed(2025)
x <- rnorm(200)
y <- factor(ifelse(x > mean(x), "b", "a"))
x <- x + rnorm(200) / 3
dat <- data.frame(x, y)
cmod_iso <- train(dat, algorithm = "Isotonic")
test_that("train() Isotonic Classification succeeds", {
  expect_s7_class(cmod_iso, Classification)
})

# SVM Classification ----
mod_c_svm <- train(
  x = datc2_train,
  dat_test = datc2_test,
  algorithm = "svm"
)
test_that("train() SVM Classification succeeds", {
  expect_s7_class(mod_c_svm, Classification)
})

# TabNet Classification ----
if (torch::torch_is_installed()) {
  mod_c_tabnet <- train(
    x = datc2_train,
    dat_test = datc2_test,
    algorithm = "tabnet",
    hyperparameters = setup_TabNet(epochs = 3L, learn_rate = .01)
  )
  test_that("train() TabNet Classification succeeds", {
    expect_s7_class(mod_c_tabnet, Classification)
  })
}

# Predict SupervisedRes ----

## Res CART Regression ----
predicted_mean <- predict(resmod_r_cart, newdata = features(datr_test))
test_that("predict() SupervisedRes succeeds", {
  expect_true(length(predicted_mean) == nrow(datr_test))
})


# Calibration ----
# Calibrate mod_c_cart trained above
model <- mod_c_lightrf
predicted_probabilities <- model$predicted_prob_training
true_labels <- model$y_training
mod_c_lightrf_cal <- calibrate(
  mod_c_lightrf,
  predicted_probabilities = mod_c_lightrf$predicted_prob_training,
  true_labels = mod_c_lightrf$y_training
)
test_that("calibrate() succeeds on Classification", {
  expect_s7_class(mod_c_lightrf_cal, CalibratedClassification)
})

# Predict CalibratedClassification ----
newdata <- features(datc2_test)
predicted_prob_test_cal <- predict(mod_c_lightrf_cal, newdata = newdata)
test_that("predict() CalibratedClassification succeeds", {
  expect_identical(
    mod_c_lightrf_cal@predicted_prob_test_calibrated,
    predicted_prob_test_cal
  )
})

# CalibrationRes ----
resmod_c_lightrf_cal <- calibrate(resmod_c_lightrf)
test_that("calibrate() succeeds on ClassificationRes", {
  expect_s7_class(resmod_c_lightrf_cal, CalibratedClassificationRes)
})

# 1. Describe Regression ----
test_that("describe.Regression returns character", {
  desc <- describe(mod_r_glm)
  expect_type(desc, "character")
})

# 2. Plot Regression ----
test_that("plot.Supervised creates a plotly object", {
  p <- plot(mod_r_glm)
  expect_s3_class(p, "plotly")
})

# 3. Plot True Pred Regression ----
test_that("plot_true_pred creates a plotly object", {
  p <- plot_true_pred(mod_r_glm)
  expect_s3_class(p, "plotly")
})

# 4. Present Regression ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_r_glm)
  expect_s3_class(p, "plotly")
})

# 1. Describe Classification ----
test_that("describe.Classification returns character", {
  desc <- describe(mod_c_glm)
  expect_type(desc, "character")
})

# 2. Plot Classification ----
test_that("plot.Supervised creates a plotly object", {
  p <- plot(mod_c_glm)
  expect_s3_class(p, "plotly")
})

# 3. Plot True Pred Classification ----
test_that("plot_true_pred creates a plotly object", {
  p <- plot_true_pred(mod_c_glm)
  expect_s3_class(p, "plotly")
})

# 4. Plot ROC Classification ----
test_that("draw_roc creates a plotly object", {
  p <- draw_roc(
    true_labels = list(
      Training = mod_c_glm@y_training,
      Test = mod_c_glm@y_test
    ),
    predicted_prob = list(
      Training = mod_c_glm@predicted_prob_training,
      Test = mod_c_glm@predicted_prob_test
    )
  )
  expect_s3_class(p, "plotly")
})
test_that("plot_roc.Classification creates a plotly object", {
  p <- plot_roc(mod_c_glm)
  expect_s3_class(p, "plotly")
})

# Plot RegressionRes ----
test_that("plot.SupervisedRes creates a plotly object", {
  p <- plot(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

# Plot ClassificationRes ----
test_that("plot.SupervisedRes creates a plotly object", {
  p <- plot(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

# Plot True Pred RegressionRes ----
test_that("plot_true_pred RegressionRes creates a plotly object", {
  p <- plot_true_pred(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

# Plot True Pred ClassificationRes ----
test_that("plot_true_pred ClassificationRes creates a plotly object", {
  p <- plot_true_pred(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

# Present Regression ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_r_glm)
  expect_s3_class(p, "plotly")
})

# Present Classification ----
test_that("present.Supervised creates a plotly object", {
  p <- present(mod_c_glm)
  expect_s3_class(p, "plotly")
})

# Present RegressionRes object ----
test_that("present() RegressionRes object creates a plotly object", {
  p <- present(resmod_r_glm)
  expect_s3_class(p, "plotly")
})

# Present ClassificationRes object ----
test_that("present() ClassificationRes object creates a plotly object", {
  p <- present(resmod_c_glm)
  expect_s3_class(p, "plotly")
})

## Present multiple RegressionRes objects ----
test_that("present() multiple RegressionRes objects creates a plotly object", {
  p <- present(list(resmod_r_glm, resmod_r_cart))
  expect_s3_class(p, "plotly")
})

## Present multiple ClassificationRes objects ----
test_that("present() multiple ClassificationRes objects creates a plotly object", {
  p <- present(list(resmod_c_glm, resmod_c_cart))
  expect_s3_class(p, "plotly")
})

## Present multiple Regression objects ----
test_that("present() multiple Regression objects creates a plotly object", {
  p <- present(list(mod_r_glm, mod_r_cart))
  expect_s3_class(p, "plotly")
})

## plot_varimp RegressionRes ----
test_that("plot_varimp RegressionRes creates a plotly object", {
  p <- plot_varimp(resmod_r_cart)
  expect_s3_class(p, "plotly")
})

# Save model using outdir ----
mod_r_glm <- train(
  x = datr_train,
  dat_test = datr_test,
  algorithm = "glm",
  outdir = "./__out/mod_r_glm"
)
test_that("train saves model to rds successfully", {
  expect_true(file.exists("./__out/mod_r_glm/train_GLM.rds"))
})
