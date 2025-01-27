# test-Supervised.R
# ::rtemis::
# EDG rtemis.org

# Data ----
## Regression ----
x <- rnormmat(200, 5, seed = 2025)
y <- x[, 3] + x[, 5] + rnorm(200)
datr <- data.frame(x, y)
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

# train Regression ----
# model <- train_CART(dat_training = datr_train, dat_testing = datr_test)
# model$method # "anova"
test_that("train() Regression succeeds", {
  mod_r_cart <- train(
    dat_training = datr_train,
    dat_testing = datr_test,
    algorithm = "cart"
  )
  expect_s7_class(mod_r_cart, Regression)
})

# train Regression + grid search ----
tuner_parameters <- setup_GridSearch()
tuner_parameters
test_that("train() Regression with grid_search() succeeds", {
  mod_r_cart_tuned <- train(
    dat_training = datr_train,
    dat_testing = datr_test,
    hyperparameters = setup_CART(
      maxdepth = c(1, 2, 10),
      minbucket = c(1L, 4L)
    )
  )
  expect_s7_class(mod_r_cart_tuned, Regression)
})

# train Regression CV ----
hyperparameters <- setup_CART()
crossvalidation <- setup_Resampler(n_resamples = 10L, type = "KFold")
crossvalidation
future::plan()
test_that("train() Regression with crossvalidation succeeds", {
  mod_r_cart_cv <- train(
    dat_training = datr,
    crossvalidation = crossvalidation
  )
  expect_s7_class(mod_r_cart_cv, RegressionCV)
})

# train Classification ----
# model <- train_CART(dat_training = datc2_train, dat_testing = datc2_test)
# model$method #"class"
test_that("train() Classification succeeds", {
  mod_c_cart <- train(
    dat_training = datc2_train,
    dat_testing = datc2_test,
    algorithm = "cart"
  )
  expect_s7_class(mod_c_cart, Classification)
})

# train Classification + grid search ----
test_that("train() Classification with grid_search() succeeds", {
  mod_c_cart_tuned <- train(
    dat_training = datc2_train,
    dat_testing = datc2_test,
    hyperparameters = setup_CART(
      maxdepth = c(1L, 2L, 5L),
      minbucket = c(1L, 4L)
    )
  )
  expect_s7_class(mod_c_cart_tuned, Classification)
})
