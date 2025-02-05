# test-Hyperparameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# Hyperparameters ----
hpr <- Hyperparameters(
  algorithm = "Custom",
  hyperparameters = list(alpha = c(0, 1), beta = 2),
  tunable_hyperparameters = "alpha",
  fixed_hyperparameters = "beta"
)
test_that("Hyperparameters succeeds", {
  expect_s7_class(hpr, Hyperparameters)
})

test_that("`tuned` is set correctly", {
  expect_identical(hpr@tuned, 0L)
})

# get_params_need_tuning ----
test_that("get_params_need_tuning() succeeds", {
  expect_type(get_params_need_tuning(hpr), "list")
})

# CARTHyperparameters ----
cart_hpr <- CARTHyperparameters()
test_that("CARTHyperparameters succeeds", {
  expect_s7_class(cart_hpr, CARTHyperparameters)
})

# setup_CART ----
test_that("setup_CART() succeeds", {
  expect_s7_class(setup_CART(), CARTHyperparameters)
})

# needs_tuning ----
test_that("needs_tuning() succeeds", {
  expect_type(needs_tuning(cart_hpr), "logical")
})

# GLMNETHyperparameters ----
test_that("GLMNETHyperparameters succeeds", {
  expect_s7_class(GLMNETHyperparameters(), GLMNETHyperparameters)
})

# setup_GLMNET ----
test_that("setup_GLMNET() succeeds", {
  expect_s7_class(setup_GLMNET(), GLMNETHyperparameters)
})

# LightRFHyperparameters ----
test_that("LightRFHyperparameters succeeds", {
  expect_s7_class(LightRFHyperparameters(), LightRFHyperparameters)
})

# setup_LightRF ----
test_that("setup_LightRF() succeeds", {
  expect_s7_class(setup_LightRF(), LightRFHyperparameters)
})
