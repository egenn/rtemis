# test-Metrics.R
# ::rtemis::
# 2025 EDG rtemis.org

# Regression ----
set.seed(2025)
true <- rnorm(500)
predicted <- true + rnorm(500) / 2

# Metrics ----
reg_metrics <- regression_metrics(true, predicted, sample = "Training")
reg_metrics
test_that("regression_metrics() succeeds", {
  expect_s7_class(regression_metrics(true, predicted), RegressionMetrics)
})

# Classification ----
true <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
predicted <- factor(c("a", "b", "a", "b", "b", "a", "b", "b", "b", "a"))
predicted_prob <- c(0.7, 0.45, 0.55, 0.25, 0.43, 0.7, 0.2, .37, .38, .61)
class_metrics <- classification_metrics(true, predicted, sample = "Training")
class_metrics
class_metrics <- classification_metrics(true, predicted, predicted_prob, sample = "Training")
class_metrics
test_that("classification_metrics() succeeds", {
  expect_s7_class(
    classification_metrics(true, predicted, predicted_prob, sample = "Training"),
    ClassificationMetrics
  )
})
