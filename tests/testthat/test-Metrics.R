# test-Metrics.R
# ::rtemis::
# 2025 EDG rtemis.org

# Regression Data ----
set.seed(2025)
true <- rnorm(500)
predicted <- true + rnorm(500) / 2
predicted2 <- true + rnorm(500) / 2

# RegressionMetrics ----
reg_metrics <- regression_metrics(true, predicted, sample = "Training")
reg_metrics
test_that("regression_metrics() succeeds", {
  expect_s7_class(regression_metrics(true, predicted), RegressionMetrics)
})
reg_metrics2 <- regression_metrics(true, predicted2, sample = "Testing")

# Classification Data ----
true <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
predicted <- factor(c("a", "b", "a", "b", "b", "a", "b", "b", "b", "a"))
predicted_prob <- 1 - c(0.7, 0.45, 0.55, 0.25, 0.43, 0.7, 0.2, .37, .38, .61)
predicted2 <- factor(c("b", "a", "a", "b", "b", "b", "a", "a", "b", "a"))
predicted_prob2 <- 1 - c(0.3, 0.55, 0.45, 0.75, 0.57, 0.3, 0.8, .63, .62, .39)

# ClassificationMetrics ----
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
class_metrics2 <- classification_metrics(true, predicted2, predicted_prob2, sample = "Testing")


# RegressionMetricsCV ----
cv_metrics <- list(mod1 = reg_metrics, mod2 = reg_metrics2)
rmcv <- RegressionMetricsCV(
  sample = "Testing",
  cv_metrics = cv_metrics
)
rmcv

# ClassificationMetricsCV ----
cv_metrics <- list(mod1 = class_metrics, mod2 = class_metrics2)
cmcv <- ClassificationMetricsCV(
  sample = "Testing",
  cv_metrics = cv_metrics
)
cmcv
