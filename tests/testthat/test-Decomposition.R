# test-Decompositiong.R
# ::rtemis::
# 2025 EDG rtemis.org

# Data ----
x <- iris[, -5]

# setup_ICA ----
parameters <- setup_ICA()
parameters
test_that("setup_ICA() succeeds", {
  expect_s7_class(parameters, ICAParameters)
})

# decompose() ICA ----
iris_ica <- decompose(x, algorithm = "ica")
iris_ica
