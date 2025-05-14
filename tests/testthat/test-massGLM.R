# test-massGLM.R
# ::rtemis::
# EDG rtemis.org

options(rt.font = "sans")

# Data ----
features <- rnormmat(500, 40)
outcome <- features[, 3] - features[, 5] + features[, 14] + rnorm(500)
massmod <- massGLM(outcome, features)

# Test ----
test_that("massGLM succeeds", {
  expect_identical(class(massmod)[1], "massGLM")
})

test_that("massGLM volcano plot succeeds", {
  out <- plot(massmod, what = "volcano")
  expect_identical(class(out)[1], "plotly")
})

test_that("massGLM manhattan plot succeeds", {
  out <- plot(massmod, what = "coefs")
  expect_identical(class(out)[1], "plotly")
})
