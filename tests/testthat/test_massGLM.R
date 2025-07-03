# test_MassGLM.R
# ::rtemis::
# 2025 EDG rtemis.org

# library(rtemis)
# library(data.table)
# library(testthat)
set.seed(2022)
y <- data.table(rnormmat(500, 40))
x <- data.table(
  x1 = y[[3]] - y[[5]] + y[[14]] + rnorm(500),
  x2 = y[[21]] + rnorm(500)
)

# massGLM ----
massmod <- massGLM(x, y)
test_that("massGLM creates MassGLM object", {
  expect_s7_class(massmod, MassGLM)
})

# plot.MassGLM ----
test_that("plot.MassGLM creates plotly object", {
  plt <- plot(massmod)
  expect_s3_class(plt, "plotly")
})
