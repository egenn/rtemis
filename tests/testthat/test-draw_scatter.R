# test-draw_scatter.R
# ::rtemis::
# 2025 EDG rtemis.org

# draw_scatter ----
test_that("draw_scatter creates a plotly object", {
  p <- draw_scatter(iris$Sepal.Length, iris$Petal.Length)
  expect_s3_class(p, "plotly")
})

# draw_scatter with fit = "gam" ----
x <- rnorm(500)
y <- x^3 + rnorm(500)
test_that("draw_scatter fit='gam' creates a plotly object", {
  p <- draw_scatter(x, y, fit = "gam")
  expect_s3_class(p, "plotly")
})
