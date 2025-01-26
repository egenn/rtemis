# test-Interactive_Graphics.R
# ::rtemis::
# EDG rtemis.org

options(rt.font = "sans")

# dplot3_box ----
test_that("dplot3_box succeeds", {
  expect_type(dplot3_box(iris[, 1:3]), "list")
})

# dplot3_bar ----
test_that("dplot3_bar succeeds", {
  x1 <- rnorm(10)
  names(x1) <- paste0("c", seq(x1))
  expect_type(dplot3_bar(x1), "list")
})

# dplot3_x ----
test_that("dplot3_x histogram succeeds", {
  expect_type(dplot3_x(rnorm(10), "hist"), "list")
})

test_that("dplot3_x density succeeds", {
  expect_type(dplot3_x(rnorm(10), "d"), "list")
})

# dplot3_xy ----
test_that("dplot3_xy succeeds", {
  expect_type(dplot3_xy(rnorm(10), rnorm(10)), "list")
})
