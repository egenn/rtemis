# test-Static_Graphics.R
# ::rtemis::
# EDG rtemis.org

options(rt.font = "sans")

# mplot3_box ----
test_that("mplot3_box succeeds", {
  expect_type(mplot3_box(iris[, 1:3]), "list")
})

# mplot3_bar ----
test_that("mplot3_bar succeeds", {
  x1 <- rnorm(10)
  names(x1) <- paste0("c", seq(x1))
  expect_type(mplot3_bar(x1), "double")
})

# mplot3_x ----
test_that("mplot3_x histogram succeeds", {
  expect_type(mplot3_x(rnorm(10), "hist"), "list")
})

test_that("mplot3_x density succeeds", {
  expect_type(mplot3_x(rnorm(10), "d"), "list")
})

# mplot3_xy ----
test_that("mplot3_xy succeeds", {
  expect_type(mplot3_xy(rnorm(10), rnorm(10)), "list")
})
