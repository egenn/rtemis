# test_colorsystem.R
# ::rtemis::
# 2025 EDG rtemis.org

# show_col ----
x <- list(
  highlight_col = highlight_col,
  col_object = col_object,
  col_outer = col_outer,
  col_tuner = col_tuner,
  col_info = col_info
)
# show_col(x, title = "rtemis Color System") |> cat()
out <- show_col(x, title = "rtemis Color System")
test_that("show_col() works", {
  expect_true(is.character(out))
})
