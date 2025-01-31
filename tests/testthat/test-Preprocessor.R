# test-Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

# Preprocessor ----
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(setup_Preprocessor(), Preprocessor)
})


testthat::test_that("setup_Preprocessor() succeeds", {
  prp <- setup_Preprocessor(
    remove_constants = TRUE,
    remove_duplicates = TRUE
  )
  expect_s7_class(prp, Preprocessor)
})
