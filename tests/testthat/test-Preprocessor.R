# test-Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

# PreprocessorParameters ----
prp <- setup_Preprocessor()
prp
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(setup_Preprocessor(), PreprocessorParameters)
})


testthat::test_that("setup_Preprocessor() succeeds", {
  prp <- setup_Preprocessor(
    remove_constants = TRUE,
    remove_duplicates = TRUE
  )
  expect_s7_class(prp, PreprocessorParameters)
})

# preprocess(PreprocessorParameters) ----
res <- resample(iris, setup_Resampler(seed = 2025))
iris_train <- iris[res$Fold_1, ]
iris_test <- iris[-res$Fold_1, ]
iris_Pre <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE)
)
test_that("preprocess(x, PreprocessorParameters) succeeds", {
  expect_s7_class(iris_Pre, Preprocessor)
})
iris_Pre
iris_Pre@preprocessed
iris_Pre@values

iris_test_Pre <- preprocess(iris_test, iris_Pre)
test_that("preprocess(x, Preprocessor) succeeds", {
  expect_s7_class(iris_test_Pre, Preprocessor)
})

iris_Pre_too <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE),
  dat_testing = iris_test
)
test_that("preprocess(x, PreprocessorParameters) succeeds", {
  expect_s7_class(iris_Pre_too, Preprocessor)
})

test_that("preprocess(x, PreprocessorParameters) and preprocess(x, Preprocessor) give same testing set", {
  expect_equal(iris_Pre_too@preprocessed$testing, iris_test_Pre@preprocessed)
})
