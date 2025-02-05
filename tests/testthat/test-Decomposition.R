# test-Decompositiong.R
# ::rtemis::
# 2025 EDG rtemis.org

# Data ----
x <- iris[, -5]

# PCA ----
parameters <- setup_PCA()
parameters
test_that("setup_PCA() succeeds", {
  expect_s7_class(parameters, PCAParameters)
})
iris_pca <- decompose(x, algorithm = "pca", parameters = parameters)
iris_pca
test_that("decompose() PCA succeeds", {
  expect_s7_class(iris_pca, Decomposition)
})

# ICA ----
parameters <- setup_ICA()
parameters
test_that("setup_ICA() succeeds", {
  expect_s7_class(parameters, ICAParameters)
})
iris_ica <- decompose(x, algorithm = "ica", parameters = parameters)
iris_ica
test_that("decompose() ICA succeeds", {
  expect_s7_class(iris_ica, Decomposition)
})

# UMAP ----
parameters <- setup_UMAP()
parameters
test_that("setup_UMAP() succeeds", {
  expect_s7_class(parameters, UMAPParameters)
})
iris_umap <- decompose(x, algorithm = "umap", parameters = parameters)
iris_umap <- decompose(x, algorithm = "umap", parameters = setup_UMAP(n_neighbors = 20L))

# t-SNE ----
parameters <- setup_tSNE()
parameters
