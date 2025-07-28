# test-Decomposition.R
# ::rtemis::
# 2025 EDG rtemis.org

library(rtemis)
# Data ----
x <- iris[, -5]

# PCA ----
parameters <- setup_PCA()
test_that("setup_PCA() succeeds", {
  expect_s7_class(parameters, PCAParameters)
})
iris_pca <- decomp(x, algorithm = "pca", parameters = parameters)
iris_pca
test_that("decomp() PCA succeeds", {
  expect_s7_class(iris_pca, Decomposition)
})

# ICA ----
parameters <- setup_ICA()
test_that("setup_ICA() succeeds", {
  expect_s7_class(parameters, ICAParameters)
})
iris_ica <- decomp(x, algorithm = "ica", parameters = parameters)
test_that("decomp() ICA succeeds", {
  expect_s7_class(iris_ica, Decomposition)
})

# NMF ----
parameters <- setup_NMF()
test_that("setup_NMF() succeeds", {
  expect_s7_class(parameters, NMFParameters)
})
iris_nmf <- decomp(x, algorithm = "nmf", parameters = parameters)
test_that("decomp() NMF succeeds", {
  expect_s7_class(iris_nmf, Decomposition)
})

# UMAP ----
parameters <- setup_UMAP()
test_that("setup_UMAP() succeeds", {
  expect_s7_class(parameters, UMAPParameters)
})
iris_umap <- decomp(x, algorithm = "umap", parameters = parameters)
iris_umap <- decomp(
  x,
  algorithm = "umap",
  parameters = setup_UMAP(n_neighbors = 20L)
)

# t-SNE ----
parameters <- setup_tSNE()
test_that("setup_tSNE() succeeds", {
  expect_s7_class(parameters, tSNEParameters)
})
# Test that t-SNE fails with duplicates
test_that("decomp() t-SNE fails with duplicates", {
  expect_error(decomp(x, algorithm = "tsne", parameters = parameters))
})

# Test that t-SNE works after removing duplicates
xp <- preprocess(x, setup_Preprocessor(remove_duplicates = TRUE))
iris_tsne <- decomp(
  xp@preprocessed,
  algorithm = "tsne",
  parameters = parameters
)
test_that("decomp() t-SNE succeeds after removing duplicates", {
  expect_s7_class(iris_tsne, Decomposition)
})

# Isomap ----
parameters <- setup_Isomap()
test_that("setup_Isomap() succeeds", {
  expect_s7_class(parameters, IsomapParameters)
})
iris_isomap <- decomp(x, algorithm = "isomap", parameters = parameters)
test_that("decomp() Isomap succeeds", {
  expect_s7_class(iris_isomap, Decomposition)
})
