# test-Decomposition.R
# ::rtemis::
# Decomposition Tests ----

options(rt.font = "sans")

# Data ----
x <- rnormmat(100, 10, seed = 2015)

# Tests ----
expect_equal(select_decom(), 9)

## CUR ----
# test_that("CUR Clustering succeeds", {
#     skip_if_not_installed("rCUR")
#     decom <- d_CUR(x, k = 2)
#     expect_s3_class(decom, "rtDecom")
# })

## H2OAE ----
test_that("H2OAE Clustering succeeds", {
  skip_if_not_installed("h2o")
  decom <- d_H2OAE(x)
  expect_s3_class(decom, "rtDecom")
})

## H2OGLRM ----
test_that("H2OGLRM Clustering succeeds", {
  skip_if_not_installed("h2o")
  decom <- d_H2OGLRM(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## ICA ----
test_that("ICA Clustering succeeds", {
  decom <- d_ICA(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## ISOMAP ----
test_that("Isomap Clustering succeeds", {
  skip_if_not_installed("vegan")
  decom <- d_Isomap(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## KPCA ----
test_that("KPCA Clustering succeeds", {
  skip_if_not_installed("kernlab")
  decom <- d_KPCA(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## LLE ----
test_that("LLE Clustering succeeds", {
  skip_if_not_installed("RDRToolbox")
  decom <- d_LLE(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## MDS ----
test_that("MDS Clustering succeeds", {
  decom <- d_MDS(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## NMF ----
# works manually, used to fail with devtools::test()
test_that("NMF Clustering succeeds", {
  skip_if_not_installed("NMF")
  xnn <- abs(rnormmat(100, 10))
  decom <- d_NMF(xnn, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## PCA ----
test_that("PCA Clustering succeeds", {
  decom <- d_PCA(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## SPCA ----
test_that("SPCA Clustering succeeds", {
  skip_if_not_installed("nsprcomp")
  decom <- d_SPCA(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## SVD ----
test_that("SVD Clustering succeeds", {
  decom <- d_SVD(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## TSNE ----
test_that("TSNE Clustering succeeds", {
  skip_if_not_installed("Rtsne")
  decom <- d_TSNE(x, k = 2)
  expect_s3_class(decom, "rtDecom")
})

## UMAP ----
# 2023-12-09: requires Matrix and irlba dependencies to be installed from source
# See [GitHub issue](https://github.com/jlmelville/uwot/issues/115)
# and [related comment](https://github.com/bwlewis/irlba/issues/70#issuecomment-1826900769)
# test_that("UMAP Clustering succeeds", {
#   decom <- d_UMAP(x, k = 2)
#   expect_s3_class(decom, "rtDecom")
# })
