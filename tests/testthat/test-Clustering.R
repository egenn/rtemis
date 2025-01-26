# test-Clustering.R
# ::rtemis::
# EDG rtemis.org

# Clustering Tests ----
options(rt.font = "sans")

# Data ----
x <- rnormmat(nrow = 100, ncol = 10)

# Tests ----
select_clust()

## CMeans ----
test_that("CMeans Clustering succeeds", {
  skip_if_not_installed("e1071")
  clust <- c_CMeans(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## EMCluster ----
test_that("EMCluster Clustering succeeds", {
  skip_if_not_installed("EMCluster")
  clust <- c_EMC(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## H2OKMeans ----
test_that("H2OKMeans Clustering succeeds", {
  skip_if_not_installed("h2o")
  clust <- c_H2OKMeans(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## HARDCL ----
test_that("HARDCL Clustering succeeds", {
  skip_if_not_installed("flexclust")
  clust <- c_HARDCL(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## KMeans ----
test_that("KMeans Clustering succeeds", {
  skip_if_not_installed("flexclust")
  clust <- c_KMeans(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## NGAS ----
test_that("NGAS Clustering succeeds", {
  skip_if_not_installed("flexclust")
  clust <- c_NGAS(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## HOPACH ----
# sometimes randomly fails
# test_that("HOPACH Clustering succeeds", {
#     skip_if_not_installed("hopach")
#     clust <- c_HOPACH(x)
#     expect_s3_class(clust, "rtClust")
# })

## PAM ----
test_that("PAM Clustering succeeds", {
  skip_if_not_installed("cluster")
  clust <- c_PAM(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## PAMK ----
test_that("PAMK Clustering succeeds", {
  skip_if_not_installed("fpc")
  clust <- c_PAMK(x, krange = 2:3)
  expect_s3_class(clust, "rtClust")
})

## SPEC ----
test_that("SPEC Clustering succeeds", {
  skip_if_not_installed("kernlab")
  clust <- c_SPEC(x, k = 2)
  expect_s3_class(clust, "rtClust")
})

## MeanShift ----
test_that("MeanShift Clustering succeeds", {
  skip_if_not_installed("meanShiftR")
  clust <- c_MeanShift(x)
  expect_s3_class(clust, "rtClust")
})
