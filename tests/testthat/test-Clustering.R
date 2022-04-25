# ::rtemis::
# Clustering Tests ----

options(rt.font = "sans")

# Data ----
x <- rnormmat(100, 10)

# Tests ----
clustSelect()

# '- CMEANS ----
test_that("CMEANS Clustering succeeds", {
    skip_if_not_installed("e1071")
    clust <- c_CMEANS(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- EMCluster ----
test_that("EMCluster Clustering succeeds", {
    skip_if_not_installed("EMCluster")
    clust <- c_EMC(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- H2OKMEANS ----
test_that("H2OKMEANS Clustering succeeds", {
    skip_if_not_installed("h2o")
    clust <- c_H2OKMEANS(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- HARDCL ----
test_that("HARDCL Clustering succeeds", {
    skip_if_not_installed("flexclust")
    clust <- c_HARDCL(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- KMEANS ----
test_that("KMEANS Clustering succeeds", {
    skip_if_not_installed("flexclust")
    clust <- c_KMEANS(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- NGAS ----
test_that("NGAS Clustering succeeds", {
    skip_if_not_installed("flexclust")
    clust <- c_NGAS(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- HOPACH ----
test_that("HOPACH Clustering succeeds", {
    skip_if_not_installed("hopach")
    clust <- c_HOPACH(x)
    expect_identical(class(clust)[1], "rtClust")
})

# '- PAM ----
test_that("PAM Clustering succeeds", {
    skip_if_not_installed("cluster")
    clust <- c_PAM(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- PAMK ----
test_that("PAMK Clustering succeeds", {
    skip_if_not_installed("fpc")
    clust <- c_PAMK(x, krange = 2:3)
    expect_identical(class(clust)[1], "rtClust")
})

# '- SPEC ----
test_that("SPEC Clustering succeeds", {
    skip_if_not_installed("kernlab")
    clust <- c_SPEC(x, k = 2)
    expect_identical(class(clust)[1], "rtClust")
})

# '- MEANSHIFT ----
test_that("MEANSHIFT Clustering succeeds", {
    skip_if_not_installed("meanShiftR")
    clust <- c_MEANSHIFT(x)
    expect_identical(class(clust)[1], "rtClust")
})
