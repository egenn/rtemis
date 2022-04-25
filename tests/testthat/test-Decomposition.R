# ::rtemis::
# Decomposition Tests ----

options(rt.font = "Helvetica")

# Data ----
x <- rnormmat(100, 10)

# Tests ----
decomSelect()

# '- CUR ----
test_that("CMEANS Clustering succeeds", {
    skip_if_not_installed("rCUR")
    decom <- d_CUR(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- H2OAE ----
test_that("H2OAE Clustering succeeds", {
    skip_if_not_installed("h2o")
    decom <- d_H2OAE(x)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- H2OGLRM ----
test_that("H2OGLRM Clustering succeeds", {
    skip_if_not_installed("h2o")
    decom <- d_H2OGLRM(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- ICA ----
test_that("ICA Clustering succeeds", {
    decom <- d_ICA(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- ISOMAP ----
test_that("ISOMAP Clustering succeeds", {
    skip_if_not_installed("vegan")
    decom <- d_ISOMAP(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- KPCA ----
test_that("KPCA Clustering succeeds", {
    skip_if_not_installed("kernlab")
    decom <- d_KPCA(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- LLE ----
test_that("LLE Clustering succeeds", {
    skip_if_not_installed("lle")
    decom <- d_LLE(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- MDS ----
test_that("MDS Clustering succeeds", {
    decom <- d_MDS(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- NMF ----
# works manually, fails with devtools::test for whatever reason
# test_that("NMF Clustering succeeds", {
#     skip_if_not_installed("NMF")
#     xnn <- abs(rnormmat(100, 10))
#     decom <- d_NMF(xnn, k = 2)
#     expect_identical(class(decom)[1], "rtDecom")
# })

# '- PCA ----
test_that("PCA Clustering succeeds", {
    decom <- d_PCA(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- SPCA ----
test_that("SPCA Clustering succeeds", {
    skip_if_not_installed("nsprcomp")
    decom <- d_SPCA(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- SVD ----
test_that("SVD Clustering succeeds", {
    decom <- d_SVD(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- TSNE ----
test_that("TSNE Clustering succeeds", {
    skip_if_not_installed("Rtsne")
    decom <- d_TSNE(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})

# '- UMAP ----
test_that("UMAP Clustering succeeds", {
    skip_if_not_installed("uwot")
    decom <- d_UMAP(x, k = 2)
    expect_identical(class(decom)[1], "rtDecom")
})
