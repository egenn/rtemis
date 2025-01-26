# test-Crossdecomposition.R
# ::rtemis::
# Crossdecomposition Tests ----

options(rt.font = "sans")

# Data ----
x <- rnormmat(50, 10)
z <- rnormmat(50, 10)

# Tests ----
xselect_decom()

# '- CCA ----
test_that("CMEANS Clustering succeeds", {
  skip_if_not_installed("PMA")
  xdecom <- x_CCA(x, z, nperms = 3, permute.niter = 3, n.cores = 1)
  expect_s3_class(xdecom, "rtXDecom")
})
