# test-Clustering.R
# ::rtemis::
# 2025 EDG rtemis.org

# Data ----
x <- iris[, -5]

# KMeansParameters ----
test_that("KMeansParameters() succeeds", {
  expect_s7_class(
    KMeansParameters(k = 3L, dist = "euclidean"),
    KMeansParameters
  )
})

# setup_KMeans ----
test_that("setup_KMeans() succeeds", {
  expect_s7_class(setup_KMeans(), KMeansParameters)
})

# setup_KMeans throws error ----
test_that("setup_KMeans() throws error", {
  expect_error(setup_KMeans(k = -1L))
  expect_error(setup_KMeans(dist = "foo"))
})

# cluster KMeans ----
iris_kmeans <- cluster(x, algorithm = "kmeans")
iris_kmeans
test_that("cluster_KMeans() succeeds", {
  expect_s7_class(iris_kmeans, Clustering)
})

# cluster KMeans with k = 10 ----
iris_kmeans10 <- cluster(
  x,
  algorithm = "kmeans",
  parameters = setup_KMeans(k = 10L)
)
iris_kmeans10
test_that("cluster_KMeans() with k = 10 succeeds", {
  expect_s7_class(iris_kmeans10, Clustering)
})

# setup_HardCL ----
test_that("setup_HardCL() succeeds", {
  expect_s7_class(setup_HardCL(), HardCLParameters)
})

# cluster HardCL ----
iris_hardcl <- cluster(x, algorithm = "HardCL")
iris_hardcl
test_that("cluster_HardCL() succeeds", {
  expect_s7_class(iris_hardcl, Clustering)
})

# setup_NeuralGas ----
test_that("setup_NeuralGas() succeeds", {
  expect_s7_class(setup_NeuralGas(), NeuralGasParameters)
})

# cluster NeuralGas ----
iris_neuralgas <- cluster(x, algorithm = "NeuralGas")
iris_neuralgas
test_that("cluster_NeuralGas() succeeds", {
  expect_s7_class(iris_neuralgas, Clustering)
})

# setup_CMeans ----
test_that("setup_CMeans() succeeds", {
  expect_s7_class(setup_CMeans(), CMeansParameters)
})

# cluster CMeans ----
parameters <- setup_CMeans(k = 3L)
parameters
iris_cmeans <- cluster(x, algorithm = "CMeans")
test_that("cluster_CMeans() succeeds", {
  expect_s7_class(iris_cmeans, Clustering)
})
