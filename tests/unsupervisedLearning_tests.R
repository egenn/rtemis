# unsupervisedLearning_tests.R
# ::rtemis::
# 2018 E.D. Gennatas egenn.github.io

# Library ====
library(rtemis)

# Data ====
x <- rnormmat(100, 10, seed = 2018)

# Clustering ====
clustSelect()
if (requireNamespace("e1071", quietly = TRUE)) {
  clust <- c_CMEANS(x, k = 2)
  clust
}

if (requireNamespace("EMCluster", quietly = TRUE)) {
  clust <- c_EMC(x, k = 2)
  clust
}

if (requireNamespace("h2o", quietly = TRUE)) {
  clust <- c_H2OKMEANS(x, k = 2)
  clust
}

if (requireNamespace("flexclust", quietly = TRUE)) {
  if (requireNamespace("R.utils", quietly = TRUE)) {
    clust <- clust(x, k = 2)
  }
  clust <- c_HARDCL(x, k = 2)
  clust
  clust <- c_KMEANS(x, k = 2)
  clust
  clust <- c_NGAS(x, k = 2)
  clust
}

if (requireNamespace("hopach", quietly = TRUE)) {
  clust <- c_HOPACH(x)
  clust
}

if (requireNamespace("cluster", quietly = TRUE)) {
  clust <- c_PAM(x, k = 2)
  clust
}

if (requireNamespace("fpc", quietly = TRUE)) {
  clust <- c_PAMK(x, krange = 2:3)
  clust
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  clust <- c_SPEC(x, k = 2)
  clust
}

if (requireNamespace("meanShiftR", quietly = TRUE)) {
  clust <- c_MEANSHIFT(x)
  clust
}

# Decomposition ====
decomSelect()
x1 <- svd1(x)
if (requireNamespace("rCUR", quietly = TRUE)) {
  decom <- d_CUR(x)
  decom
}

if (requireNamespace("h2o", quietly = TRUE)) {
  decom <- d_H2OAE(x, n.hidden.nodes = 2)
  decom
  decom <- d_H2OGLRM(x, k = 2)
  decom
}

if (requireNamespace("fastICA", quietly = TRUE)) {
  if (requireNamespace("R.utils", quietly = TRUE)) {
    decom <- decom(x, k = 2)
  }
  decom <- d_ICA(x, k = 2)
  decom
}

if (requireNamespace("vegan", quietly = TRUE)) {
  decom <- d_ISOMAP(x, k = 2)
  decom
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  decom <- d_KPCA(x, k = 2)
  decom
}

if (requireNamespace("lle", quietly = TRUE)) {
  decom <- d_LLE(x, k = 2, n.cores = 1)
  decom
}

decom <- d_MDS(x, k = 2)
decom

if (requireNamespace("NMF", quietly = TRUE)) {
  decom <- d_NMF(abs(x), k = 2)
  decom
}

decom <- d_PCA(x)
decom

if (requireNamespace("nsprcomp", quietly = TRUE)) {
  decom <- d_SPCA(x, k = 2)
  decom
}

decom <- d_SVD(x, k = 2)
decom

if (requireNamespace("Rtsne", quietly = TRUE)) {
  decom <- d_TSNE(x, k = 2)
  decom
}

if (requireNamespace("uwot", quietly = TRUE)) {
  decom <- d_UMAP(x, k = 2)
  decom
}
