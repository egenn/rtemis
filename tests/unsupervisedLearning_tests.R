# unsupervisedLearning_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

# Library ----
library(rtemis)

# Data ----
x <- rnormmat(100, 10, seed = 2018)

# Clustering ----
if (requireNamespace("e1071", quietly = TRUE)) {
  clust <- u.CMEANS(x, k = 2)
}

if (requireNamespace("EMCluster", quietly = TRUE)) {
  clust <- u.EMC(x, k = 2)
}

if (requireNamespace("h2o", quietly = TRUE)) {
  clust <- u.H2OKMEANS(x, k = 2)
}

if (requireNamespace("flexclust", quietly = TRUE)) {
  clust <- u.HARDCL(x, k = 2)
  clust <- u.KMEANS(x, k = 2)
  clust <- u.NGAS(x, k = 2)
}

if (requireNamespace("hopach", quietly = TRUE)) {
  clust <- u.HOPACH(x)
}

if (requireNamespace("cluster", quietly = TRUE)) {
  clust <- u.PAM(x, k = 2)
}

if (requireNamespace("fpc", quietly = TRUE)) {
  clust <- u.PAMK(x, krange = 2:3)
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  clust <- u.SPEC(x, k = 2)
}

# Decomposition ----
if (requireNamespace("rCUR", quietly = TRUE)) {
  decom <- d.CUR(x)
}

if (requireNamespace("h2o", quietly = TRUE)) {
  decom <- d.H2OAE(x, n.hidden.nodes = 2)
  decom <- d.H2OGLRM(x, k = 2)
}

if (requireNamespace("fastICA", quietly = TRUE)) {
  decom <- d.ICA(x, k = 2)
}

if (requireNamespace("vegan", quietly = TRUE)) {
  decom <- d.ISOMAP(x, k = 2)
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  decom <- d.KPCA(x, k = 2)
}

if (requireNamespace("lle", quietly = TRUE)) {
  decom <- d.LLE(x, k = 2)
}

decom <- d.MDS(x, k = 2)

if (requireNamespace("NMF", quietly = TRUE)) {
  decom <- d.NMF(abs(x), k = 2)
}

decom <- d.PCA(x)

if (requireNamespace("nsprcomp", quietly = TRUE)) {
  decom <- d.SPCA(x, k = 2)
}

decom <- d.SVD(x, k = 2)

if (requireNamespace("Rtsne", quietly = TRUE)) {
  decom <- d.TSNE(x, k = 2)
}

if (requireNamespace("uwot", quietly = TRUE)) {
  decom <- d.UMAP(x, k = 2)
}
