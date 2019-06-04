# unsupervisedLearning_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

# Library ====
library(rtemis)

# Data ====
x <- rnormmat(100, 10, seed = 2018)

# Clustering ====
clustSelect()
if (requireNamespace("e1071", quietly = TRUE)) {
  clust <- u.CMEANS(x, k = 2)
  clust
}

if (requireNamespace("EMCluster", quietly = TRUE)) {
  clust <- u.EMC(x, k = 2)
  clust
}

if (requireNamespace("h2o", quietly = TRUE)) {
  clust <- u.H2OKMEANS(x, k = 2)
  clust
}

if (requireNamespace("flexclust", quietly = TRUE)) {
  if (requireNamespace("R.utils", quietly = TRUE)) {
    clust <- clust(x, k = 2)
  }
  clust <- u.HARDCL(x, k = 2)
  clust
  clust <- u.KMEANS(x, k = 2)
  clust
  clust <- u.NGAS(x, k = 2)
  clust
}

if (requireNamespace("hopach", quietly = TRUE)) {
  clust <- u.HOPACH(x)
  clust
}

if (requireNamespace("cluster", quietly = TRUE)) {
  clust <- u.PAM(x, k = 2)
  clust
}

if (requireNamespace("fpc", quietly = TRUE)) {
  clust <- u.PAMK(x, krange = 2:3)
  clust
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  clust <- u.SPEC(x, k = 2)
  clust
}

# Decomposition ====
decomSelect()
x1 <- svd1(x)
if (requireNamespace("rCUR", quietly = TRUE)) {
  decom <- d.CUR(x)
  decom
}

if (requireNamespace("h2o", quietly = TRUE)) {
  decom <- d.H2OAE(x, n.hidden.nodes = 2)
  decom
  decom <- d.H2OGLRM(x, k = 2)
  decom
}

if (requireNamespace("fastICA", quietly = TRUE)) {
  if (requireNamespace("R.utils", quietly = TRUE)) {
    decom <- decom(x, k = 2)
  }
  decom <- d.ICA(x, k = 2)
  decom
}

if (requireNamespace("vegan", quietly = TRUE)) {
  decom <- d.ISOMAP(x, k = 2)
  decom
}

if (requireNamespace("kernlab", quietly = TRUE)) {
  decom <- d.KPCA(x, k = 2)
  decom
}

if (requireNamespace("lle", quietly = TRUE)) {
  decom <- d.LLE(x, k = 2)
  decom
}

decom <- d.MDS(x, k = 2)
decom

if (requireNamespace("NMF", quietly = TRUE)) {
  decom <- d.NMF(abs(x), k = 2)
  decom
}

decom <- d.PCA(x)
decom

if (requireNamespace("nsprcomp", quietly = TRUE)) {
  decom <- d.SPCA(x, k = 2)
  decom
}

decom <- d.SVD(x, k = 2)
decom

if (requireNamespace("Rtsne", quietly = TRUE)) {
  decom <- d.TSNE(x, k = 2)
  decom
}

if (requireNamespace("uwot", quietly = TRUE)) {
  decom <- d.UMAP(x, k = 2)
  decom
}
