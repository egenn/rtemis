# unsupervisedLearning_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

# Library ----
library(rtemis)

# Data ----
x <- rnormmat(100, 10, seed = 2018)

# Clustering ----
clust <- u.CMEANS(x, k = 2)
if (requireNamespace("EMCluster", quietly = TRUE)) {
clust <- u.EMC(x, k = 2)
}
if (requireNamespace("h2o", quietly = TRUE)) {
clust <- u.H2OKMEANS(x, k = 2)
}
if (requireNamespace("flexclust", quietly = TRUE)) {
clust <- u.HARDCL(x, k = 2)
}
if (requireNamespace("hopach", quietly = TRUE)) {
clust <- u.HOPACH(x)
}
if (requireNamespace("flexclust", quietly = TRUE)) {
clust <- u.KMEANS(x, k = 2)
clust <- u.NGAS(x, k = 2)
}
clust <- u.PAM(x, k = 2)
clust <- u.PAMK(x, krange = 2:3)
clust <- u.SPEC(x, k = 2)

# Decomposition ----
if (requireNamespace("rCUR", quietly = TRUE)) {
decom <- d.CUR(x)
}
if (requireNamespace("h2o", quietly = TRUE)) {
decom <- d.H2OAE(x, n.hidden.nodes = 2)
decom <- d.H2OGLRM(x, k = 2)
}
decom <- d.ICA(x, k = 2)
decom <- d.ISOMAP(x, k = 2)
decom <- d.KPCA(x, k = 2)
if (requireNamespace("lle", quietly = TRUE)) {
decom <- d.LLE(x, k = 2)
}
decom <- d.MDS(x, k = 2)
decom <- d.NMF(abs(x), k = 2)
decom <- d.PCA(x)
if (requireNamespace("nsprcomp", quietly = TRUE)) {
decom <- d.SPCA(x, k = 2)
}
decom <- d.SVD(x, k = 2)
decom <- d.TSNE(x, k = 2)
if (requireNamespace("uwot", quietly = TRUE)) {
decom <- d.UMAP(x, k = 2)
}