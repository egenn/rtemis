# unsupervisedLearning_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas egenn.github.io

# Library ----
library(rtemis)

# Data ----
x <- rnormmat(100, 10, seed = 2018)

# Clustering ----
clust <- u.CMEANS(x, k = 2)
clust <- u.EMC(x, k = 2)
clust <- u.H2OKMEANS(x, k = 2)
clust <- u.HARDCL(x, k = 2)
clust <- u.HOPACH(x)
clust <- u.KMEANS(x, k = 2)
clust <- u.NGAS(x, k = 2)
clust <- u.PAM(x, k = 2)
clust <- u.PAMK(x, krange = 2:3)
clust <- u.SPEC(x, k = 2)

# Decomposition ----
decom <- d.CUR(x)
decom <- d.H2OAE(x, n.hidden.nodes = 2)
decom <- d.H2OGLRM(x, k = 2)
decom <- d.ICA(x, k = 2)
decom <- d.ISOMAP(x, k = 2)
decom <- d.KPCA(x, k = 2)
decom <- d.LLE(x, k = 2)
decom <- d.MDS(x, k = 2)
decom <- d.NMF(abs(x), k = 2)
decom <- d.PCA(x)
decom <- d.SPCA(x, k = 2)
decom <- d.SVD(x, k = 2)
decom <- d.TSNE(x, k = 2)
decom <- d.UMAP(x, k = 2)
