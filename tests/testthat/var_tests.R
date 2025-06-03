# var_tests.R
# ::rtemmis::
# 2019- EDG rtemis.org

options(rt.font = "sans")

# Library  ----
library(rtemisalpha)

# Var ----
dat <- as.data.frame(rnormmat(50, 2))

# '- cc ----
cc(iris$Species[1:10], iris$Species[51:60])

# '- classImbalance ----
classImbalance(iris$Species)

# '- colorOp ----
colorOp("#8c1515", fn = "invert")

# '- previewcolor ----
previewcolor(sample(colors(), 20))

# '- cols2list ----
lst <- cols2list(dat)

# '- dat2bsplinemat ----
if (requireNamespace("splines2", quietly = TRUE)) {
  x <- dat2bsplinemat(dat)
}

# '- dat2ploy ----
x <- dat2poly(dat)

# '- delayTime ----
delayTime()

# ' drange ----
drange(runif(10, -10, 10))

# ' fwhm2sigma ----
fwhm2sigma(8)

# ' gtTable ----
lst <- list(one = rnorm(10), two = rnorm(10))
gtTable(lst)

# '- lotri2edgeList
x <- lotri2edgeList(cor(rnormmat(10, 10)))

# '- nCr ----
nCr(4, 2)

# '- oneHot ----
iris.oh <- oneHot(iris)
iris.species.oh <- oneHot(iris$Species)

# '- rsd ----
rsd(rnorm(20))

# '- stderror ----
stderror(rnorm(100))

# '- strict ----
x <- 10
z <- strict(x, "numeric")

# '- synthRegData ----
dat <- synthRegData()

# '- synthMultiModal ----
if (requireNamespace("matrixStats", quietly = TRUE)) {
  dat <- synthMultiModal(verbose = TRUE)
}

# '- timeProc ----
x <- timeProc(sapply(rnorm(100), exp))

# '- typeset ----
x <- typeset(iris, factor.index = 1, orderedfactor.index = 2, integer.index = 3)

# '- varSelect ----
if (requireNamespace("ranger", quietly = TRUE)) {
  x <- rnormmat(100, 10)
  y <- x[, 3]^2 + x[, 5] + x[, 8] * x[, 10]

  vs <- varSelect(x, y)
}

# eightball ----
# eightball("Are you ready?")
