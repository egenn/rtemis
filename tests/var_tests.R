# var_tests.R
# ::rtemmis::
#  2019 Efstathios D. Gennatas egenn.github.io

# Library  ====
library(rtemis)

# Var ====
dat <- as.data.frame(rnormmmat(50, 2))
# '- dat2splinemat ====
x <- dat2splinemat(dat)

# '- dat2ploy ====
x <- dat2poly(dat)

# '- delayTime ====
delayTime()

# '- nCr ====
nCr(4, 2)

# '- stderror ====
stderror(rnorm(100))

# '- timeProc ====
x <- timeProc(sapply(rnorm(100), exp))

# '- typeset ====
x <- typeset(iris, factor.index = 1, orderedfactor.index = 2, integer.index = 3)

# '- varSelect ====
x <- rnormmat(100, 10)
y <- x[, 3]^2 + x[, 5] + x[, 8] * x[, 10]
vs <- varSelect(x, y)
