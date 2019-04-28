# staticGraphics_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas MBBS AICSM PhD egenn.github.io

library(rtemis)

x <- rnorm(50)
y <- .33 * x + 12 + rnorm(50)
z <- rnormmat(10, 5, seed = 2018)

true <- sample(0:1, 20, TRUE)
estimated <- true
index <- sample(seq(20), 5)
estimated[index] <- 1 - as.numeric(estimated[index])

mhist(x)
mplot3(x)
mplot3.adsr()
mplot3.bar(x)
mplot3.box(z)
mplot3.xy(x, y)
mplot3.xym(x, y)

iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
mod <- s.CART(iris2, maxdepth = 2)

mplot3.cart(mod)
mplot3.conf(mod)
mplot3.confbin(mod$error.train$ConfusionMatrix)
# mplot3.decision(mod, iris2)
# TODO: INV mplot3.decision
mplot3.fit(x, x)
mplot3.fret()
mplot3.harmonograph()
mplot3.heatmap(cor(z))
mplot3.img(z)
res <- resample(y)
mplot3.res(res)
mplot3.roc(mod$fitted.prob, iris2$Species)

rtRandom()
