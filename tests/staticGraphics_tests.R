# staticGraphics_tests.R
# ::rtemis::
# 2018 Efstathios D. Gennatas MBBS AICSM PhD egenn.github.io

library(rtemis)

# Synth data ====
x <- rnorm(50)
y <- .33 * x + 12 + rnorm(50)
z <- rnormmat(10, 5, seed = 2018)

true <- sample(0:1, 20, TRUE)
estimated <- true
index <- sample(seq(20), 5)
estimated[index] <- 1 - as.numeric(estimated[index])

# mhist ====
mhist(x)

# mplot3 ====
mplot3(x)

# mplot3.adsr ====
mplot3.adsr()

# mplot3.bar ====
# '- a. vector ====
set.seed(2019)
x1 <- rnorm(10)
names(x1) <- paste0("C_", seq(x1))
mplot3.bar(x1, theme = "lightgrid")

# '- b. matrix ====
x1 <- rnormmat(3, 6, seed = 2019)
colnames(x1) <- paste("Feat", seq(6))
rownames(x1) <- paste("Case", seq(nrow(x1)))
mplot3.bar(x1)
mplot3.bar(x1, col = colorGrad(nrow(x1), lo = pennCol$lighterBlue, mid = pennCol$red, hi = "white")[seq(nrow(x1))])
mplot3.bar(x1, legend = T)

# mplot3.box ====
mplot3.box(z, theme = "darkbox")
mplot3.box(z, theme = "lightgrid")

# mplot3.xy ===
mplot3.xy(x, y)
mplot3.xy(x, list(Raw = x * .3,
                  N1 = x * .3 + .5*rnorm(50),
                  N2 = x * .3 + rnorm(50),
                  N2 = x * .3 + 1.5*rnorm(50),
                  N3 = x * .3 + 2*rnorm(50),
                  N4 = x * .3 + 2.5*rnorm(50)),
          fit = 'glm', theme = "darkbox")
mplot3.xym(x, y)

iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
mod <- s.CART(iris2, maxdepth = 2)

if (requireNamespace("data.tree", quietly = TRUE)) {
  mplot3.cart(mod)
}

# mplot3.conf ====
mplot3.conf(mod)
mplot3.confbin(mod$error.train$ConfusionMatrix)

# mplot3.decision(mod, iris2)
# TODO: INV mplot3.decision

# mplot3.fit ====
mplot3.fit(x, x + rnorm(50))

# mplot3.fret ====
mplot3.fret()

# mplot3.harmonograph ====
mplot3.harmonograph()

# mplot3.heatmap ====
mplot3.heatmap(cor(z))

# mplot3.img ====
mplot3.img(z)

# mplot3.res ====
res <- resample(y)
mplot3.res(res)

# mplot3.roc ====
mplot3.roc(mod$fitted.prob, iris2$Species, method = "rt")

# rtRandom ====
rtRandom()
