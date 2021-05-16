# staticGraphics_tests.R
# ::rtemis::
# 2018-20 Efstathios D. Gennatas MBBS AICSM PhD lambdamd.org

library(rtemis)

# Synth data ====
x <- rnorm(50)
y <- .33 * x + 12 + rnorm(50)/3
n.col <- 5
n.row <- 10
z <- rnormmat(n.row, n.col, seed = 2018, return.df = T)
colnames(z) <- paste0("Col", seq(n.col))
rownames(z) <- paste0("Row", seq(n.row))

true <- sample(0:1, 20, TRUE)
estimated <- true
index <- sample(seq(20), 5)
estimated[index] <- 1 - as.numeric(estimated[index])

# mhist ====
mhist(x)

# mplot3.adsr ====
mplot3.adsr()

# mplot3.bar ====
# '- a. vector ====
set.seed(2019)
x1 <- rnorm(10)
names(x1) <- paste0("c", seq(x1))
mplot3.bar(x1, theme = "lightgrid")

# '- b. matrix ====
x1 <- rnormmat(3, 6, seed = 2019)
colnames(x1) <- paste("F", seq(6))
rownames(x1) <- paste("Case", seq(nrow(x1)))
mplot3.bar(x1)
mplot3.bar(x1, col = colorGrad(nrow(x1), lo = pennCol$lighterBlue, mid = pennCol$red, hi = "white")[seq(nrow(x1))])
# Suppress group legend
mplot3.bar(x1, group.legend = FALSE)

# mplot3.box ====
mplot3.box(z)
mplot3.box(z, theme = "whitegrid")
mplot3.box(z, theme = "lightgrid")

# mplot3.xy ===
mplot3.xy(x, y)
mplot3.xy(x, y, fit = "gam")
mplot3.xy(x, list(Raw = x * .3,
                  N1 = x * .3 + .5*rnorm(50),
                  N2 = x * .3 + rnorm(50),
                  N3 = x * .3 + 1.5*rnorm(50),
                  N4 = x * .3 + 2*rnorm(50),
                  N5 = x * .3 + 2.5*rnorm(50)),
          fit = 'glm', theme = "darkgrid")
mplot3.xym(x, y)

iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
mod <- s.CART(iris2, maxdepth = 2)

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
z[] <- as.numeric(seq(n.col*n.row))
# Check column names, row names, values, with as.mat TRUE and FALSE
mplot3.img(z)
mplot3.img(z, as.mat = F)
rownames(z) <- sapply(1:10, function(i) paste0(rep(letters[i], 10), collapse = ""))
mplot3.img(z)

# mplot3.res ====
res <- resample(y)
mplot3.res(res)

# mplot3.roc ====
mplot3.roc(mod$fitted.prob, iris2$Species, method = "rt")

# rtRandom ====
rtRandom()

# rtlayout ====
rtlayout(2, 2, autolabel = T)
mplot3.x(x)
mplot3.xy(y, x)
mplot3.xy(x, y)
mplot3.x(y)
rtlayout()
