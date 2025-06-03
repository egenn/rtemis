# staticGraphics_tests.R
# ::rtemis::
# 2018- EDG rtemis.org

options(rt.font = "sans")
library(rtemisalpha)

# Synth data ----
x <- rnorm(50)
y <- .33 * x + 12 + rnorm(50) / 3
n.col <- 5
n.row <- 10
z <- rnormmat(n.row, n.col, seed = 2018, return.df = TRUE)
colnames(z) <- paste0("Col", seq(n.col))
rownames(z) <- paste0("Row", seq(n.row))

true <- sample(0:1, 20, TRUE)
estimated <- true
index <- sample(seq(20), 5)
estimated[index] <- 1 - as.numeric(estimated[index])

# mhist ----
mhist(x)

# mplot3_adsr ----
mplot3_adsr()

# mplot3_bar ----
# '- a. vector ----
set.seed(2019)
x1 <- rnorm(10)
names(x1) <- paste0("c", seq(x1))
mplot3_bar(x1, theme = "whitegrid")

# '- b. matrix ----
x1 <- rnormmat(3, 6, seed = 2019)
colnames(x1) <- paste("F", seq(6))
rownames(x1) <- paste("Case", seq_len(nrow(x1)))
mplot3_bar(x1)
mplot3_bar(
  x1,
  col = colorGrad(
    nrow(x1),
    lo = pennCol$lighterBlue,
    mid = pennCol$red,
    hi = "white"
  )[seq_len(nrow(x1))]
)
# Suppress group legend
mplot3_bar(x1, group.legend = FALSE)

# mplot3_box ----
mplot3_box(z)
mplot3_box(z, theme = "whitegrid")
mplot3_box(z, theme = "whitegrid")

# mplot3_xy ===
mplot3_xy(x, y)
mplot3_xy(x, y, fit = "gam")
mplot3_xy(
  x,
  list(
    Raw = x * .3,
    N1 = x * .3 + .5 * rnorm(50),
    N2 = x * .3 + rnorm(50),
    N3 = x * .3 + 1.5 * rnorm(50),
    N4 = x * .3 + 2 * rnorm(50),
    N5 = x * .3 + 2.5 * rnorm(50)
  ),
  fit = "glm",
  theme = "darkgrid"
)
mplot3_xym(x, y)

iris2 <- iris[51:150, ]
iris2$Species <- factor(iris2$Species)
mod <- s.CART(iris2, maxdepth = 2)

# mplot3_conf ----
mplot3_conf(mod)
mplot3_confbin(mod$error.train$ConfusionMatrix)

# mplot3_decision(mod, iris2)
# TODO: INV mplot3_decision

# mplot3_fit ----
mplot3_fit(x, x + rnorm(50))

# mplot3_fret ----
mplot3_fret()

# mplot3_harmonograph ----
mplot3_harmonograph()

# mplot3_heatmap ----
mplot3_heatmap(cor(z))

# mplot3_img ----
z[] <- as.numeric(seq(n.col * n.row))
# Check column names, row names, values, with as.mat TRUE and FALSE
mplot3_img(z)
mplot3_img(z, as.mat = FALSE)
rownames(z) <- sapply(
  1:10,
  function(i) paste0(rep(letters[i], 10), collapse = "")
)
mplot3_img(z)

# mplot3_res ----
res <- resample(y)
mplot3_res(res)

# mplot3_roc ----
mplot3_roc(mod$fitted.prob, iris2$Species, method = "rt")

# rtRandom ----
rtRandom()

# rtlayout ----
rtlayout(2, 2, autolabel = TRUE)
mplot3_x(x)
mplot3_xy(y, x)
mplot3_xy(x, y)
mplot3_x(y)
rtlayout()
