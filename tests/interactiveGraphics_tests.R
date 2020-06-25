# interactiveGraphics_tests.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

library(rtemis)

if (requireNamespace("plotly", quietly = TRUE)) {
  dplot3.bar(VADeaths)
  x <- rnormmat(20, 5, return.df = T, seed = 2019)
  dplot3.box(x)
  dplot3.box(x, type = "violin")
  dplot3.x(split(iris$Sepal.Length, iris$Species))
  dplot3.x(iris$Sepal.Length, group = iris$Species)
  dplot3.xy(iris$Sepal.Length, iris$Petal.Length, fit = "gam", se.fit = TRUE, group = iris$Species)
  dplot3.heatmap(cor(rnormmat(10, 10, seed = 2019)))
  varimp <- rnorm(10)
  names(varimp) <- paste0("Feature_", seq(10))
  dplot3.varimp(varimp)
}

if (requireNamespace("data.tree", quietly = TRUE) &
    requireNamespace("DiagrammeR", quietly = TRUE)) {
  iris2 <- iris[51:150, ]
  iris2$Species <- factor(iris2$Species)
  mod <- s.CART(iris2, maxdepth = 2)
  dplot3.cart(mod)
}
