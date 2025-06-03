# interactiveGraphics_tests.R
# ::rtemis::
# 2019- EDG rtemis.org

options(rt.font = "sans")
library(rtemisalpha)

if (requireNamespace("plotly", quietly = TRUE)) {
  dplot3_bar(VADeaths)
  x <- rnormmat(20, 5, return.df = TRUE, seed = 2019)
  dplot3_box(x)
  dplot3_box(x, type = "violin")
  dplot3_x(split(iris$Sepal.Length, iris$Species))
  dplot3_x(iris$Sepal.Length, group = iris$Species)
  dplot3_xy(
    iris$Sepal.Length,
    iris$Petal.Length,
    fit = "gam",
    se.fit = TRUE,
    group = iris$Species
  )
  dplot3_heatmap(cor(rnormmat(10, 10, seed = 2019)))
  varimp <- rnorm(10)
  names(varimp) <- paste0("Feature_", seq(10))
  dplot3_varimp(varimp)
}

if (
  requireNamespace("data.tree", quietly = TRUE) &&
    requireNamespace("DiagrammeR", quietly = TRUE)
) {
  iris2 <- iris[51:150, ]
  iris2$Species <- factor(iris2$Species)
  mod <- s_CART(iris2, maxdepth = 2)
  dplot3_cart(mod)
}
