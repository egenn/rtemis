# factoryze_tests.R
# ::rtemis::
# 2019- EDG rtemis.org

options(rt.font = "sans")

if (
  requireNamespace("psych", quietly = TRUE) &&
    requireNamespace("GPArotation", quietly = TRUE) &&
    requireNamespace("lavaan", quietly = TRUE)
) {
  # Library ----
  library(rtemisalpha)

  # Data ----
  set.seed(2019)
  n.cases <- 500
  n.factors <- 4
  n.varsPerFactor <- 5
  f <- lapply(seq(n.factors), function(i) rnorm(n.cases))
  x <- do.call(
    cbind,
    lapply(seq(n.factors), function(i) {
      sapply(seq(n.varsPerFactor), function(j) rnorm(1) * f[[i]] + rnorm(500))
    })
  )

  # factoryze ----
  x_fac <- factoryze(x)
} # if (requireNamespace(...))
