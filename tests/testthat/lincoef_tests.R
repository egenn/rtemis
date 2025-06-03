# lincoef_tests.R
# ::rtemis::
# 2019- EDG rtemis.org

options(rt.font = "sans")

# Packages ----
library(rtemisalpha)

# Data ----
x <- rnormmat(100, 50, seed = 2019)
w <- rnorm(50)
y <- c(x %*% w + rnorm(100))
x <- data.frame(x)

# lincoef ----
coefs.glm <- lincoef(x, y, method = "glm")
if (requireNamespace("glmnet", quietly = TRUE)) {
  coefs.glmnet <- lincoef(x, y, method = "glmnet")
  coefs.cvglmnet <- lincoef(x, y, method = "cv.glmnet")
}

if (requireNamespace("MASS", quietly = TRUE)) {
  coefs.lmridge <- lincoef(x, y, method = "lm.ridge")
}

if (requireNamespace("leaps", quietly = TRUE)) {
  coefs.allsubs <- lincoef(x, y, method = "allSubsets")
  coefs.forward <- lincoef(x, y, method = "forward")
  coefs.backward <- lincoef(x, y, method = "backward")
}
