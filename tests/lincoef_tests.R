# lincoef_tests.R
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

# Library ----
library(rtemis)

# Data ----
x <- rnormmat(100, 50, seed = 2019)
w <- rnorm(50)
y <- c(x %*% w + rnorm(100))
x <- data.frame(x)

# lincoef ----
coefs.glm <- lincoef(x, y, method = "glm")
coefs.glmnet <- lincoef(x, y, method = "glmnet")
coefs.cvglmnet <- lincoef(x, y, method = "cv.glmnet")
coefs.lmridge <- lincoef(x, y, method = "lm.ridge")
coefs.allsubs <- lincoef(x, y, method = "allSubsets")
coefs.forward <- lincoef(x, y, method = "forward")
coefs.backward <- lincoef(x, y, method = "backward")
