# Supervised Learning Tests ----

options(rt.font = "Helvetica")

# Data ----
# '- Regression ----
x <- rnormmat(200, 5)
y <- x[, 3] + x[, 5] + rnorm(200)
datr <- data.frame(x, y)
resr <- resample(datr)
datr_train <- datr[resr$Subsample_1, ]
datr_test <- datr[-resr$Subsample_1, ]

# '- Classification ----
# '-- binary ----
datc2 <- iris[51:150, ]
datc2$Species <- factor(datc2$Species)
resc2 <- resample(datc2)
datc2_train <- datc2[resc2$Subsample_1, ]
datc2_test <- datc2[-resc2$Subsample_1, ]

# '-- 3-class ----
resc3 <- resample(iris)
datc3_train <- iris[resc3$Subsample_1, ]
datc3_test <- iris[-resc3$Subsample_1, ]

# Tests ----
test_that("RANGER Regression succeeds", {
  mod_r <- s_RANGER(datr_train, datr_test)
  expect_identical(class(mod_r)[1], "rtMod")
})

test_that("RANGER Binary Classification succeeds", {
  mod_c <- s_RANGER(datc2_train, datc2_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})

test_that("RANGER 3-class Classification succeeds", {
  mod_c <- s_RANGER(datc3_train, datc3_test)
  expect_identical(class(mod_c)[1], "rtModClass")
})
