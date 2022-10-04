# gamselx2
# CART for categorical, GAMSEL for everything else

gamselx2 <- function(x, y,
                     init = NULL,
                     cart.params = list(maxdepth = 4, cp = .1),
                     gamsel.params1 = list(),
                     # cart.args = list(maxdepth = 10, minbucket = 20),
                     pairs.on.resid = TRUE,
                     p.adjust.method = "holm",
                     alpha = .05,
                     # final.on.resid = TRUE,
                     gamsel.params2 = gamsel.params1,
                     n.cores = 1,
                     verbose = TRUE,
                     trace = 0) {

  n.features <- NCOL(x)
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("Feature", seq_len(n.features))
  }
  xnames <- colnames(x)

  # Get categorical variables ----
  index.cat <- which(sapply(x, is.factor))
  n.cat <- length(index.cat)
  if (verbose) msg("Found", n.cat, "categorical variables", color = orange)

  # Init ----
  if (is.null(init)) init <- mean(y)
  Fval <- init # maybe make vector length n

  # 1. CART on categorical ----
  # todo: consider auto-depth based on f(n of possible combinations)
  if (n.cat > 0) {
    cart.args <- c(list(x = x[, index.cat, drop = FALSE], y = y - Fval),
                   cart.params,
                   verbose = trace > 1,
                   print.plot = FALSE)
    if (verbose) msg("Training CART on", n.cat, "categorical variables...", color = orange)
    mod1 <- do.call("s_CART", cart.args)
  } else {
    mod1 <- list()
    class(mod1) <- "nullmod"
  }

  Fval <- Fval + predict(mod1)

  # 2. GAMSEL I on continuous ----
  x.cont <- if (n.cat > 0) x[, -index.cat, drop = FALSE] else x
  if (verbose) msg("Training first stage GAMSEL...", color = orange)
  gamsel.args1 <- c(list(x = x.cont, y = y - Fval,
                         print.plot = FALSE,
                         verbose = trace > 1),
                    gamsel.params1)
  mod2 <- do.call("s_GAMSEL", gamsel.args1)

  Fval <- Fval + predict(mod2)

  # 3. Pairwise interactions ----
  if (verbose) msg("Looking for pairwise interactions...", color = orange)
  n.continuous <- NCOL(x.cont)
  if (n.continuous > 1) {
    xnames.cont <- colnames(x.cont)
    pairs <- outer(seq_len(n.continuous), seq_len(n.continuous), FUN = paste)
    pairs <- pairs[lower.tri(pairs)]
    n.pairs <- length(pairs)
    pairs <- matrix(t(sapply(strsplit(pairs, " "), as.numeric))[, c(2, 1)], n.pairs)

    if (trace > 0) msg("Running", n.pairs, "pairwise", ifelse(n.pairs == 1, "GLM", "GLMs"))
    resid <- y - Fval
    pairwise.glm.pvals.adj <- lapply(seq_len(n.pairs), function(i) {
      if (trace > 0) cat(i, "..")
      fit <- glm(resid ~ c(x.cont[, pairs[i, 1]] * x.cont[, pairs[i, 2]]))
      summary(fit)$coefficients[2, 4]
    }) |> p.adjust(method = p.adjust.method)
    if (trace > 0) msg("Done")
    pairs.index <- which(pairwise.glm.pvals.adj < alpha)
    if (verbose) msg("Found", length(pairs.index), "significant",
                     ifelse(length(pairs.index) == 1, "interaction", "interactions"))
  } else {
    pairs <- pairwise.glm.pvals.adj <- NULL
  }

  # 4. GAMSEL II on interactions ----
  if (verbose) msg("Training second stage GAMSEL...", color = orange)
  if (length(pairs.index) > 0) {
    .pairs <- pairs[pairs.index, , drop = FALSE]
    if (verbose) msg("Found", NROW(.pairs), "pairwise",
                     ifelse(NROW(.pairs) == 1, "interaction", "interactions"),
                     color = orange)
    extnames <- sapply(seq_along(pairs.index), function(i) {
      paste(xnames.cont[.pairs[i, 1]], xnames.cont[.pairs[i, 2]], sep = "x")
    })
    ext <- sapply(seq_along(pairs.index), function(i) {
      x.cont[, .pairs[i, 1]] * x.cont[, .pairs[i, 2]]
    })
    colnames(ext) <- extnames

    gamsel.args2 <- c(list(x = ext, y = resid,
                           print.plot = FALSE,
                           verbose = trace > 1),
                      gamsel.params2)
    mod3 <- do.call("s_GAMSEL", gamsel.args2)
  } else {
    .pairs <- NULL
    extnames <- NULL
    mod3 <- list(fitted = 0)
    class(mod3) <- "nullmod"
  }

  Fval <- Fval + predict(mod3)

  # mod ----
  mod <- list(init = init,
              index.cat = index.cat,
              mod1 = mod1, # CART on categorical
              mod2 = mod2, # GAMSEL on continuous
              pairwise.glm.pvals.adj = pairwise.glm.pvals.adj,
              pairs = .pairs,
              mod3 = mod3, # GAMSEL on interactions
              xnames = xnames,
              extnames = extnames,
              fitted = Fval)

  class(mod) <- c("gamselx2", "list")
  mod

}

#' Predict Method for gamselx2 Fits
#'
#' Obtains predictions from a fitted gamselx2 model object
#'
#' @author E.D. Gennatas
#' @export

predict.gamselx2 <- function(object, newdata = NULL, ...) {

  if (is.null(newdata)) return(object$fitted)

  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0("Feature", seq_len(n.features))
  }
  # xnames <- colnames(newdata)

  # autopreprocess
  if (!is.null(object$tofactor)) {
    for (i in object$tofactor) newdata[, i] <- factor(newdata[, i])
  }

  n.pairs <- NROW(object$pairs)
  index.cat <- object$index.cat

  if (n.pairs > 0) {
    # just use object$extnames
    # extnames <- sapply(seq_len(n.pairs), function(i) {
    #   paste(xnames[object$pairs[i, 1]], xnames[object$pairs[i, 2]], sep = "x")
    # })
    newdata.cont <- if (length(index.cat) > 0) newdata[, -index.cat, drop = FALSE] else newdata
    ext <- sapply(seq_len(n.pairs), function(i) {
      newdata.cont[, object$pairs[i, 1]] *
        newdata.cont[, object$pairs[i, 2]]
    })
    colnames(ext) <- object$extnames
  } else {
    ext <- NULL
  }

  object$init +
    predict(object$mod1, newdata[, object$index.cat, drop = FALSE]) +
    predict(object$mod2, if (length(index.cat) > 0) newdata[, -index.cat, drop = FALSE] else newdata) +
    predict(object$mod3, ext)

}

print.gamselx2 <- function(x, ...) {
  #todo: add n categorical etc

  n.cat <- length(x$index.cat)

  which.lambda <- x$mod2$parameters$which.lambda
  index <- if (which.lambda == "lambda.min") x$mod2$mod$index.min
  summarynz <- getFromNamespace("summarynz", "gamsel2")
  summary <- summarynz(x$mod2$mod$gamsel.fit)[index, , drop = FALSE]

  # cat(".: A GAMSELX2 model with", summary[1], "linear, ", summary[2], "nonlinear, and",
  #     NROW(x$pairs), "interaction terms")
  cat(".: A", bold(orange("GAMSELX2")), "model with\n    ",
      n.cat, "categorical features,\n    ",
      summary[1], "linear,\n    ", summary[2],
      "nonlinear, \n     and", NROW(x$pairs), "continuous interaction terms\n")

}
