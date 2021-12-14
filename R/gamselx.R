# gamselx

gamselx <- function(x, y,
                    gamsel.params1 = list(),
                    # cart.args = list(maxdepth = 10, minbucket = 20),
                    pairs.on.resid = TRUE,
                    p.adjust.method = "holm",
                    alpha = .05,
                    final.on.resid = TRUE,
                    gamsel.params2 = gamsel.params1,
                    n.cores = 1,
                    verbose = TRUE,
                    trace = 0) {

  n.features <- NCOL(x)
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("Feature", seq_len(n.features))
  }
  xnames <- colnames(x)

  # 1. GAMSEL ====
  if (verbose) msg("1: Training first stage GAMSEL", color = rtOrange)
  .gamsel.params1 <- c(list(x = x, y = y,
                            print.plot = FALSE,
                            verbose = trace > 1),
                       gamsel.params1)
  mod1 <- do.call("s.GAMSEL", .gamsel.params1)

  # # 2. CART ====
  # cart.args <- c(list(x = x, y = y - mod1$fitted),
  #                cart.args)
  # mod2 <- do.call("s.CART", cart.args)
  # rules <- as.rules.rpart(mod2$mod)[, 1]
  # index <- indexCasesByRules(x, rules)
  # table(index)

  # 2. Pairwise interactions ====
  if (verbose) msg("2: Looking for pairwise interactions...", color = rtOrange)
  pairs <- outer(seq_len(n.features), seq_len(n.features), FUN = paste)
  pairs <- pairs[lower.tri(pairs)]
  pairs <- t(sapply(strsplit(pairs, " "), as.numeric))[, c(2, 1)]
  n.pairs <- NROW(pairs)
  if (trace > 0) msg("Running", n.pairs, "pairwise GLMs")
  pairy <- if (pairs.on.resid) y - mod1$fitted else y
  lapply(seq_len(n.pairs), function(i) {
    if (trace > 0) cat(i, "..")
    fit <- glm(pairy ~ c(x[, pairs[i, 1]] * x[, pairs[i, 2]]))
    summary(fit)$coefficients[2, 4]
  }) |> p.adjust(method = p.adjust.method) -> pairwise.glm.pvals.adj
  if (trace > 0) msg("Done")
  pairs.index <- which(pairwise.glm.pvals.adj < alpha)
  if (verbose) msg("Found", length(pairs.index), "significant interactions")

  # 3. GAMSELs ====
  if (verbose) msg("3: Training second stage GAMSEL...", color = rtOrange)
  if (length(pairs.index) > 0) {
    .pairs <- pairs[pairs.index, , drop = FALSE]
    if (verbose) msg("Found", NROW(.pairs), "pairwise interactions", color = rtOrange)
    extnames <- sapply(seq_along(pairs.index), function(i) {
      paste(xnames[.pairs[i, 1]], xnames[.pairs[i, 2]], sep = "x")
    })
    ext <- sapply(seq_along(pairs.index), function(i) {
      x[, .pairs[i, 1]] * x[, .pairs[i, 2]]
    })
    colnames(ext) <- extnames
    .ext <- if (final.on.resid) ext else cbind(x, ext)

    .y <- if (final.on.resid) y - mod1$fitted else y
    .gamsel.params2 <- c(list(x = .ext, y = .y,
                              print.plot = FALSE,
                              verbose = trace > 1),
                         gamsel.params2)
    modext <- do.call("s.GAMSEL", .gamsel.params2)
  } else {
    .pairs <- NULL
    modext <- list(fitted = 0)
    class(modext) <- "nullmod"
  }

  # fitted ====
  fitted <- if (final.on.resid) {
    mod1$fitted + modext$fitted
  } else {
    modext$fitted
  }

  # mod ====
  mod <- list(modext = modext,
              mod1 = mod1,
              final.on.resid = final.on.resid,
              pairs = .pairs,
              xnames = xnames,
              fitted = fitted)

  class(mod) <- c("gamselx", "list")
  mod

}

predict.gamselx <- function(object, newdata = NULL, ...) {

  if (is.null(newdata)) return(object$fitted)

  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0("Feature", seq_len(n.features))
  }
  xnames <- colnames(newdata)

  n.pairs <- NROW(object$pairs)

  if (n.pairs > 0) {
    extnames <- sapply(seq_len(n.pairs), function(i) {
      paste(xnames[object$pairs[i, 1]], xnames[object$pairs[i, 2]], sep = "x")
    })
    ext <- sapply(seq_len(n.pairs), function(i) {
      newdata[, object$pairs[i, 1]] * newdata[, object$pairs[i, 2]]
    })
    colnames(ext) <- extnames
  }

  if (object$final.on.resid) {
    predict(object$mod1, newdata) + predict(object$modext, ext)
  } else {
    predict(object$modext, cbind(newdata, ext))
  }

}

print.gamselx <- function(x, ...) {

  which.lambda <- x$mod1$parameters$which.lambda
  index <- if (which.lambda == "lambda.min") x$mod1$mod$index.min
  summarynz <- getFromNamespace("summarynz", "gamsel2")
  summary <- summarynz(x$mod1$mod$gamsel.fit)[index, , drop = FALSE]

  # cat(".: A GAMSELX model with", summary[1], "linear, ", summary[2], "nonlinear, and",
  #     NROW(x$pairs), "interaction terms")
  cat(".: A", rtOrange$bold("GAMSELX"), "model with", summary[1], "linear,", summary[2],
      "nonlinear, and", NROW(x$pairs), "interaction terms")

}
