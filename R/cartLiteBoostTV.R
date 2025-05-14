# cartLiteBoostTV
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org
# made learning.rate into vector
# TODO: add error vector (1 * n.iter)
# TODO: make learning.rate input into max.iter vector

#' Boost an \pkg{rtemis} learner for regression
#'
#' Perform regression by boosting a base learner
#'
#' If `learning.rate` is set to 0, a nullmod will be created
#'
#' @inheritParams boost
#' @param mod.params Named list of arguments for `cartLite`
#' @param weights.p Float (0, 1]: Percent of weights to set to 1, the rest will be set to `weights.0`. Default = 1
#' @param weights.0 Float (0, 1): Set weights of excluded cases to this number. Default = 0, which is equivalent to
#' excluding them, in which case, these cases can act as a validation set
#' @param learning.rate Float (0, 1] Learning rate for the additive steps
#' @param init Float: Initial value for prediction. Default = mean(y)
#' @param seed Integer: Set seed to allow reproducibility when `weights.p` is not 1
#' @param max.iter Integer: Maximum number of iterations (additive steps) to perform. Default = 10
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param base.verbose Logical: `verbose` argument passed to learner
#' @param print.error.plot String or Integer: "final" plots a training and validation (if available) error curve at the
#' end of training. If integer, plot training and validation error curve every this many iterations
#' during training. "none" for no plot. Default = "final"
#' @param print.base.plot Logical: Passed to `print.plot` argument of base learner, i.e. if TRUE, print error plot
#' for each base learner. Default = FALSE
#' @param prefix Internal
#' @param ... Additional parameters to be passed to `cartLite`
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @export

cartLiteBoostTV <- function(
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  x.test = NULL,
  y.test = NULL,
  resid = NULL,
  boost.obj = NULL,
  mod.params = list(),
  weights.p = 1,
  weights.0 = 0,
  weights = NULL,
  learning.rate = .1,
  max.iter = 10,
  init = NULL,
  seed = NULL,
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  base.verbose = FALSE,
  verbose = TRUE,
  trace = 0,
  print.progress.every = 5,
  print.error.plot = "final",
  prefix = NULL,
  plot.theme = rtTheme,
  plot.fitted = NULL,
  plot.predicted = NULL,
  print.plot = FALSE,
  print.base.plot = FALSE,
  plot.type = "l",
  outdir = NULL,
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(boost))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      sys.calls()[[1]][[1]],
      ".",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "CARTLITEBOOSTTV"

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  # if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  extra.args <- list(...)
  mod.params <- c(mod.params, extra.args)

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    x.valid = x.valid,
    y.valid = y.valid,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  x.valid <- dt$x.valid
  y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  # .weights <- if (is.null(weights) & ifw) dt$weights else weights
  # x0 <- if (upsample|downsample) dt$x0 else x
  # y0 <- if (upsample|downsample) dt$y0 else y
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }
  if (is.null(init)) init <- mean(y)
  train.ncases <- length(y)
  if (is.null(weights)) weights <- rep(1, train.ncases)
  if (!is.null(x.valid)) {
    valid.ncases <- length(y.valid)
    trainval.ncases <- train.ncases + valid.ncases
    valid.index <- (train.ncases + 1):trainval.ncases
    weights1 <- c(weights, rep(0, valid.ncases))
    x1 <- rbind(x, x.valid)
    y1 <- c(y, y.valid)
  } else {
    x1 <- x
    y1 <- y
    weights1 <- weights
  }

  # Boost ----
  learner <- "cartLite"
  learner.name <- "Classification and Regression Tree"
  learner.short <- "CARTlite"

  if (verbose) {
    parameterSummary(
      mod.params,
      init,
      max.iter,
      learning.rate,
      weights.p,
      weights.0
    )
  }
  if (trace > 0) msg2("Initial MSE =", mse(y, init))

  # '- New series ----
  # init learning.rate vector
  if (is.null(boost.obj)) {
    mods <- list()
    Fval <- penult.fitted <- init
    .learning.rate <- numeric()

    error <- vector("numeric")
    error[[1]] <- mse(y, Fval) # will be overwritten, needed for while statement

    if (!is.null(x.valid)) {
      error.valid <- vector("numeric")
      Fvalid <- init
    } else {
      error.valid <- predicted.valid <- Fvalid <- NULL
    }
    i <- 1
    if (verbose) msg2("[ Boosting ", learner.name, "... ]", sep = "")
  } else {
    .learning.rate <- boost.obj$mod$learning.rate
    # '- Expand series ----
    mods <- boost.obj$mod$mods
    Fval <- penult.fitted <- boost.obj$mod$fitted_tv # CHECK
    error <- boost.obj$mod$error
    if (!is.null(x.valid)) {
      error.valid <- boost.obj$mod$error.valid
      Fvalid <- boost.obj$mod$predicted.valid
    } else {
      error.valid <- predicted.valid <- Fvalid <- NULL
    }
    max.iter <- max.iter + length(mods)
    i <- length(mods) + 1
    if (trace > 0) msg2("i =", i)
    if (verbose) msg2("[ Expanding boosted ", learner.name, "... ]", sep = "")
  } # / Expand

  if (is.null(resid)) resid <- y - Fval
  resid1 <- if (!is.null(x.valid)) c(resid, rep(0, valid.ncases)) else resid

  # Print error during training
  if (max.iter == 1 && is.null(boost.obj)) {
    print.progress.index <- FALSE
    print.error.plot <- "none"
  } else if (print.progress.every < max.iter) {
    print.progress.index <- seq(
      print.progress.every,
      max.iter,
      print.progress.every
    )
  } else {
    print.progress.index <- max.iter
  }

  # Print error plot
  if (max.iter > 1 && is.numeric(print.error.plot)) {
    if (print.error.plot < max.iter) {
      print.error.plot.index <- seq(
        print.error.plot,
        max.iter,
        print.error.plot
      )
    } else {
      print.error.plot.index <- max.iter
    }
    print.error.plot <- "iter"
  }

  # '- Iterate learner ----
  if (!is.null(seed)) set.seed(seed)
  while (i <= max.iter) {
    .learning.rate[i] <- learning.rate
    if (trace > 0) msg2("learning.rate is", .learning.rate[i])
    if (trace > 0) msg2("i =", i)
    if (weights.p < 1) {
      holdout.index <- sample(train.ncases, (1 - weights.p) * train.ncases)
      weights1[holdout.index] <- weights.0
    }

    mod.args <- c(
      list(
        x = x1,
        y = resid1,
        weights = weights1,
        save.fitted = TRUE
        # x.test = x.valid, y.test = y.valid,
        # verbose = base.verbose,
      ),
      mod.params
    )

    # '- Train base learner ----
    if (.learning.rate[i] != 0) {
      mods[[i]] <- do.call(learner, args = mod.args)
    } else {
      mods[[i]] <- list(fitted = Fval)
      class(mods[[i]]) <- c("nullmod", "list")
    }

    names(mods)[i] <- paste0(learner.short, ".", i)

    fitted <- mods[[i]]$fitted

    Fval <- Fval + .learning.rate[i] * fitted
    if (i == max.iter - 1) penult.fitted <- Fval # CHECK: limit to train.ncases?
    # resid <- y - Fval[seq(train.ncases)]
    resid1 <- y1 - Fval
    error[[i]] <- mse(y, Fval[seq(train.ncases)])
    if (!is.null(x.valid)) {
      predicted.valid <- fitted[valid.index]
      Fvalid <- Fval[valid.index]
      error.valid[[i]] <- mse(y.valid, Fvalid)
      if (verbose && i %in% print.progress.index) {
        if (verbose) {
          msg2(
            "Iteration #",
            i,
            ": Training MSE = ",
            ddSci(error[[i]]),
            "; Validation MSE = ",
            ddSci(error.valid[[i]]),
            sep = ""
          )
        }
      }
    } else {
      if (verbose && i %in% print.progress.index) {
        msg2("Iteration #", i, ": Training MSE = ", ddSci(error[[i]]), sep = "")
      }
    }
    if (print.error.plot == "iter" && i %in% print.error.plot.index) {
      if (is.null(x.valid)) {
        mplot3_xy(
          seq(error),
          error,
          type = plot.type,
          xlab = "Iteration",
          ylab = "MSE",
          x.axis.at = seq(error),
          main = paste0(prefix, learner.short, " Boosting"),
          zerolines = FALSE,
          theme = plot.theme
        )
      } else {
        mplot3_xy(
          seq(error),
          list(training = error, validation = error.valid),
          type = plot.type,
          xlab = "Iteration",
          ylab = "MSE",
          group.adj = .95,
          x.axis.at = seq(error),
          main = paste0(prefix, learner.short, " Boosting"),
          zerolines = FALSE,
          theme = plot.theme
        )
      }
    }
    i <- i + 1
  }
  if (verbose && i > max.iter) msg2("Reached max iterations")

  if (print.error.plot == "final") {
    if (is.null(x.valid)) {
      mplot3_xy(
        seq(error),
        error,
        type = plot.type,
        xlab = "Iteration",
        ylab = "MSE",
        x.axis.at = seq(error),
        main = paste0(prefix, learner.short, " Boosting"),
        zerolines = FALSE,
        theme = plot.theme
      )
    } else {
      mplot3_xy(
        seq(error),
        list(
          Training = error,
          Validation = error.valid
        ),
        type = plot.type,
        xlab = "Iteration",
        ylab = "MSE",
        group.adj = .95,
        x.axis.at = seq(error),
        main = paste0(prefix, learner.short, " Boosting"),
        zerolines = FALSE,
        theme = plot.theme
      )
    }
  }

  # '- boost object ----
  obj <- list(
    mod.name = mod.name,
    learning.rate = .learning.rate,
    init = init,
    penult.fitted = penult.fitted,
    train.ncases = train.ncases,
    fitted_tv = Fval,
    last.fitted_tv = fitted,
    predicted.valid = Fvalid,
    error = error,
    error.valid = error.valid,
    mods = mods
  )
  class(obj) <- c("cartLiteBoostTV", "list")

  # Fitted ----
  error.train <- mod_error(y, obj$fitted_tv[seq(train.ncases)])
  if (verbose) errorSummary(error.train)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict(obj, x.test)
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # Outro ----
  parameters <- list(
    mod = learner.short,
    mod.params = mod.params,
    init = init,
    n.its = length(error),
    learning.rate = learning.rate,
    max.iter = max.iter,
    # case.p = case.p,
    weights.p = weights.p,
    weights.0 = weights.0,
    weights = weights
  )
  extra <- list(error.valid = error.valid)
  rt <- rtModSet(
    mod = obj,
    mod.name = mod.name,
    type = type,
    parameters = parameters,
    call = NULL,
    y.train = y,
    y.test = y.test,
    x.name = x.name,
    y.name = y.name,
    xnames = xnames,
    fitted = obj$fitted_tv[seq(train.ncases)],
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    varimp = NULL,
    question = question,
    extra = extra
  )

  rtMod.out(
    rt,
    print.plot,
    plot.fitted,
    plot.predicted,
    y.test,
    mod.name,
    outdir,
    save.mod = FALSE,
    verbose,
    plot.theme
  )

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::cartLiteBoostTV


#' Print method for [cartLiteBoostTV] object
#'
#' @method print cartLiteBoostTV
#'
#' @param x `cartLiteBoostTV` object
#' @param ... Additional arguments
#'
#' @author E.D. Gennatas
#' @export

print.cartLiteBoostTV <- function(x, ...) {
  mod.name <- "CART lite"
  n.iter <- length(x$mods)
  cat("\n  A boosted", mod.name, "model with", n.iter, "iterations\n")
  cat("  and a learning rate of", x$learning.rate[1], "\n\n")
  invisible(x)
} # rtemis::print.cartLiteBoostTV


#' Predict method for `cartLiteBoostTV` object
#'
#' @param object `cartLiteBoostTV` object
#' @param newdata Set of predictors
#' @param n.feat Integer: N of features to use. Default = NCOL(newdata)
#' @param n.iter Integer: N of iterations to predict from. Default = (all available)
#' @param as.matrix Logical: If TRUE, return predictions from each iterations. Default = FALSE
#' @param verbose Logical: If TRUE, print messages to console. Default = FALSE
#' @param n.cores Integer: Number of cores to use. Default = `rtCores`
#' @param ... Unused
#'
#' @method predict cartLiteBoostTV
#' @author E.D. Gennatas
#' @export

predict.cartLiteBoostTV <- function(
  object,
  newdata = NULL,
  n.feat = NCOL(newdata),
  n.iter = NULL,
  as.matrix = FALSE,
  verbose = FALSE,
  n.cores = rtCores,
  ...
) {
  if (inherits(object, "rtMod") && inherits(object$mod, "cartLiteBoostTV")) {
    object <- object$mod
    if (verbose) msg2("Found rtemis cartLiteBoostTV object")
  } else if (inherits(object, "cartLiteBoostTV")) {
    if (verbose) msg2("Found cartLiteBoostTV object")
  } else {
    stop("Please provide cartLiteBoostTV object")
  }

  if (is.null(newdata)) {
    return(object$fitted_tv[seq(object$train.ncases)])
  }

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      .colnames <- if (!is.null(colnames(newdata))) colnames(newdata) else
        paste0("V", seq_len(NCOL(newdata)))
      newdata <- as.data.frame(newdata)
      colnames(newdata) <- .colnames
      newdata <- newdata[, seq(n.feat), drop = FALSE]
    }
  }

  if (is.null(n.iter)) n.iter <- length(object$mods)

  if (!as.matrix) {
    predicted <- rowSums(cbind(
      rep(object$init, NROW(newdata)),
      pbapply::pbsapply(
        seq(n.iter),
        \(i)
          predict.cartLite(object$mods[[i]], newdata) * object$learning.rate[i],
        cl = n.cores
      )
    ))
  } else {
    predicted.n <- pbapply::pbsapply(
      seq(n.iter),
      \(i)
        predict.cartLite(object$mods[[i]], newdata) * object$learning.rate[i],
      cl = n.cores
    )

    predicted <- matrix(nrow = NROW(newdata), ncol = n.iter)
    predicted[, 1] <- object$init + predicted.n[, 1]
    for (i in seq(n.iter)[-1]) {
      predicted[, i] <- predicted[, i - 1] + predicted.n[, i]
    }
  }

  predicted
} # rtemis::predict.cartLiteBoostTV


#' Expand boosting series
#'
#' Expand a [cartLiteBoostTV] object by adding more iterations
#'
#' @inheritParams boost
#' @param object [cartLiteBoostTV] object
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

expand.cartLiteBoostTV <- function(
  object,
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  x.test = NULL,
  y.test = NULL,
  resid = NULL,
  mod.params = NULL,
  max.iter = 10,
  learning.rate = NULL,
  # case.p = 1,
  weights.p = 1,
  weights.0 = 0,
  seed = NULL,
  prefix = NULL,
  verbose = TRUE,
  trace = 0,
  print.error.plot = "final",
  print.plot = FALSE
) {
  if (is.null(y)) y <- object$y.train
  if (is.null(mod.params)) mod.params <- object$parameters$mod.params
  if (is.null(learning.rate)) learning.rate <- object$parameters$learning.rate

  cartLiteBoostTV(
    x = x,
    y = y,
    x.valid = x.valid,
    y.valid = y.valid,
    x.test = x.test,
    y.test = y.test,
    resid = resid,
    boost.obj = object,
    mod.params = mod.params,
    # case.p = case.p,
    weights.p = weights.p,
    weights.0 = weights.0,
    learning.rate = learning.rate,
    max.iter = max.iter,
    seed = seed,
    prefix = prefix,
    verbose = verbose,
    trace = trace,
    print.error.plot = print.error.plot,
    print.plot = print.plot
  )
} # rtemis::expand.cartLiteBoostTV


#' Place model in [cartLiteBoostTV] structure
#'
#' @inheritParams as.boost
#' @param object rtMod model
#' @param learning.rate Float: Learning rate for new boost object. Default = 1
#' @param init Float: Initial value for new boost object. Default = 0
#' @param apply.lr Logical: Only considered is `x = NULL`. If TRUE, new boost object's fitted values will
#' be object$fitted * learning.rate, otherwise object$fitted
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
# TODO: add x = NULL, if not NULL calculate fitted values

as.cartLiteBoostTV <- function(
  object,
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  learning.rate = 1,
  init = 0,
  apply.lr = TRUE
) {
  if (!inherits(object, "cartLite")) {
    stop("Please provide cartLite object")
  }
  mods <- list(CARTlite.1 = object)
  fitted <- init + predict(object, x)
  if (apply.lr) fitted <- fitted * learning.rate
  error <- if (!is.null(y)) mse(y, fitted) else NULL

  if (!is.null(x.valid)) {
    predicted.valid <- init + predict(object, x.valid)
    if (apply.lr) predicted.valid <- predicted.valid * learning.rate
    if (!is.null(y.valid)) {
      error.valid <- mse(y.valid, predicted.valid)
    } else {
      error.valid <- NULL
    }
  } else {
    predicted.valid <- error.valid <- NULL
  }

  obj <- list(
    mod.name = "CARTLITEBOOSTTV",
    learning.rate = learning.rate,
    init = init,
    penult.fitted = NULL,
    train.ncases = NROW(x),
    fitted_tv = c(fitted, predicted.valid),
    last.fitted_tv = c(fitted, predicted.valid),
    predicted.valid = predicted.valid,
    error = error,
    error.valid = error.valid,
    mods = mods
  )
  class(obj) <- c("cartLiteBoostTV", "list")

  # Outro ----
  parameters <- list(
    mod = object$mod.name,
    mod.params = object$parameters,
    init = init,
    n.its = 1,
    learning.rate = learning.rate,
    max.iter = 1
  )
  extra <- list(error.valid = NULL)
  rt <- rtModSet(
    rtclass = "rtMod",
    mod = obj,
    mod.name = "CARTLITEBOOSTTV",
    type = "Regression",
    parameters = parameters,
    call = NULL,
    y.train = object$y,
    y.test = object$y.test,
    x.name = object$x.name,
    y.name = object$y.name,
    xnames = object$xnames,
    fitted = fitted,
    se.fit = NULL,
    error.train = object$error.train,
    predicted = object$predicted,
    se.prediction = NULL,
    error.test = object$error.test,
    varimp = NULL,
    question = object$question,
    extra = extra
  )
  rt
} # rtemis::as.cartLiteBoostTV


#' \pkg{rtemis} internals: Update [cartLiteBoostTV] object's fitted values
#'
#' Calculate new fitted values for a [cartLiteBoostTV] object.
#' Advanced use only: run with new `x` or after updating learning.rate in object
#'
#' @method update cartLiteBoostTV
#' @param object [cartLiteBoostTV] object
#' @param x Data frame: Features
#' @param last.step.only Logical: If TRUE, `x` must be provided and only the last meta model will be updated
#' using this `x`
#'
#' @return [cartLiteBoostTV] object
#' @author E.D. Gennatas
#' @return Nothing; updates `object` in-place
#' @keywords internal
#' @noRd

update.cartLiteBoostTV <- function(
  object,
  x = NULL,
  x.valid = NULL,
  trace = 0,
  last.step.only = FALSE,
  n.cores = rtCores,
  ...
) {
  if (trace > 0) fitted.orig <- object$fitted_tv
  # fitted <- plyr::laply(object$mod$mods, function(i) i$fitted)
  # Create n.iter x n.cases fitted values; one row per iteration
  if (is.null(x)) {
    # fitted <- t(vapply(object$mod$mods, function(i) i$fitted, vector("numeric", length(object$fitted_tv))))
    fitted <- t(sapply(object$mod$mods, \(i) i$fitted))
  } else {
    if (!last.step.only) {
      fitted <- t(as.data.frame(pbapply::pblapply(
        object$mod$mods,
        \(i) predict(i, x),
        cl = n.cores
      )))
    } else {
      u <- length(object$mod$mods)
      object$mod$mods[[u]]$fitted_tv <- predict(object$mod$mods[[u]], x)
      fitted <- t(vapply(
        object$mod$mods,
        \(i) i$fitted,
        vector("numeric", length(object$fitted))
      ))
    }
  }

  # TODO: finish x.valid
  # if (is.null(x.valid)) {
  #   predicted.valid
  # }

  # Multiply each row by its corresponding learning.rate, and sum all n.case-length vectors to get fitted value
  object$mod$fitted_tv <- object$mod$init +
    colSums(fitted * object$mod$learning.rate)
  object$fitted <- object$mod$fitted_tv[seq(object$mod$train.ncases)]
  object$error.train <- mod_error(
    object$y.train,
    object$mod$fitted_tv[seq(object$mod$train.ncases)]
  )
  if (trace > 0) {
    mse.orig <- mse(object$y.train, fitted.orig)
    mse.new <- mse(object$y.train, fitted)
    msg20("old MSE = ", mse.orig, "; new MSE = ", mse.new)
    # if (mse.new > mse.orig) warning("Whatever you did, it didn't really help:\nnew MSE is higher than original")
  }
} # rtemis::update.cartLiteBoostTV
