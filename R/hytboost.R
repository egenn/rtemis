# hytboost.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org
# 05-2020: switched to hytreew

#' Boosting of Hybrid Trees
#'
#' Boosted Hybrid Trees. This is a lower-level training function
#'
#' @inheritParams s_GLM
#' @param x Data frame: Input features
#' @param y Vector: Output
#' @param mod Algorithm to boost, for options, see [select_learn]
#' @param mod.params Named list of arguments for `mod`
#' @param learning.rate Float (0, 1] Learning rate for the additive steps
#' @param init Float: Initial value for prediction. Default = mean(y)
#' @param cxrcoef Logical: If TRUE, pass `cxr = TRUE, cxrcoef = TRUE` to
#' [predict.hytreew]
#' @param tolerance Float: If training error <= this value, training stops
#' @param tolerance.valid Float: If validation error <= this value, training stops
#' @param max.iter Integer: Maximum number of iterations (additive steps) to perform.
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param base.verbose Logical: `verbose` argument passed to learner
#' @param print.error.plot String or Integer: "final" plots a training and validation
#' (if available) error curve at the end of training. If integer, plot training and
#' validation error curve every this many iterations during training for each base
#' learner
#' @param ... Additional parameters to be passed to learner
#'
#' @return `hytboost` object
#' @author E.D. Gennatas
#'
#' @keywords internal
#' @noRd

hytboost <- function(
  x,
  y,
  x.valid = NULL,
  y.valid = NULL,
  resid = NULL,
  boost.obj = NULL,
  # mod.params = list(),
  case.p = .9,
  # weights = NULL,
  learning.rate = .1,
  # tolerance = 0,
  # tolerance.valid = 0,
  max.iter = 10,
  # ++ hytreew params ++
  max.depth = 5,
  gamma = .1,
  alpha = 0,
  lambda = 1,
  lambda.seq = NULL,
  minobsinnode = 2,
  minobsinnode.lin = 10,
  shrinkage = 1,
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  part.minbucket = 5,
  # init = mean(y),
  lin.type = "glmnet",
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = "lambda.min",
  # -- earlystop --
  earlystop.params = setup.earlystop(),
  earlystop.using = "train",
  init = mean(y),
  cxrcoef = FALSE,
  print.progress.every = 5,
  print.error.plot = "final",
  base.verbose = FALSE,
  verbose = TRUE,
  trace = 0,
  prefix = NULL,
  print.plot = TRUE,
  plot.theme = "darkgrid",
  # print.base.plot = FALSE,
  plot.type = "l"
) {
  # Arguments ----
  if (!verbose) print.plot <- FALSE
  # extra.args <- list(...)
  # mod.params <- c(mod.params, extra.args)
  if (length(max.depth) > 1) stop("max.depth must be scalar integer")

  # Boost ----
  # hytreew params ----
  mod.params <- list(
    max.depth = max.depth,
    gamma = gamma,
    shrinkage = shrinkage,
    minobsinnode.lin = minobsinnode.lin,
    # licoef params --
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    minobsinnode = minobsinnode,
    lin.type = lin.type,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda,
    # rpart params --
    part.minsplit = part.minsplit,
    part.xval = part.xval,
    part.max.depth = part.max.depth,
    part.cp = part.cp,
    part.minbucket = part.minbucket
  )
  if (trace > 0) {
    parameterSummary(
      mod.params,
      init = init,
      max.iter = max.iter,
      learning.rate = learning.rate,
      # tolerance = tolerance,
      # tolerance.valid = tolerance.valid
      title = "hytboost Parameters"
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
    error[1] <- mse(y, Fval) # will be overwritten, needed for while statement

    if (!is.null(x.valid)) {
      error.valid <- vector("numeric")
      Fvalid <- init
    } else {
      error.valid <- predicted.valid <- Fvalid <- NULL
    }
    i <- 1
    if (verbose) msg2("[ Boosting Hybrid Tree... ]", sep = "")
  } else {
    # '- Expand series ----
    .learning.rate <- boost.obj$learning.rate
    mods <- boost.obj$mods
    Fval <- penult.fitted <- boost.obj$fitted
    error <- boost.obj$error
    if (!is.null(x.valid)) {
      error.valid <- boost.obj$error.valid
      Fvalid <- boost.obj$predicted.valid
    } else {
      error.valid <- predicted.valid <- Fvalid <- NULL
    }
    max.iter <- max.iter + length(mods)
    i <- length(mods) + 1
    if (trace > 0) msg2("i =", i)
    if (verbose) msg2("[ Expanding boosted Hybrid Tree... ]", sep = "")
  }

  if (is.null(resid)) resid <- y - Fval

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
  while (i <= max.iter) {
    .learning.rate[i] <- learning.rate
    if (trace > 0) msg2("learning.rate is", .learning.rate[i])
    if (trace > 0) msg2("i =", i)
    if (case.p < 1) {
      n.cases <- NROW(x)
      index <- sample(n.cases, case.p * n.cases)
      x1 <- x[index, , drop = FALSE]
      # y1 <- y[index] # not used, we use resid1
      resid1 <- resid[index]
    } else {
      x1 <- x
      resid1 <- resid
    }
    mod.args <- c(
      list(
        x = x1,
        y = resid1,
        # x.test = x.valid, y.test = y.valid,
        verbose = base.verbose
      ),
      mod.params
    )
    mods[[i]] <- do.call(hytreew, args = mod.args)
    if (cxrcoef) {
      if (trace > 0) msg2("Updating cxrcoef")
      fitted0 <- predict.hytreew(mods[[i]], x, cxr = TRUE, cxrcoef = TRUE)
      fitted <- fitted0$yhat
      mods[[i]]$cxr <- fitted0$cxr
      mods[[i]]$cxrcoef <- fitted0$cxrcoef
    } else {
      fitted <- predict.hytreew(mods[[i]], x)
    }
    names(mods)[i] <- paste0("hytree.", i)

    Fval <- Fval + .learning.rate[i] * fitted
    if (i == max.iter - 1) penult.fitted <- Fval
    resid <- y - Fval
    error[i] <- mse(y, Fval)

    if (!is.null(x.valid)) {
      predicted.valid <- predict.hytreew(mods[[i]], x.valid)
      Fvalid <- Fvalid + .learning.rate[i] * predicted.valid
      error.valid[i] <- mse(y.valid, Fvalid)
      if (verbose && i %in% print.progress.index) {
        msg2(
          "Iteration #",
          i,
          ": Training MSE = ",
          ddSci(error[i]),
          "; Validation MSE = ",
          ddSci(error.valid[i]),
          sep = ""
        )
      }
      # if (!is.null(earlystop.params)) {
      #   es <- do.call(earlystop, c(list(x = error.valid), earlystop.params))
      #   if (es) break
      # }
    } else {
      if (verbose && i %in% print.progress.index) {
        msg2("Iteration #", i, ": Training MSE = ", ddSci(error[i]), sep = "")
      }
      # if (!is.null(earlystop)) {
      #   es <- do.call(earlystop, c(list(x = error), earlystop.params))
      #   if (es) break
      # }
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
          main = paste0(prefix, "LIHAD Boosting"),
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
          main = paste0(prefix, "LIHAD Boosting"),
          zerolines = FALSE,
          theme = plot.theme
        )
      }
    }

    # '- Early stopping ----
    if (!is.null(earlystop.params)) {
      if (earlystop.using == "valid" && !is.null(x.valid)) {
        es <- do.call(earlystop, c(list(x = error.valid), earlystop.params))
      } else {
        es <- do.call(earlystop, c(list(x = error), earlystop.params))
      }
      if (es) {
        break
        msg2("Breaking out of iteration", i)
      }
    }

    i <- i + 1
  } # /Iterate learner
  if (trace > 0) msg2("Reached max iterations")

  if (print.error.plot == "final") {
    if (is.null(x.valid)) {
      mplot3_xy(
        seq(error),
        error,
        type = plot.type,
        xlab = "Iteration",
        ylab = "MSE",
        x.axis.at = seq(error),
        main = paste0(prefix, "LIHAD Boosting"),
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
        main = paste0(prefix, "LIHAD Boosting"),
        zerolines = FALSE,
        theme = plot.theme
      )
    }
  }

  # Outro ----
  # '- boost object ----
  obj <- list(
    init = init,
    learning.rate = .learning.rate,
    penult.fitted = penult.fitted,
    fitted = Fval,
    last.fitted = fitted,
    predicted.valid = Fvalid,
    error = error,
    error.valid = error.valid,
    mod.params = mod.params,
    mods = mods
  )
  class(obj) <- c("hytboost", "list")

  obj
} # rtemis::hytboost


#' Print method for `hytboost` object
#'
#' @param x `hytboost` object
#' @param ... Not used
#'
#' @method print hytboost
#' @author E.D. Gennatas
#' @export

print.hytboost <- function(x, ...) {
  n.iter <- length(x$mods)
  cat("\n  A boosted Linear Hard Additive Tree with", n.iter, "iterations\n")
  cat("  and a learning rate of", x$learning.rate[1], "\n\n")
  # printls(x$mod[[1]]$parameters) # must teach printls to handle functions
  invisible(x)
} # rtemis::print.hytboost


#' Predict method for `hytboost` object
#'
#' @param object `hytboost` object
#' @param newdata data.frame of predictors
#  @param n.feat Integer: Use the first so many features
#' @param n.iter Integer: Use the first so many trees for prediction
#' @param fixed.cxr (internal use) Matrix: Cases by rules to use instead of matching
#' cases to rules using `newdata`
#' @param as.matrix Logical: If TRUE, output
#' @param n.cores Integer: Number of cores to use
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Not used
#'
#' @method predict hytboost
#' @author E.D. Gennatas
#' @export

predict.hytboost <- function(
  object,
  newdata = NULL,
  #  n.feat = NCOL(newdata),
  n.iter = NULL,
  fixed.cxr = NULL,
  as.matrix = FALSE,
  n.cores = 1,
  verbose = FALSE,
  ...
) {
  if (is.null(newdata)) {
    return(object$fitted)
  }

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      .colnames <- if (!is.null(colnames(newdata))) colnames(newdata) else
        paste0("V", seq_len(NCOL(newdata)))
      newdata <- as.data.frame(newdata)
      colnames(newdata) <- .colnames
      # newdata <- newdata[, seq_len(n.feat), drop = FALSE]
    }
  }

  # If n.iter is defined, only use the first so many trees, otherwise all
  if (is.null(n.iter)) n.iter <- length(object$mods)

  if (!as.matrix) {
    predicted <- rowSums(cbind(
      rep(object$init, NROW(newdata)),
      pbapply::pbsapply(
        seq(n.iter),
        function(i) {
          predict.hytreew(
            object$mods[[i]],
            newdata,
            fixed.cxr = fixed.cxr[[i]]
          ) *
            object$learning.rate[i]
        },
        cl = n.cores
      )
    ))
  } else {
    predicted.n <- pbapply::pbsapply(
      seq(n.iter),
      function(i) {
        predict.hytreew(object$mods[[i]], newdata, fixed.cxr = fixed.cxr[[i]]) *
          object$learning.rate[i]
      },
      cl = n.cores
    )

    predicted <- matrix(nrow = NROW(newdata), ncol = n.iter)
    predicted[, 1] <- object$init + predicted.n[, 1]
    for (i in seq(n.iter)[-1]) {
      predicted[, i] <- predicted[, i - 1] + predicted.n[, i]
    }
  }

  predicted
} # rtemis::predict.hytboost


#' Expand boosting series
#'
#' Add iterations to a [boost] object
#'
#' @inheritParams hytboost
#' @param object [boost] object
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

expand.hytboost <- function(
  object,
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  resid = NULL,
  mod.params = NULL,
  max.iter = 10,
  learning.rate = NULL,
  case.p = 1,
  # tolerance = NULL,
  cxrcoef = FALSE,
  prefix = NULL,
  verbose = TRUE,
  trace = 0,
  print.error.plot = "final"
) {
  if (is.null(mod.params)) mod.params <- object$mod.params
  if (is.null(learning.rate)) learning.rate <- rev(object$learning.rate)[1]
  if (trace > 0) msg2("learning.rate =", learning.rate)

  hytboost(
    x = x,
    y = y,
    x.valid = x.valid,
    y.valid = y.valid,
    resid = resid,
    boost.obj = object,
    learning.rate = learning.rate,
    max.iter = max.iter,
    init = object$init,
    case.p = case.p,
    cxrcoef = cxrcoef,
    # tolerance = tolerance,
    prefix = prefix,
    verbose = verbose,
    trace = trace,
    print.error.plot = print.error.plot
  )
} # rtemis::expand.hytboost


#' `as.hytboost` Place model in [hytboost] structure
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

as.hytboost <- function(
  object,
  x,
  y,
  x.valid,
  y.valid, # not currently used
  learning.rate = .1,
  init.learning.rate = learning.rate,
  init = 0,
  apply.lr = TRUE
  # tolerance = .00001,
  # tolerance.valid = .00001
) {
  if (!inherits(object, "hytreew")) {
    stop("Please provide hytreew object")
  }
  mods <- list()
  mods[[1]] <- object
  fitted <- if (apply.lr) predict.hytreew(object, x) * init.learning.rate else
    predict.hytreew(object, x)
  obj <- list(
    init = init,
    learning.rate = ifelse(apply.lr, init.learning.rate, 1),
    fitted = fitted,
    predicted.valid = NULL,
    error = mse(y, fitted),
    error.valid = NULL,
    mods = mods
  )
  class(obj) <- c("hytboost", "list")

  obj
} # rtemis::as.hytboost


#' Update [boost] object's fitted values
#'
#' Calculate new fitted values for a [boost].
#' Advanced use only: run after updating learning.rate
#'
#' All this will eventually be automated using an R6 object, maybe
#'
#' @method update hytboost
#' @param object [hytboost] object
#' @param x Features
#' @param y Outcome
#' @param trace Integer: If > 0, print diagnostic messages
#' @param ... Not used
#'
#' @return [hytboost] object
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
# TODO: save penultimate fitted, add last

update.hytboost <- function(object, x, y = NULL, trace = 0, ...) {
  if (trace > 0) fitted.orig <- object$fitted

  fitted <- object$penult.fitted +
    rev(object$learning.rate)[1] *
      predict.hytreew(object$mods[[length(object$mods)]], x)

  object$error[length(object$error)] <- mse(object$y.train, fitted)
  if (trace > 0 && !is.null(y)) {
    mse.orig <- mse(y, fitted.orig)
    new.mse <- mse(y, fitted)
    msg2("old mse = ", mse.orig, "; new mse = ", new.mse, sep = "")
  }
  object$fitted <- fitted
  if (trace > 0) msg2("Object updated")
  object
} # rtemis::update.hytboost


#' `as.hytboost` Place model in [hytboost] structure
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

as.hytboost2 <- function(
  object,
  x,
  y,
  learning.rate = .1,
  init.learning.rate = learning.rate,
  init = 0,
  apply.lr = TRUE
) {
  if (!inherits(object, "hytreew")) {
    stop("Please provide hytreew object")
  }
  mods <- list()
  mods[[1]] <- object
  fitted <- if (apply.lr) predict.hytreew(object, x) * init.learning.rate else
    predict.hytreew(object, x)
  obj <- list(
    init = init,
    learning.rate = c(init.learning.rate, learning.rate),
    fitted = fitted,
    predicted.valid = NULL,
    error = mse(y, fitted),
    error.valid = NULL,
    mods = mods
  )
  class(obj) <- c("hytboost", "list")

  obj
} # rtemis::as.hytboost2
