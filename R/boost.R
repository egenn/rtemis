# boost.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org
# TODO: add error vector (1 * n.iter)
# TODO: make learning.rate input into max.iter vector

#' Boost an \pkg{rtemis} learner for regression
#'
#' Train an ensemble using boosting of any learner
#'
#' If `learning.rate` is set to 0, a nullmod will be created
#'
#' @inheritParams s_GLM
#' @param x.valid Data.frame; optional: Validation data
#' @param y.valid Float, vector; optional: Validation outcome
#' @param mod Character: Algorithm to train base learners, for options, see
#' [select_learn]. Default = "cart"
#' @param resid Float, vector, length = length(y): Residuals to work on. Do
#' not change unless you know what you're doing. Default = NULL, for regular
#' boosting
#' @param boost.obj (Internal use)
#' @param mod.params Named list of arguments for `mod`
#' @param case.p Float (0, 1]: Train each iteration using this perceent of
#' cases. Default = 1, i.e. use all cases
#' @param learning.rate Float (0, 1] Learning rate for the additive steps
#' @param earlystop.params List with early stopping parameters.
#' Set using [setup.earlystop]
#' @param earlystop.using Character: "train" or "valid". For the latter,
#' requires `x.valid`
#' @param init Float: Initial value for prediction. Default = mean(y)
#' @param tolerance Float: If training error <= this value, training stops
#' @param tolerance.valid Float: If validation error <= this value, training
#' stops
#' @param max.iter Integer: Maximum number of iterations (additive steps) to
#' perform. Default = 10
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param print.progress.every Integer: Print progress over this many iterations
#' @param base.verbose Logical: `verbose` argument passed to learner
#' @param print.error.plot String or Integer: "final" plots a training and
#' validation (if available) error curve at the end of training. If integer,
#' plot training and validation error curve every this many iterations during
#' training. "none" for no plot.
#' @param print.base.plot Logical: Passed to `print.plot` argument of base
#' learner, i.e. if TRUE, print error plot for each base learner
#' @param plot.type Character: "l" or "p". Plot using lines or points.
#' @param prefix Internal
#' @param ... Additional parameters to be passed to learner define by `mod`
#'
#' @author E.D. Gennatas
#' @export

boost <- function(
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  x.test = NULL,
  y.test = NULL,
  mod = "cart",
  resid = NULL,
  boost.obj = NULL,
  mod.params = list(),
  case.p = 1,
  weights = NULL,
  learning.rate = .1,
  earlystop.params = setup.earlystop(
    window = 30,
    window_decrease_pct_min = .01
  ),
  earlystop.using = "train",
  tolerance = 0,
  tolerance.valid = .00001,
  max.iter = 10,
  init = NULL,
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
  mod.name <- "BOOST"

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
  dt <- prepare_data(x, y, x.test, y.test, x.valid, y.valid, verbose = verbose)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  x.valid <- dt$x.valid
  y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
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

  # Boost ----
  mod <- learner.short <- toupper(mod)
  learner <- select_learn(mod)
  learner.name <- select_learn(mod, desc = TRUE)
  learner.short <- toupper(mod)

  if (verbose) {
    parameterSummary(
      mod,
      mod.params,
      init,
      max.iter,
      learning.rate,
      tolerance,
      tolerance.valid
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
    Fval <- penult.fitted <- boost.obj$fitted
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
      # y1 <- y[index]
      resid1 <- resid[index]
    } else {
      x1 <- x
      resid1 <- resid
    }

    mod.args <- c(
      list(
        x = x1,
        y = resid1,
        x.test = x.valid,
        y.test = y.valid,
        verbose = base.verbose,
        print.plot = print.base.plot
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
    if (case.p < 1) {
      mods[[i]]$fitted <- fitted <- predict(mods[[i]], x)
    } else {
      fitted <- mods[[i]]$fitted
    }

    Fval <- Fval + .learning.rate[i] * fitted
    if (i == max.iter - 1) penult.fitted <- Fval
    resid <- y - Fval
    error[[i]] <- mse(y, Fval)
    if (!is.null(x.valid)) {
      predicted.valid <- mods[[i]]$predicted
      Fvalid <- Fvalid + .learning.rate[i] * predicted.valid
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
          main = paste0(prefix, learner.short, " Boosting"),
          zerolines = FALSE,
          theme = plot.theme
        )
      }
    }
    if (error[[i]] <= tolerance) {
      if (verbose)
        msg2(
          "Reached training error tolerance = ",
          tolerance,
          "; breaking",
          sep = ""
        )
      break
    }
    if (!is.null(x.valid) && error.valid[[i]] <= tolerance.valid) {
      if (verbose) msg2("Reached validationn error tolerance, breaking")
      break
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
        if (verbose) msg2("Breaking out of iteration", i)
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
        main = paste0(prefix, learner.short, " Boosting"),
        zerolines = FALSE,
        theme = plot.theme
      )
    }
  }

  # '- boost object ----
  obj <- list(
    call = NULL,
    mod.name = mod.name,
    learning.rate = .learning.rate,
    init = init,
    penult.fitted = penult.fitted,
    fitted = Fval,
    last.fitted = fitted,
    predicted.valid = Fvalid,
    error = error,
    error.valid = error.valid,
    mods = mods
  )
  class(obj) <- c("boost", "list")

  # Fitted ----
  error.train <- mod_error(y, obj$fitted)
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
    tolerance = tolerance,
    tolerance.valid = tolerance.valid,
    max.iter = max.iter
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
    fitted = obj$fitted,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    se.prediction = NULL,
    error.test = error.test,
    varimp = NULL,
    question = question,
    extra = extra
  )

  if (!is.null(outdir)) {
    filename.train <- paste0(outdir, mod.name, "_", mod, "_Fitted.vs.True.pdf")
    if (!is.null(y.test)) {
      filename.test <- paste0(
        outdir,
        mod.name,
        "_",
        mod,
        "_Predicted.vs.True.pdf"
      )
    }
  } else {
    filename.train <- filename.test <- NULL
  }

  if (print.plot || !is.null(outdir)) {
    if (plot.fitted || !is.null(outdir)) {
      plot(
        rt,
        estimate = "fitted",
        theme = plot.theme,
        print.plot = plot.fitted,
        filename = filename.train
      )
    }
    if (plot.predicted || !is.null(outdir)) {
      plot(
        rt,
        estimate = "predicted",
        theme = plot.theme,
        print.plot = plot.predicted,
        filename = filename.test
      )
    }
  }
  if (!is.null(outdir)) rt_save(rt, outdir, verbose = verbose)
  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::boost


#' Print method for [boost] object
#'
#' @method print boost
#' @param x [boost] object
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

print.boost <- function(x, ...) {
  mod.name <- x$mod$mods[[1]]$mod.name
  n.iter <- length(x$mods)
  cat("\n  A boosted", mod.name, "model with", n.iter, "iterations\n")
  cat("  and a learning rate of", x$learning.rate[1], "\n\n")
  invisible(x)
  # printls(x$mod[[1]]$parameters) # check printls can handle functions
} # rtemis::print.boost


#' Predict method for `boost` object
#'
#' @param object [boost] object
#' @param newdata data.frame: New data to predict on
#' @param n.feat Integer: Number of features to use from `newdata`
#' @param n.iter Integer: Number of iterations to use
#' @param as.matrix Logical: If TRUE, return matrix of predictions for each iteration,
#' otherwise return vector
#' @param verbose Logical: If TRUE, print messages to console
#' @param n.cores Integer: Number of cores to use
#' @param ... Not used
#'
#' @method predict boost
#' @author E.D. Gennatas
#' @export

predict.boost <- function(
  object,
  newdata = NULL,
  n.feat = NCOL(newdata),
  n.iter = NULL,
  as.matrix = FALSE,
  verbose = FALSE,
  n.cores = rtCores,
  ...
) {
  if (inherits(object, "rtMod") && inherits(object$mod, "boost")) {
    object <- object$mod
    if (verbose) msg2("Found rtemis boost object")
  } else if (inherits(object, "boost")) {
    msg2("Found boost object")
  } else {
    stop("Please provide boost object")
  }

  if (is.null(newdata)) {
    return(object$fitted)
  }

  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      .colnames <- if (!is.null(colnames(newdata))) {
        colnames(newdata)
      } else {
        paste0("V", seq_len(NCOL(newdata)))
      }
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
        function(i) {
          predict(object$mods[[i]], newdata) * object$learning.rate[i]
        },
        cl = n.cores
      )
    ))
  } else {
    predicted.n <- pbapply::pbsapply(
      seq(n.iter),
      function(i) {
        predict(object$mods[[i]], newdata) * object$learning.rate[i]
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
} # rtemis::predict.boost


#' Expand boosting series
#'
#' Expand a [boost] object by adding more iterations
#'
#' @inheritParams boost
#' @param object [boost] object
#' @author E.D. Gennatas
#' @export

expand.boost <- function(
  object,
  x,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  x.test = NULL,
  y.test = NULL,
  mod = NULL,
  resid = NULL,
  mod.params = NULL,
  max.iter = 10,
  learning.rate = NULL,
  case.p = 1,
  prefix = NULL,
  verbose = TRUE,
  trace = 0,
  print.error.plot = "final",
  print.plot = FALSE
) {
  if (is.null(y)) y <- object$y.train
  if (is.null(mod)) mod <- object$parameters$mod
  if (is.null(mod.params)) mod.params <- object$parameters$mod.params
  if (is.null(learning.rate)) learning.rate <- object$parameters$learning.rate
  tolerance <- object$parameters$tolerance
  boost(
    x = x,
    y = y,
    x.valid = x.valid,
    y.valid = y.valid,
    x.test = x.test,
    y.test = y.test,
    resid = resid,
    mod = mod,
    boost.obj = object,
    mod.params = mod.params,
    case.p = case.p,
    learning.rate = learning.rate,
    max.iter = max.iter,
    tolerance = tolerance,
    prefix = prefix,
    verbose = verbose,
    trace = trace,
    print.error.plot = print.error.plot,
    print.plot = print.plot
  )
} # rtemis::expand.boost


#' `as.boost` Place model in [boost] structure
#'
#' @param object rtMod model
#' @param x Data.frame, optional: if provided, use to calculate fitted values of new boost object
#' @param y Float, vector: Outcome
#' @param x.valid Data.frame; optional: Validation data
#' @param y.valid Float, vector; optional: Validation outcome
#' @param learning.rate Float: Learning rate for new boost object. Default = 1
#' @param init Float: Initial value for new boost object. Default = 0
#' @param apply.lr Logical: Only considered is `x = NULL`. If TRUE, new boost object's fitted values will
#' be object$fitted * learning.rate, otherwise object$fitted
#' @param tolerance Float: error tolerance for new boost object. See [boost]
#' @param tolerance.valid Float: error tolerance for validation set. See [boost]
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd
# TODO: add x = NULL, if not NULL calculate fitted values

as.boost <- function(
  object,
  x = NULL,
  y = NULL,
  x.valid = NULL,
  y.valid = NULL,
  learning.rate = 1,
  init = 0,
  apply.lr = TRUE,
  tolerance = .00001,
  tolerance.valid = .00001
) {
  if (!inherits(object, "rtMod")) {
    stop("Please provide rtMod object")
  }
  mods <- list()
  mods[[1]] <- object
  if (!is.null(x)) {
    fitted <- init + predict(object, x)
  } else {
    fitted <- object$fitted
  }
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
    call = "external",
    mod.name = "BOOST",
    learning.rate = learning.rate,
    init = init,
    fitted = fitted,
    predicted.valid = predicted.valid,
    error = error,
    error.valid = error.valid,
    mods = mods
  )
  class(obj) <- c("boost", "list")

  # Outro ----
  parameters <- list(
    mod = object$mod.name,
    mod.params = object$parameters,
    init = init,
    n.its = 1,
    learning.rate = learning.rate,
    tolerance = tolerance,
    tolerance.valid = tolerance.valid,
    max.iter = 1
  )
  extra <- list(error.valid = NULL)
  rt <- rtModSet(
    mod = obj,
    mod.name = "BOOST",
    type = object$type,
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
} # rtemis::as.boost

#' \pkg{rtemis} internals: Update `rtMod` [boost] object's fitted values in-place
#'
#' Calculate new fitted values for a [boost] object.
#' Advanced use only: run with new `x` or after updating learning.rate in object
#'
#' @param object [boost] object
#' @param x Data frame: Features
#' @param last.step.only Logical: If TRUE, `x` must be provided and only the last meta model will be updated
#' using this `x`
#' @param n.cores Integer: Number of cores to use for parallel processing
#' @param trace Integer: If > 0, print diagnostic info to console
#' @param ... Not used
#'
#' @return [boost] object
#' @author E.D. Gennatas
#' @return Nothing; updates `object` in-place
#' @keywords internal
#' @noRd

update.rtMod.boost <- function(
  object,
  x = NULL,
  last.step.only = FALSE,
  n.cores = rtCores,
  trace = 0,
  ...
) {
  if (trace > 0) fitted.orig <- object$fitted
  # Create n.iter x n.cases fitted values; one row per iteration
  if (is.null(x)) {
    fitted <- t(vapply(
      object$mod$mods,
      \(i) i$fitted,
      vector("numeric", length(object$fitted))
    ))
  } else {
    if (!last.step.only) {
      fitted <- t(as.data.frame(pbapply::pblapply(
        object$mod$mods,
        \(i) predict(i, x),
        cl = n.cores
      )))
    } else {
      u <- length(object$mod$mods)
      object$mod$mods[[u]]$fitted <- predict(object$mod$mods[[u]], x)
      fitted <- t(vapply(
        object$mod$mods,
        \(i) i$fitted,
        vector("numeric", length(object$fitted))
      ))
    }
  }

  # if (!is.null(x.valid)) {
  #   # currently predicted.valid not saved in object
  #     predicted.valid <- t(as.data.frame(pbapply::pblapply(object$mod$mods,
  #                                                          function(i) predict(i, x.valid), cl = n.cores)))
  # }

  # Multiply each row by its corresponding learning.rate, and sum all n.case-length vectors to get fitted value
  object$fitted <- object$mod$init +
    matrixStats::colSums2(fitted * object$mod$learning.rate)
  object$error.train <- mod_error(object$y.train, object$fitted)
  # object$mod$error.valid <- mod_error(object$mod$y.valid, predicted.valid)
  if (trace > 0) {
    mse.orig <- mse(object$y.train, fitted.orig)
    mse.new <- mse(object$y.train, fitted)
    msg20("old MSE = ", ddSci(mse.orig), "; new MSE = ", ddSci(mse.new))
    # if (mse.new > mse.orig) warning("Whatever you did, it didn't really help:\nnew MSE is higher than original")
  }
} # rtemis::update.rtMod.boost
