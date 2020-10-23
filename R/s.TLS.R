# s.TLS.R
# ::rtemis::
# 2015 Efstathios D. Gennatas egenn.lambdamd.org

#' Total Least Squares Regression [R]
#'
#' A minimal function to perform total least squares regression
#'
#' The main differences between a linear model and TLS is that the latter assumes error in the features as well as
#' the outcome. The solution is essentially the projection on the first principal axis.
#' Because there is no model, \code{predict} and other methods are not currently working with \code{s.TLS}
#' These features may be added in the future
#' @inheritParams s.GLM
#' @author Efstathios D. Gennatas
#' @export

s.TLS <- function(x, y = NULL,
                  x.test = NULL, y.test = NULL,
                  x.name = "x", y.name = "y",
                  print.plot = TRUE,
                  plot.fitted = NULL,
                  plot.predicted = NULL,
                  plot.theme = getOption("rt.fit.theme", "lightgrid"),
                  question = NULL,
                  rtclass = NULL,
                  verbose = TRUE,
                  outdir = NULL,
                  save.mod = ifelse(!is.null(outdir), TRUE, FALSE), ...) {

  # [ INTRO ] ====
  if (missing(x)) {
    print(args(s.TLS))
    return(invisible(9))
  }
  if (!is.null(outdir)) outdir <- normalizePath(outdir, mustWork = FALSE)
  logFile <- if (!is.null(outdir)) {
    paste0(outdir, "/", sys.calls()[[1]][[1]], ".", format(Sys.time(), "%Y%m%d.%H%M%S"), ".log")
  } else {
    NULL
  }
  start.time <- intro(verbose = verbose, logFile = logFile)
  mod.name <- "TLS"

  # [ ARGUMENTS ] ====
  if (missing(x)) { print(args(s.TLS)); stop("x is missing") }
  if (is.null(y) & NCOL(x) < 2) { print(args(s.TLS)); stop("y is missing") }
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod & is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir)) outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")

  # [ DATA ] ====
  dt <- dataPrepare(x, y, x.test, y.test)
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, "Regression", mod.name)
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (print.plot) {
    if (is.null(plot.fitted)) plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted)) plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # [ TLS ] ====
  if (verbose) msg("Running Total Least Squares...", newline.pre = TRUE)
  M <- cbind(as.matrix(x), y)
  m <- nrow(x)
  n <- NCOL(x)
  M.mean <- matrix(rep(apply(M, 2, mean), m), nrow = m, byrow = T)
  colnames(M.mean) <- c(colnames(x), "y")

  # '- SVD ====
  M.svd <- svd(M - M.mean)
  V <- M.svd$v
  a <- - V[1:n, n + 1]/V[n + 1, n + 1]
  b <- mean(M %*% V[, n + 1])/V[n + 1, n + 1]
  mod <- list(a = a, b = b)
  class(mod) <- c("rtTLS", "list")

  # [ FITTED ] ====
  fitted <- c(cbind(as.matrix(x), 1) %*% c(a, b))
  normal <- V[, n + 1]
  error <- abs((M - M.mean) %*% normal)
  ssq <- sum(error ^ 2)
  error.train <- modError(y, fitted)
  if (verbose) errorSummary(error.train, mod.name)

  # [ PREDICTED ] ====
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- c(cbind(as.matrix(x.test), 1) %*% c(a, b))
    if (!is.null(y.test)) {
      error.test <- modError(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # [ OUTRO ] ====
  extra <- list(M = M, M.svd = M.svd, a = a, b = b, error = error, ssq = ssq)
  rt <- rtModSet(rtclass = rtclass,
                 mod = mod,
                 mod.name = mod.name,
                 type = type,
                 y.train = y,
                 y.test = y.test,
                 x.name = x.name,
                 y.name = y.name,
                 xnames = xnames,
                 fitted = fitted,
                 se.fit = NULL,
                 error.train = error.train,
                 predicted = predicted,
                 se.prediction = NULL,
                 error.test = error.test,
                 question = question,
                 extra = extra)

  rtMod.out(rt,
            print.plot,
            plot.fitted,
            plot.predicted,
            y.test,
            mod.name,
            outdir,
            save.mod,
            verbose,
            plot.theme)

  outro(start.time, verbose = verbose, sinkOff = ifelse(is.null(logFile), FALSE, TRUE))
  rt

} # rtemis::s.TLS


#' \code{predict.rtTLS}: \code{predict} method for \code{rtTLS} object
#'
#' @method predict rtTLS
#' @rdname rtTLS-methods
#' @export
predict.rtTLS <- function(object, newdata, ...) {

  c(cbind(as.matrix(newdata), 1) %*% c(object$a, object$b))

} # rtemis::precit.rtTLS


#' \code{print.rtTLS}: \code{print} method for \code{rtTLS} object
#'
#' @method print rtTLS
#' @rdname rtTLS-methods
#' @export
print.rtTLS <- function(x, ...) {

  cat("rtemis Total Least Squares Regression object (rtTLS)")

} # rtemis::print.rtTLS
