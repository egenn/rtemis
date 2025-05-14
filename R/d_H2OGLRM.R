# d_H2OGLRM.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Generalized Low-Rank Models (GLRM) on H2O
#'
#' Perform GLRM decomposition using `h2o::h2o.glrm`
#' Given Input matrix `A`:
#' ` A(m x n) = X(m x k) \%*\% Y(k x n) `
#'
#' Learn more about GLRM from the H2O tutorial
#' <https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/glrm/glrm-tutorial.md>
#'
#' @param x Input data
#' @param x.test Optional test set. Will be projected on to NMF basis
#' @param x.valid Optional validation set
#' @param k Integer: Rank of decomposition
#' @param ip Character: IP address of H2O server. Default = "localhost"
#' @param port Integer: Port number for server. Default = 54321
#' @param transform Character: Transformation of input prior to decomposition
#' @param loss Character: Numeric loss function: "Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic",
#'   "Periodic". Default = "Quadratic"
#' @param regularization.x Character: Regularization function for X matrix: "None", "Quadratic", "L2", "L1",
#' "NonNegative", "OneSparse", "UnitOneSparse", "Simplex". Default = "None"
#' @param regularization.y Character: Regularization function for Y matrix: "None", "Quadratic", "L2", "L1",
#' "NonNegative", "OneSparse", "UnitOneSparse", "Simplex". Default = "None"
#' @param gamma.x Float: Regularization weight on X matrix. Default = 0
#' @param gamma.y Float: Regularization weight on Y matrix. Default = 0
#' @param max_iterations Integer: Maximum number of iterations. Default = 1000
#' @param max_updates Integer: Maximum number of iterations. Default = 2 * `max_iterations`
#' @param init_step_size Float: Initial step size. Default = 1
#' @param min_step_size Float: Minimum step size. Default = .0001
#' @param seed Integer: Seed for random number generator. Default = -1 (time-based)
#' @param init Character: Initialization mode: "Random", "SVD", "PlusPlus", "User". Default = "PlusPlus"
#' @param svd.method Character: SVD method for initialization: "GramSVD", "Power", "Randomized". Default = "Randomized"
#' @param verbose Logical: If TRUE, print console messages
#' @param print.plot Logical: If TRUE, print objective score against iteration number
#' @param plot.theme Character: Theme to pass to [mplot3_xy] if `print.plot = TRUE`
#' @param n.cores Integer: Number of cores to use
#' @param ... Additional parameters to be passed to `h2o::h2o.glrm`
#' @return `rtDecom` object
#' @author E.D. Gennatas
#' @family Decomposition
#' @export

d_H2OGLRM <- function(
  x,
  x.test = NULL,
  x.valid = NULL,
  k = 3,
  ip = "localhost",
  port = 54321,
  transform = "NONE",
  loss = "Quadratic",
  regularization.x = "None",
  regularization.y = "None",
  gamma.x = 0,
  gamma.y = 0,
  max_iterations = 1000,
  max_updates = 2 * max_iterations,
  init_step_size = 1,
  min_step_size = .0001,
  seed = -1,
  init = "PlusPlus",
  svd.method = "Randomized",
  verbose = TRUE,
  print.plot = TRUE,
  plot.theme = rtTheme,
  n.cores = rtCores,
  ...
) {
  # Intro ----
  start.time <- intro(verbose = verbose)
  decom.name <- "H2OGLRM"

  # Dependencies ----
  dependency_check("h2o")

  # Data ----
  x <- as.data.frame(x)
  if (!is.null(x.test)) x.test <- as.data.frame(x.test)
  n <- NROW(x)
  p <- NCOL(x)
  if (verbose) {
    msg2("||| Input has dimensions ", n, " rows by ", p, " columns,", sep = "")
    msg2("    interpreted as", n, "cases with", p, "features.")
  }
  if (is.null(colnames(x))) colnames(x) <- paste0("Feature_", seq_len(NCOL(x)))
  xnames <- colnames(x)
  if (!is.null(x.test)) colnames(x.test) <- xnames

  # h2o Frames
  if (verbose) msg2("Connecting to H2O server...")
  h2o::h2o.init(ip = ip, port = port, nthreads = n.cores)
  if (verbose) msg2("Creating H2O frames...")
  df.train <- h2o::as.h2o(data.frame(x), "df_train")
  if (!is.null(x.valid)) {
    df.valid <- h2o::as.h2o(data.frame(x.valid), "df_valid")
  } else {
    df.valid <- NULL
  }

  # GLRM ----
  if (verbose) msg2("Performing Generalized Low Rank Decomposition...")
  decom <- h2o::h2o.glrm(
    training_frame = df.train,
    model_id = paste0(
      "rtemis.H2OGLRM.",
      format(Sys.time(), "%b%d.%H:%M:%S.%Y")
    ),
    validation_frame = df.valid,
    k = k,
    transform = transform,
    loss = loss,
    regularization_x = regularization.x,
    regularization_y = regularization.y,
    gamma_x = gamma.x,
    gamma_y = gamma.y,
    max_iterations = max_iterations,
    max_updates = max_updates,
    init_step_size = init_step_size,
    min_step_size = min_step_size,
    seed = seed,
    init = init,
    svd_method = svd.method,
    ...
  )

  if (print.plot)
    mplot3_xy(
      decom@model$scoring_history$iteration,
      decom@model$scoring_history$objective,
      type = "l",
      zerolines = FALSE,
      xlab = "Iteration",
      ylab = "Objective",
      main = "Objective Function Value per Iteration",
      theme = plot.theme
    )

  # Projections ----
  projections.train <- data.matrix(x) %*% t(data.matrix(decom@model$archetypes))
  if (!is.null(x.test)) {
    projections.test <- data.matrix(x.test) %*%
      t(data.matrix(decom@model$archetypes))
  } else {
    projections.test <- NULL
  }

  # Outro ----
  extra <- list()
  rt <- rtDecom$new(
    decom.name = decom.name,
    decom = decom,
    xnames = xnames,
    projections.train = projections.train,
    projections.test = projections.test,
    parameters = list(
      k = k,
      transform = transform,
      loss = loss,
      regularization.x = regularization.x,
      regularization.y = regularization.y,
      gamma.x = gamma.x,
      gamma.y = gamma.y,
      max_iterations = max_iterations,
      max_updates = max_updates,
      init_step_size = init_step_size,
      min_step_size = min_step_size,
      seed = seed,
      init = init,
      svd.method = svd.method
    ),
    extra = extra
  )
  outro(start.time, verbose = verbose)
  rt
} # rtemis::d_H2OGLRM
