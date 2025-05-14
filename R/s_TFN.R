# s_TFN.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Feedforward Neural Network with \pkg{tensorflow} (C, R)
#'
#' Train an Feedforward Neural Network using \pkg{keras} and \pkg{tensorflow}
#'
#' For more information on arguments and hyperparameters, see (https://keras.rstudio.com/) and (https://keras.io/)
#' It is important to define network structure and adjust hyperparameters based on your problem. You cannot expect
#' defaults to work on any given dataset.
#' @inheritParams s_GLM
#' @param class.weights Numeric vector: Class weights for training.
#' @param net Pre-defined keras network to be trained (optional)
#' @param n.hidden.nodes Integer vector: Length must be equal to the number of hidden layers you wish to create.
#' Can be zero, in which case you get a linear model. Default = N of features, i.e. NCOL(x)
#' @param initializer Character: Initializer to use for each layer: "glorot_uniform", "glorot_normal", "he_uniform",
#' "he_normal", "cun_uniform", "lecun_normal", "random_uniform", "random_normal", "variance_scaling",
#' "truncated_normal", "orthogonal", "zeros", "ones", "constant".
#' Glorot is also known as Xavier initialization.
#' @param initializer.seed Integer: Seed to use for each initializer for reproducibility.
#' @param dropout Floar, vector, (0, 1): Probability of dropping nodes. Can be a vector of length equal to N of layers,
#' otherwise will be recycled. Default = 0
#' @param activation String vector: Activation type to use: "relu", "selu", "elu", "sigmoid", "hard_sigmoid", "tanh",
#' "exponential", "linear", "softmax", "softplus", "softsign". Defaults to "relu" for Classification and
#' "tanh" for Regression
#' @param kernel_l1 Float: l1 penalty on weights.
#' @param kernel_l2 Float: l2 penalty on weights.
#' @param activation_l1 Float: l1 penalty on layer output.
#' @param activation_l2 Float: l2 penalty on layer output.
#' @param batch.normalization Logical: If TRUE, batch normalize after each hidden layer.
#' @param output Character: Activation to use for output layer. Can be any as in `activation`.
#' Default = "linear" for Regression, "sigmoid" for binary classification, "softmax" for multiclass
#' @param loss Character: Loss to use: Default = "mean_squared_error" for regression, "binary_crossentropy" for binary
#' classification, "sparse_categorical_crossentropy" for multiclass
#' @param optimizer Character: Optimization to use: "rmsprop", "adadelta", "adagrad", "adam", "adamax", "nadam", "sgd".
#' Default = "rmsprop"
#' @param learning.rate Float: learning rate. Defaults depend on `optimizer` used and are:
#' `rmsprop = .01, adadelta = 1, adagrad = .01, adamax = .002, adam = .001, nadam = .002, sgd = .1`
#' @param metric Character: Metric used for evaluation during train. Default = "mse" for regression,
#'  "accuracy" for classification.
#' @param epochs Integer: Number of epochs. Default = 100
#' @param batch.size Integer: Batch size. Default = N of cases
#' @param validation.split Float (0, 1): proportion of training data to use for validation. Default = .2
#' @param callback Function to be called by keras during fitting.
#' Default = `keras::callback_early_stopping(patience = 150)` for early stopping.
#' @param scale Logical: If TRUE, scale featues before training.
#' column means and standard deviation will be saved in `rtMod$extra` field to allow
#' scaling ahead of prediction on new data
#' @param ... Additional parameters
#'
#' @author E.D. Gennatas
#' @seealso [train_cv] for external cross-validation
#' @family Supervised Learning
#' @family Deep Learning
#' @export

s_TFN <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  # x.valid = NULL, y.valid = NULL,
  class.weights = NULL,
  ifw = TRUE,
  ifw.type = 2,
  upsample = FALSE,
  downsample = FALSE,
  resample.seed = NULL,
  net = NULL,
  n.hidden.nodes = NULL,
  initializer = c(
    "glorot_uniform",
    "glorot_normal",
    "he_uniform",
    "he_normal",
    "lecun_uniform",
    "lecun_normal",
    "random_uniform",
    "random_normal",
    "variance_scaling",
    "truncated_normal",
    "orthogonal",
    "zeros",
    "ones",
    "constant"
  ),
  initializer.seed = NULL,
  dropout = 0,
  activation = c(
    "relu",
    "selu",
    "elu",
    "sigmoid",
    "hard_sigmoid",
    "tanh",
    "exponential",
    "linear",
    "softmax",
    "softplus",
    "softsign"
  ),
  kernel_l1 = .1,
  kernel_l2 = 0,
  activation_l1 = 0,
  activation_l2 = 0,
  batch.normalization = TRUE,
  output = NULL,
  loss = NULL,
  optimizer = c(
    "rmsprop",
    "adadelta",
    "adagrad",
    "adam",
    "adamax",
    "nadam",
    "sgd"
  ),
  learning.rate = NULL,
  metric = NULL,
  epochs = 100,
  batch.size = NULL,
  validation.split = .2,
  callback = keras::callback_early_stopping(patience = 150),
  scale = TRUE,
  x.name = NULL,
  y.name = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  question = NULL,
  verbose = TRUE,
  outdir = NULL,
  save.mod = ifelse(!is.null(outdir), TRUE, FALSE),
  ...
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_TFN))
    return(invisible(9))
  }
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
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
  mod.name <- "TFN"

  # Dependencies ----
  dependency_check("tensorflow")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE
  verbose <- verbose | !is.null(logFile)
  if (save.mod && is.null(outdir)) outdir <- paste0("./s.", mod.name)
  if (!is.null(outdir))
    outdir <- paste0(normalizePath(outdir, mustWork = FALSE), "/")
  initializer <- match.arg(initializer)
  initializer <- paste0("initializer_", initializer)
  initializer <- getFromNamespace(initializer, "keras")

  optimizer <- match.arg(optimizer)
  if (is.null(learning.rate)) {
    learning.rate <- switch(
      optimizer,
      rmsprop = .01,
      adadelta = 1,
      adagrad = .01,
      adamax = .002,
      adam = .001,
      nadam = .002,
      sgd = .1
    )
  }
  optimizer <- paste0("optimizer_", optimizer)
  optimizer <- getFromNamespace(optimizer, "keras")

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    ifw = ifw,
    ifw.type = ifw.type,
    upsample = upsample,
    downsample = downsample,
    resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  # x.valid <- dt$x.valid
  # y.valid <- dt$y.valid
  xnames <- dt$xnames
  type <- dt$type
  checkType(type, c("Classification", "Regression"), mod.name)
  .class.weights <- if (is.null(class.weights) && ifw) dt$class.weights else
    class.weights
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  x.dm <- data.matrix(x)
  n.features <- NCOL(x)

  # Activation
  if (length(activation) > 1) {
    activation <- ifelse(type == "Classification", "relu", "tanh")
  }

  # Outcome
  .class.weights.int <- NULL
  if (type == "Classification") {
    y0 <- y
    y <- as.numeric(y) - 1
    n.classes <- length(levels(y0))
    if (!is.null(.class.weights)) {
      .class.weights.int <- as.list(.class.weights)
      names(.class.weights.int) <- seq(n.classes) - 1
    }
  }

  # Loss
  if (is.null(loss)) {
    if (type == "Classification") {
      loss <- if (n.classes == 2) "binary_crossentropy" else
        "sparse_categorical_crossentropy"
    } else {
      loss <- "mean_squared_error"
    }
  }
  if (type == "Classification" && loss == "categorical_crossentropy")
    y <- keras::to_categorical(y)

  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Normalize ----
  # Normalize training data
  if (scale) {
    x.dm <- scale(x.dm)
    col_means_train <- attr(x.dm, "scaled:center")
    col_stddevs_train <- attr(x.dm, "scaled:scale")
    if (!is.null(x.test)) {
      x.test <- scale(
        x.test,
        center = col_means_train,
        scale = col_stddevs_train
      )
    }
  }

  # Default n.hidden.nodes
  if (is.null(n.hidden.nodes)) n.hidden.nodes <- n.features

  # Metric
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "accuracy"
    } else {
      metric <- "mean_squared_error"
    }
  }

  # Default batch.size
  if (is.null(batch.size)) {
    batch.size <- floor(.25 * length(y))
  }

  # Network ----
  if (n.hidden.nodes[1] == 0) {
    n.hnodes <- n.hlayers <- 0
  } else {
    n.hnodes <- n.hidden.nodes
    n.hlayers <- length(n.hidden.nodes)
  }
  if (length(dropout) < n.hlayers)
    dropout <- rep(dropout, length.out = n.hlayers)

  ### Init ----
  if (is.null(net)) {
    net <- keras::keras_model_sequential()

    ### Hidden layers ----
    if (n.hlayers > 0) {
      for (i in seq(n.hlayers)) {
        keras::layer_dense(
          net,
          units = n.hnodes[i],
          activation = activation,
          input_shape = n.features,
          kernel_initializer = initializer(seed = initializer.seed),
          kernel_regularizer = keras::regularizer_l1_l2(
            l1 = kernel_l1,
            l2 = kernel_l2
          ),
          name = paste0("rt_Dense_", i)
        )
        if (activation_l1 != 0 || activation_l2 != 0) {
          keras::layer_activity_regularization(
            net,
            l1 = activation_l1,
            l2 = activation_l2,
            name = paste0("rt_Reg_", i)
          )
        }
        if (batch.normalization) {
          keras::layer_batch_normalization(net, name = paste0("rt_BN_", i))
        }
        keras::layer_dropout(
          net,
          rate = dropout[i],
          name = paste0("rt_Dropout_", i)
        )
      }
    } # /if (n.hlayers > 0)

    ### Output ----
    n.outputs <- if (type == "Regression") 1 else n.classes
    if (loss == "binary_crossentropy") n.outputs <- 1
    if (is.null(output)) {
      if (type == "Classification") {
        output <- if (n.outputs > 1) "softmax" else "sigmoid"
      } else {
        output <- "linear"
      }
    }

    keras::layer_dense(
      net,
      units = n.outputs,
      activation = output,
      name = "rt_Output"
    )

    # Parameters ----
    parameters <- list(
      n.hidden.nodes = n.hidden.nodes,
      batch.size = batch.size,
      batch.normalization = batch.normalization,
      epochs = epochs,
      optimizer = optimizer,
      learning.rate = learning.rate,
      metric = metric
    )
    if (verbose) {
      printls(
        parameters,
        title = "ANN parameters",
        center.title = TRUE,
        pad = 0,
        newline.pre = TRUE
      )
    }

    # TF ----
    if (verbose) {
      msg20(
        "Training Neural Network ",
        type,
        " with ",
        n.hlayers,
        " hidden ",
        ifelse(n.hlayers == 1, "layer", "layers"),
        "...\n",
        newline.pre = TRUE
      )
    }

    # Compile ----
    net |>
      keras::compile(
        loss = loss,
        optimizer = optimizer(lr = learning.rate),
        metrics = metric
      )
  } else {
    if (verbose) msg2("Training pre-built Network for", type, "...")
  }

  # Fit ----
  net |>
    keras::fit(
      x.dm,
      y,
      epochs = epochs,
      batch_size = batch.size,
      validation_split = validation.split,
      callback = callback,
      class_weight = .class.weights.int
    )

  # Fitted ----
  if (type == "Regression") {
    fitted <- c(predict(net, x.dm))
    error.train <- mod_error(y, fitted, type = type)
  } else {
    fitted.prob <- keras::predict_proba(net, x.dm)
    fitted <- factor(c(keras::predict_classes(net, x.dm)))
    levels(fitted) <- levels(y0) # levels are 0, 1, 2 before conversion
    error.train <- mod_error(y0, fitted, type = type)
  }

  if (verbose) errorSummary(error.train, mod.name)

  # Predicted ----
  predicted.prob <- predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    if (type == "Regression") {
      predicted <- c(predict(net, data.matrix(x.test)))
    } else {
      predicted.prob <- keras::predict_proba(net, data.matrix(x.test))
      predicted <- factor(c(keras::predict_classes(net, data.matrix(x.test))))
      levels(predicted) <- levels(y0)
    }
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test, mod.name)
    }
  }

  # Outro ----
  extra <- list(
    scale = scale,
    col_means_train = if (scale) col_means_train else NULL,
    col_stddevs_train = if (scale) col_stddevs_train else NULL
  )
  rt <- rtModSet(
    mod.name = mod.name,
    type = type,
    y.train = if (type == "Classification") y0 else y,
    y.test = y.test,
    x.name = x.name,
    xnames = xnames,
    mod = net,
    fitted = fitted,
    fitted.prob = fitted.prob,
    se.fit = NULL,
    error.train = error.train,
    predicted = predicted,
    predicted.prob = predicted.prob,
    se.prediction = NULL,
    parameters = parameters,
    error.test = error.test,
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
    save.mod,
    verbose,
    plot.theme
  )

  if (save.mod)
    keras::save_model_hdf5(net, filepath = paste0(outdir, "rt_kerasTF"))
  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis::s_TFN
