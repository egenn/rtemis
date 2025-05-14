# s_LIHAD.R
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

# Grow rule along with tree in global env, extract leaf rules and coefficients
# Added rpart.control min.bucket = 5
# was s.ADDTminobslin
# tree structure:
# tree
#    '-x             the data that belong to this node
#    '-y             the outcomes that belong to this node
#    '-Fval          the F value at this node
#    '-index         the index that was applied to the parent node to give this
#    '-depth         the depth of the tree at this levels
#    '-partlin       holds output of partLin(), if run
#          '-lin.coef         The linear model coefficients
#          '-part.c.left      The tree-derived constant for the left partition
#          '-part.c.right     The tree-derived constant for the right partition
#          '-lin.val          Fval of the linear model
#          '-part.val         Fval of the tree
#          '-cutFeat.name     The name of the feature on which the split was performed
#          '-cutFeat.point    The point at which the split was made, left split < this point
#                             if the cut feature was continuous
#          '-cutFeat.category The factor levels that correspond to the left split,
#                             if the the cut feature was categorical
#          '-part.index       The split index, with "L" and "R" for left and right, respectively
#          '-split.rule       The rule, based on cutFeat.name and cutFeat.point or cutFeat.category,
#                             based on which the split was made.
#          '-terminal         Logical: if TRUE, partlin did not split.
#    '-left          holds the left split, if the node was split
#    '-right         holds the right split, if the node was split
#    '-terminal      TRUE if it is a terminal node
#    '-type          "split", "nosplit", "max.depth", "minobsinnode"

#' The Linear Hard Hybrid Tree: Hard Additive Tree (no gamma) with Linear Nodes \[R\]
#'
#' Train a Linear Hard Hybrid Tree for Regression
#'
#' The Hybrid Tree grows a tree using a sequence of regularized linear models and tree
#' stumps.
#' Use s_LINAD for the standard Linear Additive Tree Algorithm, which grows branches
#' stepwise and includes all observations weighted by gamma
#'
#' Grid searched parameters: max.depth, alpha, lambda, minobsinnode, learning.rate,
#' part.cp
#'
#' @inheritParams s_GLM
#' @param max.depth \[gS\] Integer: Max depth of additive tree. Default = 3
#' @param alpha \[gS\] Float: `lincoef` alpha Overrides `lincoef.params` alpha
#' @param lambda \[gS\] Float: `lincoef` lambda. Overrides `lincoef.params` lambda
#' @param lincoef.params Named List: Output of [setup.lincoef]
#' @param minobsinnode \[gS\] Integer: Minimum N observations needed in node, before
#' considering splitting
#' @param minobsinnode.lin Integer: Minimum N observations needed in node in order to
#' train linear model.
#' @param learning.rate \[gS\] Float (0, 1): Learning rate.
#' @param part.cp \[gS\] Float: Minimum complexity needed to allow split by `rpart`.
#' @param part.max.depth Integer: Max depth for each tree model within the additive tree
#' @param cxrcoef Logical: Passed to [predict.lihad], if TRUE, returns cases by
#' coefficients matrix
#'
#' @author E.D. Gennatas
#' @export

s_LIHAD <- function(
  x,
  y = NULL,
  x.test = NULL,
  y.test = NULL,
  max.depth = 3,
  alpha = 0,
  lambda = .1,
  lincoef.params = setup.lincoef("glmnet"),
  minobsinnode = 2,
  minobsinnode.lin = 10,
  learning.rate = 1,
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  weights = NULL,
  metric = "MSE",
  maximize = FALSE,
  grid.resample.params = setup.grid.resample(),
  keep.x = FALSE,
  simplify = TRUE,
  cxrcoef = FALSE,
  n.cores = rtCores,
  verbose = TRUE,
  verbose.predict = FALSE,
  trace = 0,
  x.name = NULL,
  y.name = NULL,
  question = NULL,
  outdir = NULL,
  print.plot = FALSE,
  plot.fitted = NULL,
  plot.predicted = NULL,
  plot.theme = rtTheme,
  save.mod = FALSE
) {
  # Intro ----
  if (missing(x)) {
    print(args(s_LIHAD))
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
  mod.name <- "LIHAD"

  # Dependencies ----
  dependency_check("rpart")

  # Arguments ----
  if (is.null(x.name)) x.name <- getName(x, "x")
  if (is.null(y.name)) y.name <- getName(y, "y")
  if (!verbose) print.plot <- FALSE

  # Data ----
  dt <- prepare_data(
    x,
    y,
    x.test,
    y.test,
    # ifw = ifw, ifw.type = ifw.type,
    # upsample = upsample, resample.seed = resample.seed,
    verbose = verbose
  )
  x <- dt$x
  y <- dt$y
  x.test <- dt$x.test
  y.test <- dt$y.test
  xnames <- dt$xnames
  type <- dt$type
  if (type != "Regression")
    stop(
      "This function currently only supports Regression. Use s.AADDT for Classification"
    )
  if (verbose) dataSummary(x, y, x.test, y.test, type)
  if (verbose) {
    parameterSummary(
      max.depth,
      minobsinnode,
      lincoef.params,
      newline.pre = TRUE
    )
  }

  if (print.plot) {
    if (is.null(plot.fitted))
      plot.fitted <- if (is.null(y.test)) TRUE else FALSE
    if (is.null(plot.predicted))
      plot.predicted <- if (!is.null(y.test)) TRUE else FALSE
  } else {
    plot.fitted <- plot.predicted <- FALSE
  }

  # Check lincoef method
  if (NCOL(x) == 1) {
    if (!lincoef.params$method %in% c("glm", "sgd", "solve")) {
      if (verbose) {
        msg2(
          "Cannot use lincoef method",
          lincoef.params$method,
          "with a single feature;",
          "switching to 'glm'"
        )
      }
      lincoef.params$method <- "glm"
    }
  }

  # GLOBAL ----
  .env <- environment()

  # Grid Search ----
  if (
    gridCheck(max.depth, alpha, lambda, minobsinnode, learning.rate, part.cp)
  ) {
    gs <- gridSearchLearn(
      x,
      y,
      mod.name,
      resample.params = grid.resample.params,
      grid.params = list(
        max.depth = max.depth,
        alpha = alpha,
        lambda = lambda,
        minobsinnode = minobsinnode,
        learning.rate = learning.rate,
        part.cp = part.cp
      ),
      weights = weights,
      maximize = maximize,
      verbose = verbose,
      n.cores = n.cores
    )
    max.depth <- gs$best.tune$max.depth
    alpha <- gs$best.tune$alpha
    lambda <- gs$best.tune$lambda
    minobsinnode <- gs$best.tune$minobsinnode
    learning.rate <- gs$best.tune$learning.rate
    part.cp <- gs$best.tune$part.cp
  } else {
    gs <- NULL
  }

  # lin1 ----
  if (verbose) {
    msg20(
      "Training Hybrid Additive Tree (max depth = ",
      max.depth,
      ")...",
      newline.pre = TRUE
    )
  }

  lincoef.params$alpha <- alpha
  lincoef.params$lambda <- lambda
  coef.c <- do.call(lincoef, c(list(x = x, y = y), lincoef.params))
  Fval <- learning.rate * (data.matrix(cbind(1, x)) %*% coef.c)

  # .lihad ----
  root <- list(
    x = x,
    y = y,
    Fval = Fval,
    index = rep(1, length(y)),
    depth = 0,
    partlin = NULL, # To hold the output of partLin()
    left = NULL, # \  To hold the left and right nodes,
    right = NULL, # /  if partLin splits
    lin = NULL,
    part = NULL,
    coef.c = coef.c,
    terminal = FALSE,
    type = NULL,
    rule = "TRUE"
  )
  mod <- lihad(
    node = root,
    max.depth = max.depth,
    minobsinnode = minobsinnode,
    minobsinnode.lin = minobsinnode.lin,
    learning.rate = learning.rate,
    # alpha = alpha,
    # lambda = lambda,
    lincoef.params = lincoef.params,
    coef.c = coef.c,
    part.minsplit = part.minsplit,
    part.xval = part.xval,
    part.max.depth = part.max.depth,
    part.cp = part.cp,
    .env = .env,
    keep.x = keep.x,
    simplify = simplify,
    verbose = verbose,
    trace = trace
  )
  mod$leafs <- list(
    rule = .env$leaf.rule,
    coef = .env$leaf.coef
  )
  mod$learning.rate <- learning.rate
  class(mod) <- c("lihad", "list")

  parameters <- list(
    max.depth = max.depth,
    minobsinnode = minobsinnode,
    learning.rate = learning.rate,
    alpha = alpha,
    lambda = lambda,
    lincoef.params = lincoef.params
  )

  # Fitted ----
  fitted <- predict.lihad(
    mod,
    x,
    learning.rate = learning.rate,
    trace = trace,
    verbose = verbose.predict,
    cxrcoef = cxrcoef
  )
  if (cxrcoef) {
    cxrcoef <- fitted$cxrcoef
    fitted <- fitted$yhat
    mod$cxrcoef <- cxrcoef
  } else {
    cxrcoef <- NULL
  }
  error.train <- mod_error(y, fitted)
  if (verbose) errorSummary(error.train)

  # Predicted ----
  predicted <- error.test <- NULL
  if (!is.null(x.test)) {
    predicted <- predict.lihad(
      mod,
      x.test,
      learning.rate = learning.rate,
      trace = trace,
      verbose = verbose.predict
    )
    if (!is.null(y.test)) {
      error.test <- mod_error(y.test, predicted)
      if (verbose) errorSummary(error.test)
    }
  }

  # Outro ----
  rt <- rtModSet(
    mod = mod,
    mod.name = mod.name,
    type = type,
    gridsearch = gs,
    parameters = parameters,
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
    varimp = NULL,
    question = question
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

  outro(
    start.time,
    verbose = verbose,
    sinkOff = ifelse(is.null(logFile), FALSE, TRUE)
  )
  rt
} # rtemis:: s_LIHAD


#' \pkg{rtemis} internal: Recursive function to build Additive Tree
#'
#' @keywords internal
#' @noRd
lihad <- function(
  node = list(
    x = NULL,
    y = NULL,
    Fval = NULL,
    index = NULL,
    depth = NULL,
    partlin = NULL, # To hold the output of partLin()
    left = NULL, # \  To hold the left and right nodes,
    right = NULL, # /  if partLin splits
    lin = NULL,
    part = NULL,
    coef.c = NULL,
    terminal = NULL,
    type = NULL,
    rule = NULL
  ),
  coef.c = 0,
  max.depth = 7,
  minobsinnode = 2,
  minobsinnode.lin = 5,
  learning.rate = 1,
  # alpha = 0,
  # lambda = .01,
  lincoef.params = setup.lincoef(),
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  .env = NULL,
  keep.x = FALSE,
  simplify = FALSE,
  verbose = TRUE,
  trace = 0
) {
  # EXIT ----
  if (node$terminal) {
    return(node)
  }

  # lincoef.params$alpha <- alpha
  # lincoef.params$lambda <- lambda

  x <- node$x
  y <- node$y
  depth <- node$depth
  Fval <- node$Fval
  if (trace > 1) msg2("y is", y)
  if (trace > 1) msg2("Fval is", Fval)
  resid <- y - Fval
  nobsinnode <- length(node$index)

  # Add partlin to node ----
  if (node$depth < max.depth && nobsinnode >= minobsinnode) {
    if (trace > 1) msg2("y1 (resid) is", resid)
    node$partlin <- partLin(
      x1 = x,
      y1 = resid,
      lincoef.params = lincoef.params,
      part.minsplit = part.minsplit,
      part.xval = part.xval,
      part.max.depth = part.max.depth,
      part.cp = part.cp,
      minobsinnode.lin = minobsinnode.lin,
      verbose = verbose,
      trace = trace
    )
    # Fval <- Fval + learning.rate * (node$partlin$part.val + node$partlin$lin.val)
    # resid <- y - Fval
    if (trace > 1) msg2("Fval is", Fval)

    # '- If node split ----
    if (!node$partlin$terminal) {
      node$type <- "split"
      left.index <- node$partlin$left.index
      right.index <- node$partlin$right.index
      if (trace > 1)
        msg2("Depth:", depth, "left.index:", node$partlin$left.index)
      x.left <- x[left.index, , drop = FALSE]
      x.right <- x[right.index, , drop = FALSE]
      y.left <- y[left.index]
      y.right <- y[right.index]
      if (trace > 1) msg2("y.left is", y.left)
      if (trace > 1) msg2("y.right is", y.right)
      Fval.left <- Fval[left.index] +
        learning.rate *
          (node$partlin$part.val[left.index] + node$partlin$lin.val.left)
      Fval.right <- Fval[right.index] +
        learning.rate *
          (node$partlin$part.val[right.index] + node$partlin$lin.val.right)
      coef.c.left <- coef.c.right <- coef.c

      # Cumulative sum of coef.c
      coef.c.left <- coef.c.left +
        c(
          node$partlin$lin.coef.left[1] + node$partlin$part.c.left,
          node$partlin$lin.coef.left[-1]
        )
      coef.c.right <- coef.c.right +
        c(
          node$partlin$lin.coef.right[1] + node$partlin$part.c.right,
          node$partlin$lin.coef.right[-1]
        )
      if (trace > 1)
        msg2("coef.c.left is", coef.c.left, "coef.c.right is", coef.c.right)

      if (!is.null(node$partlin$cutFeat.point)) {
        rule.left <- node$partlin$split.rule
        rule.right <- gsub("<", ">=", node$partlin$split.rule)
      } else {
        rule.left <- node$partlin$split.rule
        rule.right <- paste0("!", rule.left) # fix: get cutFeat.name levels and find complement
      }

      # Init Left and Right nodes
      node$left <- list(
        x = x.left,
        y = y.left,
        Fval = Fval.left,
        index = left.index,
        depth = depth + 1,
        coef.c = coef.c.left,
        partlin = NULL, # To hold the output of partLin()
        left = NULL, # \  To hold the left and right nodes,
        right = NULL, # /  if partLin splits
        terminal = FALSE,
        type = NULL,
        rule = paste0(node$rule, " & ", node$partlin$rule.left)
      )
      node$right <- list(
        x = x.right,
        y = y.right,
        Fval = Fval.right,
        index = right.index,
        depth = depth + 1,
        coef.c = coef.c.right,
        partlin = NULL, # To hold the output of partLin()
        left = NULL, # \  To hold the right and right nodes,
        right = NULL, # /  if partLin splits
        terminal = FALSE,
        type = NULL,
        rule = paste0(node$rule, " & ", node$partlin$rule.right)
      )

      if (!keep.x) node$x <- NULL
      node$split.rule <- node$partlin$split.rule
      if (simplify) {
        node$y <- node$Fval <- node$index <- node$depth <- node$lin <- node$part <- node$type <- node$partlin <- NULL
      }

      # Run Left and Right nodes
      # LEFT ----
      if (trace > 0)
        msg2("Depth = ", depth + 1, "; Working on Left node...", sep = "")
      node$left <- lihad(
        node$left,
        coef.c = coef.c.left,
        max.depth = max.depth,
        minobsinnode = minobsinnode,
        minobsinnode.lin = minobsinnode.lin,
        learning.rate = learning.rate,
        lincoef.params = lincoef.params,
        part.minsplit = part.minsplit,
        part.xval = part.xval,
        part.max.depth = part.max.depth,
        part.cp = part.cp,
        .env = .env,
        keep.x = keep.x,
        simplify = simplify,
        verbose = verbose,
        trace = trace
      )
      # RIGHT ----
      if (trace > 0)
        msg2("Depth = ", depth + 1, "; Working on Right node...", sep = "")
      node$right <- lihad(
        node$right,
        coef.c = coef.c.right,
        max.depth = max.depth,
        minobsinnode = minobsinnode,
        minobsinnode.lin = minobsinnode.lin,
        learning.rate = learning.rate,
        lincoef.params = lincoef.params,
        part.minsplit = part.minsplit,
        part.xval = part.xval,
        part.max.depth = part.max.depth,
        part.cp = part.cp,
        .env = .env,
        keep.x = keep.x,
        simplify = simplify,
        verbose = verbose,
        trace = trace
      )
      if (simplify) node$coef.c <- NULL
    } else {
      # partLin did not split
      node$terminal <- TRUE
      .env$leaf.rule <- c(.env$leaf.rule, node$rule)
      .env$leaf.coef <- c(.env$leaf.coef, list(node$coef.c))
      node$type <- "nosplit"
      if (trace > 0) msg2("STOP: nosplit")
      if (simplify)
        node$x <- node$y <- node$Fval <- node$index <- node$depth <- node$type <- node$partlin <- NULL
    } # !node$terminal
  } else {
    # max.depth or minobsinnode reached
    node$terminal <- TRUE
    .env$leaf.rule <- c(.env$leaf.rule, node$rule)
    .env$leaf.coef <- c(.env$leaf.coef, list(node$coef.c))
    if (node$depth == max.depth) {
      if (trace > 0) msg2("STOP: max.depth")
      node$type <- "max.depth"
    } else if (nobsinnode < minobsinnode) {
      if (trace > 0) msg2("STOP: minobsinnode")
      node$type <- "minobsinnode"
    }
    if (simplify)
      node$x <- node$y <- node$Fval <- node$index <- node$depth <- node$type <- node$partlin <- NULL
    return(node)
  } # max.depth, minobsinnode

  node
} # rtemis::lihad


#' \pkg{rtemis} internal: Ridge and Stump
#'
#' Fit a linear model on (x, y) and a tree on the residual yhat - y
#'
#' @keywords internal
#' @noRd
partLin <- function(
  x1,
  y1,
  lincoef.params = setup.lincoef(),
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  minobsinnode.lin = 5,
  verbose = TRUE,
  trace = 0
) {
  # PART ----
  dat <- data.frame(x1, y1)
  part <- rpart::rpart(
    y1 ~ .,
    dat,
    control = rpart::rpart.control(
      minsplit = part.minsplit,
      xval = part.xval,
      maxdepth = part.max.depth,
      minbucket = 5,
      cp = part.cp
    )
  )
  part.val <- predict(part)

  if (is.null(part$splits)) {
    if (trace > 0) msg2("Note: rpart did not split")
    terminal <- TRUE
    cutFeat.name <- cutFeat.point <- cutFeat.category <- NULL
    split.rule <- NULL
    part.c.left <- part.c.right <- 0
    left.index <- right.index <- split.rule.left <- split.rule.right <- NULL
    lin.val.left <- lin.val.right <- 0
    lin.coef.left <- lin.coef.right <- rep(0, NCOL(x1) + 1)
  } else {
    if (part$splits[1, 2] == 1) {
      left.yval.row <- 3
      right.yval.row <- 2
    } else {
      left.yval.row <- 2
      right.yval.row <- 3
    }
    part.c.left <- part$frame$yval[left.yval.row]
    part.c.right <- part$frame$yval[right.yval.row]
    terminal <- FALSE
    cutFeat.name <- rownames(part$splits)[1]
    cutFeat.point <- cutFeat.category <- NULL
    if (!is.null(cutFeat.name)) {
      cutFeat.index <- which(names(x1) == cutFeat.name)
      if (is.numeric(x1[[cutFeat.name]])) {
        cutFeat.point <- part$splits[1, "index"]
        if (trace > 0) {
          msg2(
            "Split Feature is \"",
            cutFeat.name,
            "\"; Cut Point = ",
            cutFeat.point,
            sep = ""
          )
        }
        split.rule.left <- paste(cutFeat.name, "<", cutFeat.point)
        split.rule.right <- paste(cutFeat.name, ">=", cutFeat.point)
        # split.rule.i <- paste0("X[, ", cutFeat.index,"]", " < ", cutFeat.point)
      } else {
        cutFeat.category <- levels(x1[[cutFeat.name]])[which(
          part$csplit[1, ] == 1
        )]
        if (trace > 0) {
          msg2(
            "Split Feature is \"",
            cutFeat.name,
            "\"; Cut Category is \"",
            cutFeat.category,
            "\"",
            sep = ""
          )
        }
        split.rule.left <- paste0(
          cutFeat.name,
          " %in% ",
          "c(",
          paste(cutFeat.category, collapse = ", ")
        )
        split.rule.right <- paste0(
          "!",
          cutFeat.name,
          " %in% ",
          "c(",
          paste(cutFeat.category, collapse = ", ")
        )
        # split.rule.i <- paste0("X[, ", cutFeat.index,"]", " %in% ", "c(", paste(cutFeat.category, collapse = ", "))
      }
      # part.index <- rep("R", length(y1))
      if (length(cutFeat.point) > 0) {
        # CHANGE: calc left+right index directly here
        # part.index[x1[, cutFeat.index] < cutFeat.point] <- "L"
        left.index <- which(x1[, cutFeat.index] < cutFeat.point)
        right.index <- seq_len(NROW(x1))[-left.index]
      } else {
        # part.index[is.element(x1[, cutFeat.index], cutFeat.category)] <- "L"
        left.index <- which(is.element(x1[, cutFeat.index], cutFeat.category))
        right.index <- seq_len(NROW(x1))[-left.index]
      }
    }
  }

  # LIN ----
  resid <- y1 - part.val
  resid.left <- resid[left.index]
  resid.right <- resid[right.index]
  if (!is.null(cutFeat.name)) {
    if (is_constant(resid.left) || length(resid.left) < minobsinnode.lin) {
      if (trace > 0) {
        msg2(
          "Not fitting any more lines here (constant resid OR under minobsinnode.lin)"
        )
      }
      lin.val.left <- rep(0, length(left.index))
      lin.coef.left <- rep(0, NCOL(x1) + 1)
    } else {
      dat <- data.frame(x1[left.index, , drop = FALSE], resid.left)
      lin.coef.left <- do.call(
        lincoef,
        c(
          list(x = x1[left.index, , drop = FALSE], y = resid.left),
          lincoef.params
        )
      )
      lin.val.left <- (data.matrix(cbind(1, x1[left.index, , drop = FALSE])) %*%
        lin.coef.left)
    } # if (is_constant(resid.left))

    if (is_constant(resid.right) || length(resid.right) < minobsinnode.lin) {
      if (trace > 0)
        msg2(
          "Not fitting any more lines here (constant resid OR under minobsinnode.lin)"
        )
      lin.val.right <- rep(0, length(right.index))
      lin.coef.right <- rep(0, NCOL(x1) + 1)
    } else {
      lin.coef.right <- do.call(
        lincoef,
        c(
          list(x = x1[right.index, , drop = FALSE], y = resid.right),
          lincoef.params
        )
      )
      lin.val.right <- (data.matrix(cbind(
        1,
        x1[right.index, , drop = FALSE]
      )) %*%
        lin.coef.right)
    } # if (is_constant(resid.right))
  } # if (!is.null(cutFeat.name))

  list(
    lin.coef.left = lin.coef.left,
    lin.coef.right = lin.coef.right,
    part.c.left = part.c.left,
    part.c.right = part.c.right,
    lin.val.left = lin.val.left,
    lin.val.right = lin.val.right,
    part.val = part.val,
    cutFeat.name = cutFeat.name,
    cutFeat.point = cutFeat.point,
    cutFeat.category = cutFeat.category,
    left.index = left.index,
    right.index = right.index,
    split.rule = split.rule.left,
    # split.rule.i = split.rule.i,
    rule.left = split.rule.left,
    rule.right = split.rule.right,
    terminal = terminal
  )
} # rtemis::partLin


#' Print method for `lihad` object
#'
#' @param x `lihad` object
#' @param ... Not used
#'
#' @method print lihad
#' @author E.D. Gennatas
#' @export

print.lihad <- function(x, ...) {
  cat("\n  An Hybrid Additive Tree model\n\n")
  invisible(x)
}


# preorderMatch lihad ----
preorderMatch.lihad <- function(node, x, trace = 0) {
  # EXIT ----
  if (node$terminal) {
    return(node)
  }

  if (trace > 1) msg2("Evaluating rule at depth", node$depth)
  if (with(x, eval(parse(text = node$split.rule)))) {
    # LEFT ----
    if (trace > 1) msg2("      <--- Left")
    node <- preorderMatch.lihad(node$left, x, trace = trace)
  } else {
    # RIGHT ----
    if (trace > 1) msg2("           Right --->")
    node <- preorderMatch.lihad(node$right, x, trace = trace)
  }

  node
} # rtemis::preorderMatch.lihad


#' Predict method for `lihad` object
#'
#' @method predict lihad
#' @param object an `rtMod` trained with [s_LIHAD] or an `lihad` object
#' @param newdata data frame of predictor features
#' @param learning.rate Float: learning rate if `object` was `lihad`
#' @param n.feat Integer: internal use only
#' @param verbose Logical: If TRUE, print messages to console.
#' @param cxrcoef Logical: If TRUE, return matrix of cases by coefficients along with
#' predictions.
#' @param ... Not used
#'
#' @export
#' @author E.D. Gennatas

predict.lihad <- function(
  object,
  newdata = NULL,
  learning.rate = NULL,
  n.feat = NULL,
  verbose = FALSE,
  cxrcoef = FALSE,
  ...
) {
  if (inherits(object, "rtMod")) {
    if (verbose) msg2("Found rtMod object")
    tree <- object$mod
    learning.rate <- object$mod$learning.rate
    if (is.null(n.feat)) n.feat <- length(object$xnames)
  } else if (inherits(object, "lihad")) {
    if (verbose) msg2("Found lihad object")
    tree <- object
    learning.rate <- object$learning.rate
    # if (is.null(learning.rate)) stop("Please provide learning rate")
    if (is.null(n.feat)) n.feat <- NCOL(newdata)
  } else {
    stop(
      "Please provide an object of class 'rtMod' with a trained hybrid tree, or an 'lihad' object"
    )
  }

  # ENH: consider removing
  if (is.null(newdata)) {
    return(object$fitted)
  }

  # newdata colnames ----
  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0(
      "V",
      seq_len(NCOL(newdata))
    )
  }

  # PREDICT ----
  newdata <- newdata[, seq_len(n.feat), drop = FALSE]
  rules <- plyr::ldply(tree$leafs$rule)[, 1]
  cxr <- matchCasesByRules(newdata, rules, verbose = verbose)
  coefs <- plyr::laply(tree$leafs$coef, c)
  .cxrcoef <- cxr %*% coefs
  newdata <- data.matrix(cbind(1, newdata))
  yhat <- sapply(seq_len(NROW(newdata)), function(n) {
    learning.rate * (newdata[n, ] %*% t(.cxrcoef[n, , drop = FALSE]))
  })

  if (cxrcoef) {
    return(list(yhat = yhat, cxrcoef = .cxrcoef))
  } else {
    return(yhat)
  }
} # rtemis:: predict.lihad


#' Extract coefficients from Additive Tree leaves
#'
#' @param object `lihad` object
#' @param newdata matrix/data.frame of features
#' @param verbose Logical: If TRUE, print output to console
#' @param trace Integer {0:2} Increase verbosity
#' @author E.D. Gennatas
#' @export
betas.lihad <- function(object, newdata, verbose = FALSE, trace = 0) {
  if (inherits(object, "rtMod")) {
    tree <- object$mod
  } else if (inherits(object, "lihad")) {
    tree <- object
  } else {
    stop(
      "Please provide an object of class 'rtMod' with a trained additive tree, or an 'lihad' object"
    )
  }

  # newdata colnames ----
  newdata <- as.data.frame(newdata)
  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0(
      "V",
      seq_len(NCOL(newdata))
    )
  }

  # BETAS ----
  ncases <- NROW(newdata)
  betas <- as.data.frame(matrix(nrow = NROW(newdata), ncol = NCOL(newdata) + 1))

  # TODO: replace with fast data.table matchRules on leafs rules and coefs list
  for (i in seq(ncases)) {
    leaf <- preorderMatch.lihad(tree, newdata[i, , drop = FALSE], trace = trace)
    # betas[i, ] <- rowSums(as.data.frame(leaf$coef.c)[-1, , drop = FALSE])
    betas[i, ] <- leaf$coef.c
  }
  colnames(betas) <- c("Intercept", colnames(newdata))
  betas
} # rtemis:: betas.lihad


# preorder adddt ----
preorder.lihad <- function(node, x, trace = 0) {
  # EXIT ----
  if (node$terminal) {
    return(node)
  }

  if (trace > 1) msg2("Evaluating rule at depth", node$depth)
  if (with(x, eval(parse(text = node$split.rule)))) {
    # LEFT ----
    if (trace > 1) msg2("      <--- Left")
    node <- preorder.lihad(node$left, x, trace = trace)
  } else {
    # RIGHT ----
    if (trace > 1) msg2("           Right --->")
    node <- preorder.lihad(node$right, x, trace = trace)
  }

  node
} # rtemis::preorder.lihad


#' Extract coefficients from Hybrid Additive Tree leaves
#'
#' @param object `lihad` object
#' @param newdata matrix/data.frame of features
#' @param verbose Logical: If TRUE, print output to console
#' @param trace Integer {0:2} Increase verbosity
#' @param ... Not used
#' @author E.D. Gennatas
#' @export
coef.lihad <- function(object, newdata, verbose = FALSE, trace = 0, ...) {
  if (inherits(object, "rtMod")) {
    tree <- object$mod
  } else if (inherits(object, "lihad")) {
    tree <- object
  } else {
    stop(
      "Please provide an object of class 'rtMod' with a trained additive tree, or an 'lihad' object"
    )
  }

  # newdata colnames ----
  newdata <- as.data.frame(newdata)
  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0(
      "V",
      seq_len(NCOL(newdata))
    )
  }

  # BETAS ----
  ncases <- NROW(newdata)
  betas <- as.data.frame(matrix(nrow = NROW(newdata), ncol = NCOL(newdata) + 1))

  # TODO: replace with fast data.table matchRules on leafs rules and coefs list
  for (i in seq(ncases)) {
    leaf <- preorderMatch.lihad(tree, newdata[i, , drop = FALSE], trace = trace)
    # betas[i, ] <- rowSums(as.data.frame(leaf$coef.c)[-1, , drop = FALSE])
    betas[i, ] <- leaf$coef.c
  }
  colnames(betas) <- c("Intercept", colnames(newdata))
  betas
} # rtemis:: betas.lihad
