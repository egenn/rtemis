# linadleaves.R
# ::rtemis::
# E.D. Gennatas rtemis.org
# g$n.nodes
# => full linear model as first step

#' \pkg{rtemis internal}: Low-level Stepwise Linear Additive Tree procedure
#'
#' Train a Linear Additive Tree for Classification & Regression
#'
#' With `max.nodes = 0`, the output is a linear model trained according to
#' `lin.type`.
#' Note that lambda is treated differently by `glmnet::glmnet` and
#' `MASS::lm.ridge`
#' @inheritParams s_LINAD
#' @param x Data frame
#' @param max.leaves Integer: Total number of terminal nodes to reach.
#' 1 is a special case where no split is performed and a linear model is
#' trained. Otherwise, this should be an even number as each split introduces
#' two children nodes.
#' @param loss.fn Function with arguments `y, Fval `
#' Allows you to define a custom loss function. Defaults to `class.loss()`
#' for classification and `mse()` for regression
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

# [[---1. linadleaves---]] ----
linadleaves <- function(
  x,
  y,
  x.valid = NULL,
  y.valid = NULL,
  type,
  lookback = FALSE,
  weights = NULL,
  max.leaves = 5,
  gamleaves = FALSE,
  gamlearner = "s_GAMSEL",
  gam.params = list(degrees = 5),
  learning.rate = 1,
  minobsinnode.lin = 10,
  lin.type = "glmnet",
  first.lin.type = "glmnet", #
  first.lin.learning.rate = 1,
  first.lin.alpha = 1,
  first.lin.lambda = NULL,
  gamma = .01,
  gamma.on.lin = FALSE,
  select.leaves.smooth = FALSE,
  alpha = 1,
  lambda = .01,
  lambda.seq = NULL,
  cv.glmnet.nfolds = 5,
  which.cv.glmnet.lambda = "lambda.1se",
  nvmax = 2,
  part.minsplit = 2,
  part.xval = 0,
  part.max.depth = 1,
  part.cp = 0,
  part.minbucket = 1,
  .rho = TRUE,
  rho.max = 1000,
  rho.def = .1,
  loss.fn = NULL,
  verbose = TRUE,
  plot.tuning = TRUE,
  trace = 0
) {
  # Arguments  ----
  .class <- type == "Classification"
  # .surv <- type == "Survival"
  if (is.null(loss.fn)) {
    loss.fn <- switch(
      type,
      Regression = mse,
      Classification = class.loss
      #  Survival = surv.loss
    )
  }
  yraw <- y
  if (.class) {
    ylevels <- levels(y)
    levels(y) <- c(1, -1)
    y <- as.numeric(as.character(y))
  } else {
    ylevels <- NULL
    .rho <- FALSE
  }

  if (gamleaves) .gamlearner <- select_learn(gamlearner)

  if (is.null(weights)) weights <- rep(1, NROW(y))

  # Check lookback
  if (lookback && is.null(x.valid) && max.leaves > 1) {
    stop("You have asked for lookback without providing a validation set.")
  }

  # Check y is not constant ----
  # Todo: init for .class
  if (is_constant(y)) {
    # No training
    coefs <- rep(0, NCOL(x) + 1)
    names(coefs) <- c("(Intercept)", colnames(x))
    .mod <- list(
      type = type,
      init = mean(y),
      learning.rate = 1,
      tree = NULL,
      n.nodes = 0,
      included = NULL,
      terminal = NULL,
      open = NULL,
      closed = NULL,
      nosplit = NULL,
      leaves = list(
        id = NULL,
        rules = "TRUE",
        coefs = coefs
      ),
      ylevels = ylevels
    )
    class(.mod) <- c("linadleaves", "list")
    return(.mod)
  }

  # Arguments  ----
  if (NCOL(x) == 1) {
    if (lin.type != "glm") {
      lin.type <- "glm"
      warning("Forcing lin.type to glm because x has a single column")
    }
  }
  if (trace > 1) msg20("Using lin.type '", lin.type, "'")

  # Global  ----
  g <- new.env()
  # g$x required by rpart, g$xm used by lincoef
  g$x <- x
  g$xm <- model.matrix(~., data = x)
  g$ncolxm <- NCOL(g$xm)
  g$y <- y
  g$x.valid <- x.valid
  g$y.valid <- y.valid
  g$weights <- weights
  g$learning.rate <- learning.rate
  g$tree <- list()
  g$n.nodes <- 0
  g$n.leaves <- 0
  g$nosplit <- integer()
  g$terminal <- integer()
  g$allrules <- character()
  g$include <- integer()
  g$error <- numeric()
  g$loss.fn <- loss.fn
  g$type <- type
  # g$min.update <- min.update
  g$.rho <- .rho
  g$rho.max <- rho.max

  # stepindex is a list with N elements = N iterations,
  # each element holds the index of all terminal nodes up to that step
  g$stepindex <- vector("list", max.leaves)
  names(g$stepindex) <- paste(seq(max.leaves))
  g$stepindex$`1` <- 1

  # Loop: step splitline  ----
  if (.class) {
    # Classification
    # vector: Initial probabilities
    # probability <- weights / NROW(y) # n
    # '- Init ----
    # scalar: Initial node value
    # g$init <- c(log(1 + probability %*% y) - log(1 - probability %*% y)) # 1
    g$init <- 0
  } else {
    # Regression, Survival
    g$init <- mean(y) # 1
  }

  # max.leaves == 1 special cases ----
  # return linear model
  if (max.leaves == 1) {
    if (lin.type == "none") lin.type <- "constant"

    # Linear model
    index <- is.na(y)
    coef <- lincoef(
      x = g$xm[!index, -1, drop = FALSE],
      y = yraw[!index],
      learning.rate = first.lin.learning.rate, # should this be fixed to 1?
      type = type,
      weights = weights[!index],
      method = first.lin.type,
      nvmax = nvmax,
      alpha = first.lin.alpha,
      lambda = first.lin.lambda,
      lambda.seq = lambda.seq,
      cv.glmnet.nfolds = cv.glmnet.nfolds,
      which.cv.glmnet.lambda = which.cv.glmnet.lambda
    )

    leaves <- list(
      rules = data.frame(
        id = 0,
        rule = "TRUE",
        stringsAsFactors = FALSE
      ),
      coefs = t(data.frame(coef))
    )
    args <- c(
      list(
        x = g$x,
        y = g$y,
        verbose = trace > 1,
        print.plot = FALSE
      ),
      gam.params
    )
    if (gamleaves) {
      leaves$gams <- list(do.call(.gamlearner, args))
      g$init <- 0
    }
    .mod <- list(
      type = type,
      init = g$init,
      tree = list(
        id = 1,
        index = TRUE,
        Fval = NA,
        weights = weights,
        depth = 0,
        terminal = TRUE,
        rule = "TRUE",
        split.rule = NA
      ),
      learning.rate = learning.rate,
      gamleaves = gamleaves,
      n.nodes = 1,
      n.leaves = 1,
      included = NULL,
      terminal = NULL,
      open = NULL,
      closed = NULL,
      nosplit = NULL,
      leaves = leaves,
      all.step.leaves = leaves, # This is needed for predict()
      stepindex = g$stepindex,
      ylevels = ylevels
    )
    class(.mod) <- c("linadleaves", "list")
    return(.mod)
  } # / if (max.leaves == 1)

  # vector: Initial observations
  Fval <- rep(g$init, NROW(y)) # n

  # '- Gradient ----
  if (.class) {
    firstDer <- -2 * g$y / (1 + exp(2 * g$y * Fval)) # n
    # } else if (.surv) {
    #     firstDer <- surv.resid(y, Fval)
  } else {
    firstDer <- -(y - Fval)
  }

  resid <- -firstDer # n

  # '- Lin1 ----
  if (verbose)
    msg20(
      "Training Linear Additive Tree ",
      type,
      " (max leaves = ",
      max.leaves,
      ")..."
    )
  if (trace > 0) msg2("Training first Linear Model...")
  if (is_constant(resid)) stop("First gradient is constant")

  index <- is.na(resid)
  coef <- lincoef(
    x = g$xm[!index, -1, drop = FALSE],
    y = resid[!index],
    learning.rate = first.lin.learning.rate,
    weights = weights[!index],
    method = first.lin.type,
    nvmax = nvmax,
    alpha = alpha,
    lambda = lambda,
    lambda.seq = lambda.seq,
    cv.glmnet.nfolds = cv.glmnet.nfolds,
    which.cv.glmnet.lambda = which.cv.glmnet.lambda
  )

  g$n.leaves <- 1 # ddlt
  linVal <- c(g$xm %*% coef) # n

  if (.class && .rho && lin.type != "none") {
    # check learning.rate
    firstDer.rho <- t(
      (-2 * 1 / learning.rate * linVal * y) / (1 + exp(2 * y * Fval))
    ) %*%
      weights
    secDer.rho <- t(
      (4 * 1 / learning.rate * linVal^2 * exp(2 * y * Fval)) /
        (1 + exp(2 * y * Fval))^2
    ) %*%
      weights
    rho <- -firstDer.rho / secDer.rho
    if (trace > 1) {
      if (any(rho > rho.max))
        warning("rho values > rho.max =", rho.max, "found")
    }
    rho <- c(sign(rho) * min(rho.max, rho, na.rm = TRUE))
  } else {
    rho <- 1
  }

  # rhoprotect
  if (anyNA(rho)) {
    if (trace > 1) warning("NAs found in rho")
    rho[is.na(rho)] <- rho.def
  }
  if (anyNA(rho)) {
    if (trace > 1) warning("NAs found in rho")
    rho[is.na(rho)] <- rho.def
  }

  # orig before linmod1
  # Fval <- Fval + learning.rate * rho * linVal # n
  # linmod1
  Fval <- Fval + rho * linVal # n

  # '- Root ----
  root <- setNodeRC(
    g = g,
    id = 1,
    index = seq_len(NROW(y)),
    Fval = Fval,
    weights = weights,
    depth = 0,
    coef = coef,
    terminal = FALSE,
    type = NULL,
    condition = "TRUE",
    split.rule = NULL,
    rule = "TRUE"
  )

  # '- Init nodes list ----
  g$tree[["1"]] <- root
  # open nodes are candidates for splitting
  g$open <- 1
  g$closed <- integer()

  # '- Start loop ----
  stepid <- 1
  while (g$n.leaves < max.leaves && length(g$open) > 0) {
    if (trace > 1) {
      msg2("g$closed is", g$closed)
      msg2("g$open is", g$open)
      msg2("g$include is", g$include)
      msg2("g$terminal is", g$terminal)
      msg2("g$n.leaves is", g$n.leaves)
    }

    # Work on all candidate splits (open nodes)
    for (i in g$open) {
      if (is.null(g$tree[[paste(i)]]$split.rule)) {
        if (trace > 0) msg20("Working on node id #", i, "...")
        splitlineRC(
          g = g,
          type = type,
          node.index = i,
          gamma = gamma,
          gamma.on.lin = gamma.on.lin,
          alpha = alpha,
          lambda = lambda,
          lambda.seq = lambda.seq,
          cv.glmnet.nfolds = cv.glmnet.nfolds,
          part.minsplit = part.minsplit,
          part.xval = part.xval,
          part.max.depth = part.max.depth,
          part.cp = part.cp,
          part.minbucket = part.minbucket,
          minobsinnode.lin = minobsinnode.lin,
          lin.type = lin.type,
          nvmax = nvmax,
          verbose = verbose,
          trace = trace
        )
        if (trace > 1)
          msg20("Node #", i, ": split.rule: ", g$tree[[paste(i)]]$split.rule)
      } else {
        if (trace > 2) msg2("Node #", i, " already processed", sep = "")
      }
    } # /for (i in g$open) splitline

    if (length(g$tree) == 3) {
      if (trace > 1) msg2("Tree length is", length(g$tree))
      g$open <- c(2, 3)
      g$include <- c(1, 2, 3)
      g$closed <- 1
      g$terminal <- c(2, 3)
      # g$n.nodes <- 2
      g$n.leaves <- 2
      g$stepindex[["2"]] <- 2:3
      # g$steprules <- c(g$steprules, getStepRules(g, 2:3))
    } else {
      # Nodes that did not split are removed from open by splitline
      if (trace > 1) msg2("+++ g$open is", g$open)
      if (trace > 1) msg2("+++ g$nosplit is", g$nosplit)

      if (length(g$open) > 0) {
        # '- Loss ----
        # Find split with max loss reduction
        # 1.Error reduction for each open node
        open.loss.red <- data.frame(
          id = g$open,
          loss.red = plyr::ldply(g$open, function(j) g$tree[[paste(j)]]$loss)[,
            1
          ] -
            plyr::ldply(g$open, function(j) g$tree[[paste(j)]]$split.loss)[, 1]
        )

        if (trace > 1) print(open.loss.red)
        selected <- open.loss.red$id[which.max(open.loss.red$loss.red)]
        # Did selected reduce loss
        selected.loss <- open.loss.red$loss.red[open.loss.red$id == selected]
        selected.red <- length(selected) > 0 &&
          !is.na(selected.loss) &&
          selected.loss > 0
        toclose <- open.loss.red$id[which(is.na(open.loss.red$loss.red))]
        if (length(toclose) > 0) {
          for (i in toclose) {
            g$open <- setdiff(g$open, i)
            g$closed <- c(g$closed, i)
            if (trace > 1)
              msg20("Node id #", i, " had NA loss.red and was closed")
          }
        }

        if ((selected.red)) {
          if (trace > 1) msg20(">>> Selected node #", selected)
          if (trace > 1) msg2("g$terminal is", g$terminal)
          # +tree: Remove selected from terminal
          if (trace > 1) msg2("Removing selected from terminal nodes")
          g$terminal <- setdiff(g$terminal, selected)
          # +tree: Add selected's childrens to terminal
          if (trace > 1) msg2("Adding selected's children to terminal nodes")
          g$terminal <- c(g$terminal, selected * 2, selected * 2 + 1)
          if (trace > 1) msg2("g$terminal is now", g$terminal)
          # +tree: Add selected to closed
          # Add selected to closed
          g$closed <- c(g$closed, selected)
          # Add selected's sibling to closed if it was a nosplit, otherwise it's still open
          # sibling <- if (selected %% 2 == 0) selected + 1 else selected - 1
          # if (sibling %in% g$nosplit) g$closed <- c(g$closed, sibling)

          # +tree: Remove selected from open
          g$open <- setdiff(g$open, selected)

          # +tree: Add selected's children to open
          g$open <- c(g$open, selected * 2, selected * 2 + 1)
          if (trace > 1) msg2("g$open is now", g$open)

          # Nodes in tree are all closed nodes plus the terminals
          g$include <- union(union(g$closed, g$terminal), g$open)
          # -1 subtracts the root from the count
          g$n.nodes <- length(g$include) - 1
          if (trace > 1) msg2("g$n.nodes is", g$n.nodes)
          g$n.leaves <- length(g$terminal)
        } else {
          g$closed <- c(g$closed, selected)
          g$open <- setdiff(g$open, selected)
          if (trace > 1) {
            msg20(
              "Node id #",
              selected,
              "
                             did not reduce loss and was closed"
            )
          }
        }
      }
    } # /if (length(g$tree) == 3)

    # Update stepindex
    if (trace > 1) msg2("Updating steprules...")
    stepid <- stepid + 1
    g$stepindex[[stepid]] <- g$terminal
  } # /while (g$n.nodes <= max.nodes)

  # Add open and nosplit nodes to included

  # Purge  ----
  # if (verbose) msg20("Reached ", g$n.leaves, " leaves (", g$n.nodes, " nodes total)")
  if (verbose) msg20("Reached ", g$n.leaves, " leaves.")
  if (g$n.nodes == 2) {
    g$tree[[paste(2)]]$terminal <- g$tree[[paste(3)]]$terminal <- TRUE
    g$closed <- c(1, 2, 3)
    g$terminal <- c(2, 3)
    included <- c(1, 2, 3)
  } else {
    if (trace > 1) msg2("Purging excluded nodes...")
    # old:
    # for (k in c(g$open, g$nosplit)) g$tree[[paste(k)]] <- NULL
    # new: remove !g$included
    included <- union(g$closed, g$terminal)
    for (k in setdiff(names(g$tree), included)) g$tree[[paste(k)]] <- NULL
  }

  # ENH: consider R6 class with active bindings for this
  leaf.rules <- data.frame(
    id = plyr::laply(g$terminal, function(j) g$tree[[paste(j)]]$id),
    rule = plyr::laply(g$terminal, function(j) g$tree[[paste(j)]]$rule),
    N = plyr::laply(g$terminal, function(j) length(g$tree[[paste(j)]]$index)),
    stringsAsFactors = FALSE
  )
  leaf.coefs <- plyr::ldply(g$terminal, function(j) g$tree[[paste(j)]]$coef)
  rownames(leaf.coefs) <- leaf.rules$id

  if (trace > 1) msg2("Getting all.step.leaves...")
  all.step.leaves <- sort(unique(unlist(g$stepindex)))
  if (trace > 1) msg2("Getting all.step.rules...")
  all.step.rules <- data.frame(
    id = plyr::laply(all.step.leaves, function(j) g$tree[[paste(j)]]$id),
    rule = plyr::laply(all.step.leaves, function(j) g$tree[[paste(j)]]$rule),
    N = plyr::laply(
      all.step.leaves,
      function(j) length(g$tree[[paste(j)]]$index)
    ),
    # Value = meanValue,
    # Label = Label,
    stringsAsFactors = FALSE
  )
  if (trace > 1) print(all.step.leaves)
  if (trace > 1) msg2("Getting all.step.coefs...")
  all.step.coefs <- plyr::ldply(
    all.step.leaves,
    function(j) g$tree[[paste(j)]]$coef
  )
  # use following for no plyr
  # all.step.coefs <- data.frame(do.call(
  #   rbind,
  #   lapply(all.step.leaves, function(j) g$tree[[paste(j)]]$coef)))
  rownames(all.step.coefs) <- all.step.rules$id

  # GAMleaves ----
  # Fit a gam on each leaf subpopulation
  if (gamleaves) {
    cxrleaf <- matchCasesByRules(g$x, leaf.rules$rule)
    xleaf.index <- apply(cxrleaf, 1, function(i) which(i == 1))
    leaf.gams <- lapply(seq(leaf.rules$id), function(i) {
      index <- xleaf.index == i
      args <- c(
        list(
          x = g$x[index, ],
          y = g$y[index],
          verbose = trace > 1,
          print.plot = FALSE
        ),
        gam.params
      )
      do.call(.gamlearner, args)
    })
  } else {
    leaf.gams <- NULL
  }

  # Mod  ----
  .mod <- list(
    type = g$type,
    init = g$init,
    learning.rate = learning.rate,
    tree = g$tree,
    gamleaves = gamleaves,
    n.nodes = g$n.nodes,
    included = included,
    terminal = g$terminal,
    open = g$open,
    closed = g$closed,
    nosplit = g$nosplit,
    leaves = list(
      rules = leaf.rules,
      coefs = leaf.coefs,
      gams = leaf.gams
    ),
    all.step.leaves = list(
      rules = all.step.rules,
      coefs = all.step.coefs
    ),
    stepindex = g$stepindex,
    ylevels = ylevels,
    n.leaves = g$n.leaves,
    # opt.n.leaves = g$n.leaves, #??
    # lookback = lookback,
    valid.error.smooth = NULL
  )
  class(.mod) <- c("linadleaves", "list")

  if (lookback) {
    if (trace > 1) msg2("Starting lookback...", color = hilite)
    opt.leaves <- selectleaves(
      .mod,
      x = x,
      y = y,
      x.valid = x.valid,
      y.valid = y.valid,
      smooth = select.leaves.smooth,
      print.plot = plot.tuning,
      verbose = trace > 0,
      trace = trace
    )
    .mod$n.leaves <- opt.leaves$n.leaves
    .mod$lookback <- opt.leaves
    if (trace > 1) msg2("Lookback complete", color = hilite)
  }

  .mod
} # rtemis::linadleaves

# [[---2. setNodeRC---]]----
setNodeRC <- function(
  g,
  id,
  index,
  Fval,
  weights,
  depth,
  coef,
  terminal = TRUE,
  type = "terminal",
  condition,
  split.rule,
  rule
) {
  list(
    id = id,
    index = index,
    Fval = Fval,
    weights = weights,
    depth = depth,
    coef = coef,
    terminal = terminal,
    type = type,
    rule = rule,
    split.rule = split.rule,
    loss = g$loss.fn(g$y, Fval),
    split.loss = NULL,
    split.loss.red = NULL
  )
} # rtemis::setNodeRC


# [[---3. splitlineRC---]] ----
#' Ridge and Stump
#'
#' Edits environment 'g' in-place (no output)
#'
#' Fit a linear model on (x, y) and split on the gradient
#' Input: environment holding tree and index of node
#' Output: None; Expands tree within environment g by splitting indexed node
#'
#' @param tree Node within tree environment
#' @param node.index Open nodes to work on
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

splitlineRC <- function(
  g,
  type,
  node.index,
  gamma,
  gamma.on.lin,
  lin.type,
  alpha,
  lambda,
  lambda.seq,
  cv.glmnet.nfolds,
  which.cv.glmnet.lambda,
  part.minsplit,
  part.xval,
  part.max.depth,
  part.cp,
  part.minbucket,
  minobsinnode.lin,
  nvmax,
  rho.def = .1,
  verbose = TRUE,
  trace = 0
) {
  # '- Node ----
  .class <- type == "Classification"
  # .surv <- type == "Survival"
  node <- g$tree[[paste(node.index)]]
  if (.class) {
    firstDer <- -2 * g$y / (1 + exp(2 * g$y * node$Fval)) # n
    resid1 <- -firstDer
    # } else if (.surv) {
    #     firstDer <- surv.resid(g$y, node$Fval)
    #     resid1 <- -firstDer
  } else {
    resid1 <- g$y - node$Fval
  }

  weights <- node$weights
  if (trace > 2) table(weights)

  # '- Split  ----
  # if (trace > 0) msg2("splitLining node ", node.index, "...", sep = "")
  dat <- data.frame(g$x, resid1)
  part <- rpart::rpart(
    resid1 ~ .,
    dat,
    weights = weights,
    control = rpart::rpart.control(
      minsplit = part.minsplit,
      xval = part.xval,
      maxdepth = part.max.depth,
      minbucket = part.minbucket,
      cp = part.cp
    )
  )

  if (is.null(part$splits)) {
    # '-- Node did not split ----
    if (trace > 1) msg20("Node #", node.index, " did not split")
    # g$tree[[paste(node.index)]]$terminal <- TRUE # now true by setNodeClass
    g$tree[[paste(node.index)]]$type <- "nosplit"
    g$nosplit <- c(g$nosplit, node.index)
    if (trace > 1) msg2("Moving nosplit nodes from open to closed list...")
    # +tree: Remove nosplit node from open
    g$open <- setdiff(g$open, g$nosplit)
    # +tree: Add nosplit node to closed
    g$closed <- c(g$closed, node.index)
    cutFeat.name <- cutFeat.point <- cutFeat.category <- NA
    g$tree[[paste(node.index)]]$split.rule <- NA
    # part.c.left <- part.c.right <- 0
    left.index <- right.index <- NA
    # split.rule.left <- split.rule.right <- FALSE # never used
    linVal.left <- linVal.right <- 0
    # linCoef.left <- linCoef.right <- rep(0, NCOL(g$xm) + 1)
    linCoef.left <- linCoef.right <- rep(0, g$ncolxm)
    # Weights remain unchanged
    weights.left <- weights.right <- weights
    Fval <- node$Fval
  } else {
    # '-- Node did split --' ----
    # Get correct Left & Right values - for Regression
    if (part$splits[1, 2] == 1) {
      left.yval.row <- 3
      right.yval.row <- 2
    } else {
      left.yval.row <- 2
      right.yval.row <- 3
    }
    # linmod1 add learning.rate on constant
    part.c.left <- g$learning.rate * part$frame$yval[left.yval.row]
    part.c.right <- g$learning.rate * part$frame$yval[right.yval.row]

    g$tree[[paste(node.index)]]$type <- "split"
    g$tree[[paste(node.index)]]$terminal <- FALSE
    cutFeat.name <- rownames(part$splits)[1]
    cutFeat.point <- cutFeat.category <- NULL
    if (!is.null(cutFeat.name)) {
      cutFeat.index <- which(names(g$x) == cutFeat.name)
      if (is.numeric(g$x[[cutFeat.name]])) {
        # Split was on a continuous feature
        cutFeat.point <- part$splits[1, "index"]
        if (trace > 1) {
          msg2(
            "Node #",
            node.index,
            ": Split Feature is \"",
            cutFeat.name,
            "\"; Cut Point = ",
            ddSci(cutFeat.point),
            sep = ""
          )
        }
        split.rule.left <- paste(cutFeat.name, "<", cutFeat.point)
        split.rule.right <- paste(cutFeat.name, ">=", cutFeat.point)
      } else {
        # Split was on a categorical feature
        cutFeat.category <- levels(g$x[[cutFeat.name]])[which(
          part$csplit[1, ] == 1
        )]
        if (trace > 1) {
          msg2(
            "Node #",
            node.index,
            ": Split Feature is \"",
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
          paste0("'", cutFeat.category, "'", collapse = ", "),
          ")"
        )
        split.rule.right <- paste0(
          "!",
          cutFeat.name,
          " %in% ",
          "c(",
          paste0("'", cutFeat.category, "'", collapse = ", "),
          ")"
        )
      }
      if (length(cutFeat.point) > 0) {
        left.index <- intersect(
          node$index,
          which(g$x[, cutFeat.index] < cutFeat.point)
        )
        right.index <- intersect(node$index, seq_len(NROW(g$x))[-left.index])
      } else {
        left.index <- intersect(
          node$index,
          which(is.element(g$x[, cutFeat.index], cutFeat.category))
        )
        right.index <- intersect(node$index, seq_len(NROW(g$x))[-left.index])
      }
    } # /Split on Categorical vs Continuous feature

    g$tree[[paste(node.index)]]$split.rule <- split.rule.left

    # '--> Split updates -' ----
    # Observations, weights and node coefficients
    # Calculate left weights and outputs
    left.y <- g$y[left.index] # < n
    left.weights <- weights[left.index] # < n
    # Calculate right weights and outputs
    right.y <- g$y[right.index] # < n
    right.weights <- weights[right.index] # < n

    # Calculate the current value of the function for right and left node
    # Vector, length < y
    Fval.left <- node$Fval[left.index] # < n
    Fval.right <- node$Fval[right.index] # < n

    # ''- Left ----
    # Compute the coefficients of the left child node
    # Vector, length < y
    # ddlt
    if (.class) {
      firstDerLeft <- -2 * left.y / (1 + exp(2 * left.y * Fval.left)) # < n
      # Remove NaNs (from when there are no cases in node)
      firstDerLeft[is.na(firstDerLeft)] <- 0 # < n
      # Scalar
      weightedFirstDerLeft <- c(left.weights %*% firstDerLeft) # 1
      # Vector
      secDerLeft <- abs(firstDerLeft) * (2 - abs(firstDerLeft))
      # Scalar
      weightedSecDerLeft <- c(left.weights %*% secDerLeft)
    } else {
      firstDerLeft <- weightedFirstDerLeft <- secDerLeft <- weightedSecDerLeft <- NA
    }

    # ''- Update nodeVal.left ----
    coef.left <- node$coef
    if (.class) {
      if (weightedFirstDerLeft == 0) {
        if (trace > 1) msg2("weightedFirstDerLeft is 0")
        nodeVal.left <- 0 # do nothing
      } else if (weightedSecDerLeft == 0) {
        if (trace > 1) msg2("weightedSecDerLeft is 0")
        nodeVal.left <- -sign(weightedFirstDerLeft) * Inf
      } else {
        if (g$.rho) {
          rho <- abs(weightedFirstDerLeft / weightedSecDerLeft)
        } else {
          rho <- 1
        }

        # rhoprotect
        if (anyNA(rho)) {
          if (trace > 1) warning("NAs found in rho")
          rho[is.na(rho)] <- rho.def
        }
        nodeVal.left <- sign((weightedFirstDerLeft)) *
          min(g$rho.max, rho) # 1
      }
    } else {
      nodeVal.left <- part.c.left
    }
    coef.left[1] <- coef.left[1] + part.c.left

    # ''- Right ----
    if (.class) {
      # Compute the coefficients for the right child node
      firstDerRight <- -2 * right.y / (1 + exp(2 * right.y * Fval.right)) # < n
      firstDerRight[is.na(firstDerRight)] <- 0
      secDerRight <- abs(firstDerRight) * (2 - abs(firstDerRight)) # < n
      weightedFirstDerRight <- c(right.weights %*% firstDerRight) # 1
      weightedSecDerRight <- c(right.weights %*% secDerRight) # 1
    } else {
      firstDerRight <- weightedFirstDerRight <- secDerRight <- weightedSecDerRight <- NA
    }

    # ''- Update nodeVal.right ----
    coef.right <- node$coef
    if (.class) {
      if (weightedFirstDerRight == 0) {
        if (trace > 1) msg2("weightedFirstDerRight is 0")
        nodeVal.right <- 0
      } else if (weightedSecDerRight == 0) {
        if (trace > 1) msg2("weightedSecDerRight is 0")
        nodeVal.right <- -sign(weightedFirstDerRight) * Inf
      } else {
        if (g$.rho) {
          rho <- abs(weightedFirstDerRight / weightedSecDerRight)
        } else {
          rho <- 1
        }

        # rhoprotect
        if (anyNA(rho)) {
          if (trace > 1) warning("NAs found in rho")
          rho[is.na(rho)] <- rho.def
        }
        nodeVal.right <- sign(weightedFirstDerRight) *
          min(g$rho.max, rho)
      }
    } else {
      nodeVal.right <- part.c.right
    }

    coef.right[1] <- coef.right[1] + part.c.right

    Fval <- node$Fval
    Fval[left.index] <- Fval[left.index] + nodeVal.left
    Fval[right.index] <- Fval[right.index] + nodeVal.right

    if (.class && trace > 1) {
      msg2(
        "weightedFirstDerLeft = ",
        weightedFirstDerLeft,
        "; weightedFirstDerRight = ",
        weightedFirstDerRight,
        sep = ""
      )
      msg2(
        "weightedSecDerLeft = ",
        weightedSecDerLeft,
        "; weightedSecDerRight = ",
        weightedSecDerRight,
        sep = ""
      )
    }

    # '- Update Weights -' ----
    weights.left <- weights.right <- weights
    weights.left[right.index] <- weights.left[right.index] * gamma
    weights.right[left.index] <- weights.right[left.index] * gamma

    # '- Line  -' ----
    if (.class) {
      firstDer <- -2 * g$y / (1 + exp(2 * g$y * Fval)) # n
      resid2 <- -firstDer
      # } else if (.surv) {
      #     firstDer <- surv.resid(g$y, Fval)
      #     resid2 <- -firstDer
    } else {
      resid2 <- g$y - Fval
    }

    if (!is.null(cutFeat.name)) {
      # Node split
      .resid2wleft <- if (gamma.on.lin) resid2 * weights.left else
        resid2[left.index]
      if (length(left.index) < minobsinnode.lin || is_constant(.resid2wleft)) {
        # Too few observations to fit linear model or weighted resid constant
        if (trace > 1) {
          msg2(
            "Looking at Node #",
            node.index * 2,
            ": ",
            length(left.index),
            " cases belong to this node: Not fitting any more lines here",
            sep = ""
          )
        }
        linVal.left <- rep(0, length(resid2))
        linCoef.left <- rep(0, g$ncolxm)
      } else {
        index <- is.na(resid2)
        if (gamma.on.lin) {
          .weights <- weights.left[!index]
        } else {
          .weights <- rep(0, length(g$y))
          .weights[left.index] <- 1
        }
        linCoef.left <- lincoef(
          x = g$xm[!index, -1, drop = FALSE],
          y = resid2[!index],
          learning.rate = g$learning.rate,
          weights = .weights[!index],
          method = lin.type,
          nvmax = nvmax,
          alpha = alpha,
          lambda = lambda,
          lambda.seq = lambda.seq,
          cv.glmnet.nfolds = cv.glmnet.nfolds,
          which.cv.glmnet.lambda = which.cv.glmnet.lambda
        )

        linVal.left <- c(g$xm %*% linCoef.left)

        # Lin Updates, Left ----
        if (.class && g$.rho && lin.type != "none") {
          firstDer.rho.left <- (t(
            (-2 * linVal.left * g$y) / (1 + exp(2 * g$y * Fval))
          ) %*%
            weights.left)[1]
          secDer.rho.left <- (t(
            (4 * linVal.left^2 * exp(2 * g$y * Fval)) /
              (1 + exp(2 * g$y * Fval))^2
          ) %*%
            weights.left)[1]
          rho.left <- -firstDer.rho.left / secDer.rho.left
          rho.left <- sign(rho.left) * min(g$rho.max, rho.left, na.rm = TRUE)[1]
        } else {
          rho.left <- 1
        }

        Fval.left <- Fval[left.index] + rho.left * linVal.left[left.index] # n
        coef.left <- coef.left + linCoef.left
      } # /if (length(left.index) < minobsinnode.lin)

      .resid2wright <- if (gamma.on.lin) resid2 * weights.right else
        resid2[right.index]
      if (
        length(right.index) < minobsinnode.lin || is_constant(.resid2wright)
      ) {
        # Too few observations to fit linear model or weighted resid constant
        if (trace > 1) {
          msg2(
            "Looking at Node #",
            node.index * 2 + 1,
            ": ",
            length(right.index),
            " cases belong to this node: Not fitting any more lines here",
            sep = ""
          )
        }
        linVal.right <- rep(0, length(resid2))
        linCoef.right <- rep(0, g$ncolxm)
      } else {
        index <- is.na(resid2)
        if (gamma.on.lin) {
          .weights <- weights.right[!index]
        } else {
          .weights <- rep(0, length(g$y))
          .weights[right.index] <- 1
        }
        linCoef.right <- lincoef(
          x = g$xm[!index, -1, drop = FALSE],
          y = resid2[!index],
          learning.rate = g$learning.rate,
          weights = .weights[!index],
          method = lin.type,
          nvmax = nvmax,
          alpha = alpha,
          lambda = lambda,
          lambda.seq = lambda.seq,
          cv.glmnet.nfolds = cv.glmnet.nfolds,
          which.cv.glmnet.lambda = which.cv.glmnet.lambda
        )
        linVal.right <- c(g$xm %*% linCoef.right)

        # Lin Updates, Right ----
      } # /if (length(right.index) < minobsinnode.lin)

      if (.class && g$.rho) {
        firstDer.rho.right <- (t(
          (-2 * linVal.right * g$y) / (1 + exp(2 * g$y * Fval))
        ) %*%
          weights.right)[1]
        secDer.rho.right <- (t(
          (4 * linVal.right^2 * exp(2 * g$y * Fval)) /
            (1 + exp(2 * g$y * Fval))^2
        ) %*%
          weights.right)[1]
        rho.right <- -firstDer.rho.right / secDer.rho.right
        rho.right <- sign(rho.right) *
          min(g$rho.max, rho.right, na.rm = TRUE)[1]
      } else {
        rho.right <- 1
      }

      # pre-linmod1
      # Fval.right <- Fval[right.index] + g$learning.rate * rho.right * linVal.right[right.index] # n
      Fval.right <- Fval[right.index] + rho.right * linVal.right[right.index] # n
      coef.right <- coef.right + linCoef.right
    }

    # '- Side-effects -' ----

    # Check: do we need this?
    depth <- g$tree[[paste(node.index)]]$depth + 1

    # Get combined error of children
    Fval1 <- Fval
    Fval1[left.index] <- Fval.left
    Fval1[right.index] <- Fval.right

    # Assign loss reduction to parent
    g$tree[[paste(node.index)]]$split.loss <- g$loss.fn(g$y, Fval1)
    g$tree[[paste(node.index)]]$split.loss.red <- node$loss -
      g$tree[[paste(node.index)]]$split.loss

    # Set id numbers by preorder indexing
    # Left
    left.id <- node$id * 2
    g$tree[[paste(left.id)]] <- setNodeRC(
      g = g,
      id = left.id,
      index = left.index,
      Fval = Fval1,
      weights = weights.left,
      depth = depth,
      coef = coef.left,
      terminal = TRUE,
      type = "terminal",
      condition = split.rule.left,
      split.rule = NULL,
      rule = crules(node$rule, split.rule.left)
    )

    # Right
    right.id <- node$id * 2 + 1
    g$tree[[paste(right.id)]] <- setNodeRC(
      g = g,
      id = right.id,
      index = right.index,
      Fval = Fval1,
      weights = weights.right,
      depth = depth,
      coef = coef.right,
      terminal = TRUE,
      type = "terminal",
      condition = split.rule.right,
      split.rule = NULL,
      rule = crules(node$rule, split.rule.right)
    )
  } # /if (is.null(part$splits)) aka Node did split
} # rtemis::splitlineRC


# [[---4. predict---]] ----
#' Predict method for `linadleaves` object
#'
#' @method predict linadleaves
#' @param object `shytreeRaw`
#' @param newdata Data frame of predictors
#' @param type Character: "response", "probability", "all", "step"
#' @param n.leaves Integer: Use the first `n.leaves` of the tree for
#' prediction
#' @param fixed.cxr (internal use) Matrix: Cases by rules to use instead of matching cases to rules using
#' `newdata`
#' @param cxr.newdata (internal use) Data frame: Use these values to match cases by rules
#' @param cxr Logical: If TRUE, return list which includes cases-by-rules matrix along with predicted values
#' @param cxrcoef Logical: If TRUE, return cases-by-rules * coefficients matrix along with predicted values
#' @param verbose Logical: If TRUE, print messages to console
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

predict.linadleaves <- function(
  object,
  newdata,
  type = c(
    "response",
    "probability",
    "all",
    "step"
  ),
  n.leaves = NULL,
  fixed.cxr = NULL,
  cxr.newdata = NULL,
  cxr = FALSE,
  cxrcoef = FALSE,
  verbose = FALSE,
  ...
) {
  init <- object$init
  type <- match.arg(type)
  .class <- object$type == "Classification"

  # Newdata ----
  if (is.null(colnames(newdata))) {
    colnames(newdata) <- paste0("V", seq_len(NCOL(newdata)))
  }

  # Add column of ones for intercept and convert factors to dummies
  newdata_lin <- if (!object$gamleaves) {
    model.matrix(~., data = newdata)
  } else {
    newdata
  }

  if (type != "step") {
    # Not "step" ----
    if (is.null(n.leaves)) {
      n.leaves <- max(sapply(object$stepindex, length))
    }

    if (n.leaves == 1) {
      # '-- n.leaves == 1 ----
      if (object$gamleaves) {
        yhat <- c(predict(object$leaves$gams[[1]], newdata))
      } else {
        if (object$type == "Regression") {
          yhat <- c(newdata_lin %*% t(object$leaves$coefs))
        } else if (object$type == "Classification") {
          # glmnet
          yhat <- -c(newdata_lin %*% t(object$leaves$coefs))
        } else if (object$type == "Survival") {
          # check: init
          yhat <- c(newdata_lin[, -1] %*% t(object$leaves$coefs))
        }
      }
    } else {
      # '-- n.leaves > 1 ----
      rule.ids <- object$stepindex[[paste(n.leaves)]]
      rules <- sapply(
        rule.ids,
        \(j)
          object$all.step.leaves$rules$rule[
            object$all.step.leaves$rules$id == j
          ]
      )

      # '---- Cases x Rules ----
      if (is.null(fixed.cxr)) {
        cases <- if (is.null(cxr.newdata)) newdata else cxr.newdata
        # n cases by n rules i.e. n leaves
        .cxr <- matchCasesByRules(cases, rules, verbose = verbose)
      } else {
        .cxr <- fixed.cxr
      }

      # each case must match one leaf and one leaf only
      # >>DIAG<<: all(apply(.cxr, 1, sum) == 1)

      if (object$gamleaves) {
        # gamleaves
        yhat <- rep(NA, NROW(newdata))
        xleaf.index <- apply(.cxr, 1, \(i) which(i == 1))
        for (i in seq(n.leaves)) {
          index <- xleaf.index == i
          yhat[index] <- predict(object$leaves$gams[[i]], newdata[index, ])
        }
      } else {
        # No gamleaves
        coefs <- lapply(
          rule.ids,
          \(j)
            object$all.step.leaves$coefs[
              object$all.step.leaves$rules$id == j,
              ,
              drop = FALSE
            ]
        )
        # coefs: n.leaves by n vars
        coefs <- data.matrix(do.call(rbind, coefs))
        # n cases by n vars
        # .cxrcoef <- .cxr %*% coefs
        .cxrcoef <- .cxr %*% coefs
        # '-- yhat ----
        # consider doing length(rules) matrix multiplications and add
        # Todo: do not apply learning rate on first linear model
        # before full lin mod step 1
        # yhat <- init + sapply(seq_len(NROW(newdata)), \(nr) {
        #     object$learning.rate * (newdata_lin[nr, ] %*% t(.cxrcoef[nr, , drop = FALSE]))
        # })
        yhat <- init +
          sapply(seq_len(NROW(newdata)), \(nr) {
            newdata_lin[nr, ] %*% t(.cxrcoef[nr, , drop = FALSE])
          })
      }
    } # / n.leaves > 1

    # Class levels ----
    if (.class) {
      if (type == "response") {
        yhat <- sign(yhat)
        yhat <- factor(yhat, levels = c(1, -1), labels = object$ylevels)
      } else if (type == "probability") {
        # Calculate probability using GBM paper equation 21.
        yhat <- exp(2 * yhat) / (1 + exp(2 * yhat))
      } else if (type == "all") {
        prob <- exp(2 * yhat) / (1 + exp(2 * yhat))
        estimate <- sign(yhat)
        estimate <- factor(estimate, levels = c(1, -1), labels = object$ylevels)
        out <- list(estimate = estimate, probability = prob)
        return(out)
      }
    } else {
      out <- yhat
    }

    if (!cxrcoef && !cxr) {
      out <- yhat
    } else {
      out <- list(yhat = yhat)
      if (cxrcoef) out$cxrcoef <- .cxrcoef
      if (cxr) out$cxr <- .cxr
    }

    out
    # /type != "step"
  } else {
    # "step" ----
    # >>> type == "step": Get predictions at each step
    max.leaves <- max(sapply(object$stepindex, length))

    if (max.leaves == 1) {
      # '-- step & n.leaves == 1 ----
      if (!.class) {
        yhat.l <- list(c(init + newdata %*% t(object$leaves$coefs)))
      } else {
        yhat.l <- list(c(newdata_lin[, -1] %*% t(object$leaves$coefs)))
      }
    } else {
      # '-- step & n.leaves > 1 ----
      if (verbose) {
        msg2(
          "Getting estimated values for each of",
          max.leaves,
          "steps..."
        )
      }

      # List of all stepwise rules starting with "TRUE"
      rules.l <- plyr::llply(seq(max.leaves), function(j) {
        paste(sapply(object$stepindex[[paste(j)]], function(k) {
          c(object$all.step.leaves$rules$rule[
            object$all.step.leaves$rules$id == k
          ])
        }))
      })

      cases <- if (is.null(cxr.newdata)) newdata else cxr.newdata
      cxr.l <- plyr::llply(seq(max.leaves), function(j) {
        matchCasesByRules(cases, rules.l[[j]], verbose = verbose)
      })

      # Get coefs for each leaf for each step in list
      coefs.l <- plyr::llply(seq(max.leaves), function(k) {
        object$all.step.leaves$coefs[object$all.step.leaves$rules$id == k, ]
      })

      # A list of length max.leaves, with a matrix of coefficients per rule
      coefs.l <- plyr::llply(seq(max.leaves), function(j) {
        data.matrix(plyr::ldply(object$stepindex[[paste(j)]], function(k) {
          object$all.step.leaves$coefs[object$all.step.leaves$rules$id == k, ]
        }))
      })

      cxrcoef.l <- plyr::llply(seq(max.leaves), function(j) {
        cxr.l[[j]] %*% coefs.l[[j]]
      })

      yhat.l <- plyr::llply(seq(max.leaves), function(j) {
        yhat <- sapply(seq_len(NROW(newdata)), function(n) {
          newdata_lin[n, ] %*% t(cxrcoef.l[[j]][n, , drop = FALSE])
        })
        if (.class) {
          yhat <- sign(yhat)
          yhat <- factor(yhat, levels = c(1, -1), labels = object$ylevels)
        }
        yhat
      })
    }
    yhat.l
  }
} # rtemis:: predict.linadleaves

# [[---5. print---]] ----
#' Print method for `linadleaves` object
#'
#' @method print linadleaves
#' @param x `linadleaves` object
#' @param ... Not used
#'
#' @author E.D. Gennatas
#' @export

print.linadleaves <- function(x, ...) {
  if (x$gamleaves) {
    cat(
      "\n  A Linear Additive GAMleaf Tree model with",
      x$n.leaves,
      "leaves\n\n"
    )
  } else {
    cat("\n  A Linear Additive Tree model with", x$n.leaves, "leaves\n\n")
  }
  invisible(x)
}

#' Convert `linadleaves` to `data.tree` object
#'
#' @param object `linadleaves` object

as.data.tree.linadleaves <- function(object) {
  as.Node.data.frame <- getFromNamespace("as.Node.data.frame", "data.tree")
  dat <- object$leaves$rules
  dat$rule <- gsub(" & ", "/", dat$rule)
  dat$rule <- gsub("TRUE", "All cases", dat$rule)
  as.Node.data.frame(dat, pathName = "rule")
} # rtemis::as.data.tree.shytreeLeaves


class.loss <- function(y, Fval) {
  -c(log(1 + exp(-2 * y * Fval)))
}

# class.lossw <- function(y, Fval, weights) {
#     c(log(1 + exp(-2 * y * Fval)) %*% weights)
# }

# [[---6. selectleaves---]] ----
#' Select leaves
#'
#' Select number of leaves based on validation error
#'
#' @keywords internal
#' @noRd

selectleaves <- function(
  object,
  x,
  y,
  x.valid,
  y.valid,
  smooth = FALSE,
  print.plot = TRUE,
  verbose = TRUE,
  trace = 0
) {
  if (trace > 1) msg2("Running selectleaves")
  n.leaves <- object$n.leaves
  .class <- object$type == "Classification"
  # .surv <- object$type == "Survival"

  train.estimate.l <- predict(
    object,
    newdata = x,
    type = "step",
    verbose = trace > 1
  )
  valid.estimate.l <- predict(
    object,
    newdata = x.valid,
    type = "step",
    verbose = trace > 1
  )

  # TODO: change bacc and mse to arg fn
  if (.class) {
    train.error <- sapply(seq(train.estimate.l), function(j) {
      1 - bacc(y, train.estimate.l[[j]])
    })
    valid.error <- sapply(seq(valid.estimate.l), function(j) {
      1 - bacc(y.valid, valid.estimate.l[[j]])
    })
    # } else if (.surv) {
    #   train.error <- sapply(seq(train.estimate.l), function(j)
    #     surv.loss(y, train.estimate.l[[j]]))
    #   valid.error <- sapply(seq(valid.estimate.l), function(j)
    #     surv.loss(y.valid, valid.estimate.l[[j]]))
  } else {
    train.error <- sapply(seq(train.estimate.l), function(j) {
      mse(y, train.estimate.l[[j]])
    })
    valid.error <- sapply(seq(valid.estimate.l), function(j) {
      mse(y.valid, valid.estimate.l[[j]])
    })
  }

  valid.error.smooth <- if (smooth) {
    # valid.error.smooth <- supsmu(seq(n.leaves), valid.error)$y
    valid.error.smooth <- suppressWarnings(
      loess(valid.error ~ seq(n.leaves))$fitted
    )
  } else {
    NULL
  }

  if (print.plot) {
    mplot3_xy(
      seq(n.leaves),
      list(
        Training = train.error,
        Validation = valid.error,
        `Smoothed Valid.` = valid.error.smooth
      ),
      type = "l",
      group.adj = .95,
      lty = c(1, 1, 2),
      line.col = c("#80ffff", "#FF99FF", "#2B27F1"),
      vline = c(which.min(valid.error), which.min(valid.error.smooth)),
      vline.col = c("#FF99FF", "#2B27F1"),
      xlab = "N leaves",
      ylab = ifelse(.class, "1 - Balanced Accuracy", "MSE")
    )
  }

  valid.error <- if (smooth) valid.error.smooth else valid.error

  n.leaves <- max(1, which.min(valid.error))
  if (verbose) {
    msg2(
      "Selected",
      n.leaves,
      "leaves of",
      length(valid.error),
      "total",
      color = hilite
    )
  }

  list(
    n.leaves = n.leaves,
    train.error = train.error,
    valid.error = valid.error,
    valid.error.smooth = valid.error.smooth
  )
} # rtemis::selectleaves
