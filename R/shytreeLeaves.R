# shytreeLeavesRC.R
# ::rtemis::
# 2018-9 Efstathios D. Gennatas egenn.github.io
# shytreeLeavesRC with no-line option

#' \pkg{rtemis internal}: Low-level Stepwise Hybrid Tree procedure
#'
#' Train a Stepwise Hybrid Tree for Classification & Regression
#'
#' With \code{max.nodes = 0}, the output is a linear model trained according to \code{lin.type}
#' Note that lambda is treated differently by \code{glmnet::glmnet} and \code{MASS::lm.ridge}
#' @inheritParams s.SHYTREE
#' @param x Data frame
#' @param max.leaves Integer: Total number of terminal nodes to reach. 1 is a special case where no split is performed and a linear
#' model is trained. Otherwise, this should be an even number as each split introduces two children nodes.
#' Note: this is total N of nodes in the tree, with the root uncounted, not the number of terminal nodes.
#' @param loss.fn Function with arguments y, Fval, weights. Allows you to define a custom loss function.
# Defaults to \code{log(1 + exp(-2 * y * Fval)) %*% weights}
#' @author Efstathios D. Gennatas
#' @keywords internal

# [[---F1---]] ====
shytreeLeavesRC <- function(x, y,
                            x.valid = NULL, y.valid = NULL,
                            lookback = FALSE,
                            weights = NULL,
                            max.leaves = 5,
                            learning.rate = 1,
                            minobsinnode = 2,
                            minobsinnode.lin = 10,
                            lin.type = "forwardStepwise",
                            gamma = .1,
                            select.leaves.smooth = TRUE,
                            alpha = 0,
                            lambda = .01,
                            lambda.seq = NULL,
                            cv.glmnet.nfolds = 5,
                            which.cv.glmnet.lambda = "lambda.1se",
                            nvmax = 2,
                            part.minsplit = 2,
                            part.xval = 0,
                            part.max.depth = 1,
                            part.cp = 0,
                            part.minbucket = 5,
                            .rho = TRUE,
                            rho.max = 1000,
                            loss.fn = if (is.factor(y)) class.loss else mse,
                            verbose = TRUE,
                            plot.tuning = TRUE,
                            trace = 0) {

  # [ Arguments ] ====
  type <- if (is.factor(y))  "Classification" else "Regression"
  .class <- type == "Classification"
  if (.class) {
    ylevels <- levels(y)
    levels(y) <- c(1, -1)
    y <- as.numeric(as.character(y))
  } else {
    ylevels <- NULL
    .rho <- FALSE
  }

  if (is.null(weights)) weights <- rep(1, NROW(y))

  # Changed: Specify lookback directly
  # lookback <- if (!is.null(x.valid) & !is.null(y.valid)) TRUE else FALSE
  if (lookback && is.null(x.valid)) stop("You have asked for early stopping without providing a validation set.")

  # [ Check y is not constant ] ====
  if (is.constant(y)) {
    coefs <- rep(0, NCOL(x) + 1)
    names(coefs) <- c("(Intercept)", colnames(x))
    .mod <- list(type = type,
                 init = mean(y),
                 learning.rate = learning.rate,
                 tree = NULL,
                 n.nodes = 0,
                 included = NULL,
                 terminal = NULL,
                 open = NULL,
                 closed = NULL,
                 nosplit = NULL,
                 leaves = list(id = NULL,
                               rules = "TRUE",
                               coefs = coefs),
                 ylevels = ylevels)
    class(.mod) <- c("shytreeLeavesRC", "list")
    return(.mod)
  }

  # [ ARGUMENTS ] ====
  # lin.type <- match.arg(lin.type)
  if (NCOL(x) == 1) lin.type <- "glm"
  if (trace > 1) msg0("Using lin.type '", lin.type, "'")

  # [ GLOBAL ] ====
  g <- new.env()
  # g$x used by rpart, g$xm used by lincoef
  g$x <- x
  g$xm <- cbind(1, model.matrix(~. - 1, data = x))
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

  # [ LOOP: step splitLine ] ====
  if (.class) {
    # vector: Initial probabilities
    probability <- weights / NROW(y) # n
    # '- Init ====
    # scalar: Initial node value
    g$init <- c(log(1 + probability %*% y) - log(1 - probability %*% y)) # 1
  } else {
    g$init <- mean(y) # 1
  }

  # vector: Initial observations
  Fval <- rep(g$init, NROW(y)) # n

  # '- Gradient ====
  if (.class) {
    firstDer <- -2 * g$y / (1 + exp(2 * g$y * Fval)) # n
  } else {
    firstDer <- -(y - Fval)
  }

  resid <- -firstDer # n

  # '- Lin1 ====
  if (verbose) msg("Training Stepwise Hybrid Tree ", type, " (max leaves = ", max.leaves, ")...", sep = "")
  if (trace > 0) msg("Training first Linear Model...")
  if (is.constant(resid)) stop("First gradient is constant")

  coef <- lincoef(x = g$xm[, -1, drop = FALSE], y = resid,
                  weights = weights,
                  method = lin.type,
                  nvmax = nvmax,
                  alpha = alpha,
                  lambda = lambda,
                  lambda.seq = lambda.seq,
                  cv.glmnet.nfolds = cv.glmnet.nfolds,
                  which.cv.glmnet.lambda = which.cv.glmnet.lambda)

  g$n.leaves <- 1 # ddlt
  linVal <- c(g$xm %*% coef) # n

  if (.class & .rho) {
    firstDer.rho <- t((-2 * linVal * y) / (1 + exp(2 * y * Fval))) %*% weights
    secDer.rho <- t((4 * linVal^2 * exp(2*y*Fval)) / (1 + exp(2*y * Fval))^2) %*% weights
    rho <- -firstDer.rho / secDer.rho
    rho <- c(sign(rho) * min(rho.max, rho, na.rm = TRUE))
  } else {
    rho <- 1
  }

  Fval <- Fval + learning.rate * rho * linVal # n

  # Special case: if max.leaves == 1 ====
  # return linear model
  if (max.leaves == 1) {
    leaves <- list(rules = data.frame(id = 0,
                                      rule = "TRUE",
                                      stringsAsFactors = FALSE),
                   coefs = t(data.frame(coef)))
    .mod <- list(type = type,
                 init = g$init,
                 learning.rate = learning.rate,
                 tree = NULL,
                 n.nodes = 0,
                 n.leaves = 1,
                 included = NULL,
                 terminal = NULL,
                 open = NULL,
                 closed = NULL,
                 nosplit = NULL,
                 leaves = leaves,
                 all.step.leaves = leaves, # This is needed for predict()
                 stepindex = g$stepindex,
                 ylevels = ylevels)
    class(.mod) <- c("shytreeLeavesRC", "list")
    return(.mod)
  }

  # '- Root ====
  root <- setNodeRC(g = g,
                    id = 1,
                    index = seq(NROW(y)),
                    Fval = Fval,
                    weights = weights,
                    depth = 0,
                    coef = coef,
                    terminal = FALSE,
                    type = NULL,
                    condition = "TRUE",
                    split.rule = NULL,
                    rule = "TRUE")

  # '- Init nodes list ====
  g$tree[["1"]] <- root
  # open nodes are candidates for splitting
  g$open <- 1
  g$closed <- integer()

  # '- Start loop ====
  stepid <- 1
  while (g$n.leaves < max.leaves && length(g$open) > 0) {
    if (trace > 1) msg("g$closed is", g$closed)
    if (trace > 1) msg("g$open is", g$open)
    if (trace > 1) msg("g$include is", g$include)
    if (trace > 1) msg("g$terminal is", g$terminal)
    if (trace > 1) msg("g$n.leaves is", g$n.leaves)
    # Work on all candidate splits (open nodes)
    for (i in g$open) {
      if (is.null(g$tree[[paste(i)]]$split.rule)) {
        if (trace > 0) msg0("Working on node id #", i, "...")
        if (trace > 1) msg0("Node #", i, ": split.rule = ", g$tree[[paste(i)]]$split.rule)
        splitLineRC(g = g,
                    type = type,
                    node.index = i,
                    gamma = gamma,
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
                    trace = trace)
      } else {
        if (trace > 2) msg("Node #", i, " already processed", sep = "")
      }
    } # /for (i in g$open) splitLine

    if (length(g$tree) == 3) {
      if (trace > 1) msg("Tree length is", length(g$tree))
      g$open <- c(2, 3)
      g$include <- c(1, 2, 3)
      g$closed <- 1
      g$terminal <- c(2, 3)
      # g$n.nodes <- 2
      g$n.leaves <- 2
      g$stepindex[["2"]] <- 2:3
      # g$steprules <- c(g$steprules, getStepRules(g, 2:3))
    } else {
      # Nodes that did not split are removed from open by splitLine
      if (trace > 1) msg("+++ g$open is", g$open)
      if (trace > 1) msg("+++ g$nosplit is", g$nosplit)

      # '- Loss ====
      # Find split with max loss reduction
      # 1.Error reduction for each open node
      open.loss.red <- data.frame(id = g$open,
                                  loss.red = plyr::ldply(g$open, function(j) g$tree[[paste(j)]]$loss)[, 1] -
                                    plyr::ldply(g$open, function(j) g$tree[[paste(j)]]$split.loss)[, 1])

      if (trace > 1) print(open.loss.red)
      selected <- open.loss.red$id[which.max(open.loss.red$loss.red)]
      # Did selected reduce loss
      selected.loss <- open.loss.red$loss.red[open.loss.red$id  == selected]
      selected.red <- length(selected) > 0 && !is.na(selected.loss) && selected.loss > 0
      toclose <- open.loss.red$id[which(is.na(open.loss.red$loss.red))]
      if (length(toclose) > 0) {
        for (i in toclose) {
          g$open <- setdiff(g$open, i)
          g$closed <- c(g$closed, i)
          if (trace > 1) msg0("Node id #", i, " had NA loss.red and was closed")
        }
      }

      if ((selected.red)) {
        if (trace > 1) msg(">>> Selected node #", selected, sep = "")
        if (trace > 1) msg("g$terminal is", g$terminal)
        # +tree: Remove selected from terminal
        if (trace > 1) msg("Removing selected from terminal nodes")
        g$terminal <- setdiff(g$terminal, selected)
        # +tree: Add selected's childrens to terminal
        if (trace > 1) msg("Adding selected's children to terminal nodes")
        g$terminal <- c(g$terminal, selected * 2, selected * 2 + 1)
        if (trace > 1) msg("g$terminal is now", g$terminal)
        # +tree: Add selected to closed
        # Add selected to closed
        g$closed <- c(g$closed, selected)
        # Add selected's sibling to closed if it was a nosplit, otherwise it's still open
        # sibling <- if (selected %% 2 == 0) selected + 1 else selected - 1
        # if (sibling %in% g$nosplit) g$closed <- c(g$closed, sibling)

        # +tree: Remove selected from open
        # Remove selected from open
        g$open <- setdiff(g$open, selected)

        # +tree: Add selected's children to open
        # Add selected's children to open
        g$open <- c(g$open, selected * 2, selected * 2 + 1)
        if (trace > 1) msg("g$open is now", g$open)

        # Nodes in tree are all closed nodes plus the terminals
        g$include <- union(union(g$closed, g$terminal), g$open)
        # -1 subtracts the root from the count
        g$n.nodes <- length(g$include) - 1
        if (trace > 1) msg("g$n.nodes is", g$n.nodes)
        g$n.leaves <- length(g$terminal)
      } else {
        g$closed <- c(g$closed, selected)
        g$open <- setdiff(g$open, selected)
        if (trace > 1) msg0("Node id #", selected, " did not reduce loss and was closed")
      }
    } # /if (length(g$tree) == 3)

    # Update stepindex
    if (trace > 1) msg("Updating steprules...")
    stepid <- stepid + 1
    g$stepindex[[stepid]] <- g$terminal

    # Update ruleset
    # new.terminal.rules <- lapply(g$terminal)
    # g$allrules <- c(g$allrules, new.terminal.rules)

  } # /while (g$n.nodes <= max.nodes)

  # Add open and nosplit nodes to included

  # [ Purge ] ====
  if (verbose) msg0("Reached ", g$n.leaves, " leaves (", g$n.nodes, " nodes total)")
  if (g$n.nodes == 2) {
    g$tree[[paste(2)]]$terminal <- g$tree[[paste(3)]]$terminal <- TRUE
    g$closed <- c(1, 2, 3)
    g$terminal <- c(2, 3)
    included <- c(1, 2, 3)
  } else {
    if (trace > 1) msg("Purging excluded nodes...")
    # old:
    # for (k in c(g$open, g$nosplit)) g$tree[[paste(k)]] <- NULL
    # new: remove !g$included
    included <- union(g$closed, g$terminal)
    for (k in setdiff(names(g$tree), included)) g$tree[[paste(k)]] <- NULL
  }

  # ENH: consider creating a special R6 class with active bindings for this
  leaf.rules <- data.frame(id = plyr::laply(g$terminal, function(j) g$tree[[paste(j)]]$id),
                           rule = plyr::laply(g$terminal, function(j) g$tree[[paste(j)]]$rule),
                           N = plyr::laply(g$terminal, function(j) length(g$tree[[paste(j)]]$index)),
                           # Value = meanValue,
                           # Label = Label,
                           stringsAsFactors = FALSE)
  leaf.coefs <- plyr::ldply(g$terminal, function(j) g$tree[[paste(j)]]$coef)
  rownames(leaf.coefs) <- leaf.rules$id

  if (trace > 1) msg("Getting all.step.leaves...")
  all.step.leaves <- sort(unique(unlist(g$stepindex)))
  if (trace > 1) msg("Getting all.step.rules...")
  all.step.rules <- data.frame(id = plyr::laply(all.step.leaves, function(j) g$tree[[paste(j)]]$id),
                               rule = plyr::laply(all.step.leaves, function(j) g$tree[[paste(j)]]$rule),
                               N = plyr::laply(all.step.leaves, function(j) length(g$tree[[paste(j)]]$index)),
                               # Value = meanValue,
                               # Label = Label,
                               stringsAsFactors = FALSE)
  if (trace > 1) print(all.step.leaves)
  if (trace > 1) msg("Getting all.step.coefs...")
  all.step.coefs <- plyr::ldply(all.step.leaves, function(j) g$tree[[paste(j)]]$coef)
  rownames(all.step.coefs) <- all.step.rules$id


  # [ MOD ] ====
  .mod <- list(type = g$type,
               init = g$init,
               learning.rate = learning.rate,
               tree = g$tree,
               n.nodes = g$n.nodes,
               included = included,
               terminal = g$terminal,
               open = g$open,
               closed = g$closed,
               nosplit = g$nosplit,
               leaves = list(rules = leaf.rules,
                             coefs = leaf.coefs),
               all.step.leaves = list(rules = all.step.rules,
                                      coefs = all.step.coefs),
               stepindex = g$stepindex,
               ylevels = ylevels,
               n.leaves = g$n.leaves,
               # opt.n.leaves = g$n.leaves, #??
               # lookback = lookback,
               valid.error.smooth = NULL)
  class(.mod) <- c("shytreeLeavesRC", "list")

  if (lookback) {
    if (trace > 1) msg("Starting lookback...", color = rtemis:::rtHighlight)
    opt.leaves = selectleaves(.mod, x = x, y = y,
                              x.valid = x.valid, y.valid = y.valid,
                              smooth = select.leaves.smooth,
                              print.plot = plot.tuning,
                              verbose = trace > 0,
                              trace = trace)
    .mod$n.leaves <- opt.leaves$n.leaves
    .mod$lookback <- opt.leaves
    if (trace > 1) msg("Lookback complete", color = rtemis:::rtHighlight)
  }

  .mod

} # rtemis::shytreeLeavesRC

# [[---F2---]]====
setNodeRC <- function(g,
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
                      rule) {

  list(id = id,
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
       split.loss.red = NULL)

} # rtemis::setNodeRC


# [[---F3---]] ====
#' \code{rtemis} internal: Ridge and Stump
#'
#' Edits environment 'g' in-place (no output)
#'
#' @param tree Node within tree environment
#' @param node.index Open nodes to work on
#'
#' Fit a linear model on (x, y) and split on the gradient
#' Input: environment holding tree and index of node
#' Output: None; Expands tree within environment g by splitting indexed node

splitLineRC <- function(g,
                        type,
                        node.index,
                        gamma = .01,
                        lin.type,
                        alpha = 0,
                        lambda = .01,
                        lambda.seq = NULL,
                        cv.glmnet.nfolds = 5,
                        which.cv.glmnet.lambda = "lambda.1se",
                        part.minsplit = 2,
                        part.xval = 0,
                        part.max.depth = 1,
                        part.cp = 0,
                        part.minbucket = 5,
                        minobsinnode.lin = 5,
                        nvmax = nvmax,
                        verbose = TRUE,
                        trace = 0) {

  # '- Node ====
  .class <- type == "Classification"
  node <- g$tree[[paste(node.index)]]
  if (.class) {
    firstDer <- -2 * g$y / (1 + exp(2 * g$y * node$Fval)) # n
    resid1 <- -firstDer
  } else {
    resid1 <- g$y - node$Fval
  }

  weights <- node$weights

  # '- [ Split ] ====
  # if (trace > 0) msg("splitLining node ", node.index, "...", sep = "")
  dat <- data.frame(g$x, resid1)
  part <- rpart::rpart(resid1 ~., dat,
                       weights = weights,
                       control = rpart::rpart.control(minsplit = part.minsplit,
                                                      xval = part.xval,
                                                      maxdepth = part.max.depth,
                                                      minbucket = part.minbucket,
                                                      cp = part.cp))

  if (is.null(part$splits)) {
    # '-- Node did not split ====
    if (trace > 1) msg0("Node #", node.index, " did not split")
    # g$tree[[paste(node.index)]]$terminal <- TRUE # now true by setNodeClass
    g$tree[[paste(node.index)]]$type <- "nosplit"
    g$nosplit <- c(g$nosplit, node.index)
    if (trace > 1) msg("Moving nosplit nodes from open to closed list...")
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
    # '-- Node did split --' ====
    # Get correct Left & Right values - for Regression
    if (part$splits[1, 2] == 1) {
      left.yval.row <- 3
      right.yval.row <- 2
    } else {
      left.yval.row <- 2
      right.yval.row <- 3
    }
    part.c.left <- part$frame$yval[left.yval.row]
    part.c.right <- part$frame$yval[right.yval.row]

    g$tree[[paste(node.index)]]$type <- "split"
    g$tree[[paste(node.index)]]$terminal <- FALSE
    cutFeat.name <- rownames(part$splits)[1]
    cutFeat.point <- cutFeat.category <- NULL
    if (!is.null(cutFeat.name)) {
      cutFeat.index <- which(names(g$x) == cutFeat.name)
      if (is.numeric(g$x[[cutFeat.name]])) {
        # Split was on a categorical feature
        cutFeat.point <- part$splits[1, "index"]
        if (trace > 1) msg("Node #", node.index, ": Split Feature is \"", cutFeat.name,
                           "\"; Cut Point = ", ddSci(cutFeat.point),
                           sep = "")
        split.rule.left <- paste(cutFeat.name, "<", cutFeat.point)
        split.rule.right <- paste(cutFeat.name, ">=", cutFeat.point)
      } else {
        # Split was on a continuous feature
        cutFeat.category <- levels(g$x[[cutFeat.name]])[which(part$csplit[1, ] == 1)]
        if (trace > 1) msg("Node #", node.index, ": Split Feature is \"", cutFeat.name,
                           "\"; Cut Category is \"", cutFeat.category,
                           "\"", sep = "")
        split.rule.left <- paste0(cutFeat.name, " %in% ", "c(", paste(cutFeat.category, collapse = ", "))
        split.rule.right <- paste0("!", cutFeat.name, " %in% ", "c(", paste(cutFeat.category, collapse = ", "))
      }
      if (length(cutFeat.point) > 0) {
        left.index <- intersect(node$index, which(g$x[, cutFeat.index] < cutFeat.point))
        right.index <- intersect(node$index, seq(NROW(g$x))[-left.index])
      } else {
        left.index <- intersect(node$index, which(is.element(g$x[, cutFeat.index], cutFeat.category)))
        right.index <- intersect(node$index, seq(NROW(g$x))[-left.index])
      }
    } # /Split on Categorical vs Continuous feature

    g$tree[[paste(node.index)]]$split.rule <- split.rule.left

    # '--> Split updates -' ====
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

    # ''- Left ====
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

    # ''- UPDATE nodeVal.left ====
    # check
    coef.left <- node$coef
    if (.class) {
      if (weightedFirstDerLeft == 0) {
        if (trace > 1) msg("weightedFirstDerLeft is 0")
        nodeVal.left <- 0 # do nothing
      } else if (weightedSecDerLeft == 0) {
        if (trace > 1) msg("weightedSecDerLeft is 0")
        nodeVal.left <- -sign(weightedFirstDerLeft) * Inf
      } else {
        if (g$.rho) {
          rho <- abs(weightedFirstDerLeft / weightedSecDerLeft)
        } else {
          rho <- 1
        }
        nodeVal.left <- g$learning.rate * sign((weightedFirstDerLeft)) *
          min(g$rho.max, rho) # 1
      }
    } else {
      nodeVal.left <- g$learning.rate * part.c.left
    }
    coef.left[1] <- coef.left[1] + part.c.left

    # ''- Right ====
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

    # ''- UPDATE nodeVal.right ====
    coef.right <- node$coef
    if (.class) {
      if (weightedFirstDerRight == 0) {
        if (trace > 1) msg("weightedFirstDerRight is 0")
        nodeVal.right <- 0
      } else if (weightedSecDerRight == 0) {
        if (trace > 1) msg("weightedSecDerRight is 0")
        nodeVal.right <- -sign(weightedFirstDerRight) * Inf
      } else {
        if (g$.rho) {
          rho <- abs(weightedFirstDerRight / weightedSecDerRight)
        } else {
          rho <- 1
        }
        nodeVal.right <- g$learning.rate * sign(weightedFirstDerRight) *
          min(g$rho.max, rho)
      }
    } else {
      nodeVal.right <- g$learning.rate * part.c.right
    }

    coef.right[1] <- coef.right[1] + part.c.right

    Fval <- node$Fval
    Fval[left.index] <- Fval[left.index] + nodeVal.left
    Fval[right.index] <- Fval[right.index] + nodeVal.right

    if (.class && trace > 1) {
      # msg("firstDerLeft = ", firstDerLeft, "; firstDerRgit = ", firstDerRight, sep = "")
      msg("weightedFirstDerLeft = ", weightedFirstDerLeft, "; weightedFirstDerRight = ",
          weightedFirstDerRight, sep = "")
      msg("weightedSecDerLeft = ", weightedSecDerLeft, "; weightedSecDerRight = ",
          weightedSecDerRight, sep = "")
      # msg("nodeVal.left = ", nodeVal.left, "; nodeVal.right = ", nodeVal.right, sep = "")
    }

    # '- Weights -' ====
    weights.left <- weights.right <- weights
    weights.left[right.index] <- weights.left[right.index] * gamma
    weights.right[left.index] <- weights.right[left.index] * gamma

    # '- [ Line ] -' ====
    if (.class) {
      firstDer <- -2 * g$y / (1 + exp(2 * g$y * Fval)) # n
      resid2 <- -firstDer
    } else {
      resid2 <- g$y - Fval
    }

    if (!is.null(cutFeat.name) & !is.constant(resid2)) {
      # Node split and resid2 is not constant
      if (length(left.index) < minobsinnode.lin) {
        # Too few observations to fit linear model
        if (trace > 1) msg("Looking at Node #", node.index * 2,
                           ": ", length(left.index),
                           " cases belong to this node: Not fitting any more lines here", sep = "")
        linVal.left <- rep(0, length(resid2))
        linCoef.left <- rep(0, g$ncolxm)
      } else {
        linCoef.left <- lincoef(x = g$xm[, -1, drop = FALSE], y = resid2,
                                weights = weights.left,
                                method = lin.type,
                                nvmax = nvmax,
                                alpha = alpha,
                                lambda = lambda,
                                lambda.seq = lambda.seq,
                                cv.glmnet.nfolds = cv.glmnet.nfolds,
                                which.cv.glmnet.lambda = which.cv.glmnet.lambda)

        linVal.left <- c(g$xm %*% linCoef.left)

        # Lin Updates, Left ====
        if (.class & g$.rho) {
          firstDer.rho.left <- (t((-2 * linVal.left * g$y) / (1 + exp(2 * g$y * Fval))) %*% weights.left)[1]
          secDer.rho.left <- (t((4 * linVal.left^2 * exp(2 * g$y * Fval)) / (1 + exp(2 * g$y * Fval))^2) %*% weights.left)[1]
          rho.left <- -firstDer.rho.left / secDer.rho.left
          rho.left <- sign(rho.left) * min(g$rho.max, rho.left, na.rm = TRUE)[1]
        } else {
          rho.left <- 1
        }

        Fval.left <- Fval[left.index] + g$learning.rate * rho.left * linVal.left[left.index] # n
        coef.left <- coef.left + linCoef.left

      } # /if (length(left.index) < minobsinnode.lin)

      if (length(right.index) < minobsinnode.lin) {
        # Too few observations to fit linear model
        if (trace > 1) msg("Looking at Node #", node.index * 2 + 1,
                           ": ", length(right.index),
                           " cases belong to this node: Not fitting any more lines here", sep = "")
        linVal.right <- rep(0, length(resid2))
        linCoef.right <- rep(0, g$ncolxm)
      } else {
        linCoef.right <- lincoef(x = g$xm[, -1, drop = FALSE], y = resid2,
                                 weights = weights.right,
                                 method = lin.type,
                                 nvmax = nvmax,
                                 alpha = alpha,
                                 lambda = lambda,
                                 lambda.seq = lambda.seq,
                                 cv.glmnet.nfolds = cv.glmnet.nfolds,
                                 which.cv.glmnet.lambda = which.cv.glmnet.lambda)
        linVal.right <- c(g$xm %*% linCoef.right)

        # Lin Updates, Right ====

      } # /if (length(right.index) < minobsinnode.lin)

      if (.class & g$.rho) {
        firstDer.rho.right <- (t((-2 * linVal.right * g$y) / (1 + exp(2 * g$y * Fval))) %*% weights.right)[1]
        secDer.rho.right <- (t((4 * linVal.right^2 * exp(2 * g$y * Fval)) / (1 + exp(2 * g$y * Fval))^2) %*% weights.right)[1]
        rho.right <- -firstDer.rho.right / secDer.rho.right
        rho.right <- sign(rho.right) * min(g$rho.max, rho.right, na.rm = TRUE)[1]
      } else {
        rho.right <- 1
      }

      Fval.right <- Fval[right.index] + g$learning.rate * rho.right * linVal.right[right.index] # n
      coef.right <- coef.right + linCoef.right

    }

    # '- Side-effects -' ====

    # Check: do we need this?
    depth <- g$tree[[paste(node.index)]]$depth + 1

    # Get combined error of children
    Fval1 <- Fval
    Fval1[left.index] <- Fval.left
    Fval1[right.index] <- Fval.right

    # Assign loss reduction to parent
    g$tree[[paste(node.index)]]$split.loss <- g$loss.fn(g$y, Fval1) # check: do we need weights?
    g$tree[[paste(node.index)]]$split.loss.red <- node$loss - g$tree[[paste(node.index)]]$split.loss

    # Set id numbers by preorder indexing
    # Left
    left.id <- node$id * 2
    g$tree[[paste(left.id)]] <- setNodeRC(g = g,
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
                                          rule = crules(node$rule, split.rule.left))

    # Right
    right.id <- node$id * 2 + 1
    g$tree[[paste(right.id)]] <- setNodeRC(g = g,
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
                                           rule = crules(node$rule, split.rule.right))

  } # /if (is.null(part$splits)) aka Node did split


} # rtemis::splitLineRC


# [[---F4---]] ====
#' Predict method for \code{shytreeLeavesRC} object
#'
#' @method predict shytreeLeavesRC
#' @param object \code{shytreeRaw}
#' @param newdata Data frame of predictors
#' @param n.feat [Internal use] Integer: Use first \code{n.feat} columns of newdata to predict.
#' Defaults to all
#' @param fixed.cxr [Internal use] Matrix: Cases by rules to use instead of matching cases to rules using
#' \code{newdata}
#' @param cxr.newdata [Internal use] Data frame: Use these values to match cases by rules
#' @param cxr Logical: If TRUE, return list which includes cases-by-rules matrix along with predicted values
#' @param cxrcoef Logical: If TRUE, return cases-by-rules * coefficients matrix along with predicted values
#' @param verbose Logical: If TRUE, print messages to console
#' @param trace Integer: 0, 1, or 2. Provides increasing amount to information printed to the
#' console
#' @export
#' @author Efstathios D. Gennatas

predict.shytreeLeavesRC <- function(object, newdata,
                                    type = c("response", "probability", "all", "step"),
                                    n.leaves = NULL,
                                    n.feat = NCOL(newdata),
                                    fixed.cxr = NULL,
                                    cxr.newdata = NULL,
                                    cxr = FALSE,
                                    cxrcoef = FALSE,
                                    verbose = FALSE, ...) {

  init <- object$init
  type <- match.arg(type)
  .class <- object$type == "Classification"

  # '-- Newdata ====
  if (is.null(colnames(newdata))) colnames(newdata) <- paste0("V", seq(NCOL(newdata)))

  newdata <- newdata[, seq(n.feat), drop = FALSE]
  # Add column of ones for intercept and convert factors to dummies
  newdata <- cbind(1, model.matrix(~. -1, data = newdata))

  if (type != "step") {
    # '-- Rules ====
    if (is.null(n.leaves)) {
      n.leaves <- max(sapply(object$stepindex, length))
    }

    if (n.leaves == 1) {
      yhat <- c(init + newdata %*% t(object$leaves$coefs))
    } else {
      rule.ids <- object$stepindex[[paste(n.leaves)]]
      rules <- sapply(rule.ids, function(j) object$all.step.leaves$rules$rule[object$all.step.leaves$rules$id == j])
      # Could get rules from tree, same difference?
      # rules <- sapply(rule.ids, function(i) object$tree[[paste(i)]]$rule)
      # coefs <- data.matrix(plyr::ldply(rule.ids, function(j) object$all.step.leaves$coefs[object$all.step.leaves$rules$id == j, ]))
      # ldply can't handle columns with duplicate names
      # sapply gives matrix of lists
      # coefs <- data.matrix(t(sapply(rule.ids, function(j) object$all.step.leaves$coefs[object$all.step.leaves$rules$id == j, ])))
      coefs <- lapply(rule.ids, function(j) object$all.step.leaves$coefs[object$all.step.leaves$rules$id == j, , drop = FALSE])
      coefs <- data.matrix(do.call(rbind, coefs))

      # '-- Cases x Rules ====
      if (is.null(fixed.cxr)) {
        cases <- if (is.null(cxr.newdata)) newdata else cxr.newdata
        .cxr <- matchCasesByRules(cases, rules, verbose = verbose)
      } else {
        .cxr <- fixed.cxr
      }

      # each case must match one leaf and one leaf only
      # DIAG: all(apply(.cxr, 1, sum) == 1)

      .cxrcoef <- .cxr %*% coefs

      # '-- yhat ====
      # TODO: Update to do length(rules) matrix multiplications and add
      yhat <- init + sapply(seq(NROW(newdata)), function(n)
        object$learning.rate * (newdata[n, ] %*% t(.cxrcoef[n, , drop = FALSE])))
    }

    if (.class) {
      if (type == "response") {
        yhat <- sign(yhat)
        yhat <- factor(yhat, levels = c(1, -1))
        # Calculate probability using GBM paper equation 21.
        levels(yhat) <- object$ylevels
      } else if (type == "probability") {
        yhat <- exp(2*yhat) / (1 + exp(2*yhat))
      } else if (type == "all") {
        prob <- exp(2*yhat) / (1 + exp(2*yhat))
        estimate <- sign(yhat)
        estimate <- factor(estimate, levels = c(1, -1))
        levels(estimate) <- object$ylevels
        out <- list(estimate = estimate, probability = prob)
        return(out)
      }
    } else {
      out <- yhat
    }

    if (!cxrcoef & !cxr) {
      out <- yhat
    } else {
      out <- list(yhat = yhat)
      if (cxrcoef) out$cxrcoef <- .cxrcoef
      if (cxr) out$cxr <- .cxr
    }

    out
    # /type != "step"

  } else {
    # >>> type == "step": Get predictions at each step
    # rules <- vector("list", length(n.leaves))
    max.leaves <- max(sapply(object$stepindex, length))

    if (max.leaves == 1) {
      yhat.l <- list(c(init + newdata %*% t(object$leaves$coefs)))
    } else {
      if (verbose) msg("Getting estimated values for each of", max.leaves, "steps...")

      rules.l <- plyr::llply(seq(max.leaves), function(j)
        paste(sapply(object$stepindex[[paste(j)]], function(k)
          c(object$all.step.leaves$rules$rule[object$all.step.leaves$rules$id == k]))))

      cases <- if (is.null(cxr.newdata)) newdata else cxr.newdata
      cxr.l <- plyr::llply(seq(max.leaves), function(j)
        matchCasesByRules(cases, rules.l[[j]], verbose = verbose))

      # get coefs for each leaf for each step in list
      coefs.l <- plyr::llply(seq(max.leaves), function(k)
        object$all.step.leaves$coefs[object$all.step.leaves$rules$id == k, ])

      # A list of length max.leaves, with a matrix of coefficients per rule
      coefs.l <- plyr::llply(seq(max.leaves), function(j)
        data.matrix(plyr::ldply(object$stepindex[[paste(j)]], function(k)
          object$all.step.leaves$coefs[object$all.step.leaves$rules$id == k, ]))
      )

      cxrcoef.l <- plyr::llply(seq(max.leaves), function(j)
        cxr.l[[j]] %*% coefs.l[[j]])

      yhat.l <- plyr::llply(seq(max.leaves), function(j) {
        yhat <- sapply(seq(NROW(newdata)), function(n)
          object$learning.rate * (newdata[n, ] %*% t(cxrcoef.l[[j]][n, , drop = FALSE])))
        if (.class) {
          yhat <- sign(yhat)
          yhat <- factor(yhat, levels = c(1, -1))
          levels(yhat) <- object$ylevels
        }
        yhat
      })
    }
    yhat.l
  }

} # rtemis:: predict.shytreeLeavesRC

# [[---F5---]] ====
#' Print method for \code{shytreeLeavesRC} object
#'
#' @method print shytreeLeavesRC
#' @param x \code{shytreeLeavesRC} object
#' @author Efstathios D. Gennatas
#' @export

print.shytreeLeavesRC <- function(x, ...) {

  cat("\n  A Stepwise Additive Tree model with", x$n.leaves, "leaves\n\n")

}


#' Convert \link{shytreeLeavesRC} to \code{data.tree} object
#'
#' @param object \link{shytreeLeavesRC} object

as.data.tree.shytreeLeavesRC <- function(object) {

  as.Node.data.frame <- getFromNamespace("as.Node.data.frame", "data.tree")
  dat <- object$leaves$rules
  dat$rule <- gsub(" & ", "/", dat$rule)
  dat$rule <- gsub("TRUE", "All cases", dat$rule)
  as.Node.data.frame(dat, pathName = "rule")

} # rtemis::as.data.tree.shytreeLeaves


class.loss <- function(y, Fval) {

  c(log(1 + exp(-2 * y * Fval)))

}

class.lossw <- function(y, Fval, weights) {

  c(log(1 + exp(-2 * y * Fval)) %*% weights)

}

# [[--F6--]] ====
selectleaves <- function(object,
                         x, y,
                         x.valid, y.valid,
                         smooth = TRUE,
                         print.plot = TRUE,
                         verbose = TRUE,
                         trace = 0) {
  if (trace > 1) msg("Running selectleaves")
  n.leaves <- object$n.leaves
  .class <- object$type == "Classification"

  train.estimate.l <- predict(object, newdata = x,
                              type = "step",
                              verbose = trace > 1)
  valid.estimate.l <- predict(object, newdata = x.valid,
                              type = "step",
                              verbose = trace > 1)

  # TODO: change bacc and mse to arg fn
  if (.class) {
    train.error <- sapply(seq(train.estimate.l), function(j)
      1 - bacc(y, train.estimate.l[[j]]))
    valid.error <- sapply(seq(valid.estimate.l), function(j)
      1 - bacc(y.valid, valid.estimate.l[[j]]))
  } else {
    train.error <- sapply(seq(train.estimate.l), function(j)
      mse(y, train.estimate.l[[j]]))
    valid.error <- sapply(seq(valid.estimate.l), function(j)
      mse(y.valid, valid.estimate.l[[j]]))
  }

  valid.error.smooth <- if (smooth) {
    # valid.error.smooth <- supsmu(seq(n.leaves), valid.error)$y
    valid.error.smooth <- suppressWarnings(loess(valid.error ~ seq(n.leaves))$fitted)
  } else {
    NULL
  }

  if (print.plot) {
    mplot3.xy(seq(n.leaves), list(Training = train.error,
                                  Validation = valid.error,
                                  `Smoothed Valid.` = valid.error.smooth),
              type = 'l', group.adj = .95, lty = c(1, 1, 2),
              line.col = c("#80ffff", "#FF99FF", "#2B27F1"),
              vline = c(which.min(valid.error), which.min(valid.error.smooth)),
              vline.col = c("#FF99FF", "#2B27F1"),
              xlab = "N leaves", ylab = ifelse(.class, "1 - Balanced Accuracy", "MSE"))
  }

  valid.error <- if (smooth) valid.error.smooth else valid.error

  n.leaves <- which.min(valid.error)
  if (verbose) msg("Selected", n.leaves, "leaves of", length(valid.error), "total",
                   color = rtHighlight)

  list(n.leaves = n.leaves,
       train.error = train.error,
       valid.error = valid.error,
       valid.error.smooth = valid.error.smooth)

} # rtemis::selectleaves
