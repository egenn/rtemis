# hytreew.R
# ::rtemis::
# 2018 E.D. Gennatas lambdamd.org

#' \code{rtemis internal}: Low-level Hybrid Tree procedure
#'
#' Train a Hybrid Tree for Regression
#'
#' Note that lambda is treated differently by \code{glmnet::glmnet} and \code{MASS::lm.ridge}
#' @inheritParams s.LIHAD
#' @param x data.frame
#' @param lin.type Character: "glmnet", "cv.glmnet" use the equivalent \code{glmnet} functions.
#' "lm.ridge" uses the MASS function of that name, "glm" uses \code{lm.fit},
#' "forward.stagewise" and "stepwise" use \code{lars::lars} with \code{type} defined accordingly
#' @author E.D. Gennatas
#' @noRd

hytreew <- function(x, y,
                    max.depth = 5,
                    weights = rep(1, NROW(y)),
                    init = mean(y),
                    gamma = .1,
                    shrinkage = 1,
                    # lincoef --
                    lin.type = "glmnet",
                    cv.glmnet.nfolds = 5,
                    which.cv.glmnet.lambda = "lambda.min",
                    alpha = 1,
                    lambda = .05,
                    lambda.seq = NULL,
                    # rpart --
                    minobsinnode = 2,
                    minobsinnode.lin = 10,
                    part.minsplit = 2,
                    part.xval = 0,
                    part.max.depth = 1,
                    part.cp = 0,
                    part.minbucket = 5,
                    #
                    save.fitted = FALSE,
                    verbose = TRUE,
                    trace = 0,
                    n.cores = rtCores) {

  # [ Check y is not constant ] ===
  if (is.constant(y)) {
    coefs <- list(rep(0, NCOL(x) + 1))
    names(coefs) <- c("(Intercept)", colnames(x))
    .mod <- list(init = init,
                 shrinkage = shrinkage,
                 rules = "TRUE",
                 coefs = coefs)
    class(.mod) <- c("hytreew", "list")
    return(.mod)
  }

  # [ GLOBAL ] ====
  .env <- environment()
  .env$x <- x
  .env$y <- y
  .env$df <- data.frame(x, y)
  .env$dm <- data.matrix(x)
  .env$gamma <- gamma

  # [ lin1 ] ====
  if (verbose) msg("Training Hybrid Tree (max depth = ", max.depth, ")...", sep = "")
  if (trace > 0) msg("Training lin1...", color = crayon::red)
  coef.c <- lincoef(x, y, method = lin.type,
                    alpha = alpha, lambda = lambda, lambda.seq = lambda.seq,
                    cv.glmnet.nfolds = cv.glmnet.nfolds,
                    which.cv.glmnet.lambda = which.cv.glmnet.lambda)
  Fval <- init + shrinkage * (data.matrix(cbind(1, x)) %*% coef.c)[, 1] # n
  if (trace > 0) msg("hytreew Fval is", head(Fval), color = crayon::red)

  # [ Run hytw ] ====
  root <- list(x = x,
               y = y,
               Fval = Fval,
               weights = weights,
               index = rep(1, length(y)),
               depth = 0,
               partlin = NULL,    # To hold the output of partLmw
               left = NULL,       # \  To hold the left and right nodes,
               right = NULL,      # /  if partLmw splits
               lin = NULL,
               part = NULL,
               coef.c = coef.c,
               terminal = FALSE,
               type = NULL,
               rule = "TRUE")
  mod <- hytw(node = root,
              max.depth = max.depth,
              minobsinnode = minobsinnode,
              minobsinnode.lin = minobsinnode.lin,
              shrinkage = shrinkage,
              alpha = alpha,
              lambda = lambda,
              lambda.seq = lambda.seq,
              cv.glmnet.nfolds = cv.glmnet.nfolds,
              which.cv.glmnet.lambda = which.cv.glmnet.lambda,
              coef.c = coef.c,
              part.minsplit = part.minsplit,
              part.xval = part.xval,
              part.max.depth = part.max.depth,
              part.cp = part.cp,
              part.minbucket = part.minbucket,
              .env = .env,
              keep.x = FALSE,
              simplify = TRUE,
              lin.type = lin.type,
              verbose = verbose,
              trace = trace)

  # [ Outro ] ====
  .mod <- list(init = init,
               shrinkage = shrinkage,
               rules = .env$leaf.rule,
               coefs = .env$leaf.coef)
  class(.mod) <- c("hytreew", "list")

  if (save.fitted) .mod$fitted <- predict(.mod, x)
  .mod

} # rtemis::hytree


# Recursive function
hytw <- function(node = list(x = NULL,
                             y = NULL,
                             Fval = NULL,
                             weights = NULL,
                             index = NULL,
                             depth = NULL,
                             partlin = NULL,    # To hold the output of partLmw
                             left = NULL,       # \  To hold the left and right nodes,
                             right = NULL,      # /  if partLmw splits
                             lin = NULL,
                             part = NULL,
                             coef.c = NULL,
                             terminal = NULL,
                             type = NULL,
                             rule = NULL),
                 coef.c = 0,
                 max.depth = 5,
                 minobsinnode = 2,
                 minobsinnode.lin = 5,
                 shrinkage = 1,
                 # lincoef --
                 lin.type = "glmnet",
                 alpha = 1,
                 lambda = .1,
                 lambda.seq = NULL,
                 cv.glmnet.nfolds = 5,
                 which.cv.glmnet.lambda = "lambda.min",
                 # rpart --
                 part.minsplit = 2,
                 part.xval = 0,
                 part.max.depth = 1,
                 part.minbucket = 5,
                 part.cp = 0,
                 .env = NULL,
                 keep.x = FALSE,
                 simplify = TRUE,
                 verbose = TRUE,
                 trace = 0) {

  # [ EXIT ] ====
  if (node$terminal) return(node)
  # x <- node$x
  # y <- node$y
  x <- .env$x
  y <- .env$y
  depth <- node$depth
  Fval <- node$Fval            # n
  resid <- y - Fval            # n
  if (trace > 0) msg("hytw Fval   is", head(Fval), color = crayon::red)
  if (trace > 0) msg("hytw resid   is", head(resid), color = crayon::red)
  nobsinnode <- length(node$index)

  # [ Add partlin to node ] ====
  if (node$depth < max.depth && nobsinnode >= minobsinnode) {
    # if (trace > 0) msg("y1 (resid) is", resid, color = crayon::red)
    node$partlin <- partLmw(x1 = x, y1 = resid,  # remove x
                            weights = node$weights,
                            .env = .env,
                            minobsinnode.lin = minobsinnode.lin,
                            # lincoef --
                            lin.type = lin.type,
                            alpha = alpha,
                            lambda = lambda,
                            lambda.seq = lambda.seq,
                            cv.glmnet.nfolds = cv.glmnet.nfolds,
                            which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                            # rpart --
                            part.minsplit = part.minsplit,
                            part.xval = part.xval,
                            part.max.depth = part.max.depth,
                            part.cp = part.cp,
                            part.minbucket = part.minbucket,
                            verbose = verbose,
                            trace = trace)
    # Fval <- Fval + shrinkage * (node$partlin$part.val + node$partlin$lin.val)
    # resid <- y - Fval
    # if (trace > 1) msg("Fval is", Fval)

    # '- If node split ====
    if (!node$partlin$terminal) {
      node$type <- "split"
      left.index <- node$partlin$left.index
      right.index <- node$partlin$right.index
      if (trace > 1) msg("Depth:", depth, "left.index:", node$partlin$left.index)
      # x.left <- x[left.index, , drop = FALSE]
      # x.right <- x[right.index, , drop = FALSE]

      # weights.left <- weights.right <- node$weights
      # weights.left[right.index] <- weights.left[right.index] * gamma
      # weights.right[left.index] <- weights.right[left.index] * gamma

      # y.left <- y[left.index]
      # y.right <- y[right.index]
      # if (trace > 1) msg("y.left is", y.left)
      # if (trace > 1) msg("y.right is", y.right)

      Fval.left <- Fval + shrinkage * (node$partlin$part.val + node$partlin$lin.val.left)
      Fval.right <- Fval + shrinkage * (node$partlin$part.val + node$partlin$lin.val.right)

      # resid.left <- y - Fval.left
      # resid.right <- y - Fval.right
      coef.c.left <- coef.c.right <- coef.c

      # Cumulative sum of coef.c
      coef.c.left <- coef.c.left + c(node$partlin$lin.coef.left[1] + node$partlin$part.c.left,
                                     node$partlin$lin.coef.left[-1])
      coef.c.right <- coef.c.right + c(node$partlin$lin.coef.right[1] + node$partlin$part.c.right,
                                       node$partlin$lin.coef.right[-1])
      if (trace > 1) msg("coef.c.left is", coef.c.left, "coef.c.right is", coef.c.right)
      # coef.c.right[[paste0("depth", depth + 1)]] <- list(coef = node$partlin$lin.coef,
      #                                                    c = node$partlin$part.c.right)
      if (!is.null(node$partlin$cutFeat.point)) {
        rule.left <- node$partlin$split.rule
        rule.right <- gsub("<", ">=", node$partlin$split.rule)
      } else {
        rule.left <- node$partlin$split.rule
        rule.right <- paste0("!", rule.left) # fix: get cutFeat.name levels and find complement
      }

      # Init Left and Right nodes
      node$left <- list(x = .env$x,
                        y = .env$y,
                        weights = node$partlin$weights.left,
                        Fval = Fval.left,
                        index = left.index,
                        depth = depth + 1,
                        coef.c = coef.c.left,
                        partlin = NULL,    # To hold the output of partLmw
                        left = NULL,       # \  To hold the left and right nodes,
                        right = NULL,      # /  if partLmw splits
                        terminal = FALSE,
                        type = NULL,
                        rule = paste0(node$rule, " & ", node$partlin$rule.left))
      node$right <- list(x = .env$x,
                         y = .env$y,
                         weights = node$partlin$weights.right,
                         Fval = Fval.right,
                         index = right.index,
                         depth = depth + 1,
                         coef.c = coef.c.right,
                         partlin = NULL,    # To hold the output of partLmw
                         left = NULL,       # \  To hold the right and right nodes,
                         right = NULL,      # /  if partLmw splits
                         terminal = FALSE,
                         type = NULL,
                         rule = paste0(node$rule, " & ", node$partlin$rule.right))

      # if (!keep.x) node$x <- NULL
      node$split.rule <- node$partlin$split.rule
      if (simplify) {
        node$y <- node$Fval <- node$index <- node$depth <- node$lin <- node$part <- node$type <- node$partlin <- NULL
      }

      # Run Left and Right nodes
      # [ LEFT ] ====
      if (trace > 0) msg("Depth = ", depth + 1, "; Working on Left node...", sep = "")
      node$left <- hytw(node$left,
                        coef.c = coef.c.left,
                        max.depth = max.depth,
                        minobsinnode = minobsinnode,
                        minobsinnode.lin = minobsinnode.lin,
                        shrinkage = shrinkage,
                        # lincoef --
                        lin.type = lin.type,
                        alpha = alpha,
                        lambda = lambda,
                        lambda.seq = lambda.seq,
                        cv.glmnet.nfolds = cv.glmnet.nfolds,
                        which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                        # rpart --
                        part.minsplit = part.minsplit,
                        part.xval = part.xval,
                        part.max.depth = part.max.depth,
                        part.cp = part.cp,
                        part.minbucket = part.minbucket,
                        .env = .env,
                        keep.x = keep.x,
                        simplify = simplify,
                        verbose = verbose,
                        trace = trace)
      # [ RIGHT ] ====
      if (trace > 0) msg("Depth = ", depth + 1, "; Working on Right node...", sep = "")
      node$right <- hytw(node$right,
                         coef.c = coef.c.right,
                         max.depth = max.depth,
                         minobsinnode = minobsinnode,
                         minobsinnode.lin = minobsinnode.lin,
                         shrinkage = shrinkage,
                         # lincoef --
                         lin.type = lin.type,
                         alpha = alpha,
                         lambda = lambda,
                         lambda.seq = lambda.seq,
                         cv.glmnet.nfolds = cv.glmnet.nfolds,
                         which.cv.glmnet.lambda = which.cv.glmnet.lambda,
                         # rpart --
                         part.minsplit = part.minsplit,
                         part.xval = part.xval,
                         part.max.depth = part.max.depth,
                         part.cp = part.cp,
                         part.minbucket = part.minbucket,
                         .env = .env,
                         keep.x = keep.x,
                         simplify = simplify,
                         verbose = verbose,
                         trace = trace)
      if (simplify) node$coef.c <- NULL
    } else {
      # partLmw did not split
      node$terminal <- TRUE
      .env$leaf.rule <- c(.env$leaf.rule, node$rule)
      .env$leaf.coef <- c(.env$leaf.coef, list(node$coef.c))
      node$type <- "nosplit"
      if (trace > 0) msg("STOP: nosplit")
      if (simplify) node$x <- node$y <- node$Fval <- node$index <- node$depth <- node$type <- node$partlin <- NULL
    } # !node$terminal

  } else {
    # max.depth or minobsinnode reached
    node$terminal <- TRUE
    .env$leaf.rule <- c(.env$leaf.rule, node$rule)
    .env$leaf.coef <- c(.env$leaf.coef, list(node$coef.c))
    if (node$depth == max.depth) {
      if (trace > 0) msg("STOP: max.depth")
      node$type <- "max.depth"
    } else if (nobsinnode < minobsinnode) {
      if (trace > 0) msg("STOP: minobsinnode")
      node$type <- "minobsinnode"
    }
    if (simplify) node$x <- node$y <- node$Fval <- node$index <- node$depth <- node$type <- node$partlin <- NULL
    return(node)
  } # max.depth, minobsinnode

  node

} # rtemis::hytw


#' \pkg{rtemis} internal: Ridge and Stump
#'
#' Fit a linear model on (x, y) and a tree on the residual yhat - y
partLmw <- function(x1, y1,    # remove x, use .env$x
                    weights,
                    .env,
                    minobsinnode.lin,
                    # lincoef --
                    lin.type,
                    alpha,
                    lambda,
                    lambda.seq,
                    cv.glmnet.nfolds,
                    which.cv.glmnet.lambda,
                    # rpart --
                    part.minsplit,
                    part.xval,
                    part.max.depth,
                    part.cp,
                    part.minbucket,
                    verbose,
                    trace) {

  # [ PART ] ====
  if (trace > 1) msg("partLmw")
  dat <- data.frame(x1, y1)
  part <- rpart::rpart(y1 ~., dat,
                       weights = weights,
                       control = rpart::rpart.control(minsplit = part.minsplit,
                                                      xval = part.xval,
                                                      maxdepth = part.max.depth,
                                                      minbucket = part.minbucket,
                                                      cp = part.cp))
  part.val <- predict(part) # n

  if (is.null(part$splits)) {
    if (trace > 0) msg("Note: rpart did not split")
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
        if (trace > 0) msg("Split Feature is \"", cutFeat.name,
                           "\"; Cut Point = ", cutFeat.point,
                           sep = "")
        split.rule.left <- paste(cutFeat.name, "<", cutFeat.point)
        split.rule.right <- paste(cutFeat.name, ">=", cutFeat.point)
      } else {
        cutFeat.category <- levels(x1[[cutFeat.name]])[which(part$csplit[1, ] == 1)]
        if (trace > 0) msg("Split Feature is \"", cutFeat.name,
                           "\"; Cut Category is \"", cutFeat.category,
                           "\"", sep = "")
        split.rule.left <- paste0(cutFeat.name, " %in% ", "c(", paste(cutFeat.category, collapse = ", "))
        split.rule.right <- paste0("!", cutFeat.name, " %in% ", "c(", paste(cutFeat.category, collapse = ", "))
      }
      if (length(cutFeat.point) > 0) {
        left.index <- which(x1[, cutFeat.index] < cutFeat.point)
        right.index <- seq(NROW(x1))[-left.index]
      } else {
        left.index <- which(is.element(x1[, cutFeat.index], cutFeat.category))
        right.index <- seq(NROW(x1))[-left.index]
      }
      # ? should weights be assigned after this so are available even if there was no split
      weights.left <- weights.right <- weights
      weights.left[right.index] <- weights.left[right.index] * .env$gamma
      weights.right[left.index] <- weights.right[left.index] * .env$gamma
    } # !is.null(cutFeat.name)
  }

  # [ LIN ] ====
  resid <- y1 - part.val               # n
  # resid.left <- resid[left.index]
  # resid.right <- resid[right.index]
  if (!is.null(cutFeat.name)) {
    if (is.constant(resid) | length(resid) < minobsinnode.lin) {
      if (trace > 0) msg("Not fitting any more lines here")
      lin.val.left <- rep(0, length(y1))
      lin.coef.left <- rep(0, NCOL(x1) + 1)
    } else {
      # dat <- data.frame(x1[left.index, , drop = FALSE], resid.left)
      # dat.mm <- model.matrix(resid.left ~ ., dat)
      lin.coef.left <- lincoef(x1, resid,
                               weights = weights.left,
                               method = lin.type,
                               alpha = alpha, lambda = lambda, lambda.seq = lambda.seq,
                               cv.glmnet.nfolds = cv.glmnet.nfolds,
                               which.cv.glmnet.lambda = which.cv.glmnet.lambda)
      lin.val.left <- (cbind(1, .env$dm) %*% lin.coef.left)[, 1]
    } # if (is.constant(resid.left))

    if (is.constant(resid) | length(resid) < minobsinnode.lin) {
      if (trace > 0) msg("Not fitting any more lines here")
      lin.val.right <- rep(0, length(y1))
      lin.coef.right <- rep(0, NCOL(x1) + 1)
    } else {
      lin.coef.right <- lincoef(x1, resid,
                                weights = weights.right,
                                method = lin.type,
                                alpha = alpha, lambda = lambda, lambda.seq = lambda.seq,
                                cv.glmnet.nfolds = cv.glmnet.nfolds,
                                which.cv.glmnet.lambda = which.cv.glmnet.lambda)
      lin.val.right <- (cbind(1, .env$dm) %*% lin.coef.right)[, 1]
    } # if (is.constant(resid.right))

  } # if (!is.null(cutFeat.name))

  list(weights.left = weights.left,
       weights.right = weights.right,
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
       rule.left = split.rule.left,
       rule.right = split.rule.right,
       terminal = terminal)

} # rtemis::partLmw


#' Predict method for \code{hytreew} object
#'
#' @method predict hytreew
#' @param object \code{hytreew}
#' @param newdata Data frame of predictors
#' @param n.feat [Internal use] Integer: Use first \code{n.feat} columns of newdata to predict.
#' Defaults to all
#' @param fixed.cxr [Internal use] Matrix: Cases by rules to use instead of matching cases to rules using
#' \code{newdata}
#' @param cxr.newdata [Internal use] Data frame: Use these values to match cases by rules
#' @param cxr Logical: If TRUE, return list which includes cases-by-rules matrix along with predicted values
#' @param cxrcoef Logical: If TRUE, return cases-by-rules * coefficients matrix along with predicted values
#' @param verbose Logical: If TRUE, print messages to console
#' @param trace Not used
#' @export
#' @author E.D. Gennatas

predict.hytreew <- function(object, newdata,
                            n.feat = NCOL(newdata),
                            fixed.cxr = NULL,
                            cxr.newdata = NULL,
                            cxr = FALSE,
                            cxrcoef = FALSE,
                            verbose = FALSE,
                            trace = 0, ...) {

  # [ newdata colnames ] ====
  if (is.null(colnames(newdata))) colnames(newdata) <- paste0("V", seq(NCOL(newdata)))

  # [ PREDICT ] ====
  newdata <- newdata[, seq(n.feat), drop = FALSE]
  rules <- plyr::ldply(object$rules)[, 1]
  if (is.null(fixed.cxr)) {
    cases <- if (is.null(cxr.newdata)) newdata else cxr.newdata
    .cxr <- matchCasesByRules(cases, rules, verbose = verbose)
  } else {
    .cxr <- fixed.cxr
  }

  coefs <- plyr::laply(object$coefs, c)
  # Match coefficients to each case by matrix multiplication
  # Each case only has "1" in .cxr for the corresponding leaf it belongs
  .cxrcoef <- .cxr %*% coefs
  # Add column of ones for intercept
  newdata <- data.matrix(cbind(1, newdata))
  yhat <- sapply(seq(NROW(newdata)), function(n)
    object$init + object$shrinkage * (newdata[n, ] %*% t(.cxrcoef[n, , drop = FALSE])))

  if (!cxrcoef & !cxr) {
    out <- yhat
  } else {
    out <- list(yhat = yhat)
    if (cxrcoef) out$cxrcoef <- .cxrcoef
    if (cxr) out$cxr <- .cxr
  }

  out

} # rtemis:: predict.hytreew
