# addtree.R
# ::rtemis::
# G. Valdes (MATLAB), E.D. Gennatas (R) lambdamd.org

#' Additive Tree
#'
#' Trains a binary decision tree classifier using the Additive Tree
#'
#' Outcome must be factor with two levels, the first level is the 'positive' class
#'
#' lambda = gamma/(1 - gamma)
#' @param x Matrix / Data frame of features
#' @param y Integer {-1, 1}: Vector of binary outcomes
#' @param catPredictors Optional: Logical vector indicating categorical features
#' @param depthLimit Integer: Maximum depth of tree to grow
#' @param learningRate learning rate for the Newton Raphson step that updates the
#' function values of the node for \code{update = "exponential"}
#' @param gamma Float (0, 1): Accelaration factor
#' @param update Character: "Exponential" or "Polynomial"
#' @param verbose Logical: If TRUE, print messages to output
#' @author E.D. Gennatas (R), Gilmer Valdes, Jose Marcio Luna Castaneda (original MATLAB)
#' @references Valdes Gilmer, Luna Jose, Eaton Eric, Ungar Lyle, Simone Charles
#' and Solberg Timothy. MediBoost: a Patient Stratification Tool for
#' Interpretable Decision Making in the Era of Precision Medicine.
#' @noRd

addtree <- function(x, y,
                    catPredictors = NULL,
                    depthLimit = 8,
                    learningRate = 1,
                    gamma = 0,
                    update = "exponential",
                    min.update = ifelse(update == "polynomial", 10, 1000),
                    weights = NULL,
                    autoweights = FALSE,
                    min.hessian = .01,
                    min.membership = 0,
                    steps.past.min.membership = 2,
                    rpart.params = NULL,
                    save.rpart = FALSE,
                    verbose = TRUE,
                    trace = 1) {

  # Arguments ====
  if (is.null(catPredictors)) catPredictors <- lapply(x, class) == "factor"

  # Data ====
  y0 <- y
  # replace dataPrepare
  dt <- dataPrepare(x, y, NULL, NULL)
  if (dt$type == "Classification") {
    if (length(levels(y)) > 2) stop("Only binary classification supported")
    levels(y) <- c(1, -1)
    y <- as.numeric(as.character(y))
  }

  # Weights ====
  if (!is.null(weights)) if (length(weights) != length(y0))
    stop("weights must be length equal to N cases")
  if (is.null(weights)) weights <- rep(1, NROW(y0))
  # autoweighted is deprecated as we use ipw by default
  if (autoweights) {
    # Overrides user-supplied weights
    # Account for imbalanced classes
    freq <- table(y0)
    weights[y0 == levels(y0)[1]] <- max(freq) / freq[levels(y0)[1]]
    weights[y0 == levels(y0)[2]] <- max(freq) / freq[levels(y0)[2]]
    weights <- weights / max(weights)
  }
  # root.prob <- min(freq)/max(freq)

  # vector: Initial probabilities
  probability <- weights / NROW(y)

  # scalar: Initial node value
  nodeValue <- log(1 + probability %*% y) - log(1 - probability %*% y)

  # vector: Initial observations
  # obsValues <- rep(1, NROW(y)) * c(log(1 + probability %*% y) - log(1 - probability %*% y))
  obsValues <- rep(nodeValue, NROW(y))

  # min.mem.counter: Count steps past min.membership
  min.mem.counter <- 0

  # Split node ====
  membership <- rep(TRUE, length(y))

  # Recursive function to build tree
  tree <- likelihoodMediboostSplitNode(x, y,
                                       weights = weights,
                                       catPredictors,
                                       obsValues,
                                       nodeValue,
                                       colIdx = seq(NCOL(x)),
                                       depth = 0,
                                       depthLimit,
                                       learningRate,
                                       gamma,
                                       membership,
                                       min.membership,
                                       steps.past.min.membership,
                                       min.mem.counter = min.mem.counter,
                                       update,
                                       min.update,
                                       hessian = Inf,
                                       min.hessian,
                                       save.rpart,
                                       verbose = ifelse(trace < 2, FALSE, TRUE))

  # Output ====
  tree$y <- y0
  tree$yfreq <- table(y0)
  class(tree) <- c("addtree", "list")
  tree
} # rtemis::addtree

#
#  inputs:
#   x               -   N x D matrix of N examples with D features
#   y               -   N x 1 vector of labels with values in {-1,1}
#   xRange          -   cell array containing the range of values for each feature
#   weights         -   Double variable that stores the weight of each
#                       observation for each node
#   catPredictors   -   Logical vector with the same length as the feature
#                       vector, where a true entry means that the corresponding column of x is
#                       a categorical variable
#   obsValues       -   Double vector containing the current observations
#   nodeValue       -   the default value of the node if y is empty
#   colIdx          -   the indices of features (columns) under consideration
#   depth           -   current depth of the tree
#   depthLimit      -   maximum depth of the tree
#   learningRate    -   learning rate for the Newton Raphson step that
#                       updates the function values of the node
#   lambda          -   acceleration factor
#
# Returns a linked hierarchy of structs with the following fields:
#
#   node.terminal               -   Logical variable that indicates whether or not this
#                                   node is a terminal (leaf) node
#   node.fIdx, node.cutPoint    -   variables used to carry out the feature
#   node.cutCategory                based tests associated with this node.
#                                   If the features are continuous then the
#                                   test is (is x(fIdx) > cutPoint?) if
#                                   the features are categorical the test is
#                                   (is x(fIdx) in cutCategory{2}?)
#   node.weights                -   Double variable that stores the weight of each
#                                   observation for each node
#   node.obsValues              -   Double variable that stores the current
#                                   observations in the node
#   node.value                  -   Double variable that stores the summation of all
#                                   weights of the parent nodes and the current node
#   node.left                   -   Struct of the child node on left branch (f <= value)
#   node.right                  -   Struct of the child node on right branch (f > value)
#   node.maxig                  -   Double variable containing the information gain
#                                   associated with this node

#' \pkg{rtemis} internal: likelihoodMediboostSplitNode
#'
#' Recursive function that returns a node structure under the MediBoost paradigm
#' @noRd

likelihoodMediboostSplitNode <- function(x, y,
                                         weights,
                                         catPredictors,
                                         obsValues,
                                         nodeValue,
                                         colIdx,
                                         depth,
                                         depthLimit,
                                         learningRate,
                                         gamma,
                                         membership,
                                         min.membership,
                                         steps.past.min.membership,
                                         min.mem.counter,
                                         update,
                                         min.update = ifelse(update == "exponential", 1000, 10),
                                         hessian,
                                         min.hessian,
                                         save.rpart,
                                         verbose = TRUE) {

  # Intro ====
  if (verbose) msg("Depth =", depth)

  # Initialize node
  node <- list(terminal = TRUE,
               fIdx = NULL,
               cutPoint = NULL,
               cutCategory = NULL,
               weights = weights,
               obsValues = obsValues,
               value = nodeValue,
               left = NULL,
               right = NULL,
               membership = membership,
               depth = depth)

  # Choose feature; split checks ====
  if (sum(membership) <= min.membership) {
    min.mem.counter <- min.mem.counter + 1
  }
  if (verbose) msg("sign(node$value) is", sign(node$value),
                   "sign(y %*% node$weights) is", sign(y %*% node$weights),
                   "node$value is", node$value,
                   "depth is", depth,
                   "sum(membership) is", sum(membership),
                   "min.mem.counter is", min.mem.counter)
  if ((sign(node$value) != sign(y %*% node$weights) |
       depth < depthLimit) &
      !is.infinite(node$value) &
      hessian >= min.hessian &
      sum(membership) >= min.membership &
      min.mem.counter <= steps.past.min.membership) {

    # if (verbose) msg("sign(node$value) is", sign(node$value), "sign(y %*% node$weights) is",
    #                  sign(y %*% node$weights), "node$value is", node$value,
    #                  "and Depth is", depth)
    # Indicating that the structure is not a leaf
    node$terminal <- FALSE

    # Choose a feature to split on using regression of the first derivative of the loss function.
    nodefeat <- likelihoodMediboostChooseFeat(x, y,
                                              catPredictors,
                                              funcValue = node$obsValues,
                                              weights = node$weights,
                                              colIdx,
                                              verbose = verbose)
    node$fIdx <- nodefeat$fIdx
    node$cutPoint <- nodefeat$cutPoint
    node$cutCategory <- nodefeat$cutCategory
    # node$tree <- if (save.rpart) nodefeat$tree else NA # each ~ 1.7MB; can add up a LOT
    if (save.rpart) {
      node$tree <- nodefeat$tree
    } else {
      node$tree <- list(frame = nodefeat$tree$frame)
    }
    node$membership <- membership
    # cutCategory contains all levels of categorical var that should go Left
    if (length(node$cutPoint) > 0 && !is.na(node$cutPoint) |
        length(node$cutCategory) > 0 && !is.na(node$cutCategory)) {
      # The node is not terminal
      node$terminal <- FALSE

      # Split the data based on this feature
      if (length(node$cutPoint) > 0) {
        leftIdx <- x[, node$fIdx] < node$cutPoint
        rightIdx <- x[, node$fIdx] >= node$cutPoint
      } else {
        leftIdx <- is.element(x[, node$fIdx], node$cutCategory)
        rightIdx <- !is.element(x[, node$fIdx], node$cutCategory)
      }

      # Assign new memberships
      membershipRight <- membership & rightIdx
      membershipLeft <- membership & leftIdx

      # Calculate the observations, weights and node coefficients
      # Calculate left weights and outputs
      leftY <- y[leftIdx]
      leftWeight <- weights[leftIdx]
      # Calculate right weights and outputs
      rightY <- y[rightIdx]
      rightWeight <- weights[rightIdx]

      # Calculate the current value of the function for right and left node
      # Vector, length < y
      funcValueLeft <- node$obsValues[leftIdx]
      funcValueRight <- node$obsValues[rightIdx]

      # Compute the coefficients of the left child node
      # Vector, length < y
      firstDerLeft <- -2 * leftY / (1 + exp(2 * leftY * funcValueLeft))
      # Remove NaNs (from when there are no cases in node)
      firstDerLeft[is.na(firstDerLeft)] <- 0
      # Scalar
      weightedFirstDerLeft <- c(leftWeight %*% firstDerLeft)
      # Vector
      secDerLeft <- abs(firstDerLeft) * (2 - abs(firstDerLeft))
      # Scalar
      weightedSecDerLeft <- c(leftWeight %*% secDerLeft)

      # Update nodeValueLeft ====
      if (weightedFirstDerLeft == 0) {
        nodeValueLeft <- 0
      } else if (weightedSecDerLeft == 0) {
        # nodeValueLeft <- c(sign(y[leftIdx] %*% leftWeight) * Inf)
        nodeValueLeft <- -sign(weightedFirstDerLeft) * Inf
      } else {
        updateVal <- learningRate * sign((weightedFirstDerLeft)) *
          min(min.update, abs(weightedFirstDerLeft / weightedSecDerLeft))
        nodeValueLeft <- c(node$value - updateVal) # scalar
      }

      # Protect from taking too big a step: ensure step is always smaller than optimal
      # Using exp(log(a)-log(b)) = a/b to avoid singularity errors in
      # the gradient step when the weights approach zero
      # nodeValueLeft <- node$value - Re(exp(log(learningRate * (weightedFirstDerLeft)) - log(weightedSecDerLeft)))
      # observValuesLeft <- funcValueLeft - Re(exp(log(learningRate * (weightedFirstDerLeft)) - log(weightedSecDerLeft)))

      # vector, length < y
      observValuesLeft <- funcValueLeft - nodeValueLeft

      # Compute the coefficients for the right child node
      firstDerRight <- -2 * rightY / (1 + exp(2 * rightY * funcValueRight))
      firstDerRight[is.na(firstDerRight)] <- 0
      secDerRight <- abs(firstDerRight) * (2 - abs(firstDerRight))
      weightedFirstDerRight <-  c(rightWeight %*% firstDerRight) # delta 09.18
      weightedSecDerRight <- c(rightWeight %*% secDerRight)

      # UPDATE nodeValueRight ====
      if (weightedFirstDerRight == 0) {
        nodeValueRight <- 0
      } else if (weightedSecDerRight == 0) {
        # nodeValueRight <- c(sign(y[rightIdx] %*% rightWeight) * Inf)
        nodeValueRight <- -sign(weightedFirstDerRight) * Inf
      } else {
        # 12.31.2017 add learningRate
        updateVal <- learningRate * sign(weightedFirstDerRight) *
          min(min.update, abs(weightedFirstDerRight / weightedSecDerRight))
        nodeValueRight <- c(node$value - updateVal) # scalar
      }

      if (verbose) {
        msg("weightedFirstDerLeft = ", weightedFirstDerLeft, "; weightedFirstDerRight = ",
            weightedFirstDerRight, sep = "")
        msg("weightedSecDerLeft = ", weightedSecDerLeft, "; weightedSecDerRight = ",
            weightedSecDerRight, sep = "")
        msg("nodeValueLeft = ", nodeValueLeft, "; nodeValueRight = ", nodeValueRight, sep = "")
      }

      # Using exp(log(a)-log(b)) = a/b to avoid singularity errors in
      # the gradient step when the weights approach zero
      observValuesRight <- funcValueRight - nodeValueRight

      # Update the observation values at this depth
      newObservValue <- node$obsValues
      newObservValue[leftIdx] <- observValuesLeft
      newObservValue[rightIdx] <- observValuesRight

      # Indicator function that assigns 1 to the samples in the left
      # branch and gamma to the remaining ones

      # Update Weights ====
      if (update == "polynomial") {
        # Polynomial
        leftRule <- y
        leftRule[leftIdx] <- 1
        leftRule[rightIdx] <- gamma

        # Assign 1 to the samples in the right branch and gamma to the remaining
        rightRule <- y
        rightRule[rightIdx] <- 1
        rightRule[leftIdx] <- gamma

        # Update the weights
        leftWeights <- node$weights * leftRule
        rightWeights <- node$weights * rightRule
      } else {
        # Exponential
        leftRule <- y
        leftRule[leftIdx] <- 1
        leftRule[rightIdx] <- -1

        # Assign 1 to the samples in the right branch and -1 to the remaining
        rightRule <- y
        rightRule[leftIdx] <- -1
        rightRule[rightIdx] <- 1

        # Update the weights
        lambda <- gamma/(1 - gamma)
        if (!is.infinite(lambda)) {
          leftWeights <- node$weights*(exp((leftRule - 1)*lambda/2) /
                                         (exp((leftRule - 1)*lambda/2) + exp((rightRule - 1)*lambda/2) ))
          rightWeights <- node$weights*(exp((rightRule - 1)*lambda/2) /
                                          (exp((leftRule - 1)*lambda/2) + exp((rightRule - 1)*lambda/2)))
        } else {
          leftWeights <- rightWeights <- node$weights
        }

      } # / update

      # Normalize the new weights to the total sum of weights
      leftWeights <- leftWeights / sum(leftWeights)
      rightWeights <- rightWeights / sum(rightWeights)

      # L&R SPLIT ====
      # Create the right and left terminal nodes
      node$right <- likelihoodMediboostSplitNode(x, y,
                                                 weights = rightWeights,
                                                 catPredictors,
                                                 obsValues = newObservValue,
                                                 nodeValue = nodeValueRight,
                                                 colIdx = seq(NCOL(x)),
                                                 depth = depth + 1,
                                                 depthLimit,
                                                 learningRate,
                                                 gamma,
                                                 membership = membershipRight,
                                                 min.membership = min.membership,
                                                 steps.past.min.membership = steps.past.min.membership,
                                                 min.mem.counter = min.mem.counter,
                                                 update = update,
                                                 min.update = min.update,
                                                 hessian = weightedSecDerRight,
                                                 min.hessian = min.hessian,
                                                 save.rpart = save.rpart,
                                                 verbose = verbose)
      node$left <- likelihoodMediboostSplitNode(x, y,
                                                weights = leftWeights,
                                                catPredictors,
                                                obsValues = newObservValue,
                                                nodeValue = nodeValueLeft,
                                                colIdx = seq(NCOL(x)),
                                                depth = depth + 1,
                                                depthLimit,
                                                learningRate,
                                                gamma,
                                                membership = membershipLeft,
                                                min.membership = min.membership,
                                                steps.past.min.membership = steps.past.min.membership,
                                                min.mem.counter = min.mem.counter,
                                                update = update,
                                                min.update = min.update,
                                                hessian = weightedSecDerLeft,
                                                min.hessian = min.hessian,
                                                save.rpart = save.rpart,
                                                verbose = verbose)
    } else {
      node$terminal <- TRUE
    }
  } else {
    if (verbose) {
      if (depth == depthLimit) msg("Reached max depth")
      msg("Total members = ", sum(membership))
      if (is.infinite(node$value)) {
        msg("Node value is", ifelse(sign(node$value) == 1, "positive", "negative"), "infinity")
      }
      if (hessian < min.hessian) msg("hessian = ", hessian, " (min.hessian = ", min.hessian, ")",
                                     sep = "")
      if (min.mem.counter > steps.past.min.membership) msg("Reached max steps past min.membership")
    }
    return(node)
  }
  return(node)
} # rtemis::likelihoodMediboostSplitNode


# likelihoodMediboostChooseFeat - Selects a feature with maximum information
# gain and provides the decision values and column index for the chosen
# feature
#
# Usage:
#
# [fIdx,cutPoint,cutCategory] = likelihoodMediboostChooseFeat(x, y, catPredictors, funcValue, weights, colIdx)
#
#  inputs:
#   x               -   N x D matrix of N examples with D features
#   y               -   N x 1 vector of labels with values in {-1,1}
#   catPredictors   -   Logical vector with the same length as the feature
#                       vector, where a true entry means that the corresponding column of x is
#                       a categorical variable
#   funcValue       -   function of observation values
#   weights         -   distributions of observatios
#   colIdx          -   the indices of features (columns) under consideration
#
#  outputs:
#   fIdx            -   index of the feature with maximum information gain
#   cutPoint        -   decision value of feature with maximum information gain
#   cutCategory     -   decision value of category with maximum information gain
#
# SEE ALSO
#   likelihoodmediboostdrawtree, likelihoodmediboostprunetree, likelihoodmediboosttrain, likelihoodmediboostvalue

# {M} function [fIdx,cutPoint,cutCategory] = likelihoodMediboostChooseFeat(x,y,catPredictors,funcValue,weights,colIdx)

#' \pkg{rtemis} internal: likelihoodMediboostChooseFeat
#' Selects a feature with maximum information gain and provides the decision values
#' and column index for the chosen feature
#' @noRd

likelihoodMediboostChooseFeat <- function(x, y,
                                          catPredictors,
                                          funcValue,
                                          weights,
                                          colIdx,
                                          verbose = TRUE) {

  # Initialize variables
  cutPoint <- cutCategory <- NULL

  # Compute current Score
  firstDer <- -2 * y / (1 + exp(2 * y * funcValue))

  # Choose the split using rpart and check if the split was on a continuous or
  # categorical variable
  y <- -firstDer
  x1 <- x[, colIdx]
  df <- data.frame(y, x1)

  # rpart ====
  tree <- rpart::rpart(y ~ ., data = df,
                       weights = weights,
                       control = rpart::rpart.control(maxdepth = 1,
                                                      minsplit = 1,
                                                      minbucket = 1,
                                                      maxsurrogate = 0,
                                                      cp = 0,
                                                      xval = 0),
                       parms = rpart.params)

  # Get the split feature
  cutFeatName <- rownames(tree$splits)[1]
  if (!is.null(cutFeatName)) {
    fIdx <- which(names(x) == cutFeatName)

    if (is.numeric(x[[cutFeatName]])) {
      cutPoint <- tree$splits[1, "index"]
      if (verbose) msg("Split Feature is \"", cutFeatName, "\"; Cut Point = ", cutPoint,
                       sep = "")
    } else {
      cutCategory <- levels(x[[cutFeatName]])[which(tree$csplit[1, ] == 1)]
      if (verbose) msg("Split Feature is \"", cutFeatName, "\"; Cut Category is \"", cutCategory,
                       "\"", sep = "")
    }
  } else {
    fIdx <- cutPoint <- cutCategory
  }

  # OUT ====
  list(fIdx = fIdx,
       cutPoint = cutPoint,
       cutCategory = cutCategory,
       tree = tree)
} # rtemis::likelihoodMediboostChooseFeat


# LIKELIHOODMEDIBOOSTVALUE - Get the value of the decision tree given an input of
# features. For more information see:
#
# VALDES Gilmer, LUNA Jose, EATON Eric, UNGAR Lyle, SIMONE Charles
# and SOLBERG Timothy. MediBoost: a Patient Stratification Tool for
# Interpretable Decision Making in the Era of Precision Medicine. Under
# Review. 2016.
#
#   yPred = LIKELIHOODMEDIBOOSTVALUE(tree, x)
#
#  inputs:
#   tree    -   binary decision tree classifier built under
#               the Mediboost paradigm
#   x       -   N x D matrix of N examples with D features
#
#  output:
#   yPred   -   Logical prediction of a binary classifier using the
#               Mediboost paradigm
#
# SEE ALSO
#   likelihoodMediboostChooseFeat, likelihoodmediboostdrawtree, likelihoodmediboostprunetree, likelihoodmediboosttrain

#' Predict Method for MediBoost Model
#'
#' Obtains predictions from a trained MediBoost model
#'
#' @param object A trained model of class \code{"addtree"}
#' @param newdata Optional: a matrix / data.frame of features with which to predict
#' @param verbose Logical: If TRUE, print messages to output
#' @param ... Not used
#' @author E.D. Gennatas
#' @export

predict.addtree <- function(object, newdata, verbose = FALSE, ...) {

  tree <- object
  n.samples <- NROW(newdata)
  yPred <- rep(NA, n.samples)

  if (verbose) msg("tree$fIdx is", tree$fIdx)

  for (i in seq_len(n.samples)) {
    # Start at root
    node <- tree
    x_i <- newdata[i, ]
    while (!is.null(node) && !node$terminal) {

      if (!is.null(node$cutPoint)) {
        if (x_i[node$fIdx] < node$cutPoint && !is.null(node$left)) {
          node <- node$left
          if (verbose) msg("node$fIdx is ", node$fIdx, " and node$cutPoint is", node$cutPoint, "and path is left")
        } else if (!is.null(node$right) && !is.null(node$right)) {
          node <- node$right
          if (verbose) msg("node$fIdx is ", node$fIdx, " and node$cutPoint is", node$cutPoint, "and path is right")
        } else {
          break
        }
      } else {
        if (is.element(x_i[, node$fIdx], node$cutCategory) && !is.null(node$left)) {
          node <- node$left
          if (verbose) msg("node$fIdx is ", node$fIdx, " and node$cutCategory is", node$cutCategory, "and path is left")
        } else if (!is.null(node$right)) {
          node <- node$right
          if (verbose) msg("node$fIdx is ", node$fIdx, " and node$cutCategory is", node$cutCategory, "and path is right")
        } else {
          break
        }
      }
    }
    # Calculate the prediction
    if (verbose) msg("node$value is", node$value)
    yPred[i] <- sign(node$value)
    if (yPred[i] == 0) yPred[i] <- c(1, -1)[which.max(object$yfreq)]
  }
  yPred <- factor(yPred, levels = c(1, -1))
  levels(yPred) <- levels(object$y)
  yPred

} # rtemis::predict.addtree


#' \code{rtemis-internal} Traverse ADDTREE tree by preorder
#'
#' Recursively Traverses ADDTREE tree by preorder function and builds data frame representation
#'
#' @param rt rt Object from \link{s.ADDTREE}
#' @param x Features
#' @param verbose Logical: If TRUE, print messages to stdout
#'
#' @author E.D. Gennatas


# preorder + Include Rules ====
# Traverse ADDTREE tree
preorderTree.addtree <- function(rt, x, verbose = FALSE) {

  varnames <- rt$xnames
  tree <- rt$mod

  # Recursive preorder function ====
  preorder <- function(node, out = data.frame(), n = 1,
                       left = "left", right = "right",
                       condition = "All cases",
                       rule = "All cases",
                       verbose = FALSE) {
    if (is.null(node)) return(out)
    name <- if (node$terminal) "<leaf>" else varnames[node$fIdx]
    row <- data.frame(n,
                      Condition = condition,
                      Path = rule,
                      SplitVar = name,
                      N = sum(node$membership),
                      Value = node$value,
                      EstimateInt = if (node$value == 0) c(1, -1)[which.max(rt$mod$yfreq)] else sign(node$value),
                      Depth = node$depth, stringsAsFactors = FALSE)
    out <- rbind(out, row)
    if (verbose) print(out)
    conditionLeft <- if (length(node$cutPoint) > 0) {
      paste(name, "<", node$cutPoint)
    } else {
      paste0(name, " = ", '{"', paste(node$cutCategory, collapse = '", "'), '"}')
    }
    ruleLeft <- paste0(c(rule, conditionLeft), collapse = "/")
    out <- preorder(node[[left]], out, n*2, left, right,
                    condition = conditionLeft,
                    rule = ruleLeft,
                    verbose)

    conditionRight <- if (length(node$cutPoint) > 0) {
      paste(name, ">=", node$cutPoint)
    } else {
      compLevels <- setdiff(levels(x[[name]]), node$cutCategory)
      paste0(name, " = ", '{"', paste(compLevels, collapse = '", "'), '"}')
    }
    ruleRight <- paste0(c(rule, conditionRight), collapse = "/")
    out <- preorder(node[[right]], out, n*2 + 1, left, right,
                    condition = conditionRight,
                    rule = ruleRight,
                    verbose)
  } # recursive fn

  # PREORDER ====
  frame <- preorder(tree)
  frame$Estimate <- factor(frame$EstimateInt, levels = c(1, -1))
  levels(frame$Estimate) <- levels(rt$y.train)
  return(frame)

} # rtemis::preorderTree.addtree


addtree_path_to_rules <- function(x) {

  x[1] <- TRUE
  x <- gsub("All cases/", "", x)
  x <- gsub(" = ", " %in% ", x)
  x <- gsub("/", " & ", x) # && does not always work with data.table as of 12.12.2017
  x <- gsub("\\{", "c(", x)
  x <- gsub("\\}", ")", x)

} # rtemis::addtree_path_to_rules


#' Print method for \code{addtree} object created using \link{s.ADDTREE}
#'
#' @param x \code{rtMod} object created using \link{s.ADDTREE}
#' @author E.D. Gennatas
#' @export

print.addtree <- function(x, ...) {

  if (!inherits(x, "addtree")) stop("Please supply addtree model")

  frame <- x$frame
  frame$prefix <- sapply(frame$Depth, function(d) paste(rep("  ", d), collapse = ""))
  n.nodes <- NROW(frame)
  n.leaves <- sum(frame$SplitVar == "<leaf>")
  depth <- max(frame$Depth)
  boxcat(paste("AddTree with", n.nodes, "nodes total,", n.leaves, "leaves, and max depth of", depth))
  cat("Index [Condition] N| Value| Estimate| Depth (* leaf node)\n\n")
  for (i in 1:nrow(frame)) {
    cat(paste(c(sprintf("%3s", frame$n[i]), " ", frame$prefix[i], "[", frame$Condition[i], "] ",
                frame$N[i], "| ",
                ddSci(frame$Value[i]), "| ",
                frame$Estimate[i], "| ",
                frame$Depth[i])),
        ifelse(frame$SplitVar[i] == "<leaf>", " *", ""), "\n", sep = "")
  }

} # rtemis::print.addtree
