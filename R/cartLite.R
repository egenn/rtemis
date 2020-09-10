# cartLite.R
# ::rtemis::
# Efstathios D. Gennatas MBBS PhD egenn.github.io

#' Bare bones decision tree derived from \code{rpart}
#'
#' A super-stripped down decision tree for when space and performance are critical
#'
#' @inheritParams s.CART
#' @param save.fitted Logical: If TRUE, save fitted values in output. Default = FALSE
#' @author Efstathios D Gennatas
#' @keywords internal
#' @export

cartLite <- function(x, y,
                     weights = NULL,
                     minsplit = 2,
                     minbucket = 1,
                     cp = 0,
                     maxcompete = 0,
                     usesurrogate = 2,
                     surrogatestyle = 0,
                     maxdepth = 3,
                     xval = 0,
                     save.fitted = FALSE,
                     trace = 0, ...) {

  # Arguments ====
  # '- rpart.control ====
  control <- list(minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp,
                  maxcompete = maxcompete,
                  usesurrogate = usesurrogate,
                  surrogatestyle = surrogatestyle,
                  maxdepth = maxdepth,
                  xval = xval)

  dat <- data.frame(x, y)
  .formula <- formula(y ~ .)
  args <- c(list(formula = .formula,
                 data = dat,
                 weights = weights,
                 control = control),
            list(...))

  obj <- do.call(rpart::rpart, args)

  frame <- obj$frame
  leaves.index <- which(frame$var == "<leaf>")
  leaves.frame <- frame[leaves.index, ]
  node.id <- rownames(leaves.frame)
  rules <- rpart::path.rpart(obj, node.id, print.it = trace > 0)
  rules <- plyr::ldply(rules, function(s) paste(s, collapse = " & "), .id = NULL)
  names(rules) <- "Condition"
  rules <- list(Condition = gsub("root", "TRUE", rules$Condition),
                Estimate = frame$yval[leaves.index])
  if (save.fitted) rules$fitted <- obj$frame$yval[obj$where]

  class(rules) <- "cartLite"
  rules

} # rtemis::cartLite


#' Predict method for \code{cartLite} object
#'
#' @param object \link{cartLite} object
#' @param newdata Data frame of predictors
#' @param verbose Logical: If TRUE, print messages to console. Default = FALSE
#' @method predict cartLite
#' @author Efstathios D. Gennatas
#' @export

predict.cartLite <- function(object, newdata, verbose = FALSE, ...) {

  cxr <- matchCasesByRules(x = newdata, rules = object$Condition, verbose = verbose)
  cxr.index <- apply(cxr, 1, function(i) which(i == 1))
  object$Estimate[cxr.index]

} # rtemis::predict.cartLite
