# as.data.tree.rpart
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Convert `rpart` rules to `data.tree` object
#'
#' Convert an `rpart` object to a `data.tree` object, which can be plotted with
#' [dplot3_cart]
#' @param object `rpart` object
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @return `data.tree` object
#' @author E.D. Gennatas
#' @export

as.data.tree.rpart <- function(object, verbose = FALSE) {
  # Dependencies ----
  dependency_check("rpart", "data.tree")

  # Object ----
  if (inherits(object, "rpart")) {
    # frame <- object$frame
    if (verbose) msg2("Working on rpart object")
  } else if (inherits(object, "rtMod") && inherits(object$mod, "rpart")) {
    object <- object$mod
    if (verbose) msg2("Working on rtemis CART object")
  } else {
    stop("Input must be rpart object or rtemis CART model")
  }

  # To data.tree ----
  frame <- object$frame
  node.id <- rownames(frame)
  rules <- rpart::path.rpart(object, node.id, print.it = verbose)
  rules <- plyr::ldply(rules, function(s) paste(s, collapse = "/"), .id = NULL)
  names(rules) <- "Condition"
  rules$Condition <- gsub("root", "All cases", rules$Condition)
  # rules$Condition <- gsub(">=", " >= ", rules$Condition)
  rules$Condition <- gsub("< ", "<", rules$Condition)
  # rules$Condition <- gsub("=", " = ", rules$Condition)
  rules$Var <- frame$var
  rules$N <- frame$n
  rules$Weight <- frame$wt
  rules$Deviance <- frame$dev
  rules$Estimate <- frame$yval
  rules$EstimateLabel <- levels(object$model$y)[frame$yval]
  rules$Complexity <- frame$complexity
  rules$Ncompete <- frame$ncompete
  rules$Nsurrogate <- frame$nsurrogate
  rules$Depth <- floor(log(as.numeric(node.id), base = 2))
  if (object$method == "class") {
    nclasses <- (ncol(frame$yval2) - 2) / 2
    for (i in 1:nclasses) {
      rules[[paste0("ProbClass", i)]] <- frame$yval2[, -(1:(nclasses + 1))][, i]
    }
  }
  rules$node.id <- node.id
  tree <- data.tree::as.Node(rules, pathName = "Condition")

  tree
} # rtemis::as.data.tree.rpart
