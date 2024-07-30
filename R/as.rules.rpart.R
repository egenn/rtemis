# as.rules.rpart
# ::rtemis::
# 2019 E.D. Gennatas rtemis.org

#' Convert `rpart` object to rules
#'
#' Convert an `rpart` object to a set of rules
#'
#' @param object `rpart` object
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @return `data.tree` object
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

as.rules.rpart <- function(object, verbose = FALSE) {
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

  # To rules ----
  leaf.index <- which(object$frame$var == "<leaf>")
  frame <- object$frame[leaf.index, ]
  node.id <- rownames(frame)
  rules <- rpart::path.rpart(object, node.id, print.it = verbose)
  rules <- plyr::ldply(rules, function(s) paste(s, collapse = "/"), .id = NULL)
  names(rules) <- "Condition"
  rules$Condition <- gsub("root/", "", rules$Condition)
  rules$Condition <- gsub(">=", " >= ", rules$Condition)
  rules$Condition <- gsub("< ", " < ", rules$Condition)
  # rules$Condition <- gsub("=", " = ", rules$Condition)
  rules$Condition <- gsub("/", " & ", rules$Condition)
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
    for (i in seq_len(nclasses)) {
      rules[[paste0("ProbClass", i)]] <- frame$yval2[, -(1:(nclasses + 1))][, i]
    }
  }
  rules$node.id <- node.id
  rules
} # rtemis::as.rules.rpart
