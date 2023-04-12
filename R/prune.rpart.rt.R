#' `prune.rpart` experimental replacement
#' @author E.D. Gennatas
#' @export

prune.rpart.rt <- function(tree, cp, ...) {
  
  importance <- getFromNamespace("importance", "rpart")
  
  ff <- tree$frame
  id <- as.integer(row.names(ff))
  toss <- id[ff$complexity <= cp & ff$var != "<leaf>"]
  if (length(toss) == 0L) 
    return(tree)
  newx <- rpart::snip.rpart(tree, toss)
  temp <- pmax(tree$cptable[, 1L], cp)
  keep <- match(unique(temp), temp)
  newx$cptable <- tree$cptable[keep, , drop = FALSE]
  row.id <- which(rownames(newx$cptable) == max(keep)) # delta
  newx$cptable[row.id, 1L] <- cp
  newx$variable.importance <- importance(newx)
  newx
}
