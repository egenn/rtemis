# matchcases.R
# ::rtemis::
# 2021 E D Gennatas lambdamd.org

#' Match cases by covariates
#'
#' Match cases by exact match and by distance between covariates
#'
#' @param target data.frame you are matching to
#' @param pool data.frame where you are looking for matches from
#' @param target.id String: Column name in \code{target} that holds unique cases IDs. Default = NULL,
#' in which case integer case numbers will be used
#' @param pool.id String: Same as \code{target.id} for \code{pool}
#' @param exactmatch.factors Logical: If TRUE, selected cases will have to exactly match factors
#' available in \code{target}
#' @param exactmatch.cols String: Names of columns of variables that should be matched exactly
#' @author E.D. Gennatas
#' @export
#' @examples
#' target <- data.frame(Sex = factor(c(1, 1, 0, 0)),
#'                      Handedness = factor(c(1, 1, 0, 1)),
#'                      Age = c(21, 27, 39, 24),
#'                      Var = c(.7, .8, .9, .6))
#'
#' set.seed(2021)
#' pool <- data.frame(Sex = factor(sample(c(0, 1), 50, TRUE)),
#'                    Handedness = factor(sample(c(0, 1), 50, TRUE, c(.1, .9))),
#'                    Age = sample(16:42, 50, TRUE),
#'                    Var = rnorm(50))
#' mc <- matchcases(target, pool, 2)

matchcases <- function(target, pool,
                       n.matches = 1,
                       target.id = NULL,
                       pool.id = NULL,
                       exactmatch.factors = TRUE,
                       exactmatch.cols = NULL,
                       verbose = TRUE) {

  # For each target, select matches on categoricals,
  # then order pool by distance.
  if (is.null(exactmatch.cols) & exactmatch.factors) {
    exactmatch.cols <- colnames(target)[sapply(target, is.factor)]
  }

  distmatch.cols <- colnames(target)[!colnames(target) %in% exactmatch.cols]

  # Convert all non-exact-matching to numeric
  # index_num <- which(sapply(target, is.numeric))
  tonumeric <- distmatch.cols[!sapply(target[, distmatch.cols], is.numeric)]
  if (length(tonumeric) > 0) {
    target[, tonumeric] <- lapply(target[, tonumeric], as.numeric)
  }

  # Normalize all
  ntarget <- nrow(target)
  npool <- nrow(pool)
  vcat_scaled <- scale(rbind(target[, distmatch.cols], pool[, distmatch.cols]))
  targetID <- if (!is.null(target.id)) target[, target.id] else seq(ntarget)
  poolID <- if (!is.null(pool.id)) pool[, pool.id] else seq(npool)
  target_s <- cbind(targetID = targetID, vcat_scaled[seq(ntarget), ])
  pool_s <- cbind(poolID = poolID, vcat_scaled[-seq(ntarget), ])
  rm(vcat_scaled)
  # can use split on vcat_scaled definition to create a list of the two instead

  mc <- data.frame(targetID = targetID, match = matrix(NA, ntarget, n.matches))
  for (i in seq(ntarget)) {
    if (verbose) msg("Working on case", i, "of", ntarget)
    if (!is.null(exactmatch.cols)) {
      ind <- sapply(seq(nrow(pool)), function(j)
        all(target[i, exactmatch.cols] == pool[j, exactmatch.cols]))
      subpool <- pool_s[ind, , drop = FALSE]
    } else {
      subpool <- pool_s
    }
    if (nrow(subpool) > n.matches) {
      distord <- order(sapply(seq(nrow(subpool)), function(j) sum((target_s[i, -1] - subpool[j, -1])^2)))
      # Get index of n.matches best matches
      mc[i, 2:(n.matches + 1)] <- subpool[, 1][distord[seq(n.matches)]]
    } else {
      # This may be < n.matches including 0
      mc[i, 2:(nrow(subpool) + 1)] <- unname(subpool[, 1])
    }
  }

  mc

} # rtemis::matchcases
