# matchcases.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Match cases by covariates
#'
#' Find one or more cases from a `pool` DataFrame that match cases in a target DataFrame.
#' Match exactly and/or by distance (sum of squared distance).
#'
#' @param target data.frame you are matching against
#' @param pool data.frame you are looking for matches from
#' @param target.id Character: Column name in \code{target} that holds unique cases IDs. Default = NULL,
#' in which case integer case numbers will be used
#' @param pool.id Character: Same as \code{target.id} for \code{pool}
#' @param exactmatch.factors Logical: If TRUE, selected cases will have to exactly match factors
#' available in \code{target}
#' @param exactmatch.cols Character: Names of columns that should be matched exactly
#' @param distmatch.cols Character: Names of columns that should be distance-matched
#' @param norepeats Logical: If TRUE, cases in \code{pool} can only be chosen once. Default = TRUE
#' @param ignore.na Logical: If TRUE, ignore NA values during exact matching. Default = FALSE.
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @author E.D. Gennatas
#' @export
#' @examples
#' set.seed(2021)
#' cases <- data.frame(PID = paste0("PID", seq(4)),
#'                     Sex = factor(c(1, 1, 0, 0)),
#'                     Handedness = factor(c(1, 1, 0, 1)),
#'                     Age = c(21, 27, 39, 24),
#'                     Var = c(.7, .8, .9, .6),
#'                     Varx = rnorm(4))
#' controls <- data.frame(CID = paste0("CID", seq(50)),
#'                        Sex = factor(sample(c(0, 1), 50, TRUE)),
#'                        Handedness = factor(sample(c(0, 1), 50, TRUE, c(.1, .9))),
#'                        Age = sample(16:42, 50, TRUE),
#'                        Var = rnorm(50),
#'                        Vary = rnorm(50))
#'
#' mc <- matchcases(cases, controls, 2, "PID", "CID")

matchcases <- function(target, pool,
                       n.matches = 1,
                       target.id = NULL,
                       pool.id = NULL,
                       exactmatch.factors = TRUE,
                       exactmatch.cols = NULL,
                       distmatch.cols = NULL,
                       norepeats = TRUE,
                       ignore.na = FALSE,
                       verbose = TRUE) {

  ntarget <- nrow(target)
  npool <- nrow(pool)

  # Get IDs ====
  if (is.null(target.id)) {
    targetID <- seq(ntarget)
  } else {
    targetID <- target[, target.id]
    target[, target.id] <- NULL
  }
  if (is.null(pool.id)) {
    poolID <- seq(npool)
  } else {
    poolID <- pool[, pool.id]
    pool[, pool.id] <- NULL
  }

  # exact- & dist-matched column names
  if (is.null(exactmatch.cols) & exactmatch.factors) {
    exactmatch.cols <- colnames(target)[sapply(target, is.factor)]
  }
  # Keep exactmatch.cols present in pool
  exactmatch.cols <- exactmatch.cols[exactmatch.cols %in% colnames(pool)]

  if (is.null(distmatch.cols)) {
    distmatch.cols <- colnames(target)[!colnames(target) %in% exactmatch.cols]
  }
  # Keep distmatch.cols present in pool
  distmatch.cols <- distmatch.cols[distmatch.cols %in% colnames(pool)]

  # Remove unused columns, if any
  .remove <- colnames(target)[!colnames(target) %in% c(exactmatch.cols, distmatch.cols)]
  target[, .remove] <- NULL
  .remove <- colnames(pool)[!colnames(pool) %in% c(exactmatch.cols, distmatch.cols)]
  pool[, .remove] <- NULL

  # Convert all non-exact-matching to numeric
  # index_num <- which(sapply(target, is.numeric))
  tonumeric <- distmatch.cols[!sapply(target[, distmatch.cols], is.numeric)]
  if (length(tonumeric) > 0) {
    target[, tonumeric] <- lapply(target[, tonumeric, drop = FALSE], as.numeric)
  }
  tonumeric <- distmatch.cols[!sapply(pool[, distmatch.cols], is.numeric)]
  if (length(tonumeric) > 0) {
    pool[, tonumeric] <- lapply(pool[, tonumeric, drop = FALSE], as.numeric)
  }

  # Normalize all
  vcat <- rbind(target, pool)
  vcat[, distmatch.cols] <- lapply(vcat[, distmatch.cols, drop = FALSE], scale)
  target_s <- cbind(targetID = targetID, vcat[seq(ntarget), ])
  pool_s <- cbind(poolID = poolID, vcat[-seq(ntarget), ])
  rm(vcat)

  # For each target, select matches on categoricals,
  # then order pool by distance.
  mc <- data.frame(targetID = targetID, match = matrix(NA, ntarget, n.matches))
  for (i in seq(ntarget)) {
    if (verbose) msg("Working on case", i, "of", ntarget)
    if (is.null(exactmatch.cols)) {
      subpool <- pool_s
    } else {
      ind <- sapply(seq(nrow(pool_s)), function(j)
        all(target_s[i, exactmatch.cols] == pool_s[j, exactmatch.cols], na.rm = ignore.na))
      subpool <- pool_s[ind, , drop = FALSE]
    }
    # distord <- order(sapply(seq(nrow(subpool)),
    #                           function(j) sum((target_s[i, distmatch.cols] - subpool[j, distmatch.cols])^2)))
    distord <- order(sapply(seq(nrow(subpool)),
                            function(j) mse(unlist(target_s[i, distmatch.cols]),
                                            unlist(subpool[j, distmatch.cols]),
                                            na.rm = ignore.na)))
    n_matched <- min(n.matches, nrow(subpool))
    mc[i, 2:(n_matched + 1)] <- subpool[, 1][distord[seq(n_matched)]]
    if (norepeats) pool_s <- pool_s[!pool_s[, 1] %in% mc[i, 2:(n.matches + 1)], ]
  }

  mc

} # rtemis::matchcases
