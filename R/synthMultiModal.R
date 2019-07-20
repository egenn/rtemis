# synthMultiModal.R
# ::rtemis::
# 2019 Efstathios D. Gennatas

#' Create "Multimodal" Synthetic Data
#'
#' Create "Multimodal" Synthetic Data using squares and arctangents
#'
#' There are no checks yet for compatibility among inputs and certain combinations may not work.
#'
#' @param n.cases Integer: Number of cases to create. Default = 10000
#' @param init.fn String: "runifmat" or "rnormmat". Use the respective functions to generate features as random uniform
#' and random normal variables, respectively.
#' Default = "runifmat"
#' @param init.fn.params Named list with arguments "min", "max" for "runifmat" and "mean", "sd" for "rnormmat".
#' Default = \code{list(min = -10, max = 10)}
#' @param n.groups Integer: Number of feature groups / modalities to create. Default = 4
#' @param n.feat.per.group Integer, vector, length \code{n.groups}: Number of features per group to create.
#' Default = \code{c(50, 100, 200, 300)}
#' @param contrib.p Float (0, 1]: Ratio of features contributing to outcome per group. Default = .33, i.e.
#' a third of the features in each group will be used to produce the outcome y
#' @param linear.p Float [0, 1]: Ratio of contributing features to be included linearly. Default = .1, i.e. .1 of .33 of
#' features in each group will be included
#' @param square.p Float [0, 1]: Ratio of contributing features to be squared. Default = .1, i.e. .1 of .33 of features
#' in each group will be squared
#' @param atan.p Float [0, 1]: Ratio of contributing features whose \code{atan} will be used. These will be selected
#' from the features that were NOT sampled for squaring. Default = .1,
#' i.e. .1 of .33 of features in each group will be transformed using \code{atan}, but given these features were not
#' already picked to be squared (see \code{square.p})
#' @param pair.multiply.p Float [0, 1] Ratio of features will be divided into pairs and multiplied. Default = .05
#' @param pair.square.p Float [0, 1] Ratio of features which will be divided into pairs, multiplied and squared.
#' Default = .05
#' @param pair.atan.p Float [0, 1] Ratio of features which will be divided into pairs, multiplied and transformed using
#' \code{atan}. Default = .05
#' @param trace Integer: If > 0, print progress messages to console. Default = 0
#' @param seed Integer: If set, pass to \code{set.seed} for reproducibility
#' @author Efstathios D Gennatas
#' @export
#' @return List with elements x, y, index.square, index.atan, index.pair.square

synthMultiModal <- function(n.cases = 10000,
                            init.fn = "runifmat",
                            init.fn.params = list(min = -10, max = 10),
                            n.groups = 4,
                            n.feat.per.group = round(seq(10, 300, length.out = n.groups)),
                            contrib.p = .33,
                            linear.p = .66,
                            square.p = .1,
                            atan.p = .1,
                            pair.multiply.p = .05,
                            pair.square.p = .05,
                            pair.atan.p = .05,
                            trace = 0,
                            seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # [ Synth features ] ====
  x <- lapply(seq(n.groups), function(i) do.call(init.fn,
                                                 c(list(nrow = n.cases,
                                                        ncol = n.feat.per.group[i],
                                                        return.df = FALSE),
                                                   init.fn.params)))
  names(x) <- paste0("Group_", seq(x))

  # [ Indexes ] ====
  # '- contrib ====
  # index.contrib: The variables from each group contributing to the group's outcome
  index.contrib <- lapply(seq(n.groups), function(i)
    sort(sample(seq(n.feat.per.group[i]), contrib.p * n.feat.per.group[i])))
  names(index.contrib) <- names(x)
  if (trace > 0) cat("  index.contrib Done\n")

  # '- linear ====
  #index.linear: The features within index.contrib that will be included linearly
  index.linear <- lapply(seq(n.groups), function(i)
    sort(sample(index.contrib[[i]], linear.p * length(index.contrib[[i]]))))
  names(index.linear) <- names(x)
  if (trace > 0) cat("  index.square Done\n")

  # '- square ====
  # index.square: The features within index.contrib that will be squared
  index.square <- lapply(seq(n.groups), function(i)
    sort(sample(index.contrib[[i]], square.p * length(index.contrib[[i]]))))
  names(index.square) <- names(x)
  if (trace > 0) cat("  index.square Done\n")

  # '- atan ====
  # index.atan: The features within index.contrib that will be arctanned
  index.atan <- lapply(seq(n.groups), function(i) {
    index.open <- index.contrib[[i]][!index.contrib[[i]] %in% index.square[[i]]]
    sort(sample(index.open, atan.p * length(index.contrib[[i]])))
  })
  names(index.atan) <- names(x)
  if (trace > 0) cat("  index.atan Done\n")

  # '- pair.multiply ====
  # index.pair.multiply
  index.pair.multiply <- lapply(seq(n.groups), function(i) {
    n.pairs <- 2 * round(pair.multiply.p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  names(index.pair.multiply) <- names(x)
  if (trace > 0) cat("  index.pair.multiply Done\n")

  # '- pair.square ====
  # index.pair.square
  index.pair.square <- lapply(seq(n.groups), function(i) {
    n.pairs <- 2 * round(pair.square.p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  names(index.pair.square) <- names(x)
  if (trace > 0) cat("  index.pair.square Done\n")

  # index.pair.atan
  index.pair.atan <- lapply(seq(n.groups), function(i) {
    n.pairs <- 2 * round(pair.atan.p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  if (trace > 0) cat("  index.pair.atan Done\n")

  # [ Outcome ] ====
  if (trace > 0) cat("  Adding linear terms...")
  y0 <- lapply(seq(n.groups), function(i)
    matrixStats::rowSums2(rnorm(1) * x[[i]][, index.linear[[i]], drop = FALSE])
  )
  if (trace > 0) cat(" Done\n")

  if (trace > 0) cat("  Adding squares and atans...")
  y1 <- lapply(seq(n.groups), function(i)
    matrixStats::rowSums2(x[[i]][, index.square[[i]], drop = FALSE]^2) +
      matrixStats::rowSums2(atan(x[[i]][, index.atan[[i]], drop = FALSE]))
  )
  names(y1) <- names(x)
  if (trace > 0) cat(" Done\n")

  if (trace > 0) cat("  Squaring pair products...")
  y2 <- vector("list", n.groups)
  names(y2) <- names(x)
  for (i in seq_len(n.groups)) {
    y2[[i]] <- if (!is.null(index.pair.square[[i]])) {
      matrixStats::rowSums2(sapply(seq_len(NROW(index.pair.square[[i]])), function(k)
        matrixStats::rowProds(x[[i]][, index.pair.square[[i]][k, ]])^2))
    } else {
      rep(0, n.cases)
    }
  }
  if (trace > 0) cat(" Done\n")

  if (trace > 0) cat("  Atan of pair products...")
  y3 <- vector("list", n.groups)
  names(y3) <- names(x)
  for (i in seq_len(n.groups)) {
    y3[[i]] <- if (!is.null(index.pair.atan[[i]])) {
      matrixStats::rowSums2(sapply(seq_len(NROW(index.pair.atan[[i]])), function(k)
        matrixStats::rowProds(x[[i]][, index.pair.atan[[i]][k, ]])^2))
    } else {
      rep(0, n.cases)
    }
  }
  if (trace > 0) cat(" Done\n")

  y <- lapply(seq_len(n.groups), function(i) y1[[i]] + y2[[i]] + y3[[i]])
  names(y) <- names(x)

  list(x = x,
       y = y,
       index.square = index.square,
       index.atan = index.atan,
       index.pair.square = index.pair.square,
       index.pair.atan = index.pair.atan)

} # rtemis::synthMultiModal
