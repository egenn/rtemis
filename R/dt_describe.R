# dt_describe.R
# ::rtemis::
# 2022 EDG rtemis.org

#' Describe data.table
#'
#' @param x data.table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' origin <- as.POSIXct("2022-01-01 00:00:00", tz = "America/Los_Angeles")
#' x <- data.table(
#'   ID = paste0("ID", 1:10),
#'   V1 = rnorm(10),
#'   V2 = rnorm(10, 20, 3),
#'   V1_datetime = as.POSIXct(
#'     seq(
#'       1, 1e7,
#'       length.out = 10
#'     ),
#'     origin = origin
#'   ),
#'   V2_datetime = as.POSIXct(
#'     seq(
#'       1, 1e7,
#'       length.out = 10
#'     ),
#'     origin = origin
#'   ),
#'   C1 = sample(c("alpha", "beta", "gamma"), 10, TRUE),
#'   F1 = factor(sample(c("delta", "epsilon", "zeta"), 10, TRUE))
#' )
#' }
dt_describe <- function(x) {
  if (!is.data.table(x)) setDT(x)
  nrows <- NROW(x)

  # appease R CMD check: do not use ..var in DT frame, use with = FALSE instead

  # Numeric ----
  index_nm <- which(sapply(x, is.numeric))

  nm_summary <- if (length(index_nm) > 0) {
    data.table(
      Variable = x[, index_nm, with = FALSE] |> names(),
      Min = sapply(x[, index_nm, with = FALSE], min, na.rm = TRUE),
      Max = sapply(x[, index_nm, with = FALSE], max, na.rm = TRUE),
      Median = sapply(x[, index_nm], with = FALSE, median, na.rm = TRUE),
      Mean = sapply(x[, index_nm, with = FALSE], mean, na.rm = TRUE),
      SD = sapply(x[, index_nm, with = FALSE], sd, na.rm = TRUE),
      Pct_missing = sapply(
        x[, index_nm, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.table(
      Variable = character(),
      Min = numeric(),
      Max = numeric(),
      Median = numeric(),
      Mean = numeric(),
      SD = numeric(),
      Pct_missing = numeric()
    )
  }

  # Characters & factors ----
  index_cf <- c(which(sapply(x, is.character)), which(sapply(x, is.factor)))

  cf_summary <- if (length(index_cf) > 0) {
    data.table(
      Variable = x[, index_cf, with = FALSE] |> names(),
      N_unique = sapply(
        x[, index_cf, with = FALSE],
        \(col) length(unique(col))
      ),
      Mode = sapply(x[, index_cf, with = FALSE], get_mode),
      Counts = sapply(x[, index_cf, with = FALSE], fct_describe),
      Pct_missing = sapply(
        x[, index_cf, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.table(
      Variable = numeric(),
      N_unique = integer(),
      Mode = character(),
      Counts = character(),
      Pct_missing = numeric()
    )
  }

  # Dates ----
  index_dt <- which(sapply(
    x,
    \(col) any(class(col) %in% c("Date", "IDate", "POSIXct", "POSIXt"))
  ))

  dt_summary <- if (length(index_dt) > 0) {
    data.table(
      Variable = x[, index_dt, with = FALSE] |> names(),
      Min = do.call(c, lapply(x[, index_dt, with = FALSE], min, na.rm = TRUE)),
      Max = do.call(c, lapply(x[, index_dt, with = FALSE], max, na.rm = TRUE)),
      Median = do.call(
        c,
        lapply(x[, index_dt, with = FALSE], median, na.rm = TRUE)
      ),
      Mean = do.call(
        c,
        lapply(x[, index_dt, with = FALSE], mean, na.rm = TRUE)
      ),
      Pct_missing = sapply(
        x[, index_dt, with = FALSE],
        \(col) sum(is.na(col)) / nrows
      )
    )
  } else {
    data.table(
      Variable = character(),
      Min = numeric(),
      Max = numeric(),
      Median = numeric(),
      Mean = numeric(),
      Pct_missing = numeric()
    )
  }

  invisible(list(
    Numeric = nm_summary,
    Categorical = cf_summary,
    Date = dt_summary
  ))
} # rtemis::dt_describe

#' Decribe factor
#'
#' Outputs a single character with names and counts of each level of the input factor
#'
#' @param x factor
#' @param max_n Integer: Return counts for up to this many levels
#' @param return_ordered Logical: If TRUE, return levels ordered by count, otherwise
#' return in level order
#'
#' @return Character with level counts
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' # Small number of levels
#' fct_describe(iris$Species)
#'
#' # Large number of levels: show top n by count
#' x <- factor(sample(letters, 1000, TRUE))
#' fct_describe(x)
#' fct_describe(x, 3)
#' }
# fct_describe <- function(x) {
#     x_levels <- levels(x)
#     n_unique <- length(x_levels)
#     x_freqs <- as.integer(table(x))
#     paste(x_levels, x_freqs, sep = ": ", collapse = "; ")
# }
#'
fct_describe <- function(x, max_n = 5, return_ordered = TRUE) {
  x <- factor(x)
  x_levels <- levels(x)
  n_unique <- length(x_levels)
  x_freqs <- as.integer(table(x))
  if (return_ordered) {
    idi <- order(x_freqs, decreasing = TRUE)
  }

  if (n_unique <= max_n) {
    if (return_ordered) {
      paste(x_levels[idi], x_freqs[idi], sep = ": ", collapse = "; ")
    } else {
      paste(x_levels, x_freqs, sep = ": ", collapse = "; ")
    }
  } else {
    idi <- order(x_freqs, decreasing = TRUE)
    if (return_ordered) {
      idi <- idi[seq_len(max_n)]
      paste0(
        "(Top ",
        max_n,
        " of ",
        n_unique,
        ") ",
        paste(x_levels[idi], x_freqs[idi], sep = ": ", collapse = "; ")
      )
    } else {
      paste0(
        "(First ",
        max_n,
        " of ",
        n_unique,
        ") ",
        paste(x_levels, x_freqs, sep = ": ", collapse = "; ")
      )
    }
  }
} # rtemis::fct_describe
