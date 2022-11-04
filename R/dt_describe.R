# dt_describe.R
# ::rtemis::
# 2022 EDG lambdamd.org

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
#'     ID = paste0("ID", 1:10),
#'     V1 = rnorm(10),
#'     V2 = rnorm(10, 20, 3),
#'     V1_datetime = as.POSIXct(
#'         seq(
#'             1, 1e7,
#'             length.out = 10
#'         ),
#'         origin = origin
#'     ),
#'     V2_datetime = as.POSIXct(
#'         seq(
#'             1, 1e7,
#'             length.out = 10
#'         ),
#'         origin = origin
#'     ),
#'     C1 = sample(c("alpha", "beta", "gamma"), 10, TRUE),
#'     F1 = factor(sample(c("delta", "epsilon", "zeta"), 10, TRUE))
#' )
#' }

dt_describe <- function(x) {

    if (!is.data.table(x)) setDT(x)
    nrows <- NROW(x)

    # Numeric ----
    index_nm <- which(sapply(x, is.numeric))

    nm_summary <- if (length(index_nm) > 0) {
        data.table(
            Variable = x[, ..index_nm] |> names(),
            Min = sapply(x[, ..index_nm], min, na.rm = TRUE),
            Max = sapply(x[, ..index_nm], max, na.rm = TRUE),
            Median = sapply(x[, ..index_nm], median, na.rm = TRUE),
            Mean = sapply(x[, ..index_nm], mean, na.rm = TRUE),
            SD = sapply(x[, ..index_nm], sd, na.rm = TRUE),
            Pct_missing = sapply(x[, ..index_nm], \(col) sum(is.na(col)) / nrows)
        )
    } else {
        NULL
    }
    
    # Characters & factors ----
    index_cf <- c(which(sapply(x, is.character)), which(sapply(x, is.factor)))

    cf_summary <- if (length(index_cf) > 0) {
        data.table(
            Variable = x[, ..index_cf] |> names(),
            N_unique = sapply(x[, ..index_cf], \(col) length(unique(col))),
            Mode = sapply(x[, ..index_cf], getMode),
            Pct_missing = sapply(x[, ..index_cf], \(col) sum(is.na(col)) / nrows)
        )
    }

    # Dates ----
    index_dt <- which(sapply(
        x,
        \(col) any(class(col) %in% c("Date", "POSIXct", "POSIXt"))
    ))

    dt_summary <- if (length(index_dt) > 0) {
        data.table(
            Variable = x[, ..index_dt] |> names(),
            Min = do.call(c, lapply(x[, ..index_dt], min, na.rm = TRUE)),
            Max = do.call(c, lapply(x[, ..index_dt], max, na.rm = TRUE)),
            Median = do.call(c, lapply(x[, ..index_dt], median, na.rm = TRUE)),
            Mean = do.call(c, lapply(x[, ..index_dt], mean, na.rm = TRUE)),
            Pct_missing = sapply(x[, ..index_dt], \(col) sum(is.na(col)) / nrows)
        )
    }

    invisible(list(Numeric = nm_summary, Categorical = cf_summary, Date = dt_summary))

} # rtemis::dt_describe
