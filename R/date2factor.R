# timebin.R
# ::rtemis::
# E.D. Gennatas lambdamd.org

#' Date to factor time bin
#'
#' Convert Date to time bin factor.
#'
#' Order of levels will be chronological (important e.g. for plotting)
#' Additionally, can output ordered factor with \code{ordered = TRUE}
#'
#' @param x Date vector
#' @param time_bin Character: "year", "quarter", "month", or "day"
#' @param make_bins Character: "range" or "preseent". If "range" the factor levels will include all
#' time periods define by \code{time_bin} within \code{bin_range}. This means factor levels can be
#' empty. Otherwise, if "present", factor levels only include time periods present in data.
#' @param bin_range Date, vector, length 2: Range of dates to make levels for. Defaults to range of
#' input dates \code{x}
#' @param ordered Logical: If TRUE, factor output is ordered. Default = FALSE
#'
#' @returns factor of time periods
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' startDate <- as.Date("2018-01-01")
#' endDate <- as.Date("2020-12-31")
#' time <- sample(seq(startDate, endDate, length.out = 100))
#' date2factor(time)
#' date2factor(time, "quarter")
#' date2factor(time, "month")
#' date2factor(time, "day")
#' # range vs present
#' x <- sample(seq(as.Date("2018-01-01"), as.Date("2021-01-01"), by = 1), 10)
#' date2factor(x, time_bin = "quarter", make_bins = "present")
#' date2factor(x, time_bin = "quarter", make_bins = "range")
#' }

# date2factor <- function(x,
#                         time_bin = c("year", "quarter", "month", "day"),
#                         ordered = FALSE) {
#
#   time_bin <- match.arg(time_bin)
#
#   if (time_bin == "year") {
#     factor(data.table::year(x), ordered = ordered)
#   } else if (time_bin == "quarter") {
#     factor(paste0(data.table::year(x), " Q", data.table::quarter(x)),
#            ordered = ordered)
#   } else if (time_bin == "month") {
#     ym <- paste(substr(months(x), 1, 3), data.table::year(x))
#     .levels <- unique(ym[order(x)])
#     factor(ym, levels = .levels, ordered = ordered)
#   } else if (time_bin == "day") {
#     factor(x, levels = as.character(unique(x[order(x)])), ordered = ordered)
#   }
#
# } # rtemis::date2factor

date2factor <- function(x,
                        time_bin = c("year", "quarter", "month", "day"),
                        make_bins = c("range", "present"),
                        bin_range = range(x),
                        ordered = FALSE) {

  time_bin <- match.arg(time_bin)
  make_bins <- match.arg(make_bins)

  if (time_bin == "year") {
    if (make_bins == "present") {
      factor(data.table::year(x), ordered = ordered)
    } else {
      out <- as.character(data.table::year(x))
      factor(out,
             levels = as.character(seq(data.table::year(bin_range[1]), data.table::year(bin_range[2]))),
             ordered = ordered)
    }
  } else if (time_bin == "quarter") {
    if (make_bins == "present") {
      factor(paste0(data.table::year(x), " Q", data.table::quarter(x)),
             ordered = ordered)
    } else {
      factor(paste0(data.table::year(x), " Q", data.table::quarter(x)),
             levels = levels(date2yq(seq(bin_range[1], bin_range[2], by = "quarter"))),
             ordered = ordered)
    }
  } else if (time_bin == "month") {
    ym <- paste(substr(months(x), 1, 3), data.table::year(x))
    if (make_bins == "present") {
      .levels <- unique(ym[order(x)])
      factor(ym, levels = .levels, ordered = ordered)
    } else {
      factor(ym,
             levels = levels(date2ym(seq(bin_range[1], bin_range[2], by = "month"))),
             ordered = ordered)
    }
  } else if (time_bin == "day") {
    if (make_bins == "present") {
      factor(x, levels = as.character(unique(x[order(x)])), ordered = ordered)
    } else {
      factor(x,
             levels = as.character(seq(bin_range[1], bin_range[2], by = 1)),
             ordered = ordered)
    }
  }

} # rtemis::date2factor


#' Date to year-quarter factor
#'
#' @param x Date vector
#' @param ordered Logical: If TRUE, return ordered factor. Default = FALSE
#'
#' @author E.D. Gennatas
#' @export

date2yq <- function(x, ordered = FALSE) {
  factor(paste0(data.table::year(x), " Q", data.table::quarter(x)),
         ordered = ordered)
}


#' Date to year-month factor
#'
#' @param x Date vector
#' @param ordered Logical: If TRUE, return ordered factor. Default = FALSE
#'
#' @author E.D. Gennatas
#' @export

date2ym <- function(x, ordered = FALSE) {
  ym <- paste(substr(months(x), 1, 3), data.table::year(x))
  .levels <- unique(ym[order(x)])
  factor(ym, levels = .levels, ordered = ordered)
}
