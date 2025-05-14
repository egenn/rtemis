# date_ops.R
# ::rtemis::
# 2024 EDG rtemis.org

#' Extract features from dates
#'
#' @details weekday and month will be extracted as factors, year as integer
#'
#' @param dates Date vector
#' @param features Character vector: features to extract
#' @param drop.dates Logical: If TRUE, drop original date column
#'
#' @author EDG
#' @export
#'
#' @return data.table with extracted features
dates2features <- function(
  dates,
  features = c("weekday", "month", "year"),
  drop.dates = TRUE
) {
  # to factors: dow, month
  dt <- data.table(dates = dates)
  if ("weekday" %in% features) {
    dt[, weekday := factor(weekdays(dates))]
  }
  if ("month" %in% features) {
    dt[, month := factor(months(dates))]
  }
  if ("year" %in% features) {
    dt[, year := year(dates)]
  }
  if (drop.dates) {
    dt[, dates := NULL]
  }
  dt
} # rtemis::dates2features


#' Get holidays from date vector
#'
#' @param dates Date vector
#' @param holidays Character vector: holidays to extract
#'
#' @return Factor of length `length(dates)` with levels "Not Holiday", "Holiday"
get_holidays <- function(
  dates,
  holidays = c("LaborDay", "NewYearsDay", "ChristmasDay")
) {
  # Get years from dates
  years <- unique(data.table::year(dates))
  # Get all holidays in all years
  .holidays <- do.call(
    "c",
    lapply(years, function(year) {
      do.call(
        "c",
        lapply(holidays, function(holiday) {
          timeDate::as.Date.timeDate(timeDate::holiday(
            year = year,
            Holiday = holiday
          ))
        })
      )
    })
  )
  # Return intersection of dates and holidays
  holidays_fct <- factor(
    rep(0, length(dates)),
    levels = c(0, 1),
    labels = c("Not Holiday", "Holiday")
  )
  holidays_fct[dates %in% .holidays] <- "Holiday"
  holidays_fct
} # rtemis::get_holidays
