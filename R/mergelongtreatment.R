# mergelongtreatment.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Merge panel data treatment and outcome data
#'
#' Merge long format treatment and outcome data from multiple sources with possibly hierarchical
#' matching IDs using **data.table**
#'
#' @param x Named list: Long form datasets to merge. Will be converted to `data.table`
#' @param group_varnames Vector, character: Variable names to merge by, in order. If first is present on
#' a given pair of datasets, merge on that, otherwise try the next in line.
#' @param time_varname Character: Name of column that should be present in all datasets containing
#' time information. Default = "Date"
#' @param start_date Date or characcter: Start date for final dataset in format "YYYY-MM-DD"
#' @param end_date Date or character: End dat for final dataset in format "YYYY-MM-DD"
#' @param interval_days Integer: Starting with `start_date` create timepoints every this many
#' days. Default = 14
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @param trace Integer: If > 0 print additional info to console. Default = 1
#' @returns Merged **data.table**

mergelongtreatment <- function(
  x,
  group_varnames,
  time_varname = "Date",
  start_date,
  end_date,
  interval_days = 14,
  verbose = TRUE,
  trace = 1
) {
  # Arguments
  if (!is.list(x)) stop("x must be a named list")
  n_sets <- length(x)
  if (is.null(names(x))) names(x) <- paste0("Dataset", seq(x))
  .names <- names(x)

  # Check there are at least 2 inputs
  if (n_sets < 2)
    stop("Please provide at least 2 datasets as a named list in 'x'")

  # Check all inputs contain at least one of group_varname and the time_varname
  for (i in seq(x)) {
    .names <- names(x[[i]])
    if (!time_varname %in% .names) {
      stop("dataset", .names[i], "does not include time variable", time_varname)
    }
    if (any(!group_varnames %in% .names)) {
      stop(
        "Dataset",
        .names[i],
        "does not include any variable named",
        paste(group_varnames, collapse = " or ")
      )
    }
  }

  # Print input summary
  if (verbose) {
    msg2("There are", n_sets, "input datasets:")
    .summary <- t(data.frame(sapply(
      x,
      function(i) paste(NROW(i), "x", NCOL(i))
    )))
    printdf1(.summary, pad = 4)
  }

  # [ Base dataset ] ----
  # Contains final number of rows,
  # with "Date" and "ID" columns.
  # Each merge will add columns (not rows) by rolling joins
  dat <- data.table::as.data.table(expand.grid(
    Date = seq(as.Date(start_date), as.Date(end_date), interval_days),
    ID = group_varnames[1]
  ))

  # [ Merges ] ----
  for (i in seq(x)) {
    .key <- group_varnames[min(which(group_varnames %in% names(x[[i]])))]
    setkeyv(dat, c(.key, time_varname))
    setkeyv(x[[i]], c(.key, time_varname))
    if (verbose) {
      msg20(
        "Merge ",
        orange(i),
        " of ",
        orange(n_sets),
        ": Using keys ",
        paste0(hilite(.key), ", ", hilite(time_varname))
      )
    }
    # if (try({
    dat <- x[[i]][dat, roll = TRUE]
    # })) msg20("Successfully merged ", .names[i], ":")
    if (verbose) {
      msg2(
        "Merged dataset now contains",
        hilite(NROW(dat)),
        "rows and",
        hilite(NCOL(dat)),
        "columns"
      )
    }
  }

  dat
}
