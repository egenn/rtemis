# pfread.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' fread delimited file in parts
#'
#' @param x Character: Path to delimited file
#' @param part_nrows Integer: Number of rows to read in each part
#' @param nrows Integer: Number of rows in the file
#' @param header Logical: If TRUE, the file is assumed to include a header row
#' @param sep Character: Delimiter
#' @param verbose Logical: If TRUE, print messages to console
#' @param stringsAsFactors Logical: If TRUE, characters will be converted to
#' factors
#' @param ... Additional arguments to pass to `data.table::fread()`
#'
#' @author E.D. Gennatas
#' @export

pfread <- function(
  x,
  part_nrows,
  nrows = NULL,
  header = TRUE,
  sep = "auto",
  verbose = TRUE,
  stringsAsFactors = TRUE,
  ...
) {
  # nrows <- as.integer(R.utils::countLines(x))
  # nrows <- system(paste("wc -l", x))
  if (is.null(nrows)) {
    nrows <- system2("wc", c("-l", x), stdout = TRUE)
    nrows <- gsub("^ ", "", nrows)
    nrows <- strsplit(nrows, " ")[[1]][1] |> as.integer()
    if (header) nrows <- nrows - 1
  }

  nparts <- ceiling(nrows / part_nrows)
  if (verbose) {
    msg2("Reading part 1...")
    i <- 1
  }
  dat1 <- fread(
    x,
    nrows = part_nrows,
    header = header,
    sep = sep,
    stringsAsFactors = stringsAsFactors,
    ...
  )
  if (nparts == 1) {
    return(dat1)
  }
  ndone <- part_nrows
  col_classes <- sapply(dat1, \(i) class(i)[1])
  .col.names <- names(col_classes)
  .colClasses <- unname(col_classes)
  parts <- lapply(seq_len(nparts)[-1], \(i) {
    fread(
      x,
      nrows = part_nrows,
      skip = ndone + header,
      header = FALSE,
      sep = sep,
      col.names = .col.names,
      colClasses = .colClasses,
      ...
    )
  })

  dat <- rbindlist(c(list(dat1), parts))
  if (verbose) msg2("Read", hilitebig(nrow(dat)), "rows")

  invisible(dat)
} # rtemis::pfread


pfread1 <- function(
  x,
  part_nrows,
  nrows = NULL,
  header = TRUE,
  sep = "auto",
  verbose = TRUE,
  stringsAsFactors = TRUE,
  ...
) {
  # nrows <- as.integer(R.utils::countLines(x))
  # nrows <- system(paste("wc -l", x))
  if (is.null(nrows)) {
    nrows <- system2("wc", c("-l", x), stdout = TRUE)
    nrows <- gsub("^ ", "", nrows)
    nrows <- strsplit(nrows, " ")[[1]][1] |> as.integer()
    if (header) nrows <- nrows - 1
  }

  nparts <- ceiling(nrows / part_nrows)
  if (verbose) {
    msg2("Reading part 1...")
    i <- 1
  }
  dat <- fread(
    x,
    nrows = part_nrows,
    header = header,
    sep = sep,
    stringsAsFactors = stringsAsFactors,
    ...
  )
  if (nparts == 1) {
    return(dat)
  }
  ndone <- part_nrows
  if (verbose) msg2("Total read =", hilitebig(ndone))
  col_classes <- sapply(dat, \(i) class(i)[1])
  .col.names <- names(col_classes)
  .colClasses <- unname(col_classes)
  for (i in seq_len(nparts)[-1]) {
    if (verbose) {
      msg20("Reading part ", i, "...")
      i <- i + 1
    }

    dat <- rbind(
      dat,
      fread(
        x,
        nrows = part_nrows,
        skip = ndone + header,
        header = FALSE,
        sep = sep,
        col.names = .col.names,
        colClasses = .colClasses,
        ...
      )
    )
    ndone <- nrow(dat)
    if (verbose) msg2("Total read =", hilitebig(ndone))
  }

  invisible(dat)
} # rtemis::pfread1
