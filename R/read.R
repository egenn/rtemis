# read.R
# ::rtemis::
# 2022-3 E.D. Gennatas rtemis.org
# need a way to ignore errors with duckdb::duckdb_read_csv()
# polars nullstring is buggy, only recognizes NULL

#' Read tabular data from a variety of formats
#'
#' Read data and optionally clean column names, keep unique rows, and convert
#' characters to factors
#'
#' @details
#' `read` is a convenience function to read:
#'
#' - **Delimited** files using `data.table:fread()`, `arrow:read_delim_arrow()`,
#'   `vroom::vroom()`, `duckdb::duckdb_read_csv()`
#   or `polars::pl$read_csv()`
#' - **ARFF** files using `farff::readARFF()`
#' - **Parquet** files using `arrow::read_parquet()`
#' - **XLSX** files using `readxl::read_excel()`
#' - **DTA** files from Stata using `haven::read_dta()`
#' - **FASTA** files using `seqinr::read.fasta()`
#' - **RDS** files using `readRDS()`
#'
#' @param filename Character: filename or full path if `datadir = NULL`
#' @param datadir Character: Optional path to directory where `filename`
#' is located. If not specified, `filename` must be the full path.
#' @param make.unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to
#' factors
#' @param clean.colnames Logical: If TRUE, clean columns names using
#' [clean_colnames]
#' @param delim.reader Character: package to use for reading delimited data
#' @param xlsx.sheet Integer or character: Name or number of XLSX sheet to read
#' @param sep Single character: field separator. If `delim.reader = "fread"`
#' and `sep = NULL`, this defaults to "auto", otherwise defaults to ","
#' @param quote Single character: quote character
#' @param na.strings Character vector: Strings to be interpreted as NA values.
#' For `delim.reader = "duckdb"`, this must be a single string.
# For `delim.reader = "polars"`, this must be a single string, otherwise, if an
# unnamed character vector, it maps each string to each column. If named, the names
# should match columns. See `?polars::csv_reader` for more details.
#' @param output Character: "default" or "data.table", If default, return the delim.reader's
#' default data structure, otherwise convert to data.table
#' @param attr Character: Attribute to set (Optional)
#' @param value Character: Value to set (if `attr` is not NULL)
#' @param verbose Logical: If TRUE, print messages to console
#' @param fread_verbose Logical: Passed to `data.table::fread`
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional parameters to pass to `data.table::fread`,
#' `arrow::read_delim_arrow()`, `vroom::vroom()`,
#' or `openxlsx::read.xlsx()`
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' datadir <- "~/icloud/Data"
#' dat <- read("iris.csv", datadir)
#' }
read <- function(
  filename,
  datadir = NULL,
  make.unique = TRUE,
  character2factor = FALSE,
  clean.colnames = TRUE,
  delim.reader = c("data.table", "vroom", "duckdb", "arrow"),
  xlsx.sheet = 1,
  sep = NULL,
  quote = "\"",
  na.strings = c(""),
  #  polars_ignore_errors = TRUE,
  #  polars_infer_schema_length = 100,
  #  polars_encoding = "utf8-lossy",
  #  polars_parse_dates = TRUE,
  output = c("data.table", "default"),
  attr = NULL,
  value = NULL,
  verbose = TRUE,
  fread_verbose = FALSE,
  timed = verbose,
  ...
) {
  dependency_check("data.table")
  if (timed) start.time <- intro(verbose = FALSE)
  delim.reader <- match.arg(delim.reader)
  output <- match.arg(output)
  ext <- tools::file_ext(filename)
  path <- if (is.null(datadir)) {
    filename
  } else {
    file.path(datadir, filename)
  }
  path <- path.expand(path)

  if (ext == "parquet") {
    dependency_check("arrow")
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using arrow::read_parquet()..."
      )
    }
    .dat <- arrow::read_parquet(path, ...)
    if (output == "data.table") setDT(.dat)
  } else if (ext == "rds") {
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        "..."
      )
    }
    .dat <- readRDS(path)
  } else if (ext == "xlsx") {
    dependency_check("openxlsx")
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using openxlsx::read.xlsx()..."
      )
    }
    .dat <- openxlsx::read.xlsx(filename, xlsx.sheet, ...)
    if (output == "data.table") setDT(.dat)
  } else if (ext == "dta") {
    dependency_check("haven")
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using haven::read_dta()..."
      )
    }
    .dat <- haven::read_dta(path, ...)
    if (output == "data.table") setDT(.dat)
  } else if (ext == "fasta") {
    dependency_check("seqinr")
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using seqinr::read.fasta()..."
      )
    }
    .dat <- seqinr::read.fasta(path, ...)
    # if single sequence, return as character
    if (length(.dat) == 1) .dat <- as.character(.dat[[1]])
    return(.dat)
  } else if (ext == "arff") {
    dependency_check("farff")
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using farff::readARFF()..."
      )
    }
    .dat <- farff::readARFF(path, ...)
    if (output == "data.table") setDT(.dat)
  } else {
    if (verbose) {
      msg20(
        bold(green("\u25B6")),
        " Reading ",
        hilite(basename(path)),
        " using ",
        delim.reader,
        "..."
      )
    }
    if (delim.reader == "data.table") {
      if (is.null(sep)) sep <- "auto"
      .dat <- data.table::fread(
        path,
        sep = sep,
        quote = quote,
        na.strings = na.strings,
        verbose = fread_verbose,
        ...
      )
    } else if (delim.reader == "duckdb") {
      dependency_check("DBI", "duckdb")
      if (is.null(sep)) sep <- ","
      if (length(na.strings) > 1) {
        msg2(
          "Note: 'na.strings' must be a single string for duckdb; setting to '",
          na.strings[1],
          "'"
        )
        na.strings <- na.strings[1]
      }
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      .db <- duckdb::duckdb_read_csv(
        con,
        "data",
        path,
        header = TRUE,
        na.strings = na.strings,
        nrow.check = 500,
        delim = sep,
        quote = quote,
        ...
      )
      .dat <- DBI::dbReadTable(con, "data")
      # .dat <- ddb_data(path,
      #     sep = sep, quote = quote, ...
      # )
      if (output == "data.table") setDT(.dat)
    } else if (delim.reader == "arrow") {
      dependency_check("arrow")
      if (is.null(sep)) sep <- ","
      .dat <- arrow::read_delim_arrow(
        path,
        delim = sep,
        quote = quote,
        na = na.strings,
        ...
      )
      if (output == "data.table") setDT(.dat)
      # } else if (delim.reader == "polars") {
      #   dependency_check("polars")
      #   attachNamespace("polars")
      #   if (is.null(sep)) sep <- ","
      #   .dat <- polars::pl$read_csv(
      #     path,
      #     sep = sep,
      #     has_header = TRUE,
      #     ignore_errors = polars_ignore_errors,
      #     quote_char = quote,
      #     # null_values = na.strings
      #     infer_schema_length = polars_infer_schema_length,
      #     encoding = polars_encoding,
      #     parse_dates = polars_parse_dates, ...
      #   )$as_data_frame()
      #   if (output == "data.table") setDT(.dat)
    } else {
      dependency_check("vroom")
      .dat <- vroom::vroom(
        path,
        delim = sep,
        quote = quote,
        na = na.strings,
        progress = verbose,
        ...
      )
      if (output == "data.table") setDT(.dat)
    }
  }

  .nrow <- nrow(.dat)
  .ncol <- ncol(.dat)
  if (verbose) {
    msg2(
      "Read in",
      hilitebig(.nrow),
      "x",
      hilitebig(.ncol)
    )
  }
  if (make.unique) {
    .dat <- unique(.dat)
    .nrowp <- nrow(.dat)
    .dup <- .nrow - .nrowp
    if (verbose && .dup > 0) {
      msg2(
        "Removed",
        orange(format(.dup, big.mark = ","), bold = TRUE),
        "duplicate",
        paste0(ngettext(.dup, "row", "rows"), ".")
      )
      msg2(
        "New dimensions:",
        hilitebig(.nrowp),
        "x",
        hilitebig(.ncol)
      )
    }
  }

  if (clean.colnames) {
    setnames(.dat, names(.dat), clean_colnames(.dat))
  }

  if (character2factor) {
    .dat <- preprocess(.dat, character2factor = TRUE)
  }

  if (!is.null(attr) && !is.null(value)) {
    for (i in seq_len(ncol(.dat))) setattr(.dat[[i]], attr, value)
  }

  if (timed) outro(start.time)

  return(.dat)
} # rtemis::read

msgread <- function(x, caller = "", use_basename = TRUE) {
  if (use_basename) x <- basename(x)
  msg20(bold(green("\u25B6")), " Reading ", hilite(x), "...", caller = caller)
}
