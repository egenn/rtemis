# read.R
# ::rtemis::
# 2022-3 E.D. Gennatas www.lambdamd.org
# need a way to ignore errors with duckdb::duckdb_read_csv()
# rpolars nullstring is buggy, only recognizes NULL

#' Read delimited file or XLSX into a data.table
#'
#' Convenience function to read delimited data using
#' \code{data.table:fread()}, \code{arrow:read_delim_arrow()},
#' \code{vroom::vroom()}, or \code{duckdb::duckdb_read_csv()}.
#' Read XLSX files using \code{readxl::read_excel()}.
#'
#' @param filename Character: filename or full path if \code{datadir = NULL}
#' @param datadir Character: Optional path to directory where \code{filename}
#' is located. If not specified, \code{filename} must be the full path.
#' @param make.unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to
#' factors
#' @param clean.colnames Logical: If TRUE, clean columns names using
#' \link{clean_colnames}
#' @param delim.reader Character: "data.table" or "arrow", to use
#' \code{data.table::fread()} or \code{arrow::read_delim_arrow()}, respectively,
#' to read \code{filename}
#' @param xlsx.sheet Integer or character: Name or number of XLSX sheet to read
#' @param sep Single character: field separator. If \code{delim.reader = "fread"}
#' and \code{sep = NULL}, this defaults to "auto", otherwise defaults to ","
#' @param quote Single character: quote character
#' @param na.strings Character vector: Strings to be interpreted as NA values.
#' For \code{delim.reader = "duckdb"}, this must be a single string.
#' For \code{delim.reader = "rpolars"}, this must be a single string, otherwise, if an
#' unnamed character vector, it maps each string to each column. If named, the names 
#' should match columns. See \code{?rpolars::csv_reader} for more details.
#' @param output Character: "default" or "data.table", If default, return the delim.reader's
#' default data structure, otherwise convert to data.table
#' @param verbose Logical: If TRUE, print messages to console
#' @param fread_verbose Logical: Passed to \code{data.table::fread}
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional parameters to pass to \code{data.table::fread},
#' \code{arrow::read_delim_arrow()}, \code{vroom::vroom()}, 
#' or \code{openxlsx::read.xlsx()}
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' datadir <- "~/icloud/Data"
#' dat <- read("iris.csv", datadir)
#' }
read <- function(filename,
                 datadir = NULL,
                 make.unique = TRUE,
                 character2factor = FALSE,
                 clean.colnames = TRUE,
                 delim.reader = c("data.table", "vroom", "duckdb", "arrow", "rpolars"),
                 xlsx.sheet = 1,
                 sep = NULL,
                 quote = "\"",
                 na.strings = c(""),
                 rpolars_ignore_errors = TRUE,
                 rpolars_infer_schema_length = 100,
                 rpolars_encoding = "utf8-lossy",
                 rpolars_parse_dates = TRUE,
                 output = c("data.table", "default"),
                 attr = NULL,
                 value = NULL,
                 verbose = TRUE,
                 fread_verbose = FALSE,
                 timed = verbose, ...) {

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
    # if (verbose) msgread(path, caller = "read")
    if (verbose) {
        msg20(bold(green("\u25B6")), " Reading ",
            hilite(basename(path)), " using ", 
            if (ext == "xlsx") "openxlsx" else delim.reader, "..."
        )
    }

    if (ext == "xlsx") {
       .dat <- openxlsx::read.xlsx(filename, xlsx.sheet, ...)
       if (output == "data.table") setDT(.dat)
    } else {
        if (delim.reader == "data.table") {
            if (is.null(sep)) sep <- "auto"
            .dat <- data.table::fread(
                path,
                sep = sep,
                quote = quote,
                na.strings = na.strings,
                verbose = fread_verbose, ...
            )
        } else if (delim.reader == "duckdb") {
            dependency_check("DBI", "duckdb")
            if (is.null(sep)) sep <- ","
            if (length(na.strings) > 1) {
                msg2("Note: 'na.strings' must be a single string for duckdb; setting to '", na.strings[1], "'")
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
            .dat <- dbReadTable(con, "data")
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
                na = na.strings, ...
            )
            if (output == "data.table") setDT(.dat)
        } else if (delim.reader == "rpolars") {
            dependency_check("rpolars")
            if (is.null(sep)) sep <- ","
            .dat <- rpolars::csv_reader(
                path,
                sep = sep,
                has_header = TRUE,
                ignore_errors = rpolars_ignore_errors,
                quote_char = quote,
                # null_values = na.strings
                infer_schema_length = rpolars_infer_schema_length,
                encoding = rpolars_encoding,
                parse_dates = rpolars_parse_dates, ...
            )$as_data_frame()
            if (output == "data.table") setDT(.dat)
        } else {
            dependency_check("vroom")
            .dat <- vroom::vroom(
                path,
                delim = sep,
                quote = quote,
                na = na.strings,
                progress = verbose, ...
            )
            if (output == "data.table") setDT(.dat)
        }
    } 
    
    .nrow <- nrow(.dat)
    .ncol <- ncol(.dat)
    if (verbose) {
        msg2(
            "Read in", hilitebig(.nrow),
            "x", hilitebig(.ncol)
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
                "New dimensions:", hilitebig(.nrowp),
                "x", hilitebig(.ncol)
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
    .dat
} # rtemis::read

msgread <- function(x, caller = "", use_basename = TRUE) {
    if (use_basename) x <- basename(x)
    msg20(bold(green("\u25B6")), " Reading ", hilite(x), "...", caller = caller)
}
