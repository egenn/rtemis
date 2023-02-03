# read.R
# ::rtemis::
# 2022-3 E.D. Gennatas www.lambdamd.org

#' Read delimited file into a data.table
#'
#' Convenience function to read data using
#' \code{data.table:fread()}, \code{arrow:read_delim_arrow()},
#' or \code{vroom::vroom()}
#'
#' @param filename Character: filename or full path if \code{datadir = NULL}
#' @param datadir Character: Optional path to directory where \code{filename}
#' is located. If not specified, \code{filename} must be the full path.
#' @param make.unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to
#' factors
#' @param clean.colnames Logical: If TRUE, clean columns names using
#' \link{clean_colnames}
#' @param csv.reader Character: "data.table" or "arrow", to use
#' \code{data.table::fread()} or \code{arrow::read_delim_arrow()}, respectively,
#' to read \code{filename}
#' @param xlsx.sheet Integer or character: Name or number of XLSX sheet to read
#' @param sep Single character: field separator. If \code{csv.reader = "fread"}
#' and \code{sep = NULL}, this defaults to "auto", otherwise defaults to ","
#' @param quote Single character: quote character
#' @param na.strings Character vector: Strings to be interpreted as NA values
#' @param output Character: "default" or "data.table", If default, return the csv.reader's
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
                 csv.reader = c("data.table", "arrow", "vroom"),
                 xlsx.sheet = 1,
                 sep = NULL,
                 quote = "\"",
                 na.strings = c("NA", ""),
                 output = c("data.table", "default"),
                 attr = NULL,
                 value = NULL,
                 verbose = TRUE,
                 fread_verbose = FALSE,
                 timed = verbose, ...) {

    dependency_check("data.table")
    if (timed) start.time <- intro(verbose = FALSE)
    csv.reader <- match.arg(csv.reader)
    output <- match.arg(output)
    ext <- tools::file_ext(filename)
    path <- if (is.null(datadir)) {
        filename
    } else {
        file.path(datadir, filename)
    }
    if (verbose) msgread(path, caller = "get_data")

    if (ext == "xlsx") {
       .dat <- openxlsx::read.xlsx(filename, xlsx.sheet, ...)
       if (output == "data.table") setDT(.dat)
    } else {
        if (csv.reader == "data.table") {
            if (is.null(sep)) sep <- "auto"
            .dat <- data.table::fread(
                path,
                sep = sep,
                quote = quote,
                na.strings = na.strings,
                verbose = fread_verbose, ...
            )
        } else if (csv.reader == "arrow") {
            dependency_check("arrow")
            if (is.null(sep)) sep <- ","
            .dat <- arrow::read_delim_arrow(
                path,
                delim = sep,
                quote = quote,
                na = na.strings, ...
            )
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
