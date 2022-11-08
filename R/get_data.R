# get_data.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Read data from file
#'
#' Convenience function to read data into a data.table using either
#' \code{data.table:fread()} or \code{arrow:read_delim_arrow()}
#'
#' @param filename Character: filename or full path if \code{datadir = NULL}
#' @param datadir Character: Optional path to directory where \code{filename} 
#' is located. If not specified, \code{filename} must be the full path.
#' @param make.unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to 
#' factors
#' @param clean.colnames Logical: If TRUE, clean columns names using 
#' \link{clean_colnames}
#' @param reader Character: "data.table" or "arrow", to use 
#' \code{data.table::fread()} or \code{arrow::read_delim_arrow()}, respectively,
#' to read \code{filename}
#' @param sep Single character: field separator. If \code{reader = "fread"}
#' and \code{sep = NULL}, this defaults to "auto", otherwise defaults to ","
#' @param verbose Logical: If TRUE, print messages to console
#' @param fread_verbose Logical: Passed to \code{data.table::fread}
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional parameters to pass to \code{data.table::fread}, 
#' \code{arrow::read_delim_arrow()} or \code{vroom::vroom()}
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' datadir <- "~/icloud/Data"
#' dat <- get_data("iris.csv", datadir)
#' }

get_data <- function(filename,
                     datadir = NULL,
                     make.unique = TRUE,
                     character2factor = FALSE,
                     clean.colnames = TRUE,
                     reader = c("data.table", "arrow", "vroom"),
                     sep = NULL,
                     verbose = TRUE,
                     fread_verbose = FALSE,
                     timed = verbose, ...) {

    dependency_check("data.table")
    reader <- match.arg(reader)
    if (timed) start.time <- intro(verbose = FALSE)
    path <- if (is.null(datadir)) {
        filename
    } else {
        file.path(datadir, filename)
    }
    if (verbose) msgread(path, caller = "get_data")
    if (reader == "data.table") {
        if (is.null(sep)) sep <- "auto"
        .dat <- data.table::fread(
            path,
            sep = sep,
            verbose = fread_verbose, ...
        )
    } else if (reader == "arrow") {
        dependency_check("arrow")
        if (is.null(sep)) sep <- ","
        .dat <- arrow::read_delim_arrow(
            path,
            delim = sep, ...) |>
            data.table::setDT()
    } else {
        dependency_check("vroom")
        .dat <- vroom::vroom(
            path,
            delim = sep,
            progress = verbose, ...
        ) |>
            data.table::setDT()
    }
    
    .nrow <- nrow(.dat)
    .ncol <- ncol(.dat)
    if (verbose) {
        msg(
            "Read in", hilitebig(.nrow),
            "rows by", hilitebig(.ncol),
            "columns."
        )
    }
    if (make.unique) {
        .dat <- unique(.dat)
        .nrowp <- nrow(.dat)
        .dup <- .nrow - .nrowp
        if (verbose && .dup > 0) {
            msg(
                "Removed",
                orange(format(.dup, big.mark = ","), TRUE),
                "duplicate",
                paste0(ngettext(.dup, "row", "rows"), ".")
            )
            msg(
                "New dimensions:", hilitebig(.nrowp),
                "by", hilitebig(.ncol),
                "columns."
            )
        }
    }
    
    if (clean.colnames) {
        setnames(.dat, names(.dat), clean_colnames(.dat))
    }
    
    if (character2factor) {
        .dat <- preprocess(.dat, character2factor = TRUE)
    }

    if (timed) outro(start.time)
    .dat

} # rtemis::get_data

msgread <- function(x, caller = "", use_basename = TRUE) {
    if (use_basename) x <- basename(x)
    msg0(bold(green("\u25B6")), " Reading ", hilite(x), "...", caller = caller)
}
