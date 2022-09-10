# get_data.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Read data from file
#'
#' Convenience function to read data into a data.table
#'
#' @param filename Character: filename or full path if \code{datadir = NULL}
#' @param datadir Character: path to directory where \code{filename} is located
#' @param make.unique Logical: If TRUE, keep unique rows only
#' @param character2factor Logical: If TRUE, convert character variables to 
#' factors
#' @param clean.colnames Logical: If TRUE, clean columns names using 
#' \link{clean_colnames}
#' @param verbose Logical: If TRUE, print messages to console
#' @param timed Logical: If TRUE, time the process and print to console
#' @param ... Additional parameters to pass to \code{data.table::fread}
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
                     character2factor = TRUE,
                     clean.colnames = TRUE,
                     verbose = TRUE,
                     timed = verbose, ...) {

    dependency_check("data.table")
    if (timed) start.time <- intro(verbose = FALSE)
    if (verbose) msgread(filename)
    .dat <- data.table::fread(file.path(datadir, filename), ...)
    .nrow <- nrow(.dat)
    .ncol <- ncol(.dat)
    if (verbose) {
        msg(
            "Read in",
            rtHighlight$bold(format(.nrow, big.mark = ",")), 
            "rows by", 
            rtHighlight$bold(format(.ncol, big.mark = ",")), "columns."
        )
    }
    if (make.unique) {
        .dat <- unique(.dat)
        .nrowp <- nrow(.dat)
        .dup <- .nrow - .nrowp
        if (verbose && .dup > 0) {
            msg(
                "Removed",
                rtOrange$bold(format(.dup, big.mark = ",")),
                "duplicate",
                paste0(ngettext(.dup, "row", "rows"), ".")
            )
            msg(
                "New dimensions:",
                rtHighlight$bold(format(.nrowp, big.mark = ",")),
                "by",
                rtHighlight$bold(format(.ncol, big.mark = ",")),
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

msgread <- function(x) msg0("Reading ", rtHighlight(x), "...", caller = "")