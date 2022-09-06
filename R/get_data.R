# get_data.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' Read data from file
#'
#' Convenience function to read data into a data.table
#'
#' @param filename Character: filename or full path if \code{datadir = NULL}
#' @param datadir Character: path to directory where \code{filename} is located
#' @param make_unique Logical: If TRUE, keep unique rows only
#' @param verbose Logical: If TRUE, print messages to console
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
                     make_unique = TRUE,
                     verbose = TRUE, ...) {

    dependency_check("data.table")
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
    if (make_unique) {
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
    .dat

} # rtemis::get_data
