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
                     verbose = TRUE) {

    dependency_check("data.table")
    .dat <- data.table::fread(file.path(datadir, filename))
    .nrow <- nrow(.dat)
    .ncol <- ncol(.dat)
    if (verbose) {
        msg(
            "Read in",
            rtHighlight$bold(.nrow), 
            "rows by", 
            rtHighlight$bold(.ncol), "columns."
        )
    }
    if (make_unique) {
        .dat |> unique()
        .nrowp <- nrow(.dat)
        .dup <- .nrow - .nrowp
        if (verbose && .dup > 0) {
            msg(
                "Removed",
                rtHighlight$bold(.dup),
                "duplicate rows:\n"
            )
            msg(
                "New dimensions:",
                rtHighlight$bold(.nrowp),
                "by",
                rtHighlight(.ncol),
                "columns."
            )
        }
    }
    .dat

} # rtemis::get_data
