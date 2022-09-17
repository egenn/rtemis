# pfread.R
# ::rtemis::
# 2022 E.D. Gennatas www.lambdamd.org

#' fread delimited file in parts
#' 
#' @param x Character: Path to delimited file
#' @param header Logical: If TRUE, the file is assumed to include a header row
#' @param verbose Logical: If TRUE, print messages to console
#' @param stringsAsFactors Logical: If TRUE, characters will be converted to 
#' factors
#' @param ... Additional arguments to pass to \code{data.table::fread()}
#' @author E.D. Gennatas
#' @export

pfread <- function(x, nrows, 
                   header = TRUE,
                   verbose = TRUE,
                   stringsAsFactors = TRUE, ...) {
    nlines <- as.integer(R.utils::countLines(x))
    if (header) nlines <- nlines - 1
    nparts <- ceiling(nlines / nrows)
    if (verbose) {
        msg("Reading part 1...")
        i <- 1
    }
    dat <- fread(x,
        nrows = nrows, 
        header = header,
        stringsAsFactors = stringsAsFactors, ...
    )
    if (nparts == 1) {
        return(dat)
    }
    ndone <- nrows
    col_classes <- sapply(dat, class)
    .col.names <- names(col_classes)
    .colClasses <- unname(col_classes)
    for (i in seq_len(nparts)[-1]) {
        if (verbose) {
            msg0("Reading part ", i, "...")
            i <- i + 1
        }

        dat <- rbind(
            dat,
            fread(x,
                nrows = nrows, 
                skip = ndone + header,
                header = FALSE,
                col.names = .col.names,
                colClasses = .colClasses, ...
            )
        )
        ndone <- nrow(dat)
        msg("ndone =", ndone)
    }

    invisible(dat)
} # rtemis::pfread
