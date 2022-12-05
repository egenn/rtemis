# decom.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Matrix Decomposition with \pkg{rtemis}
#'
#' Convenience function to perform any \pkg{rtemis} decomposition
#'
#' \code{decom} returns an R6 class object \code{rtDecom}
#'
#' @param x Numeric matrix / data frame: Input data
#' @param decom Character: Decomposer name. See ]link{decomSelect}. Default = "ICA"
#' @param verbose Logical: if TRUE, print messages to screen
#' @param ... Additional arguments to be passed to \code{decom}
#' 
#' @return \link{rtDecom} object
#' @author E.D. Gennatas
#' @export

decom <- function(x,
                  decom = "ICA",
                  verbose = TRUE, ...) {

    if (missing(x)) {
        cat('Usage:\n  decom(x, "nmf", ...)\n\n')
        return(decomSelect())
    }

    args <- c(
        list(
            x = x,
            verbose = verbose
        ),
        list(...)
    )
    
    decom <- do.call(decomSelect(decom, fn = TRUE), args)

    decom

} # rtemis::decom
