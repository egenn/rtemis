# xselect_decom.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Select \pkg{rtemis} cross-decomposer
#'
#' Accepts decomposer name (supports abbreviations) and returns \pkg{rtemis} function name or
#'   the function itself.
#'   If run with no parameters, prints list of available algorithms.
#'
#' @param xdecom Character: Cross-decomposition name; case insensitive
#' @param fn Logical: If TRUE, return function, otherwise return name of function. Default = FALSE
#' @param desc Logical: If TRUE, return full name of algorithm. Default = FALSE
#' @return Function or name of function (see param `fn`) or full name of algorithm (`desc`)
#' @author E.D. Gennatas
#' @family Cross-Decomposition
#' @export

xselect_decom <- function(xdecom, fn = FALSE, desc = FALSE) {
  description <- list(
    "CCA" = "Sparse Canonical Correlation Analysis"
    # "SD2RES" = "ANTsR sparse decomposition"
    # "SD2RESDEF" = "ANTsR sparse decomposition with deflation"
  )
  description <- t(as.data.frame(description))
  description <- data.frame(
    Name = rownames(description),
    Description = description
  )

  if (missing(xdecom)) {
    cat(
      ".:xselect_decom\nrtemis supports the following cross-decomposition algorithms:\n\n"
    )
    print(description, quote = FALSE, row.names = FALSE)
    return(invisible(9))
  }

  # name.vec <- c("CCA", "SD2RES")
  name.vec <- c("CCA")
  name <- name.vec[pmatch(toupper(xdecom), name.vec)]
  if (is.na(name)) {
    print(description, quote = FALSE)
    stop("Incorrect cross-decomposer specified")
  }

  if (desc) {
    return(as.character(description[description$Name == name, 2]))
  }

  if (name == "CCA") xdecomposer <- if (fn) x_CCA else "x_CCA"
  # if (name == "SD2RES") xdecomposer <- if (fn) x.SD2RES else "x.SD2RES"
  # if (name == "SD2RESDEF") xdecomposer <- if (fn) x.SD2RESDEF else "x.SD2RESDEF"

  return(xdecomposer)
} # rtemis::xselect_decom
