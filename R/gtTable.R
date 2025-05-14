# gtTable.R
# ::rtemis::
# 2018 E.D. Gennatas rtemis.org

#' Greater-than Table
#'
#' Compare vectors element-wise, and tabulate N times each vector is greater than the others
#'
#' @param x List of vectors of same length
#' @param x.name Character: Name of measure being compared
#' @param na.rm Passed to `sum` to handle missing values
#' @param verbose Logical: If TRUE, write output to console
#' @author E.D. Gennatas
#' @export

gtTable <- function(x = list(), x.name = NULL, na.rm = TRUE, verbose = TRUE) {
  if (is.null(x.name)) x.name <- deparse(substitute(x))

  if (is.null(names(x))) names(x) <- paste0("x", seq_along(x))
  names <- names(x)

  grid <- expand.grid(names, names, stringsAsFactors = FALSE)
  grid$Nwin <- vector("numeric", NROW(grid))
  for (i in seq_len(NROW(grid))) {
    name1 <- grid[i, 1]
    name2 <- grid[i, 2]
    grid$Nwin[i] <- sum(x[[name1]] > x[[name2]], na.rm = na.rm)
  }

  dat <- as.data.frame(matrix(grid$Nwin, length(x)))
  colnames(dat) <- rownames(dat) <- names

  if (verbose) {
    .nchar <- nchar(paste("Row > than Column :", x.name))
    cat("Row > than Column :", x.name, "\n")
    cat(rep("-", .nchar), "\n", sep = "")
    print(dat)
  }

  invisible(dat)
} # rtemis::gtTable
