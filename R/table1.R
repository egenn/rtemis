# table1.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Table 1
#'
#' Build Table 1. Subject characteristics
#'
#' The output will look like "summaryFn1 (summaryFn2)".
#' Using defaults this will be "mean (sd)"
#' @param x data.frame or matrix: Input data, cases by features
#' @param summaryFn1 Function: Summary function 1. Default = `mean`. See Details
#' @param summaryFn2 Function: Summary function 2. Default = `sd`. See Details
#' @param summaryFn1.extraArgs List: Extra arguments for `summaryFn1`.
#' @param summaryFn2.extraArgs List: Extra arguments for `summaryFn2`.
#' @param labelify Logical: If TRUE, apply [labelify] to column names of  `x`
#' @param verbose Logical: If TRUE, print messages to console.
#' @param filename Character: Path to output CSV file to save table.
#'
#' @return
#' A data.frame, invisibly, with two columns: "Feature", "Value mean (sd) | N"
#' @examples
#' table1(iris)
#' @author E.D. Gennatas
#' @export

table1 <- function(
  x,
  summaryFn1 = mean,
  summaryFn2 = sd,
  summaryFn1.extraArgs = list(na.rm = TRUE),
  summaryFn2.extraArgs = list(na.rm = TRUE),
  labelify = TRUE,
  verbose = TRUE,
  filename = NULL
) {
  if (is.null(dim(x))) stop("Please provide a matrix or data frame")
  .dim <- dim(x)
  if (verbose)
    msg2("Input:", hilite(.dim[1]), "cases with", hilite(.dim[2]), "features")

  .names <- colnames(x)
  if (is.null(.names)) {
    warning(
      "No column names found, please check input. Generic names will be used."
    )
    .names <- paste("Feature", seq_len(NCOL(x)))
  } else {
    if (labelify) .names <- labelify(.names)
  }

  x <- as.data.frame(x)

  # Get index for continuous and discrete features
  index.cont <- which(sapply(x, is.numeric))
  index.disc <- which(sapply(x, is.factor) | sapply(x, is.character))

  # Get summary statistics ----

  ## '- Continuous Features ----
  if (length(index.cont) > 0) {
    # .summary1_cont <- apply(x[, index.cont, drop = FALSE], 2, summaryFn1)
    .summary1_cont <- apply(x[, index.cont, drop = FALSE], 2, function(i) {
      do.call(summaryFn1, c(list(i), summaryFn1.extraArgs))
    })
    # .summary2_cont <- apply(x[, index.cont, drop = FALSE], 2, summaryFn2)
    .summary2_cont <- apply(x[, index.cont, drop = FALSE], 2, function(i) {
      do.call(summaryFn2, c(list(i), summaryFn2.extraArgs))
    })
    .summary_cont <- paste0(
      ddSci(.summary1_cont),
      " (",
      ddSci(.summary2_cont),
      ")"
    )
  } else {
    .summary_cont <- NULL
  }

  ## '- Discrete Features ----
  if (length(index.disc) > 0) {
    .summary1_disc <- lapply(index.disc, function(i) table(x[, i]))
    .summary_disc <- sapply(
      .summary1_disc,
      function(i) paste0(names(i), ": ", i, collapse = "; ")
    )
  } else {
    .summary_disc <- NULL
  }

  # Table 1 ----
  .table1 <- data.frame(
    Feature = c(.names[index.cont], .names[index.disc]),
    Value = c(.summary_cont, .summary_disc)
  )
  colnames(.table1)[2] <- "Mean (sd) | Count per group"

  if (verbose) {
    .table1.f <- .table1
    colnames(.table1.f) <- NULL
    cat(bold("Table 1."), "Subject Characteristics\n")
    print(.table1.f, row.names = FALSE)
    cat(
      "\nAll values are displayed as ",
      deparse(substitute(summaryFn1)),
      " (",
      deparse(substitute(summaryFn2)),
      ") or Count per group\n",
      sep = ""
    )
  }

  if (!is.null(filename)) {
    # Add .csv extension if not present
    filename <- ifelse(
      grepl("\\.csv$", filename),
      filename,
      paste0(filename, ".csv")
    )
    i <- 1
    while (file.exists(filename)) {
      filename <- gsub("\\.csv$", paste0("_", i, ".csv"), filename)
      i <- i + 1
    }
    write.csv(.table1, filename, row.names = FALSE)
  }

  invisible(.table1)
} # rtemis::table1
