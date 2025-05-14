# xlsx2list.R
# ::rtemis::
# E.D. Gennatas rtemis.org

#' Read all sheets of an XLSX file into a list
#'
#' @param x Character: path or URL to XLSX file
#' @param sheet Integer, vector: Sheet(s) to read. If NULL, will read all
#' sheets in `x`
#' @param startRow Integer, vector: First row to start reading. Will be
#' recycled as needed for all sheets
#' @param colNames Logical: If TRUE, use the first row of data
#' @param na.strings Character vector: stringd to be interpreted as NA
#' @param detectDates Logical: If TRUE, try to automatically detect dates
#' @param skipEmptyRows Logical: If TRUE, skip empty rows
#' @param skipEmptyCols Logical: If TRUE, skip empty columns
#'
#' @author E.D. Gennatas
#' @export

xlsx2list <- function(
  x,
  sheet = NULL,
  startRow = 1,
  colNames = TRUE,
  na.strings = "NA",
  detectDates = TRUE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE
) {
  if (is.null(sheet)) {
    sheet <- openxlsx::getSheetNames(x)
  }

  if (length(startRow) != length(sheet)) {
    startRow <- recycle(startRow, sheet)
  }

  out <- lapply(seq_along(sheet), \(i) {
    openxlsx::read.xlsx(
      x,
      sheet = i,
      startRow = startRow[i],
      colNames = colNames,
      na.strings = na.strings,
      detectDates = detectDates,
      skipEmptyRows = skipEmptyRows,
      skipEmptyCols = skipEmptyCols
    )
  })

  names(out) <- sheet

  out
} # rtemis::xlsx2list
