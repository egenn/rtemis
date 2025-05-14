# make_key.R
# ::rtemis::
# 2022 E.D. Gennatas rtemis.org

#' Make key from data.table id - description columns
#'
#' @param x Input data.table
#' @param code_name Character: Name of column name that holds codes
#' @param description_name Character: Name of column that holds descriptions
#' @param filename Character: Path to file to save CSV with key
#'
#' @author E.D. Gennatas
#' @export

make_key <- function(x, code_name, description_name, filename = NULL) {
  dependency_check("data.table")

  .key <- unique(
    x,
    by = code_name
  )[, .SD, .SDcols = c(code_name, description_name)] |>
    setkeyv(code_name)

  if (!is.null(filename)) {
    fwrite(.key, filename)
  }

  .key
} # rtemis:: make_key
