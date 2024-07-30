# padnums.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

padnums <- function(x, pad = "X") {
  for (i in seq_along(x)) {
    if (grepl("[[:digit:]]", substr(x[i], 1, 1))) {
      x[i] <- paste0(pad, x[i])
    }
  }
  x
} # rtemis::padnums
