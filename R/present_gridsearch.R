# present_gridsearch.R
# ::rtremis::
# 2022 E.D. Gennatas rtemis.org

#' Present gridsearch results
#'
#' @param x rtMod or rtModCV objects
#' @param rtModCV.repeat Integer: Which repeat to use, when x is rtModCV object
#' @param ... Additional arguments to pass to `knitr::kable` used to print
#' the best tune table
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' mod <- s_CART(iris, cp = c(0, .1), maxdepth = c(3, 5))
#' mod_10ss <- elevate(iris, mod = "cart", cp = c(0, .1), maxdepth = c(3, 5))
#' present_gridsearch(mod)
#' present_gridsearch(mod_10ss)
#' }
present_gridsearch <- function(x, rtModCV.repeat = 1, ...) {
  if (inherits(x, "rtMod")) {
    search <- x$gridsearch$params$search
    best_tune <- x$gridsearch$best.tune
  } else if (inherits(x, "rtModCV")) {
    search <- x$mod[[rtModCV.repeat]][[1]]$mod1$gridsearch$params$search
    best_tune <- sapply(
      x$mod[[rtModCV.repeat]],
      \(i) i$mod1$gridsearch$best.tune
    )
  }

  # Print search params
  cat("Search hyperparameters:\n\n")
  printls(search)

  # Print best.tune
  cat("\nBest hyperparameters:")
  knitr::kable(best_tune, ...)
} # rtemis::present_gridsearch
