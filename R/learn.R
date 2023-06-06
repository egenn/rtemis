# learn.R
# ::rtemis::
# 2016 E.D. Gennatas www.lambdamd.org

#' Supervised Learning with \pkg{rtemis}
#'
#' Train any \pkg{rtemis} model
#'
#' `args` and `...` allow you to either pass named arguments, or a list of arguments (or both)
#'
#' @inheritParams s_GLM
#' @param mod Character: Learner to use. To get list of options, run `algSelect()`
#' @param args Optional list of parameters to be passed to learner
#' @param ... Additional arguments to be passed to learner
#' @author E.D. Gennatas
#' @export

learn <- function(x, y = NULL,
                  mod = "ppr",
                  x.test = NULL, y.test = NULL,
                  x.name = NULL, y.name = NULL,
                  args = list(),
                  question = NULL,
                  verbose = TRUE,
                  print.plot = TRUE, ...) {

  # [ Intro ] ----
  if (missing(x) | missing(y)) {
    cat("Usage:\n  learn(x, y, 'mod', x.test[optional], y.test[optional], ...)\n\n")
    return(algSelect())
  }

  # [ LEARNER ] ----
  mod <- R.utils::doCall(algSelect(mod),
                         x = x, y = y,
                         x.test = x.test, y.test = y.test,
                         x.name = x.name, y.name = y.name,
                         question = question,
                         verbose = verbose,
                         args = c(args, ...))

  mod

} # rtemis::learn
