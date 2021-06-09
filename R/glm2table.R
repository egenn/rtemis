# glm2table.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Collect summary table from list of massGLMs with same predictors, different outcome ("massy")
#'
#' @param x list of \link{glm} models
#' @param xnames Character, vector: names of models
#' @return \code{data.table} with glm summaries
#' @author E.D. Gennatas
#' @export

glm2table <- function(x, xnames = NULL) {

  if (is.null(xnames)) {
    xnames <- if (!is.null(names(x))) {
      names(x)
    } else {
      paste0("Model_", seq_along(x))
    }
  }

  data.table(Variable = xnames,
             do.call(rbind,
                     c(lapply(x, function(y) {
                       out <- t(coef(summary(y))[-1, , drop = FALSE])
                       varnames <- gsub(".*\\$", "", colnames(out))
                       parnames <- c("Coefficient", "SE", "t_value", "p_value")
                       out <- c(out)
                       names(out) <- c(outer(parnames, varnames, paste))
                       out
                     })))
  )

} # rtemis::glm2table
