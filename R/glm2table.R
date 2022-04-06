# glm2table.R
# ::rtemis::
# 2021 E.D. Gennatas lambdamd.org

#' Collect summary table from list of massGLMs with same predictors, different outcome ("massy")
#'
#' @param x list of \link{glm} models
#' @param xnames Character, vector: names of models
#' 
#' @return \code{data.table} with glm summaries
#' @author E.D. Gennatas
#' @export

glm2table <- function(
    x, xnames = NULL,
    include_anova_pvals = NA) {

  if (is.null(xnames)) {
    xnames <- if (!is.null(names(x))) {
      names(x)
    } else {
      paste0("Model_", seq_along(x))
    }
  }

if (!is.na(include_anova_pvals)) dependency_check("car")

  out <- data.table(
      Variable = xnames,
      do.call(
          rbind,
          c(lapply(x, function(y) {
              out <- t(coef(summary(y))[-1, , drop = FALSE])
              varnames <- gsub(".*\\$", "", colnames(out))
              parnames <- c("Coefficient", "SE", "t_value", "p_value")
              out <- c(out)
              names(out) <- c(outer(parnames, varnames, paste))
              out
          }))
      )
  )

  if (1 %in% include_anova_pvals) {
      pvals2 <- t(sapply(x, \(i) car::Anova(i, type = 2)[, 3]))
      colnames(pvals2) <- paste(
          "p_value type II",
          x[[1]] |> terms() |> attr("term.labels")
      )
      out <- c(out, pvals2)
  }

  if (3 %in% include_anova_pvals) {
      pvals3 <- t(sapply(x, \(i) car::Anova(i, type = 3)[, 3]))
      colnames(pvals3) <- paste(
          "p_value type III",
          x[[1]] |> terms() |> attr("term.labels")
      )
      out <- cbind(out, pvals3)
  }

  out

} # rtemis::glm2table
