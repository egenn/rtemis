# glm2table.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Collect summary table from list of massGLMs with same predictors, different outcome
#' ("massy")
#'
#' @param x list of [glm] models
#' @param xnames Character, vector: names of models
#' @param include_anova_pvals Integer: 1 or 3; to output ANOVA I or III p-vals. NA to not
#' @param warn Logical: If TRUE, warn when values < than machine eps are replaced by
#' machine eps
#'
#' @return `data.table` with glm summaries
#' @author E.D. Gennatas
#'
#' @keywords internal
#' @noRd

glm2table <- function(x, xnames = NULL, include_anova_pvals = NA, warn = TRUE) {
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
      c(lapply(x, function(l) {
        out <- t(coef(summary(l))[-1, , drop = FALSE])
        varnames <- gsub(".*\\$", "", colnames(out))
        parnames <- c("Coefficient", "SE", "t_value", "p_value")
        out <- c(out)
        names(out) <- c(outer(parnames, varnames, paste))
        out
      }))
    )
  )

  # Convert p-vals equal to 0 to machine double eps
  eps <- .Machine$double.eps
  pvals_idi <- getnames(out, ends_with = "p_value")
  # appease R CMD check:, use with = FALSE, not ..i
  for (i in pvals_idi) {
    lteps <- out[, i, with = FALSE] < eps
    if (length(lteps) > 0) {
      if (warn) warning("Values < machine double eps converted to double eps")
      out[, i, with = FALSE][lteps] <- eps
    }
  }

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


#' Collect summary table (p-values) from list of massGAMs with same predictors,
#' different outcome ("massy")
#'
#' @param x list of [mgcv::gam] models
#' @param xnames Character, vector: names of models
#' @param include_anova_pvals Integer: 1 or 3; to output ANOVA I or III p-vals. NA to not
#'
#' @return `data.table` with glm summaries
#' @keywords internal
#' @noRd
#' @author E.D. Gennatas

gam2table <- function(mods, modnames = NULL) {
  if (is.null(modnames)) {
    modnames <- if (!is.null(names(mods))) {
      names(mods)
    } else {
      paste0("Model_", seq_along(mods))
    }
  }

  out <- data.table(
    Variable = modnames,
    do.call(
      rbind,
      c(lapply(mods, get_gam_pvals))
    )
  )
  setnames(out, names(out)[-1], paste("p_value", names(out)[-1]))
  out
}

#' Get GAM model's p-values for parametric and spline terms
#'
#' @keywords internal
#' @noRd
get_gam_pvals <- function(m, warn = TRUE) {
  eps <- .Machine$double.eps
  ms <- summary(m)
  pvals <- cbind(
    # s terms
    as.data.frame(t(ms$s.table[, 4])),
    # p terms
    as.data.frame(t(ms$p.table[, 4]))[-1]
  )
  lteps <- pvals < eps
  if (length(lteps) > 0) {
    if (warn) warning("Values < machine double eps converted to double eps")
    pvals[lteps] <- eps
  }
  pvals
} # rtemis::get_gam_pvals
