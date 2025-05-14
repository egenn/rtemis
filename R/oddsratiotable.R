# oddsratiotable.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Odds ratio table from logistic regression
#'
#' @param x [glm] object fit with `family = binomial`
#' @param confint.method "default" or "profilelikelihood"
#'
#' @return matrix with 4 columns: OR, 2.5% & 97.5% CI, p_val
#'
#' @author E.D. Gennatas
#' @export
#'
#' @examples
#' \dontrun{
#' ir2 <- iris[51:150, ]
#' ir2$Species <- factor(ir2$Species)
#' ir.fit <- glm(Species ~ ., data = ir2, family = binomial)
#' oddsratiotable(ir.fit)
#' }

oddsratiotable <- function(
  x,
  confint.method = c("default", "profilelikelihood")
) {
  confint.method <- match.arg(confint.method)
  .cs <- coef(summary(x))
  if (confint.method == "default") {
    cbind(
      OR = exp(.cs[, "Estimate"]),
      exp(confint.default(x)),
      p_val = .cs[, 4]
    )
  } else {
    cbind(OR = exp(.cs[, "Estimate"]), exp(confint(x)), p_val = .cs[, 4])
  }
}
