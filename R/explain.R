# explain.R
# ::rtemis::
# 2023 EDG rtemis.org

#' Explain individual-level model predictions
#'
#' @param mod `rtMod` object.
#' @param x Single-row data.frame of predictors.
#' @param digits Integer: Number of digits to round coefficients to.
#' @param top Integer: Number of top rules to show by absolute coefficient.
#' @param trace Integer: If > 0, print more messages to output.
#'
#' @author ED Gennatas
#' @export

explain <- function(mod, x, digits = 2, top = NULL, trace = 0) {
  stopifnot(inherits(mod, "rtMod"))
  stopifnot(mod$mod.name %in% c("RuleFit", "LightRuleFit"))
  stopifnot(inherits(x, "data.frame"))
  if (nrow(x) > 1) {
    stop("x must be a single row")
  }
  pred <- predict(mod$mod, x, return.cases.by.rules = TRUE, verbose = trace > 0)
  rules <- mod$mod$lgbm_rules
  coefs <- coef(mod$mod$mod_glmnet_select$mod)[-1, 1]
  stopifnot(length(rules) == length(coefs))
  idi <- which(as.logical(pred$cases.by.rules[1, ]))
  rules_applied <- rules[idi]
  coefs_applied <- coefs[idi]
  expl <- data.table(Rules = rules_applied, Coefficients = coefs_applied)
  Coefficients <- AbsCoefficients <- NULL # appease R CMD check
  expl <- expl[Coefficients != 0]
  expl[, AbsCoefficients := abs(Coefficients)]
  setorder(expl, -AbsCoefficients)
  expl[, AbsCoefficients := NULL]
  txt <- paste(
    bold(nrow(expl)),
    "out of",
    bold(length(mod$mod$rules_selected)),
    "rules apply to this case.\n"
  )
  if (is.null(top) || top > nrow(expl)) {
    txt <- paste0(txt, "By decreasing absolute coefficient, they are:\n")
    top <- nrow(expl)
  } else {
    txt <- paste0(
      txt,
      "The top ",
      top,
      " rules by decreasing absolute coefficient are:\n"
    )
  }
  col <- c(green, red)[ifelse(expl$Coefficients[1:top] > 0, 1, 2)]
  rule_coef_txt <- paste(
    sapply(expl$Coefficients[1:top], \(x) {
      if (x > 0) {
        green(
          paste0("+", format(round(x, digits = digits), nsmall = digits)),
          bold = TRUE
        )
      } else {
        red(format(round(x, digits = digits), nsmall = digits), bold = TRUE)
      }
    }),
    expl$Rules[1:top],
    collapse = "\n"
  )
  txt <- paste0(txt, rule_coef_txt)
  cat(txt, "\n")
} # rtemis::explain
