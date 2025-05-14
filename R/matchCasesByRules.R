# matchCasesByRules.R
# ::rtemis::
# 2017 E.D. Gennatas rtemis.org

#' Match Rules to Cases
#'
#' @param x Matrix / data frame: Input features
#' @param rules Character vector: Rules
#' @param verbose Logical: If TRUE, print messages to console
#'
#' @author E.D. Gennatas
#'
#' @return cases-by-rules matrix (binary; 1: match, 0: no match)
#' @keywords internal
#' @noRd

matchCasesByRules <- function(x, rules, verbose = TRUE) {
  n_cases <- NROW(x)
  n_rules <- length(rules)
  if (!is.data.table(x)) x <- data.table::as.data.table(x)
  # appease R CMD check
  ID <- NULL
  x[, ID := seq_len(n_cases)]
  cxr <- matrix(0, n_cases, n_rules)
  if (verbose) msg2start("Matching", n_rules, "rules to", n_cases, "cases...")
  for (i in seq_along(rules)) {
    match <- x[eval(parse(text = rules[i])), ID]
    cxr[match, i] <- 1
  }
  if (verbose) msg2done()

  cxr
} # rtemis::matchCasesByRules

#' Index cases by rules
#'
#' Get an index of which cases match which rule - meant for cases where each case matches one rule
#' and one rule only
#'
#' @inheritParams matchCasesByRules
#'
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

indexCasesByRules <- function(x, rules, verbose = TRUE) {
  cxr <- matchCasesByRules(x, rules, verbose)
  apply(cxr, 1, \(i) which(i == 1))
}
