# matchCasesByRules.R
# ::rtemis::
# 2017 Efstathios D. Gennatas egenn.github.io

#' Match Rules to Cases
#'
#' @param x Matrix / data frame: Input features
#' @param rules Vector, string: Rules (MUST be string, not factor)
#' @param verbose Logical: If TRUE, print messages to console
#' @author Efstathios D. Gennatas
#' @export
#' @return cases-by-rules matrix (binary; 1: match, 0: no match)

matchCasesByRules <- function(x, rules, verbose = TRUE) {

  n.cases <- NROW(x)
  n.rules <- length(rules)
  x <- data.table::as.data.table(x)
  x$ID <- seq(n.cases)
  # rules <- gsub("&&", "&", rules)
  cxr <- matrix(0, n.cases, n.rules)
  if (verbose) msg("Matching", n.rules, "rules to", n.cases, "cases...")
  for (i in seq(rules)) {
    match <- x[eval(parse(text = rules[i]))]$ID
    cxr[match, i] <- 1
  }

  cxr

} # rtemis::matchCasesByRules

#' Index cases by rules
#'
#' Get an index of which cases match which rule - meant for cases where each case matches one rule
#' and one rule only
#'
#' @inheritParams matchCasesByRules
#' @author Efstathios D. Gennatas
#' @keywords internal

indexCasesByRules <- function(x, rules, verbose = TRUE) {
  cxr <- matchCasesByRules(x, rules, verbose)
  apply(cxr, 1, function(i) which(i == 1))
}
