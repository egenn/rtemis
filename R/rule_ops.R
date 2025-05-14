# Rule operations

#' Prune a rule to a maximum length
#'
#' @param rule Character: A rule.
#' @param max_length Integer: The maximum number of conditions to keep.
#' @param sep Character: The separator between conditions.
#'
#' @return Character: The pruned rule.
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

simple_prune_ <- function(rule, max_length, sep = " & ") {
  conditions <- strsplit(rule, sep)[[1]]
  if (length(conditions) > max_length) {
    conditions <- conditions[1:max_length]
    paste(conditions, collapse = sep)
  } else {
    rule
  }
} # rtemis::simple_prune_


#' Prune rules to a maximum length
#'
#' @param rule Character vector: Rules.
#' @param max_length Integer: The maximum number of conditions to keep.
#' @param sep Character: The separator between conditions.
#'
#' @return Character: The pruned rule.
#' @author E.D. Gennatas
#' @keywords internal
#' @noRd

simple_prune <- function(rules, max_length, sep = " & ") {
  rules <- sapply(
    rules,
    simple_prune_,
    max_length = max_length,
    sep = sep,
    USE.NAMES = FALSE
  )
  rules
} # rtemis::simple_prune


#' Extract variable names from rules
#'
#' @param rules Character vector: Rules.
#' @param unique Logical: If TRUE, return only unique variables.
#'
#' @return Character vector: Variable names.
#' @author E.D. Gennatas
#' @export

get_vars_from_rules <- function(rules, unique = FALSE) {
  # Extract variables from rules
  vars <- unique(unlist(strsplit(rules, " & ")))
  # Get string up to first "<", ">", "=", "!", or "%in%"
  vars <- gsub("(<|>|=|!|%in%).*", "", vars)
  vars <- gsub(" .*", "", vars)
  if (unique) {
    vars <- unique(vars)
  }
  vars
}

#' Calculate variable statistics from rules
#'

# N times variable appears in each rule divided by N variables,
# averaged across C rules
# rule_varstats
