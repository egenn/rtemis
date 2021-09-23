# rules2medmod
# ::rtemis::
# 2018 E.D. Gennatas lambdamd.org

#' \code{rtemis-internals}: Convert rules from cutoffs to median/mode and range
#'
#' Convert rules from cutoffs to \code{median (range)} and \code{mode (range)} format
#'
#' @param rules Character, vector: Input rules
#' @param x Data frame: Data to evaluate rules
#' @param .ddSci Logical: If TRUE, format all continuous variables using \link{ddSci}, which will
#' give either 2 decimal places, or scientific notation if two decimal places result in 0.00
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' @author E.D. Gennatas
#' @export

rules2medmod <- function(rules, x,
                         .ddSci = TRUE,
                         verbose = TRUE,
                         trace = 0) {

  cxr <- matchCasesByRules(x, rules, verbose = verbose)
  nrules <- length(rules)
  rules.f <- vector("character", nrules)
  frmt <- if (.ddSci) ddSci else I
  if (verbose) msg("Converting rules...")
  for (i in seq(rules)) {
    if (trace > 0) cat("#", i, "/", nrules, "...\n", sep = "")
    dat <- x[cxr[, i] == 1, ]
    sub <- strsplit(rules[i], "&")[[1]]
    rule <- character()
    for (j in seq(sub)) {
      categorical <- grepl("%in%", sub[j])
      if (categorical) {
        var <- gsub("\\s", "", strsplit(sub[j], "%in%")[[1]][1])
        vals <- dat[[var]]
        value <- paste0(getMode(vals), " (", paste(levels(droplevels(vals)), collapse = ", "), ")")
        rule[j] <- paste0(var, " = ", value)
      } else {
        sub[j] <- gsub(">|>=|<|<=", "@", sub[j])
        var <- gsub("\\s", "", strsplit(sub[j], "@")[[1]][1])
        vals <- dat[[var]]
        value <- paste0(frmt(median(vals)), " (", frmt(min(vals)), ":", frmt(max(vals)), ")")
        rule[j] <- paste0(var, " = ", value)
      }
    } # /loop through each rule's conditions
    # This consolidates conditions like a > 3 & a > 5 to one
    rules.f[i] <- paste(unique(rule), collapse = " & ")
  } # /loop through rules

  if (verbose) msg("Done")
  rules.f

} # rtemis::rules2medmod
