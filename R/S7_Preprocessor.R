# S7_Preprocessor.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# preprocessor_args <- c(
#   "factor2integer",
#   "scale",
#   "center",
#   "ifw",
#   "ifw_type",
#   "upsample",
#   "downsample",
#   "resample_seed"
# )

# Preprocessor ----
#' @title Preprocessor
#'
#' @description
#' Preprocessor class.
#'
#' @author EDG
#' @export
Preprocessor <- new_class(
  name = "Preprocessor",
  properties = list(
    complete_cases = class_logical,
    remove_features_thres = class_numeric | NULL,
    remove_cases_thres = class_numeric | NULL,
    missingness = class_logical,
    impute = class_logical,
    impute_type = class_character,
    impute_missRanger_params = class_list,
    impute_discrete = class_function,
    impute_numeric = class_function,
    integer2factor = class_logical,
    integer2numeric = class_logical,
    logical2factor = class_logical,
    logical2numeric = class_logical,
    numeric2factor = class_logical,
    numeric2factor_levels = class_character | NULL,
    numeric_cut_n = class_numeric,
    numeric_cut_labels = class_logical,
    numeric_quant_n = class_numeric,
    numeric_quant_NAonly = class_logical,
    len2factor = class_numeric,
    character2factor = class_logical,
    factorNA2missing = class_logical,
    factorNA2missing_level = class_character,
    factor2integer = class_logical,
    factor2integer_startat0 = class_logical,
    scale = class_logical,
    center = class_logical,
    remove_constants = class_logical,
    remove_constants_skip_missing = class_logical,
    remove_duplicates = class_logical,
    oneHot = class_logical,
    add_date_features = class_logical,
    date_features = class_character,
    add_holidays = class_logical,
    exclude = class_character | NULL
  )
) # /Preprocessor

# Names Preprocessor ----
method(names, Preprocessor) <- function(x) {
  names(props(x))
}

# Make props `$`-accessible ----
method(`$`, Preprocessor) <- function(x, name) {
  props(x)[[name]]
}

# DollarSign tab-complete property names ----
method(`.DollarNames`, Preprocessor) <- function(x, pattern = "") {
  all_names <- names(props(x))
  grep(pattern, all_names, value = TRUE)
}

# Make proprs `[[`-accessible ----
method(`[[`, Preprocessor) <- function(x, name) {
  props(x)[[name]]
}

#' Print `Preprocessor` object
#'
#' @param x `Preprocessor` object.
#' @param ... Ignored.
#'
#' @export
print.Preprocessor <- function(x, ...) {
  objcat("Preprocessor")
  printls(props(x))
}
method(print, Preprocessor) <- function(x, ...) {
  print.Preprocessor(x, ...)
}

#' Create a `Preprocessor` object
#'
#' @param factor2integer Logical: If TRUE, convert factors to integers.
#' @param scale Logical: If TRUE, scale numeric features.
#' @param center Logical: If TRUE, center numeric features.
#' @param ifw Logical: If TRUE, return class weights for inverse frequency
#' weighting for Classification.
#' @param ifw_type {1, 2}: Type of inverse frequency weighting for Classification.
#' @param upsample Logical: If TRUE, upsample minority class to match size of majority class.
#' @param downsample Logical: If TRUE, downsample majority class to match size of minority class.
#' @param resample_seed Integer: Seed for resampling.
#'
#' @author EDG
#' @export
setup_Preprocessor <- function(
    complete_cases = FALSE,
    remove_features_thres = NULL,
    remove_cases_thres = NULL,
    missingness = FALSE,
    impute = FALSE,
    impute_type = c(
      "missRanger",
      "micePMM",
      "meanMode"
    ),
    impute_missRanger_params = list(
      pmm.k = 3,
      maxiter = 10,
      num.trees = 500
    ),
    impute_discrete = get_mode,
    impute_numeric = mean,
    integer2factor = FALSE,
    integer2numeric = FALSE,
    logical2factor = FALSE,
    logical2numeric = FALSE,
    numeric2factor = FALSE,
    numeric2factor_levels = NULL,
    numeric_cut_n = 0,
    numeric_cut_labels = FALSE,
    numeric_quant_n = 0,
    numeric_quant_NAonly = FALSE,
    len2factor = 0,
    character2factor = FALSE,
    factorNA2missing = FALSE,
    factorNA2missing_level = "missing",
    #    nonzeroFactors = FALSE,
    factor2integer = FALSE,
    factor2integer_startat0 = TRUE,
    scale = FALSE,
    center = scale,
    remove_constants = FALSE,
    remove_constants_skip_missing = TRUE,
    remove_duplicates = FALSE,
    oneHot = FALSE,
    #    cleanfactorlevels = FALSE,
    add_date_features = FALSE,
    date_features = c("weekday", "month", "year"),
    add_holidays = FALSE,
    exclude = NULL) {
  Preprocessor(
    complete_cases = complete_cases,
    remove_features_thres = remove_features_thres,
    remove_cases_thres = remove_cases_thres,
    missingness = missingness,
    impute = impute,
    impute_type = impute_type,
    impute_missRanger_params = impute_missRanger_params,
    impute_discrete = impute_discrete,
    impute_numeric = impute_numeric,
    integer2factor = integer2factor,
    integer2numeric = integer2numeric,
    logical2factor = logical2factor,
    logical2numeric = logical2numeric,
    numeric2factor = numeric2factor,
    numeric2factor_levels = numeric2factor_levels,
    numeric_cut_n = numeric_cut_n,
    numeric_cut_labels = numeric_cut_labels,
    numeric_quant_n = numeric_quant_n,
    numeric_quant_NAonly = numeric_quant_NAonly,
    len2factor = len2factor,
    character2factor = character2factor,
    factorNA2missing = factorNA2missing,
    factorNA2missing_level = factorNA2missing_level,
    factor2integer = factor2integer,
    factor2integer_startat0 = factor2integer_startat0,
    scale = scale,
    center = center,
    remove_constants = remove_constants,
    remove_constants_skip_missing = remove_constants_skip_missing,
    remove_duplicates = remove_duplicates,
    oneHot = oneHot,
    add_date_features = add_date_features,
    date_features = date_features,
    add_holidays = add_holidays,
    exclude = exclude
  )
} # /setup_Preprocessor
