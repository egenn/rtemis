# S7_Decomposition.R
# ::rtemis::
# 2025 EDG rtemis.org

# Decomposition ----
#' @title Decomposition
#'
#' @description
#' Decomposition class.
#'
#' @field algorithm Character: Algorithm name.
#' @field decom Any: Decomposition object.
#' @field parameters List: Algorithm-specific parameters.
#' @field decom: Decomposition model.
#' @field transformed: transformedransformed data, i.e. either a projection or an embedding of the input data.
#'
#' @author EDG
Decomposition <- new_class(
  name = "Decomposition",
  properties = list(
    algorithm = class_character,
    parameters = DecompositionParameters,
    decom = class_any,
    transformed = class_any
  )
) # /Decomposition

# Make Decomposition properties `$`-accessible
method(`$`, Decomposition) <- function(x, name) {
  prop_names <- names(props(x))
  if (name %in% prop_names) {
    prop(x, name)
  } else {
    stop(paste0("No property named '", name, "' in Decomposition object."))
  }
}
method(`.DollarNames`, Decomposition) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}

# Make Decomposition@decom `[[`-accessible
method(`[[`, Decomposition) <- function(x, index) {
  props(x, "decom")[[index]]
}

# Print Decomposition ----
method(print, Decomposition) <- function(x, pad = 0L) {
  cat(gray(".:"))
  objcat(paste(x@algorithm, "Decomposition"), pad = pad)
  cat("\n")
  printls(props(x)[-1], pad = pad)
  invisible(x)
}
