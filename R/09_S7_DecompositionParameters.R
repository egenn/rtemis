# S7_DecompositionParameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# DecompositionParameters ----
#' @title DecompositionParameters
#'
#' @description
#' Decomposition parameters class.
#'
#' @field algorithm Character: Algorithm name.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
DecompositionParameters <- new_class(
  name = "DecompositionParameters",
  properties = list(
    algorithm = class_character,
    parameters = class_list
  )
) # /DecompositionParameters

# Make DecompositionParameters@parameters `$`-accessible
method(`$`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# Make DecompositionParameters@parameters `[[`-accessible
method(`[[`, DecompositionParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete DecompositionParameters@parameters ----
.DollarNames.DecompositionParameters <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}
