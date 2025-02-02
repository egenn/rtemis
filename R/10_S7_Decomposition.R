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
#' @field xnames Character vector: feature names.
#' @field parameters List: Algorithm-specific parameters.
#'
#' @author EDG
Decomposition <- new_class(
  name = "Decomposition",
  properties = list(
    algorithm = class_character,
    decom = class_any,
    xnames = class_character,
    parameters = DecompositionParameters
  )
) # /Decomposition
