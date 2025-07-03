# S7_MassUni.R
# ::rtemis::
# 2025 EDG rtemis.org

# MassGLM ----
#' @title MassGLM
#'
#' @description
#' Superclass for mass-univariate models.
#'
#' @author EDG
#' @noRd
MassGLM <- new_class(
  name = "MassGLM",
  properties = list(
    summary = class_data.table,
    xnames = class_character,
    ynames = class_character,
    family = class_character
  )
) # /rtemis::MassGLM

# Print MassGLM ----
#' Print MassGLM
#'
#' @param x MassGLM object
#' @param ... Not used
#'
#' @author EDG
#' @noRd
print.MassGLM <- function(x, ...) {
  cat(gray(".:"))
  objcat("MassGLM")
  cat(
    hilite(length(x@ynames)),
    "GLMs of family",
    bold(x@family),
    "with",
    hilite(length(x@xnames)),
    "predictors each\n"
  )
} # /rtemis::print.MassGLM

method(print, MassGLM) <- print.MassGLM

# Plot MassGLM ----
#' Plot MassGLM using volcano plot
#'
#' @param x MassGLM object
#' @param xname Character: Name of covariate to get data for. If `NULL`, the first covariate is used.
#' @param theme Theme object
#' @param ... Additional arguments passed to [draw_volcano] or [draw_bar]
#'
#' @author EDG
#' @export
plot.MassGLM <- function(
  x,
  xname = NULL,
  theme = choose_theme(),
  ...
) {
  if (is.null(xname)) {
    xname <- x@xnames[1]
  }
  if (!xname %in% x@xnames) {
    stop("xname must be one of the xnames in the MassGLM object.")
  }

  # Plot ----
  coefs <- x@summary[[paste0("Coefficient_", xname)]]
  pvals <- x@summary[[paste0("p_value_", xname)]]
  draw_volcano(
    x = coefs,
    pvals = pvals,
    theme = theme,
    ...
  )
} # /rtemis::plot.MassGLM

method(plot, MassGLM) <- plot.MassGLM
