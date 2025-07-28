# rtemis color system
# ::rtemis::
# 2025 EDG rtemis.org

# Violet: Class names (structure)
# Blue: Outer resampling (evaluation)
# Orange: Hyperparameter tuning (optimization)
# Green: Model training + important highlights (execution)
# Cyan: Info messages (communication)

# References
# ANSI escape codes
# https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
# Xterm color names: https://jonasjacek.github.io/colors/
# CSS color keywords: https://www.uxgem.com/docs/css-color-keywords
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html
# UTF-8 icons: https://www.utf8icons.com/

#' rtemis Color System
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
rtemis_teal <- "#00b2b2"
kaimana_red <- "#ff004c"
kaimana_light_blue <- "#479cff"
genlib_orange <- "#ff9f20"
kaimana_green <- "#00ffb3"
rtemis_purple <- "#6125f7"
rtemis_magenta <- "#912ac8"

rt_red <- kaimana_red
rt_blue <- kaimana_light_blue
rt_green <- kaimana_green
rt_orange <- genlib_orange
rt_teal <- rtemis_teal
rt_purple <- rtemis_purple
rt_magenta <- rtemis_magenta

col_object <- rt_green
highlight_col <- rt_green
col_info <- highlight2_col <- rt_blue
# Used to highlight sections & info related to outer resampling
col_outer <- rt_red
# Used to highlight sections & info related to hyperparameter tuning
col_tuner <- rt_orange # genlib orange

# rtlogo2 purple "#6125f7", green "#19f0be"
