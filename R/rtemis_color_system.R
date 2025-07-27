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

# Emojis
# wave <- "\U1F30A"
# mountain <- "\U26F0\UFE0F"
# alien <- "\U1F47D"

# rt console colors
MediumSpringGreen <- "49"
Cyan2 <- "50"
CornflowerBlue <- "69"
MediumOrchid3 <- "133"
MediumOrchid <- "134"
SteelBlue1 <- "75"
SlateBlue1 <- "99"
MediumPurple <- "104"
LightSlateBlue <- "105"
SkyBlue2 <- "111"
Magenta3 <- "164"
MediumOrchid1 <- "171"
Violet <- "177"
DarkOrange <- "208"
Turquoise4 <- "30"
DarkCyan <- "36"

hilite_col <- DarkCyan # green, really
hilite1_col <- SteelBlue1
hilite2_col <- DarkOrange # info
hilite3_col <- Magenta3 # warning

# Used for general highlighting + check marks
rt_green <- col_info <- highlight2_col <- "#00ffb3" # kaimana green
# Used to print S7 class names - beautiful red for structural elements
col_object <- "#ff004c" # kaimana red
# Used to highlight sections & info related to outer resampling
col_outer <- "#00b2b2" # rtemis teal
# Used to highlight sections & info related to hyperparameter tuning
rt_orange <- col_tuner <- "#ff9f20" # genlib orange
# Used for info messages
rt_blue <- highlight_col <- "#479cff" # kaimana light blue
