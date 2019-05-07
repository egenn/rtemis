# rtPalette.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

# Penn ====
#' rtemis Color Palettes
#'
#' \code{pennCol}: Penn color palette (http://www.upenn.edu/about/styleguide-color-type)
#' @name rtPalettes
#' @export

pennCol <- list(darkestBlue = "#000f3a",
                darkerBlue = "#00144d",
                blue = "#01256e",
                lighterBlue = "#045ea7",
                lightestBlue = "#82afd3",
                darkestRed = "#57000a",
                darkerRed = "#74000e",
                red = "#95001a",
                lighterRed = "#c2004d",
                lightestRed = "#e180a6",
                darkestYellow = "#af7f00",
                darkerYellow = "#eaa900",
                yellow = "#f2c100",
                lighterYellow = "#f8de00",
                lightestYellow = "#fcef80",
                darkestGreen = "#005200",
                darkerGreen = "#006e00",
                green = "#008e00",
                lighterGreen = "#00be00",
                lightestGreen = "#80df80",
                darkestOrange = "#812d00",
                darkerOrange = "#ac3c00",
                orange = "#c35a00",
                lighterOrange = "#df9700",
                lightestOrange = "#efcb80",
                darkestPurple = "#23001f",
                darkerPurple = "#2f0029",
                purple = "#4a0042",
                lighterPurple = "#890082",
                lightestPurple = "#c480c1")


#' \code{pennPalette}: Subset of \code{pennCol}. This is the default palette of the \link{mplot3} family
#'
#' @name rtPalettes
#' @export
pennPalette <- pennCol[c("lighterBlue", "red", "green", "yellow", "lighterPurple", "orange",
                         "lightestBlue", "lighterRed", "lighterGreen", "lightestPurple",
                         "lighterOrange")]

#' \code{pennLightPalette}: Subset of \code{pennCol}. This is the lighter Penn palette for use with the dark themes
#' @name rtPalettes
#' @export
pennLightPalette <- pennCol[c("lightestBlue", "lightestRed", "lightestGreen",
                              "lightestYellow", "lightestPurple")]


# Imperial ====
#' Imperial Colors
#'
#' \code{imperialCol}: Imperial College London color palette (https://www.imperial.ac.uk/brand-style-guide/visual-identity/brand-colours/)
#'
#' @name rtPalettes
#' @export

imperialCol <- list(navy = "#002147",
                    imperialBlue = "#003E74",
                    lightGrey = "#EBEEEE",
                    coolGrey = "#9D9D9D",
                    lightBlue = "#D4EFFC",
                    blue = "#006EAF",
                    processBlue = "#0091D4",
                    poolBlue = "#00ACD7",
                    darkTeal = "#0F8291",
                    teal = "#009CBC",
                    seaglass = "#379f9f",
                    darkGreen = "#02893B",
                    kermitGreen = "#66A40A",
                    lime = "#BBCE00",
                    orange = "#D24000",
                    tangerine = "#EC7300",
                    lemonYellow = "#FFDD00",
                    brick = "#A51900",
                    red = "#DD2501",
                    cherry = "#E40043",
                    raspberry = "#9F004E",
                    magentaPink = "#C81E78",
                    iris = "#751E66",
                    violet = "#960078",
                    plum = "#321E6D",
                    purple = "#653098")

# UCSF ====
#' UCSF Colors
#'
#' \code{ucsfCol}: UCSF color palette (http://identity.ucsf.edu/color)
#'
#' @name rtPalettes
#' @export
ucsfCol <- list(navy = "#052049",
                teal = "#18A3AC",
                green = "#90BD31",
                blue = "#178CCB",
                orange = "#F48024",
                purple = "#716FB2",
                red = "#EC1848",
                yellow = "#FFDD00",
                iTeal = "#058488",
                iGreen = "#6EA400",
                iBlue = "#007CBE",
                iOrange = "#F26D04",
                iRed = "#EB093C")

#' UCSF Color Palette
#'
#' \code{ucsfPalette}: Subset of \code{ucsfCol} for use with \link{mplot3}, etc
#'
#' @name rtPalettes
#' @export
# ucsfPalette <- ucsfCol[c("iTeal", "iRed", "iBlue", "yellow", "purple", "iOrange", "iGreen")]
ucsfPalette <- ucsfCol[c("teal", "orange", "blue", "yellow", "purple", "red", "navy", "green")]


# Berkeley ====
#' Berkeley Colors
#'
#' \code{berkeleyCol}: Berkeley color palette (https://brand.berkeley.edu/colors/)
#'
#' @name rtPalettes
#' @export

berkeleyCol <- list(berkeleyBlue = "#003262",
                    foundersRock = "#3B7EA1",
                    californiaGold = "#FDB515",
                    medalist = "#C4820E",
                    wellmanTile = "#D9661F",
                    roseGarden = "#EE1F60",
                    goldenGate = "#ED4E33",
                    southHall = "#6C3302",
                    bayFog = "#DDD5C7",
                    lawrence = "#00B0DA",
                    lapLane = "#00A598",
                    pacific = "#46535E",
                    satherGate = "#B9D3B6",
                    ion = "#CFDD45",
                    soyBean = "#859438",
                    stonePine = "#584F29",
                    grey = "#EEEEEE",
                    webGrey = "#888888")


# Stanford ====
#' Stanford Colors
#'
#' \code{stanfordCol}: Stanford color palette (https://identity.stanford.edu/color.html#digital-color)
#'
#' @name rtPalettes
#' @export

stanfordCol <- list(cardinal = "#8c1515",
                    coolGrey = "#4d4f53",
                    birghtRed = "#B1040E",
                    chocolate = "#2F2424",
                    stone = "#544948",
                    fog = "#F4F4F4",
                    lightSandstone = "#F9F6EF",
                    sandstone = "#d2c295",
                    warmGrey = "#3f3c30",
                    beige = "#9d9573",
                    lightSage = "#c7d1c5",
                    clay = "#5f574f",
                    cloud = "#dad7cb",
                    driftwood = "#b6b1a9",
                    sandhill = "#b3995d",
                    paloAlto = "#175e54",
                    teal = "#00505c",
                    purple = "#53284f",
                    redwood = "#8d3c1e",
                    brown = "#5e3032",
                    sky = "#0098db",
                    lagunita = "#007c92",
                    mint = "#009b76",
                    gold = "#b26f16",
                    sun = "#eaab00",
                    poppy = "#e98300")


# USF ====
#' USF Colors
#'
#' \code{usfCol}: USF color palette (https://myusf.usfca.edu/marketing-communications/resources/graphics-resources/brand-standards/color-palette)
#' Color conversions performed using https://www.pantone.com/color-finder/
#' @name rtPalettes
#' @export

usfCol <- list(green = "#205C40",
               yellow = "#ffb81c",
               gray = "#75787B")


# UC San Diego ====
#' UC San Diego Colors
#'
#' \code{ucsdCol}: UC San Diego color palette (https://ucpa.ucsd.edu/brand/elements/color-palette/)
#' @name rtPalettes
#' @export

ucsdCol <- list(blue = "#182B49",
                mediumBlue = "#006A96",
                gold = "#C69214",
                yellow = "#FFCD00",
                cyan = "#00C6D7",
                green = "#6E963B",
                lightYellow = "#F3E500",
                orange = "#FC8900",
                coolGray = "#747678",
                lightGray = "#B6B1A9",
                darkGold = "#84754E")


# UCLA ====
#' UCLA Colors
#'
#' \code{uclaCol}: UCLA color palette (http://brand.ucla.edu/identity/colors)
#' @name rtPalettes
#' @export

uclaCol <- list(blue = "#2774AE",
                gold = "#FFD100",
                darkestBlue = "#003B5C",
                darkerBlue = "#005587",
                lighterBlue = "#8BB8E8",
                lightestBlue = "#C3D7EE",
                darkestGold = "#FFB81C",
                darkerGold = "#FFC72C",
                yellow = "#FFFF00",
                green = "#00FF87",
                magenta = "#FF00A5",
                cyan = "#00FFFF",
                purple = "#8237FF")


# University of California ====
#' University of California Colors
#'
#' \code{ucCol}: University of California color palette
#' (http://brand.universityofcalifornia.edu/guidelines/color.html#!primary-colors)
#' @name rtPalettes
#' @export

ucCol <- list(ucBlue = "#1295D8",
              ucGold = "#FFB511",
              blue = "#005581",
              lightBlue = "#72CDF4",
              gold = "#FFD200",
              lightgold = "#FFE552",
              orange = "#FF6E1B",
              lightOrange = "#FF8F28",
              pink = "#E44C9A",
              lightPink = "#FEB2E0",
              teal = "#00778B",
              lightTeal = "#00A3AD",
              ucGray = "#7C7E7F",
              warmGray8 = "#8F8884",
              warmGray3 = "#BEB6AF",
              warmGray1 = "#DBD5CD",
              metallicGold = "#B4975A")


# Washington ====
#' University of Washington Colors
#'
#' \code{uwCol}: University of Washington color palette
#' (http://www.washington.edu/brand/graphic-elements/primary-color-palette/)
#' @name rtPalettes
#' @export

uwCol <- list(purple = "#4b2e83",
              gold = "#b7a57a",
              metallicGold = "#85754d")


# NIH ====
#' NIH Colors
#'
#' \code{nihCol}: NIH color palette (https://www.nlm.nih.gov/about/nlm_logo_guidelines_030414_508.pdf)
#' @name rtPalettes
#' @export

nihCol <- list(blue = "#20558a",
               gray = "#616265")


# Apple ====
#' Apple Colors
#'
#' \code{appleCol}: Apple Human Interface Guidelines color palette
#' (https://developer.apple.com/design/human-interface-guidelines/ios/visual-design/color/)
#' @name rtPalettes
#' @export

appleCol <- list(red = "#FF3B30",
                 orange = "#FF9500",
                 yellow = "#FFCC00",
                 green = "#4CD964",
                 tealBlue = "#5AC8FA",
                 blue = "#007AFF",
                 purple = "#5856D6",
                 pink = "#FF2D55")


# Google ====
#' Google Colors
#'
#' \code{googleCol}: Google brand palette (https://brandpalettes.com/google-colors/)
#' @name rtPalettes
#' @export

googleCol <- list(blue = "#4285F4",
                  red = "#DB4437",
                  yellow = "#F4B400",
                  green = "#0F9D58")

# Amazon ====
#' Amazon Colors
#'
#' \code{amazonCol}: Amazon brand palette
#' (https://images-na.ssl-images-amazon.com/images/G/01/AdvertisingSite/pdfs/AmazonBrandUsageGuidelines.pdf)
#' @name rtPalettes
#' @export

amazonCol <- list(orange = "#FF9900",
                  blue = "#146EB4")


# Microsoft ====
#' Microsoft Colors
#'
#' \code{microsoftCol}: Microsoft brand palette
#' (https://brandcolors.net/b/microsoft)
#' @name rtPalettes
#' @export

microsoftCol <- list(orange = "#f65314",
                     green = "#7cbb00",
                     blue = "#00a1f1",
                     yellow = "#ffbb00")

rtCol <- c(ucsfCol, berkeleyCol)

rtPalettes <- list(rtCol = rtCol,
                   pennCol = pennCol,
                   imperialCol = imperialCol,
                   ucsfCol = ucsfCol,
                   ucsfPalette = ucsfPalette,
                   berkeleyCol = berkeleyCol,
                   stanfordCol = stanfordCol,
                   usfCol = usfCol,
                   ucsdCol = ucsdCol,
                   uclaCol = uclaCol,
                   uwCol = uwCol,
                   nihCol = nihCol,
                   appleCol = appleCol,
                   googleCol = googleCol,
                   amazonCol = amazonCol,
                   microsoftCol = microsoftCol)

#' \pkg{rtemis} Color Palettes
#'
#' \code{rtPalettes} prints names of available color palettes
#' Each palette is a named list of hexadecimal color definitions which can be used with any
#' graphics function.
#' @param palette String: Name of palette to return. Default = NULL: available palette names
#' are printed and no palette is returned
#' @return
#' A list of available palettes, invisibly
#' @examples
#' rtPalette()
#' @export

rtPalette <- function(palette = NULL) {

  if (is.null(palette)) {
    msg(crayon::cyan("The following palettes are available:"))
    print(paste(c("imperialCol", "pennCol", "stanfordCol", "ucCol", "ucsfCol", "berkeleyCol",
                   "ucsdCol", "uclaCol", "usfCol", "uwCol", "nihCol", "appleCol", "googleCol",
                  "amazonCol", "microsoftCol", "rtCol")))
  } else {
    palette <- match.arg(palette,
                         c("imperialCol", "pennCol", "stanfordCol", "ucCol", "ucsfCol", "berkeleyCol",
                           "ucsdCol", "uclaCol", "usfCol", "uwCol", "nihCol", "appleCol", "googleCol",
                           "amazonCol", "microsoftCol", "rtCol"))
    rtPalettes[[palette]]
  }

} # rtemis::rtPalettes


# Custom crayon styles ====
teal <- make_style(teal = "#18A3AC")
rtBlue <- make_style(rtBlue = "#005581")
rtOrange <- make_style(rtOrange = "#F48024")
rtHighlight.color <- getOption("rt.highlight.color", "#18A3AC")
rtHighlight <- make_style(rtHighlight = rtHighlight.color)
