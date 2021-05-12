# rtPalette.R
# ::rtemis::
# 2016-9 E.D. Gennatas lambdamd.org
# TODO: consider including all in one big list

# Penn ====
#' rtemis Color Palettes
#'
#' \code{pennCol}: Penn color palette
#' (http://www.upenn.edu/about/styleguide-color-type)
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


#' \code{pennPalette}: Subset of \code{pennCol}.
#'
#' @name rtPalettes
#' @export
pennPalette <- pennCol[c("lighterBlue", "red", "green", "yellow", "lighterPurple", "orange",
                         "lightestBlue", "lighterRed", "lighterGreen", "lightestPurple",
                         "lighterOrange")]

#' \code{pennLightPalette}: Subset of \code{pennCol}. This is the lighter Penn palette for use with
#' the dark themes
#' @name rtPalettes
#' @export
pennLightPalette <- pennCol[c("lightestBlue", "lightestRed", "lightestGreen",
                              "lightestYellow", "lightestPurple")]


# Imperial ====
#' Imperial Colours
#'
#' \code{imperialCol}: Imperial College London colour palette
#' (https://www.imperial.ac.uk/brand-style-guide/visual-identity/brand-colours/)
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


# Stanford ====
#' Stanford Colors
#'
#' \code{stanfordCol}: Stanford color palette
#' (https://identity.stanford.edu/color.html#digital-color)
#'
#' @name rtPalettes
#' @export

stanfordCol <- list(cardinal = "#8c1515",
                    coolGrey = "#4d4f53",
                    black = "#2e2d29",
                    brightRed = "#B1040E",
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

ucsfPalette <- ucsfCol[c("teal", "orange", "blue", "yellow", "purple",
                         "red", "navy", "green")]


# UC Davis ====
#' UC Davis Colors
#'
#' \code{ucdCol}: UC Davis color palette
#' (https://marketingtoolbox.ucdavis.edu/visual-identity/color.html)
#' @name rtPalettes
#' @export

ucdCol <- list(davisBlue = "#002855",
               davisGold = "#DAAA00",
               unitransRed = "#BA0C2F",
               westernRedbud = "#C6007E",
               californiaPoppy = "#ED8B00",
               goldenLupine = "#FFCD00",
               sunnyGrass = "#78BE20",
               skyBlue = "#00B5E2",
               recPoolBlue = "#008EAA",
               wineGrape = "#642667",
               muBrick = "#C26E60",
               hartHallStucco = "#E6A65D",
               sageGreen = "#9CAF88",
               evergreen = "#00573F",
               winterSkyGray = "#5B7F95",
               centennialWalkGray = "#B1B3B3",
               corkOak = "#ACA39A",
               southHallShingleBrown = "#4F2C1D")


# Berkeley ====
#' Berkeley Colors
#'
#' \code{berkeleyCol}: Berkeley color palette
#' (https://brand.berkeley.edu/colors/)
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


# UC Santa Cruz ====
#' UC Santa Cruz Colors
#'
#' \code{ucscCol}: UC Santa Cruz color palette
#' (https://communications.ucsc.edu/visual-design/color/)
#'
#' @name rtPalettes
#' @export

ucscCol <- list(blue = "#003c6c",
                gold = "#fdc700",
                mediumBlue = "#006aad",
                lightBlue = "#13a5dc",
                teal = "#007988",
                orange = "#f29813",
                yellow = "#ffbf00",
                green = "#93c02d",
                rubineRed = "#da216d")


# UC Merced ====
#' UC Merced Colors
#'
#' \code{ucmercedCol}: UC Merced color palette
#' (https://publicrelations.ucmerced.edu/color-guidelines)
#'
#' @name rtPalettes
#' @export

ucmercedCol <- list(mercedRiverBlue = "#092f44",
                    foothillsGold = "#a29061",
                    sierraSkyBlue = "#5f8498",
                    wildflowerBabyBlueEyes = "#2980b9",
                    yosemiteSnowWhite = "#F8F5EC",
                    halfDomeSlate = "#5B5B5B",
                    mercedRyeGreen = "#235B16")


# UC Santa Barbara ====
#' UC Santa Barbara Colors
#'
#' \code{ucsbCol}: UC Santa Barbara color palette
#' (https://www.ucsb.edu/visual-identity/color)
#' @name rtPalettes
#' @export

ucsbCol <- list(navy = "#003660",
                gold = "#FEBC11",
                aqua = "#04859B",
                moss = "#7A8D39",
                seaGreen = "#0BA89A",
                coral = "#EF5645",
                mist = "#9CBEBE",
                clay = "#DCD6CC",
                sandstone = "#C9BF9D",
                lightGray = "#DCE1E5")


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


# UC Riverside ====
#' UC Riverside Colors
#'
#' \code{ucrCol}: UC Riverside color palette (https://brand.ucr.edu/ucr-colors)
#' @name rtPalettes
#' @export

ucrColor <- list(ucrBlue = "#2d6cc0",
                 ucrGold = "#f1ab00",
                 ucrGray = "#393b41")


# UCI ====
#' UCI Colors
#'
#' \code{uciCol}: UCI color palette (https://communications.uci.edu/campus-resources/graphic-standards/colors.php)
#' @name rtPalettes
#' @export

uciCol <- list(blue = "#0064a4",
               yellow = "#ffd200",
               teal = "#6aa2b8",
               lightGray = "#c6beb5",
               navy = "#1b3d6d",
               orange = "#f78d2d",
               darkGray = "#555759",
               lightYellow = "#f7eb5f")


# UC San Diego ====
#' UC San Diego Colors
#'
#' \code{ucsdCol}: UC San Diego color palette
#' (https://ucpa.ucsd.edu/brand/elements/color-palette/)
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


# University of California ====
#' University of California Colors
#'
#' \code{californiaCol}: University of California color palette
#' (http://brand.universityofcalifornia.edu/guidelines/color.html#!primary-colors)
#' @name rtPalettes
#' @export

californiaCol <- list(ucBlue = "#1295D8",
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

# California State University ====
#' California State University Colors
#'
#' \code{csuCol}: California State University color palette
#' (https://www2.calstate.edu/csu-system/csu-branding-standards/Documents/CSU-Brand-Guidelines-8-2018.pdf)
#' @name rtPalettes
#' @export

csuCol <- list(red = "#CC0B2A",
               coolGray = "#D9D9D6",
               black = "#2F2F2F")


# California Polytechnic State University ====
#' California Polytechnic State University Colors
#'
#' \code{calpolyCol}: Cal Poly color palette
#' (https://universitymarketing.calpoly.edu/brand-guidelines/colors/)
#' @name rtPalettes
#' @export

calpolyCol <- list(calpolygreen = "#154734",
                   calpolygold = "#C69214",
                   stadiumgold = "#F8E08E",
                   polycanyon = "#F2C75C",
                   dextergreen = "#A4D65E",
                   farmersmarket = "#3A913F",
                   skyblue = "#B5E3D8",
                   surfblue = "#5CB8B2",
                   serenity = "#D3E3F4",
                   morroblue = "#ABCAE9",
                   missionbeige = "#E4E1D1",
                   pismosand = "#CAC7A7",
                   coastsage = "#B6CCC2",
                   sycamore = "#789F90",
                   kennedygray = "#8E9089",
                   sealgray = "#54585A",
                   heritageorange = "#FF6A39",
                   avodaco = "#D0DF00")

# Caltech ====
#' Caltech Colors
#'
#' \code{caltechCol}: Caltech color palette (http://identity.caltech.edu/colors)
#' @name rtPalettes
#' @export

caltechCol <- list(orange = "#FF6C0C",
                   coolGray9 = "#76777B",
                   coolGray3c = "#C8C8C8",
                   pms414 = "#AAA99F",
                   pms5497c = "#849895",
                   pms7494c = "#9DAE88",
                   pms451c = "#C7B784",
                   pms7403c = "#F1D384",
                   pms548c = "#003B4C",
                   pms3292c = "#005851",
                   pms668c = "#644B78",
                   pms195c = "#7A303F",
                   pms186c = "#CF0A2C",
                   pms299c = "#00A1DF",
                   pms7473c = "#1E988A",
                   pms7489c = "#73A950",
                   pms7408c = "#F9BE00",
                   pms605c = "#E2CC00",
                   pms1915c = "#F54D80")


# CMU ====
#' CMU Colors
#'
#' \code{cmuCol}: Carnegie Mellon color palette
#' (https://www.cmu.edu/marcom/brand-standards/web-standards.html#colors)
#' @name rtPalettes
#' @export

cmuCol <- list(cmuRed = "#bb0000",
               gray = "#e0e0e0",
               darkGray = "#666666",
               gold = "#aa6600",
               teal = "#006677",
               blue = "#224477",
               green = "#008855",
               darkGreen = "#224433")


# MIT ====
#' MIT Colors
#'
#' \code{mitCol}: MIT color palette
#' (http://web.mit.edu/graphicidentity/colors.html)
#' @name rtPalettes
#' @export

mitCol <- list(red = "#A31F34",
               gray = "#8A8B8C",
               lightGray = "#C2C0BF")


# Princeton ====
#' Princeton Colors
#'
#' \code{princetonCol}: Princeton color palette
#' (https://communications.princeton.edu/guides-tools/logo-graphic-identity)
#' @name rtPalettes
#' @export

princetonCol <- list(orangeOnWhite = "#e77500",
                     orangeOnBlack = "#f58025")


# Columbia ====
#' Columbia Colors
#'
#' \code{columbiaCol}: Columbia color palette
#' (https://visualidentity.columbia.edu/content/web-0)
#' @name rtPalettes
#' @export

columbiaCol <- list(blue = "#000d74",
                    blue1 = "#C4D8E2",
                    blue2 = "#75AADB",
                    blue3 = "#6CADDF",
                    blue4 = "#008EE0",
                    blue5 = "#2C6BAC",
                    blue6 = "#0046A6",
                    white = "#F9F9F9",
                    lightGray = "#EFEFEF",
                    sandstone = "#D2D2C0",
                    gray = "#555555",
                    slate = "#41516C",
                    tarawera = "#093552",
                    yellow = "#FFB400",
                    lightGreen = "#C0CD3F",
                    lime = "#90C134",
                    orange = "#C14D00",
                    red = "#841C1C",
                    purple = "#8E0F56")


# Brown ====
#' Brown Colors
#'
#' \code{brownCol}: Brown color palette
#' (https://www.brown.edu/university-identity/sites/university-identity/files/Brown_Visual_Identity_Policy_2016-07-22.pdf)
#' @name rtPalettes
#' @export

brownCol <- list(red = "#ED1C24",
                 brown = "#4E3629",
                 gold = "#FFC72C",
                 gray = "#98A4AE",
                 skyBlue = "#59CBE8",
                 emerald = "#00B398",
                 navy = "#003C71",
                 taupe = "#B7B09C")


# Yale ====
#' Yale Colors
#'
#' \code{yaleCol}: Yale color palette (https://yaleidentity.yale.edu/web)
#' @name rtPalettes
#' @export

yaleCol <- list(yaleBlue = "#00356b",
                mediumBlue = "#286dc0",
                lightBlue = "#63aaff",
                darkestGray = "#222222",
                darkGray = "#4a4a4a",
                sandstone = "#978d85",
                lightGray = "#dddddd",
                lightestGray = "#f9f9f9",
                green = "#5f712d",
                orange = "#bd5319")


# Cornell ====
#' Cornell Colors
#'
#' \code{cornellCol}: Yale color palette
#' (https://brand.cornell.edu/design-center/colors/
#' @name rtPalettes
#' @export

cornellCol <- list(carnellian = "#B31B1B",
                   darkGrey = "#222222",
                   lightGrey = "#F7F7F7",
                   linkBlue = "#006699",
                   greenGraphics = "#6EB43F",
                   greenText = "#4B7B2B",
                   greenLargeText = "#578E32",
                   orangeGraphics = "#F8981D",
                   orangeLargeText = "#D47500",
                   redGraphics = "#EF4035",
                   redText = "#DF1E12",
                   navy = "#073949",
                   darkWarmGrey = "#A2998B",
                   seaGrey = "#9FAD9F")

# Harvard Medical School ====
#' HMS Colors
#'
#' \code{hmsCol}: Harvard Medical School color palette
#' (https://identityguide.hms.harvard.edu/color)
#' @name rtPalettes
#' @export

hmsCol <- list(crimson = "#A51C30",
               black = "#1E1E1E",
               mortar = "#8C8179",
               parchment = "#F3F3F1",
               slate = "#8996A0",
               shade = "#BAC5C6",
               indigo = "#293352",
               blueBonnet = "#4E84C4",
               ivy = "#52854C",
               pear = "#C3D7A4",
               lemon = "#FFDB6D",
               saffron = "#D16103",
               gold = "#C4961A",
               creme = "#F4EDCA")


# Dartmouth ====
#' Dartmouth Colors
#'
#' \code{dartmouthCol}: Dartmouth color palette
#' (https://communications.dartmouth.edu/visual-identity/design-elements/color-palette#web%20palette)
#' @name rtPalettes
#' @export

dartmouthCol <- list(dartmouthGreen = "#00693e",
                     forestGreen = "#12312b",
                     webGray1 = "#f7f7f7",
                     webGray2 = "#e2e2e2",
                     graniteGray = "#424141",
                     autumnBrown = "#643c20",
                     bonfireRed = "#9d162e",
                     tuckOrange = "#e32d1c",
                     summerYellow = "#f5dc69",
                     springGreen = "#c4dd88",
                     riverNavy = "#003c73",
                     riverBlue = "#267aba",
                     webViolet = "#8a6996",
                     bonfireOrange = "#ffa00f")

# USF ====
#' USF Colors
#'
#' \code{usfCol}: USF color palette
#' (https://myusf.usfca.edu/marketing-communications/resources/graphics-resources/brand-standards/color-palette)
#' Color conversions performed using https://www.pantone.com/color-finder/
#' @name rtPalettes
#' @export

usfCol <- list(green = "#205C40",
               yellow = "#ffb81c",
               gray = "#75787B")


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


# Johns Hopkins ====
#' Johns Hopkins University Colors
#'
#' \code{jhuCol}: Johns Hopkins University color palette
#' (https://brand.jhu.edu/color/)
#' @name rtPalettes
#' @export

jhuCol <- list(heritageBlue = "#002d72",
               spiritBlue = "#68ace5",
               orange  = "#cf4520",
               maroon = "#76232f",
               pink = "#a15a95",
               green = "#009b77",
               blue = "#0072ce",
               yellow = "#f1c400",
               pms7407c = "#cba052",
               pms1375c = "#ff9e1b",
               pms1505c = "#ff6900",
               pms7586c = "#9e5330",
               pms4625c = "#4f2c1d",
               pms486c = "#e8927c",
               pms187c = "#a6192e",
               pms262c = "#51284f",
               pms666c = "#a192b2",
               pms279c = "#418fde",
               pms564c = "#86c8bc",
               pms7734c = "#286140",
               pms7490c = "#719949")


# NYU ====
#' NYU Colors
#'
#' \code{nyuCol}: NYU color palette
#' (https://www.nyu.edu/employees/resources-and-services/media-and-communications/styleguide/website/graphic-visual-design.html)
#' @name rtPalettes
#' @export

nyuCol <- list(brightPurple = "#8900e1",
               nyuPurple = "#57068c",
               darkerPurple = "#330662",
               darkestPurple = "#220337",
               mediumGray = "#6d6d6d",
               lightGray = "#b8b8b8",
               lighterGray = "#d6d6d6",
               lightestGray = "#f2f2f2",
               red = "#cb0200",
               orange = "#e86c00",
               green = "#489141",
               blue = "#28619e",
               lightBlue = "#3dbbdb",
               accentGreen = "#007c70",
               brightRed = "#d71e5e",
               brightOrange = "#e86c00",
               yellow = "#ffc107")


# Chicago ====
#' U Chicago Colors
#'
#' \code{chicagoCol}: University of Chicago color palette
#' (https://news.uchicago.edu/sites/default/files/attachments/_uchicago.identity.guidelines.pdf)
#' @name rtPalettes
#' @export

chicagoCol <- list(maroon = "#800000",
                   darkGray  = "#767676",
                   lightGray = "#D6D6CE",
                   yellowTint = "#FFB547",
                   yellowCore = "#FFA319",
                   yellowShade = "#C68220",
                   orangeTint = "#D49464",
                   orangeCore = "#C16622",
                   orangeShade = "#9A5324",
                   redTint = "#B1746F",
                   redCore = "#8F3931",
                   redShade = "#642822",
                   lightGreenTint = "#ADB17D",
                   lightGreenCore = "#8A9045",
                   lightGreenShade = "#616530",
                   darkGreenTint = "#8A8B79",
                   darkGreenCore = "#58593F",
                   darkGreenShade = "#3E3E23",
                   blueTint = "#5B8FA8",
                   blueCore = "#155F83",
                   blueShade = "#0F425C",
                   violetTint = "#725663",
                   violetCore = "#350E20",
                   cyan = "#47B5FF",
                   magenta = "#FF3399")


# Penn State ====
#' Penn State Colors
#'
#' \code{texasCol}: Penn State color palette
#' (https://brand.psu.edu/design-essentials.html#color)
#' @name rtPalettes
#' @export

pennstateCol <- list(nittanyNavy = "#001E44",
                     beaverBlue = "#1E407C",
                     pennsylvaniaSky = "#009CDE",
                     limestone = "#91959C",
                     creek = "#3EA39E",
                     slate = "#314D64",
                     pennsForest = "#4A7729",
                     oldCoaly = "#54585A",
                     landGrant = "#6A3028",
                     lionsRoar = "#BF8226",
                     lionShrine = "#B88965",
                     statelyAtherton = "#AC8DCE",
                     pughBlue = "#96BEE6",
                     original1887 = "#BC204B",
                     brightkeystone = "#FFD100",
                     inventOrange = "#E98300",
                     dawnOfDiscovery = "#F2665E",
                     perpetualWonder = "#491D70",
                     greenOpportunity = "#008755",
                     futuresCalling = "#99CC00")


# SFSU ====
#' SF State
#'
#' \code{sfsuCol}: SF State Colors
#'
#' \code{sfsuCol}: SF State color palette
#' (https://logo.sfsu.edu/color-system)
#' @name rtPalettes
#' @export

sfsuCol <- list(`2755C` = "#231161",
                `2755C_85pc` = "#463077",
                `117C` = "#C99700",
                `117C_60pc` = "#E9D597",
                `3025C` = "#004F71",
                `383C` = "#ABAD00",
                `7419C` = "#B04A5A",
                `484C` = "#9A3324",
                coolGray11 = "#53565A")

# U Illinois ====
#' University of Illinois Colors
#'
#' \code{illinoisCol}: University of Illinois color palette
#' (https://www.uillinois.edu/OUR/brand/color_palettes)
#' @name rtPalettes
#' @export

illinoisCol <- list(uofiblue = "#13294b",
                    urbanaOrange = "#E84A27",
                    uicRed = "#D50032",
                    uisBlue = "#003366",
                    teal = "#0d605e",
                    grayBlue = "#6fafc7",
                    citron = "#bfd46d",
                    darkYellow = "#ffd125",
                    salmon = "#ee5e5e",
                    periwinkle = "#4f6898",
                    gray = "#E8E9EA",
                    coolGray6 = "#A5A8AA",
                    coolGray1 = "#5E6669",
                    secondaryBlue1 = "#0455A4",
                    secondaryBlue2 = "#1F4096")

# U Maryland ====
#' University of Maryland Colors
#'
#' \code{umdCol}: University of Maryland color palette
#' (https://osc.umd.edu/licensing-trademarks/brand-standards/logos/#color)
#' @name rtPalettes
#' @export

umdCol <- list(umdRed = "#E21833",
               umdYellow = "#ffd200",
               umdBrown = "#AD7C59")

# U Texas ====
#' U Texas Colors
#'
#' \code{texasCol}: University of Texas color palette
#' (https://brand.utexas.edu/identity/color/)
#' @name rtPalettes
#' @export

texasCol <- list(burntOrange = "#bf5700",
                 gray = "#333f48",
                 brightOrange = "#f8971f",
                 yellow = "#ffd600",
                 lightGreen = "#a6cd57",
                 green = "#579d42",
                 teal = "#00a9b7",
                 blue = "#005f86",
                 lightBlue = "#9cadb7",
                 stone = "#d6d2c4")


# Georgia Tech ====
#' Georgia Tech Colors
#'
#' \code{techCol}: Georgia Tech color palette
#' (http://www.licensing.gatech.edu/super-block/239)
#' @name rtPalettes
#' @export

techCol <- list(techGold = "#B3A369",
                buzzGold = "#EAAA00",
                blue = "#00263A")


# Jefferson ====
#' Jefferson University Colors
#'
#' \code{jeffersonCol}: Jefferson color palette (http://creative.jefferson.edu/downloads/Jefferson-Brand-Guidelines.pdf)
#' @name rtPalettes
#' @export

jeffersonCol <- list(jeffDeepBlue = "#152456",
                     jeffBrightBlue = "#59B7df",
                     legacyMaroon = "#9f2943",
                     red = "#e53e30",
                     voltGreen = "#ece819",
                     silver = "#dfe1df",
                     darkGray = "#8e9089",
                     black = "#231f20")


# Hawaii ====
#' University of Hawaii Colors
#'
#' \code{hawaiiCol}: University of Hawaii color palette (https://www.hawaii.edu/offices/eaur/graphicsstandards.pdf)
#' @name rtPalettes
#' @export

hawaiiCol <- list(manoa = "#024731",
                  hilo = "#DA291C",
                  westOahu = "#A71930",
                  hawaiiCC = "#91004B",
                  honoluluCC = "#00747A",
                  kapiolaniCC = "#002395",
                  kauaiCC = "#716FB3",
                  leeward = "#3D7EDB",
                  mauiCC = "#005172",
                  windward = "#7AB800",
                  system = "#B3995D")


# NIH ====
#' NIH Colors
#'
#' \code{nihCol}: NIH color palette (https://www.nlm.nih.gov/about/nlm_logo_guidelines_030414_508.pdf)
#' @name rtPalettes
#' @export

nihCol <- list(blue = "#20558a",
               gray = "#616265")

# UBC ====
#' UBC Colors
#'
#' \code{ubcCol}: UBC color palette (http://assets.brand.ubc.ca/downloads/ubc_colour_guide.pdf)
#' @name rtPalettes
#' @export

ubcCol <- list(ubcBlue = "#002145",
               blue2 = "#0055B7",
               blue3 = "#00A7E1",
               blue4 = "#40B4E5",
               blue5 = "#6EC4E8",
               blue6 = "#97D4E9")

# U Toronto ====
#' U Toronto Colors
#'
#' \code{torontoCol}: U Toronto color palette (https://trademarks.utoronto.ca/colors-fonts/)
#' @name rtPalettes
#' @export

torontoCol <- list(blue = "#002043",
                   red = "#bb133e")


# McGill ====
#' McGill Colors
#'
#' \code{mcgillCol}: McGill color palette (https://www.mcgill.ca/visual-identity/visual-identity-guide)
#' @name rtPalettes
#' @export

mcgillCol <- list(mcgillRed = "#ED1B2F",
                  grey = "#5D6770",
                  pastelOrange = "#FFD794",
                  brightOrange = "#F7941D",
                  mutedOrange = "#D3674A",
                  darkOrange = "#AA4B31",
                  pastelYellow = "#FFF193",
                  brightYellow = "#FFD400",
                  mutedYellow = "#E8B92E",
                  darkYellow = "#B28C35",
                  pastelTeal = "#B5E1E1",
                  brightTeal = "#27BDBE",
                  mutedTeal = "#087F8C",
                  darkTeal = "#0A6266",
                  pastelBlue = "#C8EAF5",
                  brightBlue = "#44C8F5",
                  mutedBlue = "#0096C9",
                  darkBlue = "#024F6D",
                  pastelGreen = "#D5E6A8",
                  brightGreen = "#B2D235",
                  mutedGreen = "#8BA04E",
                  darkGreen = "#305534",
                  pastelPink = "#E2A7CC",
                  brightPink = "#C768A9",
                  mutedPink = "#9B5678",
                  darkPink = "#673567",
                  darkRed = "#9E0918")


# UCL ====
#' UCL Colours
#'
#' \code{uclCol}: UCL colour palette (https://www.ucl.ac.uk/cam/brand/guidelines/colour)
#' @name rtPalettes
#' @export

uclCol <- list(darkGreen = "#555025",
               darkRed = "#651D32",
               darkPurple = "#4B384C",
               darkBlue = "#003D4C",
               darkBrown = "#4E3629",
               midGreen = "#8F993E",
               midRed = "#93272C",
               midPurple = "#500778",
               midBlue = "#002855",
               stone = "#D6D2C4",
               brightGreen = "#B5BD00",
               brightRed = "#D50032",
               brightBlue = "#0097A9",
               brightPink = "#AC145A",
               lightGreen = "#BBC592",
               lightRed = "#E03C31",
               lightPurple = "#C6B0BC",
               lightBlue = "#8DB9CA",
               yellow = "#F6BE00",
               orange = "#EA7600",
               grey = "#8C8279",
               blueCeleste = "#A4DBE8",
               IOEblue = "#24509A")


# Oxford ====
#' Oxford Colours
#'
#' \code{oxfordCol}: Oxford University colour palette (https://www.ox.ac.uk/sites/files/oxford/media_wysiwyg/Oxford%20Blue%20LR.pdf)
#' @name rtPalettes
#' @export

oxfordCol <- list(oxfordBlue = "#002147",
                  pantone279 = "#4891DC",
                  pantone291 = "#9ECEEB",
                  pantone5405 = "#44687D",
                  pantone549 = "#5F9BAF",
                  pantone551 = "#A1C4D0",
                  pantone562 = "#007770",
                  pantone624 = "#7BA296",
                  pantone559 = "#BCD2C3",
                  pantone576 = "#69913B",
                  pantone578 = "#B9CF96",
                  pantone580 = "#CEDBAF",
                  pantone583 = "#AAB300",
                  pantone585 = "#DBDE72",
                  pantone587 = "#E3E597",
                  pantone7412 = "#CF7A30",
                  pantone129 = "#F5CF47",
                  pantone127 = "#F3DE74",
                  pantone202 = "#872434",
                  pantone200 = "#BE0F34",
                  pantone196 = "#EBC4CB",
                  pantoneWarmGray6 = "#A79D96",
                  pantoneWarmGray3 = "#C7C2BC",
                  pantoneWarmGray1 = "#E0DED9")


# NHS ====
#' NHS Colours
#'
#' \code{nhsCol}: NHS colour palette (https://www.england.nhs.uk/nhsidentity/identity-guidelines/colours/)
#' @name rtPalettes
#' @export

nhsCol <- list(nhsDarkBlue = "#003087",
               nhsBlue = "#005EB8",
               nhsBrightBlue = "#0072CE",
               nhsLightBlue = "#41B6E6",
               nhsAquaBlue = "#00A9CE",
               nhsBlack = "#231f20",
               nhsDarkGrey = "#425563",
               nhsMidGrey = "#768692",
               nhsPaleGrey = "#E8EDEE",
               nhsDarkGreen = "#006747",
               nhsGreen = "#009639",
               nhsLightGreen = "#78BE20",
               nhsAquaGreen = "#00A499",
               nhsPurple = "#330072",
               nhsDarkPink = "#7C2855",
               nhsPink = "#AE2573",
               nhsDarkRed = "#8A1538",
               emergencyServicesRed = "#DA291C",
               nhsOrange = "#ED8B00",
               nhsWarmYellow = "#FFB81C",
               nhsYellow = "#FAE100")


# ETH ====
#' ETH Colours
#'
#' \code{ethCol}: ETH color palette (https://ethz.ch/services/en/service/communication/corporate-design/colour.html)
#' @name rtPalettes
#' @export

ethCol <- list(eth1 = "#1F407A",
               eth2 = "#3C5A0F",
               eth3 = "#0069B4",
               eth4 = "#72791C",
               eth5 = "#91056A",
               eth6 = "#6F6F6E",
               eth7 =  "#A8322D",
               eth8 = "#007A92",
               eth9 = "#956013",
               eth10 = "#82BE1E")


# RWTH Aachen ====
#' RWTH Aachen Colours
#'
#' \code{rwthCol}: RWTH Aachen color palette (http://www9.rwth-aachen.de/global/show_document.asp?id=aaaaaaaaaadpbhq)
#' @name rtPalettes
#' @export
rwthCol <- list(blau1 = "#00549F",
                blau2 = "#407FB7",
                blau3 = "#8EBAE5",
                blau4 = "#C7DDF2",
                blau5 = "#E8F1FA",
                magenta1 = "#E30066",
                magenta2 = "#E96088",
                magenta3 = "#F19EB1",
                magenta4 = "#F9D2DA",
                magenta5 = "#FDEEF0",
                gelb1 = "#FFED00",
                gelb2 = "#FFF055",
                gelb3 = "#FFF59B",
                gelb4 = "#FFFAD1",
                gelb5 = "#FFFDEE",
                petrol1 = "#006165",
                petrol2 = "#2D7F83",
                petrol3 = "#7DA4A7",
                petrol4 = "#BFD0D1",
                petrol5 = "#E6ECEC",
                tuerkis1 = "#0098A1",
                tuerkis2 = "#00B1B7",
                tuerkis3 = "#89CCCF",
                tuerkis4 = "#CAE7E7",
                tuerkis5 = "#EBF6F6",
                gruen1 = "#57AB27",
                gruen2 = "#8DC060",
                gruen3 = "#B8D698",
                gruen4 = "#DDEBCE",
                gruen5 = "#F2F7EC",
                maigruen1 = "#BDCD00",
                maigruen2 = "#D0D95C",
                maigruen3 = "#E0E69A",
                maigruen4 = "#F0F3D0",
                maigruen5 = "#F9FAED",
                orange1 = "#F6A800",
                orange2 = "#FABE50",
                orange3 = "#FDD48F",
                orange4 = "#FEEAC9",
                orange5 = "#FFF7EA",
                rot1 = "#CC071E",
                rot2 = "#D85C41",
                rot3 = "#E69679",
                rot4 = "#F3CDBB",
                rot5 = "#FAEBE3",
                bordeaux1 = "#A11035",
                bordeaux2 = "#B65256",
                bordeaux3 = "#CD8B87",
                bordeaux4 = "#E5C5C0",
                bordeaux5 = "#F5E8E5",
                violett1 = "#612158",
                violett2 = "#834E75",
                violett3 = "#A8859E",
                violett4 = "#D2C0CD",
                violett5 = "#EDE5EA",
                lila1 = "#7A6FAC",
                lila2 = "#9B91C1",
                lila3 = "#BCB5D7",
                lila4 = "#DEDAEB",
                lila5 = "#F2F0F7")

# Mozilla ====
#' Mozilla Colors
#'
#' \code{mozillaCol}: Mozilla design colors
#' (https://mozilla.design/mozilla/color/)
#' @name rtPalettes
#' @export

mozillaCol <- list(neonBlue = "#00ffff",
                   lemonYellow = "#fff44f",
                   warmRed = "#ff4f5e",
                   neonGreen = "#54ffbd",
                   darkPurple = "#6e008b",
                   darkGreen = "#005e5e",
                   darkBlue = "#00458b",
                   lightGrey = "#e7e5e2")

# Firefox ====
#' Firefox Colors
#'
#' \code{firefoxCol}: Firefox design colors
#' (https://mozilla.design/firefox/color/)
#' @name rtPalettes
#' @export

firefoxCol <- list(green = "#53FEBE",
                   blue = "#0290EE",
                   purple = "#AC71FF",
                   lightPurple = "#D64CF1",
                   magenta = "#FE4AA3",
                   salmon = "#FF6A75",
                   orange = "#FE8A4F",
                   yellow = "#FFBD4F")

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

# rtemis palettes ====
rtCol <- desaturate(c(pennCol$lighterBlue,
                      pennCol$lightestBlue,
                      pennCol$blue,
                      pennCol$lighterRed,
                      pennCol$lightestRed,
                      pennCol$red,
                      pennCol$orange,
                      pennCol$lighterOrange,
                      pennCol$darkerOrange,
                      pennCol$lighterPurple,
                      pennCol$lightestPurple,
                      pennCol$purple), .3)

rtCol0 <- list(ucsfCol$teal,
               ucsfCol$orange,
               pennCol$lighterBlue,
               pennCol$lighterRed,
               pennCol$lighterOrange,
               pennCol$lighterPurple,
               pennCol$lightestBlue,
               pennCol$lightestRed,
               pennCol$lightestOrange,
               pennCol$lightestPurple,
               pennCol$blue,
               pennCol$red,
               pennCol$orange,
               pennCol$purple)

rtCol1 <-      desaturate(c(
               ucsfCol$teal,
               ucsfCol$orange,
               pennCol$lighterBlue,
               pennCol$lighterRed,
               pennCol$lighterOrange,
               pennCol$lighterPurple,
               pennCol$lightestBlue,
               pennCol$lightestRed,
               pennCol$lightestOrange,
               pennCol$lightestPurple,
               pennCol$blue,
               pennCol$red,
               pennCol$orange,
               pennCol$purple), .3)

rtCol1a <- list(rtblue = "#80ffff",
               rtorange = "#FFDB71",
               # ucsfCol$teal,
               # ucsfCol$orange,
               pennCol$lighterBlue,
               pennCol$lighterRed,
               pennCol$lighterOrange,
               pennCol$lighterPurple,
               pennCol$lightestBlue,
               pennCol$lightestRed,
               pennCol$lightestOrange,
               pennCol$lightestPurple,
               pennCol$blue,
               pennCol$red,
               pennCol$orange,
               pennCol$purple)

rtCol1b <- c(list(rtblue = "#80ffff",
                 rtorange = "#FFDB71"),
                 desaturate(c(# ucsfCol$teal,
                       # ucsfCol$orange,
                       pennCol$lighterBlue,
                       pennCol$lighterRed,
                       pennCol$lighterOrange,
                       pennCol$lighterPurple,
                       pennCol$lightestBlue,
                       pennCol$lightestRed,
                       pennCol$lightestOrange,
                       pennCol$lightestPurple,
                       pennCol$blue,
                       pennCol$red,
                       pennCol$orange,
                       pennCol$purple), .3))

rtCol2 <- desaturate(c(pennCol$lightestBlue,
                       pennCol$lighterBlue,
                       pennCol$blue,
                       pennCol$lightestRed,
                       pennCol$lighterRed,
                       pennCol$red,
                       pennCol$lighterOrange,
                       pennCol$orange,
                       pennCol$darkerOrange,
                       pennCol$lightestPurple,
                       pennCol$lighterPurple,
                       pennCol$purple), .3)

rtCol3 <- desaturate(colorMix(list(blue = c(pennCol$lightestBlue, pennCol$darkestBlue),
                                   gray = c("gray10", "gray85")), 6), .3)

rtcoldev <- list(rtemisblue = "#80ffff",
                  rtemisbluetoo = "#00D6FF",
                  lavender = "#ff80ffff",
                  orange = "#ffb200ff")

grays <- list("gray10", "gray30", "gray50", "gray70", "gray90")

# rtPalettes ====
rtPalettes <- list(pennCol = pennCol,
                   imperialCol = imperialCol,
                   stanfordCol = stanfordCol,
                   ucsfCol = ucsfCol,
                   ucdCol = ucdCol,
                   berkeleyCol = berkeleyCol,
                   ucscCol = ucscCol,
                   ucmercedCol = ucmercedCol,
                   ucsbCol = ucsbCol,
                   uclaCol = uclaCol,
                   ucrColor = ucrColor,
                   uciCol = uciCol,
                   ucsdCol = ucsdCol,
                   californiaCol = californiaCol,
                   caltechCol = caltechCol,
                   cmuCol = cmuCol,
                   princetonCol = princetonCol,
                   columbiaCol = columbiaCol,
                   yaleCol =  yaleCol,
                   brownCol = brownCol,
                   cornellCol = cornellCol,
                   hmsCol = hmsCol,
                   dartmouthCol = dartmouthCol,
                   usfCol = usfCol,
                   uwCol = uwCol,
                   jhuCol = jhuCol,
                   nyuCol = nyuCol,
                   chicagoCol = chicagoCol,
                   pennstateCol = pennstateCol,
                   texasCol = texasCol,
                   techCol = techCol,
                   jeffersonCol = jeffersonCol,
                   hawaiiCol = hawaiiCol,
                   nihCol = nihCol,
                   torontoCol = torontoCol,
                   mcgillCol = mcgillCol,
                   uclCol = uclCol,
                   oxfordCol = oxfordCol,
                   nhsCol = nhsCol,
                   ethCol = ethCol,
                   rwthCol = rwthCol,
                   firefoxCol = firefoxCol,
                   mozillaCol = mozillaCol,
                   appleCol = appleCol,
                   googleCol = googleCol,
                   amazonCol = amazonCol,
                   microsoftCol = microsoftCol,
                   rtCol = rtCol,
                   rtCol1 = rtCol1,
                   rtCol2 = rtCol2,
                   rtCol3 = rtCol3,
                   grays = grays)

#' \pkg{rtemis} Color Palettes
#'
#' \code{rtPalette} prints names of available color palettes
#' Each palette is a named list of hexadecimal color definitions which can be used with any
#' graphics function.
#' @param palette Character: Name of palette to return. Default = NULL: available palette names
#' are printed and no palette is returned
#' @return
#' A list of available palettes, invisibly
#' @examples
#' rtPalette("imperial")
#' @export

rtPalette <- function(palette = NULL) {

  if (is.null(palette)) {
    msg(crayon::cyan("The following palettes are available:"))
    print(names(rtPalettes))
  } else {
    palette <- match.arg(palette,
                         names(rtPalettes))
    rtPalettes[[palette]]
  }

} # rtemis::rtPalette


# Custom crayon styles ====
teal <- crayon::make_style(teal = "#18A3AC")
rtBlue <- crayon::make_style(rtBlue = "#005581")
# rtOrange <- crayon::make_style(rtOrange = "#F48024")
rtOrange <- crayon::make_style(rtOrange = "#FF9933")
rtHighlight.color <- getOption("rt.highlight.color", "#18A3AC")
rtHighlight <- crayon::make_style(rtHighlight = rtHighlight.color)
rtemisblue <- crayon::make_style(rtemisblue = "#80ffff")

#' Access rtemis palette colors
#'
#' Allows you to get `n` colors of a defined palette, useful for passing to other functions, like ggplot
#'
#' @param n Integer: Number of colors to output
#' @param palette Character: Palette to use. See available options with \code{rtPalette()}.
#' Default = \code{getOption("rt.palette", "rtCol1")}
#' @export
#' @author E.D. Gennatas
#' @examples
#' rtemis_palette(3)

rtemis_palette <- function(n,
                           palette = getOption("rt.palette", "rtCol1")) {

  .palette <- unlist(rtPalette(palette))
  names(.palette) <- NULL
  .palette[seq_len(n)]

} # rtemis::rtemis_palette


#' Convert R color to hexadecimal code
#'
#' Convert a color that R understands into the corresponding hexadecimal code
#'
#' @param color Color(s) that R understands
#' @author E.D. Gennatas
#' @export
#' @examples
#' col2hex(c("gray50", "skyblue"))

col2hex <- function(color) {

  .rgb <- col2rgb(color)
  sapply(seq(color), function(i) {
    paste0("#", paste0(sprintf("%02s",
           c(as.character(as.hexmode(.rgb[1, i])),
             as.character(as.hexmode(.rgb[2, i])),
             as.character(as.hexmode(.rgb[3, i])))
           ), collapse = ""
           ))
  })

} # rtemis::col2hex
