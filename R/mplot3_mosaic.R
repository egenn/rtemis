# mplot3_mosaic.R
# ::rtemis::
# 2021 E.D. Gennatas rtemis.org

#' Mosaic plot
#'
#' Plots a mosaic plot using `graphics::mosaicplot`
#'
#' @param x contingency table, e.g. output of `table()`
#' @param main Character: Main title
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param border Color vector for cell borders or FALSE to turn off. Default = FALSE
#' @param theme Character: Run `themes()` for available themes
#' @param theme.args List of arguments to pass to `theme`. Optional, same args can be passed
#' to theme function
#' @param palette Vector of colors, or Character defining a builtin palette - get options with
#' `rtpalette()`
#' @param new Logical: If TRUE, add plot to existing plot. See `par("new")`
#' @param filename Character: Path to file to save plot. Default = NULL
#' @param pdf.width Float: Width in inches for PDF output, if `filename` is defined
#' @param pdf.height Float: Height in inches for PDF output, if `filename` is defined
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' party <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(party) <- list(gender = c("F", "M"),
#'                         party = c("Democrat","Independent", "Republican"))
#' mplot3_mosaic(party)
#' }
mplot3_mosaic <- function(
  x,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  border = FALSE,
  theme = rtTheme,
  theme.args = list(),
  palette = rtPalette,
  mar = NULL,
  oma = rep(0, 4),
  par.reset = TRUE,
  new = FALSE,
  autolabel = letters,
  filename = NULL,
  pdf.width = 5,
  pdf.height = 5,
  ...
) {
  # Arguments ----
  # Compatibility with rtlayout()
  if (!is.null(rtenv$rtpar)) par.reset <- FALSE

  # Theme ----
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), theme.args)
  } else {
    for (i in seq(theme.args)) {
      theme[[names(theme.args)[i]]] <- theme.args[[i]]
    }
  }
  if (is.character(palette)) palette <- rtpalette(palette)

  # Output directory
  if (!is.null(filename) && !dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }

  # Main Title ----
  if (!is.null(rtenv$autolabel)) {
    autolab <- autolabel[rtenv$autolabel]
    main <- paste(autolab, main)
    rtenv$autolabel <- rtenv$autolabel + 1
  }

  # Plot ----
  if (is.null(mar)) {
    topmar <- ifelse(is.null(main), 1, 2.5)
    mar <- c(2.5, 2.5, topmar, 1)
  }

  if (!is.null(filename))
    pdf(
      filename,
      width = pdf.width,
      height = pdf.height,
      title = "rtemis Graphics"
    )
  par.orig <- par(no.readonly = TRUE)
  par(
    bg = theme$bg,
    fg = theme$fg,
    cex = theme$cex,
    col.axis = theme$axes.col,
    col.lab = theme$labs.col,
    col.main = theme$main.col,
    col.sub = theme$main.col,
    mar = mar,
    new = new,
    family = theme$font.family
  )
  if (exists("rtpar", envir = rtenv)) {
    par.reset <- FALSE
  } else {
    par(oma = oma)
  }
  if (par.reset) on.exit(suppressWarnings(par(par.orig)))

  mosaicplot(
    x,
    main = NULL,
    xlab = xlab,
    ylab = ylab,
    color = unlist(palette),
    border = border,
    cex.axis = theme$cex,
    ...
  )

  if (!is.null(main)) {
    mtext(
      main,
      line = theme$main.line,
      font = theme$main.font,
      adj = theme$main.adj,
      cex = theme$cex,
      col = theme$main.col,
      family = theme$font.family
    )
  }

  # Outro ----
  if (!is.null(filename)) dev.off()
} # rtemis::mplot3_mosaic
