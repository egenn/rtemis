# colorPreview.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Color Preview
#'
#' Very simple preview of colors
#'
#' @param col Input color. Hexadecimal or any string that R recognizes.
#'   (See all with \code{colors()})
#' @param main Character: Plot title. Default = NULL
#' @param names Logical: If TRUE, add l\code{names(col)} if not NULL. Default = TRUE
#' @param names.y Float: y position to place \code{names}. Adjust as needed. Default = 1.045
#' @param srt Float: String rotation in degrees. Default = 45
#' @param alpha Float: Alpha level. Default = 1
#' @param ylim Float, vector, length 2: y axis limits. Default = c(0.95, 1.1)
#' @param pch Integer or single character: The \code{pch} parameter of \code{par}
#' @param bg Background color
#' @param cex Float: Character expansion factor. Default = 26
#' @param mar Float, vector, length 4: Plot margins. Default = c(0, 1.7, 0.6, 2.8)
#' @param main.line Float: Line to plot \code{main} Default = 0
#' @param filename String, optional: Path to file to save plot
#' @param par.reset Logical: If TRUE, reset \code{par} before exiting
#' @param pdf.width Float: PDF output width
#' @param pdf.height Float: PDF output height
#' @author Efstathios D. Gennatas
#' @export

colorPreview <- function(col,
                         main = NULL,
                         names = TRUE,
                         names.y = 1.045,
                         srt = 45,
                         alpha = 1,
                         ylim = c(0.95, 1.1),
                         cex = 12,
                         pch = 18,
                         bg = "white",
                         mar = c(0, 1.7, 1, 2.8),
                         main.col = "black",
                         main.line = 0,
                         filename = NULL,
                         par.reset = TRUE,
                         pdf.width = length(col) * .5,
                         pdf.height = 2) {


  if (!is.null(filename)) if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height, title = "rtemis Graphics")

  if (is.null(main)) main <- deparse(substitute(col))

  par.orig <- par(no.readonly = TRUE)
  if (par.reset) on.exit(try(par(par.orig), silent = TRUE))
  # sometimes get:
  # Error in par(par.orig) : invalid value specified for graphical parameter "pin"
  par(mar = mar, bg = bg)

  .col <- adjustcolor(col, alpha)
  plot(seq(col), rep(1, length(col)),
       # xlim = c(0.9, 1.1),
       ylim = ylim, ann = FALSE, axes = FALSE, pch = pch, col = .col, cex = cex, xpd = TRUE,
       xaxs = "r", yaxs = "i")
  if (!is.null(main)) mtext(main, line = main.line,
                            font = 2, xpd = TRUE, adj = 0,
                            col = main.col)

  if (names) {
    if (!is.null(names(col))) {
      text(seq(col), y = names.y, names(col),
           col = .col, adj = 0,
           srt = srt, xpd = TRUE)
    }
  }

  if (!is.null(filename)) dev.off()

} # rtemis::colorPreview
