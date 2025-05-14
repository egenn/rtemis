# dplot3_protein
# ::rtemis::
# 2022 EDG rtemis.org

#' Plot the amino acid sequence with annotations
#'
#' @param x Character vector: amino acid sequence (1-letter abbreviations) OR
#' `a3` object OR Character: path to JSON file OR Character: UniProt accession number
#' @param site Named list of lists with indices of sites. These will be
#' highlighted by coloring the border of markers
#' @param region Named list of lists with indices of regions. These will be
#' highlighted by coloring the markers and lines of regions using the
#' `palette` colors
#' @param ptm List of post-translational modifications
#' @param clv List of cleavage sites
#' @param variant List of variant information
#' @param disease.variants List of disease variant information
#' @param n.per.row Integer: Number of amino acids to show per row
#' @param main Character: Main title
#' @param main.xy Numeric vector, length 2: x and y coordinates for title.
#' e.g. if `main.xref` and `main.yref` are `"paper"`:
#' `c(0.055, .975)` is top left, `c(.5, .975)` is top and
#' middle
#' @param main.xref Character: xref for title
#' @param main.yref Character: yref for title
#' @param main.xanchor Character: xanchor for title
#' @param main.yanchor Character: yanchor for title
#' @param layout Character: "1curve", "grid": type of layout to use
#' @param show.markers Logical: If TRUE, show amino acid markers
#' @param show.labels Logical: If TRUE, annotate amino acids with elements
#' @param font.size Integer: Font size for labels
#' @param label.col Color for labels
#' @param scatter.mode Character: Mode for scatter plot
#' @param marker.size Integer: Size of markers
#' @param marker.col Color for markers
#' @param marker.alpha Numeric: Alpha for markers
#' @param marker.symbol Character: Symbol for markers
#' @param line.col Color for lines
#' @param line.alpha Numeric: Alpha for lines
#' @param line.width Numeric: Width for lines
#' @param show.full.names Logical: If TRUE, show full names of amino acids
#' @param region.scatter.mode Character: Mode for scatter plot
#' @param region.style Integer: Style for regions
#' @param region.marker.size Integer: Size of region markers
#' @param region.marker.alpha Numeric: Alpha for region markers
#' @param region.marker.symbol Character: Symbol for region markers
#' @param region.line.dash Character: Dash for region lines
#' @param region.line.shape Character: Shape for region lines
#' @param region.line.smoothing Numeric: Smoothing for region lines
#' @param region.line.width Numeric: Width for region lines
#' @param region.line.alpha Numeric: Alpha for region lines
#' @param theme Character: Theme to use: Run `themes()` for available themes
#' @param region.palette Named list of colors for regions
#' @param region.outline.only Logical: If TRUE, only show outline of regions
#' @param region.outline.pad Numeric: Padding for region outline
#' @param region.pad Numeric: Padding for region
#' @param region.fill.alpha Numeric: Alpha for region fill
#' @param region.fill.shape Character: Shape for region fill
#' @param region.fill.smoothing Numeric: Smoothing for region fill
#' @param bpadcx Numeric: Padding for region border
#' @param bpadcy Numeric: Padding for region border
#' @param site.marker.size Integer: Size of site markers
#' @param site.marker.symbol Character: Symbol for site markers
#' @param site.marker.alpha Numeric: Alpha for site markers
#' @param site.border.width Numeric: Width for site borders
#' @param site.palette Named list of colors for sites
#' @param variant.col Color for variants
#' @param disease.variant.col Color for disease variants
#' @param showlegend.ptm Logical: If TRUE, show legend for PTMs
#' @param ptm.col Named list of colors for PTMs
#' @param ptm.symbol Character: Symbol for PTMs
#' @param ptm.offset Numeric: Offset for PTMs
#' @param ptm.pad Numeric: Padding for PTMs
#' @param ptm.marker.size Integer: Size of PTM markers
#' @param clv.col Color for cleavage site annotations
#' @param clv.symbol Character: Symbol for cleavage site annotations
#' @param clv.offset Numeric: Offset for cleavage site annotations
#' @param clv.pad Numeric: Padding for cleavage site annotations
#' @param clv.marker.size Integer: Size of cleavage site annotation markers
#' @param annotate.position.every Integer: Annotate every nth position
#' @param annotate.position.alpha Numeric: Alpha for position annotations
#' @param annotate.position.ay Numeric: Y offset for position annotations
#' @param position.font.size Integer: Font size for position annotations
#' @param legend.xy Numeric vector, length 2: x and y coordinates for legend
#' @param legend.xanchor Character: xanchor for legend
#' @param legend.yanchor Character: yanchor for legend
#' @param legend.orientation Character: Orientation for legend
#' @param legend.col Color for legend
#' @param legend.bg Color for legend background
#' @param legend.border.col Color for legend border
#' @param legend.borderwidth Numeric: Width for legend border
#' @param legend.group.gap Numeric: Gap between legend groups
#' @param margin List: Margin settings
#' @param showgrid.x Logical: If TRUE, show x grid
#' @param showgrid.y Logical: If TRUE, show y grid
#' @param automargin.x Logical: If TRUE, use automatic margin for x axis
#' @param automargin.y Logical: If TRUE, use automatic margin for y axis
#' @param xaxis.autorange Logical: If TRUE, use automatic range for x axis
#' @param yaxis.autorange Character: If TRUE, use automatic range for y axis
#' @param scaleanchor.y Character: Scale anchor for y axis
#' @param scaleratio.y Numeric: Scale ratio for y axis
#' @param hoverlabel.align Character: Alignment for hover label
#' @param displayModeBar Logical: If TRUE, display mode bar
#' @param modeBar.file.format Character: File format for mode bar
#' @param scrollZoom Logical: If TRUE, enable scroll zoom
#' @param filename Character: File name to save plot
#' @param file.width Integer: Width for saved file
#' @param file.height Integer: Height for saved file
#' @param file.scale Numeric: Scale for saved file
#' @param width Integer: Width for plot
#' @param height Integer: Height for plot
#' @param verbosity Integer: If > 0, print messages to console. If > 1, print
#' trace messages
#' @param ... Additional arguments to pass to the theme function
#'
#' @return A plotly object
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' tau <- seqinr::read.fasta("https://rest.uniprot.org/uniprotkb/P10636.fasta",
#'   seqtype = "AA"
#' )
#' dplot3_protein(as.character(tau[[1]]))
#'
#' # or directly using the UniProt accession number:
#' dplot3_protein("P10636")
#' }
dplot3_protein <- function(
  x,
  site = NULL,
  region = NULL,
  ptm = NULL,
  clv = NULL,
  variant = NULL,
  disease.variants = NULL,
  # label.group = NULL,
  n.per.row = NULL,
  main = NULL,
  main.xy = c(0.055, .975),
  main.xref = "paper",
  main.yref = "paper",
  main.xanchor = "middle",
  main.yanchor = "top",
  layout = c("simple", "grid", "1curve", "2curve"),
  show.markers = TRUE,
  show.labels = TRUE,
  font.size = 18,
  label.col = NULL,
  scatter.mode = "markers+lines",
  # AA marker
  marker.size = 28,
  marker.col = NULL, # "gray18",
  marker.alpha = 1,
  marker.symbol = "circle",
  # AA line
  line.col = NULL, # "gray18",
  line.alpha = 1,
  line.width = 2,
  # Hover names
  show.full.names = TRUE,
  # regions
  region.scatter.mode = "markers+lines",
  region.style = 3,
  region.marker.size = marker.size,
  region.marker.alpha = .6,
  region.marker.symbol = "circle",
  region.line.dash = "solid",
  region.line.shape = "line",
  region.line.smoothing = 1,
  region.line.width = 1,
  region.line.alpha = .6,
  theme = rtTheme,
  region.palette = rtPalette,
  region.outline.only = FALSE,
  region.outline.pad = 2, # for fake polys
  region.pad = .35, # for real polys
  region.fill.alpha = .1666666,
  region.fill.shape = "line",
  region.fill.smoothing = 1,
  bpadcx = .5,
  bpadcy = .5,
  # Sites - colored marker border
  site.marker.size = marker.size,
  site.marker.symbol = marker.symbol,
  site.marker.alpha = 1,
  site.border.width = 1.5,
  site.palette = rtPalette,
  # Variants
  variant.col = "#FA6E1E",
  # Text groups
  disease.variant.col = "#E266AE", # "#c982d7"
  # PTMs
  showlegend.ptm = TRUE,
  ptm.col = NULL,
  ptm.symbol = "circle",
  ptm.offset = .12,
  ptm.pad = .35,
  ptm.marker.size = marker.size / 4.5,
  # Cleavage sites
  clv.col = NULL,
  clv.symbol = "triangle-down",
  clv.offset = .12,
  clv.pad = .35,
  clv.marker.size = marker.size / 4,
  # Position annotations
  annotate.position.every = 10,
  annotate.position.alpha = .5,
  annotate.position.ay = -.4 * marker.size,
  position.font.size = font.size - 6,
  # Legend
  legend.xy = c(.97, .954),
  legend.xanchor = "left",
  legend.yanchor = "top",
  legend.orientation = "v",
  legend.col = NULL,
  legend.bg = "#FFFFFF00",
  legend.border.col = "#FFFFFF00",
  legend.borderwidth = 0,
  legend.group.gap = 0,
  margin = list(b = 0, l = 0, t = 0, r = 0, pad = 0),
  # Axes
  showgrid.x = FALSE,
  showgrid.y = FALSE,
  automargin.x = TRUE,
  automargin.y = TRUE,
  xaxis.autorange = TRUE,
  yaxis.autorange = "reversed",
  scaleanchor.y = "x",
  scaleratio.y = 1,
  # Layout
  hoverlabel.align = "left",
  # config
  displayModeBar = TRUE,
  modeBar.file.format = "svg",
  scrollZoom = TRUE,
  # file out
  filename = NULL,
  file.width = 1320,
  file.height = 990,
  file.scale = 1,
  width = NULL,
  height = NULL,
  verbosity = 1,
  ...
) {
  # Data ----
  if (inherits(x, "a3")) {
    dat <- x
    x <- dat$Sequence
    site <- iflengthy(dat$Annotations$Site)
    region <- iflengthy(dat$Annotations$Region)
    ptm <- iflengthy(dat$Annotations$PTM)
    clv <- iflengthy(dat$Annotations$Cleavage_site)
    variant <- iflengthy(dat$Annotations$Variant)
    disease.variants <- iflengthy(
      dat$Annotations$Site$Disease_associated_variant
    )
  }
  if (length(x) == 1) {
    if (grepl(".json$", x)) {
      dat <- jsonlite::read_json(
        x,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
      )
      x <- dat$Sequence
      disease.variants <- dat$Annotations$Site[["Disease_associated_variant"]]
      # dat$Annotations$Site[["Disease_associated_variant"]] <- NULL
      site <- dat$Annotations$Site
      region <- dat$Annotations$Region
      ptm <- dat$Annotations$PTM
      clv <- dat$Annotations$Cleavage_site
    } else {
      dat <- uniprot_get(x, verbosity = verbosity)
      x <- dat[["Sequence"]]
      if (is.null(main)) main <- dat[["Identifier"]]
    }
  }
  x <- toupper(x)
  position <- seq_along(x)
  n <- length(x)
  if (is.null(n.per.row)) n.per.row <- ceiling(sqrt(n))

  # Arguments ----
  layout <- match.arg(layout)

  # Coordinates ----
  if (layout == "grid") {
    # '- grid ----
    # 1:n.per.row, n.per.row:1, till n
    xs <- rep(c(1:n.per.row, n.per.row:1), length.out = n)
    nrows <- ceiling(n / n.per.row)
    ys <- rep(1:nrows, each = n.per.row, length = n)
  } else if (layout == "1curve") {
    # '- 1curve ----
    xs <- rep(c(1:n.per.row, (n.per.row - 1):2), length.out = n)
    nrows <- ceiling(1 + (n / n.per.row - 1))
    ys <- c(
      1,
      rep(seq(1, nrows * 4, 3), each = n.per.row - 1, length = n - 1)
    )
    # drop the n.per.row, then n.per.row - 1
    ys[seq(n.per.row, n, n.per.row - 1)] <-
      ys[seq(n.per.row, n, n.per.row - 1)] + 1.5
  } else if (layout == "simple") {
    # '- simple ----
    # if each point is 1 unit apart, border points must be sqrt(3)/2 away
    xs <- rep(c(1:n.per.row, (n.per.row - 1):2), length.out = n)
    nrows <- ceiling(1 + (n / n.per.row))
    ys <- c(
      1,
      rep(seq(1, nrows), each = n.per.row - 1, length = n - 1)
    )
    # every n.per.row, move to .5 up and sqrt(3)/2 right, left from previous
    # Right border
    ys[seq(n.per.row, n, (2 * n.per.row - 2))] <-
      ys[seq(n.per.row, n, (2 * n.per.row - 2))] + .5
    xs[seq(n.per.row, n, (2 * n.per.row - 2))] <-
      xs[seq(n.per.row, n, 2 * n.per.row - 2)] - 1 + sqrt(3) / 2
    # Left border
    ys[seq((2 * n.per.row) - 1, n, (2 * n.per.row - 2))] <-
      ys[seq((2 * n.per.row) - 1, n, (2 * n.per.row - 2))] + .5
    xs[seq((2 * n.per.row) - 1, n, (2 * n.per.row - 2))] <-
      xs[seq((2 * n.per.row) - 1, n, (2 * n.per.row - 2))] + 1 - sqrt(3) / 2
  } else if (layout == "2curve") {
    # '- 2curve ----
    xs <- rep(c(1:n.per.row, n.per.row:1), length.out = n)
    nrows <- ceiling(n / n.per.row)
    ys <- rep(1:nrows * 3 - 2, each = n.per.row, length = n)
    ys[seq(n.per.row, n, n.per.row)] <-
      ys[seq(n.per.row, n, n.per.row)] + 1
    ys[seq(n.per.row, n, n.per.row) + 1] <-
      ys[seq(n.per.row, n, n.per.row)] + 1
  }

  # Theme ----
  extraargs <- list(...)
  if (is.character(theme)) {
    theme <- do.call(paste0("theme_", theme), extraargs)
  } else {
    for (i in seq(extraargs)) {
      theme[[names(extraargs)[i]]] <- extraargs[[i]]
    }
  }
  if (is.null(label.col)) label.col <- theme$fg
  label.col <- recycle(label.col, x)
  if (is.null(marker.col)) marker.col <- color_fade(theme$fg, theme$bg, .9)
  marker.col <- plotly::toRGB(marker.col, alpha = marker.alpha)
  if (is.null(line.col)) line.col <- color_fade(theme$fg, theme$bg, .9)
  line.col <- plotly::toRGB(line.col, alpha = marker.alpha)

  main.col <- plotly::toRGB(theme$main.col)
  labs.col <- plotly::toRGB(theme$labs.col)
  if (is.null(legend.col)) legend.col <- labs.col
  grid.col <- plotly::toRGB(theme$grid.col, theme$grid.alpha)

  # Palette ----
  if (is.character(region.palette)) region.palette <- rtpalette(region.palette)
  if (is.character(site.palette)) site.palette <- rtpalette(site.palette)

  # Match abbreviations to full names ----
  if (show.full.names) {
    input <- switch(max(nchar(x)), "1" = "1", "3" = "3", "full")

    if (input == "full") {
      xnames <- x
    } else {
      if (input == "1") {
        xnames <- factor(x, levels = aa$Abbreviation1, labels = aa$Name) |>
          as.character()
      } else {
        xnames <- factor(
          x,
          levels = toupper(aa$Abbreviation3),
          labels = aa$Name
        ) |>
          as.character()
      }
    }
  } else {
    xnames <- x
  }

  # Variants: overwrite xnames with tooltip info
  if (!is.null(variant)) {
    for (i in seq_along(variant)) {
      varidi <- variant[[i]][["Position"]]
      xnames[varidi] <- paste0(
        xnames[varidi],
        "\n\n",
        list2html(variant[[i]], col = variant.col)
      )
    }
  }

  # plotly ----
  plt <- plotly::plot_ly(
    width = width,
    height = height
  )

  # AA markers and lines ----
  aaname <- if (is.null(disease.variants)) {
    "1&#176; structure"
  } else {
    paste0(
      "1&#176; structure (",
      "<span style='color:",
      disease.variant.col,
      "'>Disease variants</span>)"
    )
  }
  if (show.markers) {
    clvtext <- if (!is.null(clv)) {
      # Get cleavage sites for each amino acid
      sapply(position, \(i) {
        if (i %in% unlist(clv)) {
          paste0(
            "\n<b><em>Cleavage site for:</em></b>\n",
            paste0(names(clv)[sapply(clv, \(x) i %in% x)], collapse = "\n")
          )
        } else {
          ""
        }
      })
    } else {
      NULL
    }
    plt <- plt |>
      plotly::add_trace(
        x = xs,
        y = ys,
        type = "scatter",
        mode = scatter.mode,
        marker = list(
          color = plotly::toRGB(marker.col, alpha = marker.alpha),
          size = marker.size,
          symbol = marker.symbol
        ),
        line = list(
          color = plotly::toRGB(line.col, alpha = line.alpha),
          width = line.width
        ),
        text = paste0(position, ": ", xnames, clvtext),
        name = aaname,
        # hoverinfo = marker.hoverinfo
        hoverinfo = "text"
      )
  }

  # regions ----
  if (!is.null(region)) {
    regionnames <- names(region)
    if (is.null(regionnames)) {
      regionnames <- paste("region", seq_along(region))
    }

    if (region.style == 1) {
      # '- region style 1 ----
      # for overlapping sets within each region
      for (i in seq_along(region)) {
        for (j in seq_along(region[[i]])) {
          plt <- plt |>
            plotly::add_trace(
              x = xs[region[[i]][[j]]],
              y = ys[region[[i]][[j]]],
              type = "scatter",
              mode = region.scatter.mode,
              marker = list(
                color = plotly::toRGB(
                  region.palette[[i]],
                  alpha = region.marker.alpha
                ),
                size = region.marker.size,
                symbol = region.marker.symbol
              ),
              line = list(
                color = plotly::toRGB(
                  region.palette[[i]],
                  alpha = region.line.alpha
                ),
                dash = region.line.dash,
                shape = region.line.shape,
                smoothing = region.line.smoothing,
                width = region.line.width
              ),
              name = regionnames[i],
              legendgroup = regionnames[i],
              showlegend = j == 1
            )
          if (region.outline.only) {
            # simulate rounded selection around AAs
            # need region.marker.size & line.width > marker.size
            plt <- plt |>
              plotly::add_trace(
                x = xs[region[[i]][[j]]],
                y = ys[region[[i]][[j]]],
                type = "scatter",
                mode = region.scatter.mode,
                marker = list(
                  color = plotly::toRGB(
                    # marker.col,
                    theme$bg,
                    alpha = marker.alpha
                  ),
                  size = region.marker.size - region.outline.pad,
                  symbol = region.marker.symbol
                ),
                line = list(
                  color = plotly::toRGB(
                    # line.col,
                    theme$bg,
                    alpha = line.alpha
                  ),
                  shape = region.line.shape,
                  smoothing = region.line.smoothing,
                  width = region.line.width - region.outline.pad
                ),
                name = NULL,
                legendgroup = regionnames[i],
                showlegend = FALSE
              )
            plt <- plt |>
              plotly::add_trace(
                x = xs[region[[i]][[j]]],
                y = ys[region[[i]][[j]]],
                type = "scatter",
                mode = scatter.mode,
                marker = list(
                  color = plotly::toRGB(marker.col, alpha = marker.alpha),
                  size = marker.size,
                  symbol = marker.symbol
                ),
                line = list(
                  color = plotly::toRGB(line.col, alpha = line.alpha),
                  width = line.width
                ),
                name = NULL,
                legendgroup = regionnames[i],
                showlegend = FALSE
              )
          }
        }
      }
    } else if (region.style == 2) {
      # '- region style 2 ----
      # for non-overlapping sets within each region
      for (i in seq_along(region)) {
        plt <- plt |>
          plotly::add_trace(
            x = xs[unlist(region[[i]])],
            y = ys[unlist(region[[i]])],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = plotly::toRGB(
                region.palette[[i]],
                alpha = region.marker.alpha
              ),
              size = region.marker.size,
              symbol = region.marker.symbol
            ),
            name = regionnames[i]
          )
      }
    } else {
      # '- region style 3 ----
      # for 1curve only
      # region polys: get marker direction and location:
      # left, leftborder, right, rightborder
      dl <- c(
        "r",
        rep(c("r", "l"), each = n.per.row - 1, length = n - 1)
      )
      dl[seq(n.per.row, n, n.per.row - 1)] <-
        paste0(dl[seq(n.per.row, n, n.per.row - 1)], "b")
      # i: IDI of region group
      for (i in seq_along(region)) {
        # each region's directions
        region_dl <- lapply(seq_along(region[[i]]), \(j) {
          dl[region[[i]][[j]]]
        })

        region_poly_xy <- lapply(seq_along(region[[i]]), \(j) {
          poly_xys(
            xs = xs[region[[i]][[j]]],
            ys = ys[region[[i]][[j]]],
            d = region_dl[[j]],
            pad = region.pad,
            bpadcx = bpadcx,
            bpadcy = bpadcy
          )
        })

        for (j in seq_along(region[[i]])) {
          plt <- plt |>
            plotly::add_polygons(
              x = region_poly_xy[[j]]$px,
              y = region_poly_xy[[j]]$py,
              line = list(
                color = region.palette[[i]],
                width = region.line.width,
                shape = region.fill.shape,
                smoothing = region.fill.smoothing
              ),
              fillcolor = plotly::toRGB(
                region.palette[[i]],
                alpha = region.fill.alpha
              ),
              name = regionnames[i],
              legendgroup = regionnames[i],
              showlegend = j == 1
            )
        }
      } # each region's individual regions' coords
    }
  } # /regions

  # Sites ----
  if (!is.null(site)) {
    sitenames <- names(site)
    if (is.null(sitenames)) {
      sitenames <- paste("Site", seq_along(site))
    }
    # for overlapping sets within each region
    for (i in seq_along(site)) {
      for (j in seq_along(site[[i]])) {
        plt <- plt |>
          plotly::add_trace(
            x = xs[site[[i]][[j]]],
            y = ys[site[[i]][[j]]],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = plotly::toRGB(
                "#000000",
                alpha = 0
              ),
              size = site.marker.size,
              symbol = site.marker.symbol,
              line = list(
                color = plotly::toRGB(
                  site.palette[[i]],
                  alpha = site.marker.alpha
                ),
                width = site.border.width
              )
            ),
            name = sitenames[i],
            legendgroup = sitenames[i],
            showlegend = j == 1
          )
      }
    }
  } # /sites

  # PTMs ----
  # Note: Do not show both PTMs and cleavage sites using the same padding
  if (!is.null(ptm)) {
    if (verbosity > 1) msg2("Adding PTM markers...")
    if (is.null(ptm.col)) ptm.col <- 1 + seq_along(ptm)
    ptm.symbol <- recycle(ptm.symbol, ptm)
    ptm.names <- names(ptm)
    for (i in seq_along(ptm)) {
      polyoffset <- npad(i, n = length(ptm), pad = ptm.pad)
      plt <- plt |>
        plotly::add_trace(
          x = xs[ptm[[i]]] + polyoffset[1],
          y = ys[ptm[[i]]] + polyoffset[2],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = plotly::toRGB(ptm.col[[i]]),
            size = ptm.marker.size,
            symbol = ptm.symbol[i]
          ),
          name = ptm.names[i],
          showlegend = showlegend.ptm
        )
    }
  }

  # Cleavage sites ----
  # Note: Do not show both PTMs and cleavage sites using the same padding
  if (!is.null(clv)) {
    if (verbosity > 1) msg2("Adding cleavage site markers...")
    if (is.null(clv.col)) {
      clv.col <- colorspace::qualitative_hcl(
        n = (length(clv)),
        h = c(40, 360),
        c = 120,
        l = 50
      )
    }
    clv.symbol <- recycle(clv.symbol, clv)
    clv.names <- names(clv)
    for (i in seq_along(clv)) {
      polyoffset <- npad(i, n = length(clv), pad = clv.pad)
      plt <- plt |>
        plotly::add_trace(
          x = xs[clv[[i]]] + polyoffset[1],
          y = ys[clv[[i]]] + polyoffset[2],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = plotly::toRGB(clv.col[[i]]),
            size = clv.marker.size,
            symbol = clv.symbol[i]
          ),
          name = clv.names[i],
          showlegend = showlegend.ptm
        )
    }
  }

  # AA labels ----
  if (show.labels) {
    # Variants
    if (!is.null(variant)) {
      variant.idi <- sapply(variant, \(v) v$Position)
      label.col[variant.idi] <- variant.col
    }
    # Disease variants
    if (!is.null(disease.variants)) {
      label.col[disease.variants] <- disease.variant.col
    }
    label.group <- factor(label.col)
    label.group.col <- levels(label.group)
    for (i in seq_along(label.group.col)) {
      idx <- label.group == label.group.col[i]
      plt <- plt |>
        plotly::add_annotations(
          xref = "x",
          yref = "y",
          x = xs[idx],
          y = ys[idx],
          text = x[idx],
          font = list(
            family = theme$font.family,
            size = font.size,
            color = label.group.col[[i]]
          ),
          showarrow = FALSE
          # name = label.group.levels[[i]],
          # showlegend = nchar(label.group.levels[[i]]) > 0
        )
    }
    # }
  }

  # Position annotations ----
  if (
    !is.null(annotate.position.every) && length(x) > annotate.position.every
  ) {
    idxpos <- seq(annotate.position.every, n, annotate.position.every)
    plt <- plt |>
      plotly::add_annotations(
        x = xs[idxpos],
        y = ys[idxpos],
        xref = "x",
        yref = "y",
        xanchor = "middle",
        yanchor = "bottom",
        ax = 0,
        ay = annotate.position.ay,
        text = idxpos,
        showarrow = TRUE,
        arrowcolor = "#ffffff00",
        font = list(
          size = position.font.size,
          family = theme$font.family,
          color = plotly::toRGB(theme$fg, alpha = annotate.position.alpha)
        )
      )
  }

  # Layout ----
  .legend <- list(
    x = legend.xy[1],
    xanchor = legend.xanchor,
    y = legend.xy[2],
    yanchor = legend.yanchor,
    font = list(
      family = theme$font.family,
      size = font.size,
      color = legend.col
    ),
    orientation = legend.orientation,
    bgcolor = plotly::toRGB(legend.bg),
    bordercolor = plotly::toRGB(legend.border.col),
    borderwidth = legend.borderwidth,
    tracegroupgap = legend.group.gap
  )

  plt <- plotly::layout(
    plt,
    xaxis = list(
      autorange = xaxis.autorange,
      showgrid = showgrid.x,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      zeroline = FALSE,
      showticklabels = FALSE,
      automargin = automargin.x
    ),
    yaxis = list(
      autorange = yaxis.autorange,
      showgrid = showgrid.y,
      gridcolor = grid.col,
      gridwidth = theme$grid.lwd,
      zeroline = FALSE,
      showticklabels = FALSE,
      automargin = automargin.y,
      scaleanchor = scaleanchor.y,
      scaleratio = scaleratio.y
    ),
    title = list(
      text = main,
      font = list(
        family = theme$font.family,
        size = font.size,
        color = main.col
      ),
      xref = main.xref,
      yref = main.yref,
      xanchor = main.xanchor,
      yanchor = main.yanchor,
      x = main.xy[1],
      y = main.xy[2]
    ),
    paper_bgcolor = theme$bg,
    plot_bgcolor = theme$plot.bg,
    margin = margin,
    legend = .legend,
    hoverlabel = list(
      align = "hoverlabel.align"
    )
  )

  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar.file.format,
      width = file.width,
      height = file.height
    ),
    scrollZoom = TRUE
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = file.path(filename),
      width = file.width,
      height = file.height,
      scale = file.scale
    )
  }

  return(plt)
} # rtemis::dplot3_protein

aa <- data.frame(
  Abbreviation1 = c(
    "A",
    "R",
    "N",
    "D",
    "C",
    "Q",
    "E",
    "G",
    "H",
    "I",
    "L",
    "K",
    "M",
    "F",
    "P",
    "S",
    "T",
    "W",
    "Y",
    "V",
    "B",
    "Z",
    "X",
    ""
  ),
  Abbreviation3 = c(
    "Ala",
    "Arg",
    "Asn",
    "Asp",
    "Cys",
    "Gln",
    "Glu",
    "Gly",
    "His",
    "Ile",
    "Leu",
    "Lys",
    "Met",
    "Phe",
    "Pro",
    "Ser",
    "Thr",
    "Trp",
    "Tyr",
    "Val",
    "Asx",
    "Glx",
    "Xaa",
    "TERM"
  ),
  Name = c(
    "Alanine",
    "Arginine",
    "Asparagine",
    "Aspartate",
    "Cysteine",
    "Glutamine",
    "Glutamate",
    "Glycine",
    "Histidine",
    "Isoleucine",
    "Leucine",
    "Lysine",
    "Methionine",
    "Phenylalanine",
    "Proline",
    "Serine",
    "Threonine",
    "Tryptophan",
    "Tyrosine",
    "Valine",
    "Aspartic acid or Asparagine",
    "Glutamine or Glutamic acid",
    "(Any)",
    "Termination codon"
  )
)


poly_xys <- function(xs, ys, d, pad = 1, bpadcx = .5, bpadcy = .5) {
  n <- length(xs)
  dk <- rep(1, n)
  kinks <- which("rb" == d | "lb" == d)
  for (i in kinks) {
    if ((i + 1) <= n) {
      dk[(i + 1):n] <- -dk[(i + 1):n]
    }
  }

  # première ----
  px_1 <- switch(
    d[1],
    "r" = xs[1] - pad,
    "l" = xs[1] + pad,
    "rb" = c(xs[1] - pad, xs[1]),
    "lb" = c(xs[1] + pad, xs[1])
  )
  py_1 <- switch(
    d[1],
    "rb" = rep(ys[1] - pad, 2),
    "lb" = rep(ys[1] - pad, 2),
    ys[1] - pad
  )

  # aller ----
  # k: IDI of individual amino acid within individual region
  px_aller <-
    sapply(seq_along(d), \(k) {
      if (d[k] == "rb") {
        # rep(xs[k] + sqrt(.5 * pad^2), 2)
        rep(xs[k] + pad, 2)
      } else if (d[k] == "lb") {
        # rep(xs[k] - sqrt(.5 * pad^2), 2)
        rep(xs[k] - pad, 2)
      } else {
        xs[k]
      }
    }) |>
    unlist()

  py_aller <-
    sapply(seq_along(d), \(k) {
      if (d[[k]] %in% c("l", "r")) {
        if (dk[k] == -1) {
          ys[k] + pad
        } else {
          ys[k] - pad
        }
      } else {
        if (k == 1) {
          c(ys[k] - pad, ys[k] + sqrt(.5 * pad^2))
        } else if (k == length(d)) {
          c(ys[k] - sqrt(.5 * pad^2), ys[k] + pad)
        } else {
          c(ys[k] - sqrt(.5 * pad^2), ys[k] + sqrt(.5 * pad^2))
        }
      }
    }) |>
    unlist()

  # centre ----
  dr <- rev(d)
  dkr <- rev(dk)
  xsr <- rev(xs)
  ysr <- rev(ys)
  px_centre <-
    switch(
      dr[1],
      "r" = rep(xsr[1] + pad, 2),
      "l" = rep(xsr[1] - pad, 2),
      # "rb" = c(xsr[1], xsr[1] - sqrt(.5 * pad^2)),
      # "lb" = c(xsr[1], xsr[1] + sqrt(.5 * pad^2))
      "rb" = c(xsr[1], xsr[1] - pad),
      "lb" = c(xsr[1], xsr[1] + pad)
    )

  py_centre <-
    if (dr[1] %in% c("r", "l")) {
      if (length(kinks) > 0) {
        c(ysr[1] + pad, ysr[1] - pad)
      } else {
        c(ysr[1] - pad, ysr[1] + pad)
      }
    } else {
      rep(ysr[1] + pad, 2)
    }

  # retour ----
  px_retour <-
    sapply(seq_along(dr), \(k) {
      if (dr[k] == "rb") {
        if (k == 1 | k == length(dr)) {
          rep(xsr[k] - pad, 2)
        } else {
          rep(xsr[k] - 1.5 * sqrt(.5 * pad^2), 2)
        }
      } else if (dr[k] == "lb") {
        if (k == 1 | k == length(dr)) {
          rep(xsr[k] + pad, 2)
        } else {
          rep(xsr[k] + 1.5 * sqrt(.5 * pad^2), 2)
        }
      } else {
        xsr[k]
      }
    }) |>
    unlist()

  py_retour <-
    sapply(seq_along(dr), \(k) {
      if (dr[[k]] %in% c("l", "r")) {
        if (dkr[k] == -1) {
          ysr[k] - pad
        } else {
          ysr[k] + pad
        }
      } else {
        rep(ysr[k], 2)
      }
    }) |>
    unlist()

  # find point before and after rb/lb
  idirb <- which(d == "rb")
  if (length(idirb) > 0) {
    if (idirb > 1) {
      px_aller[idirb - 1] <- px_aller[idirb - 1] + sqrt(.5 * pad^2)
    }
    if ((idirb + 1) <= length(d)) {
      px_aller[idirb + 2] <- px_aller[idirb + 2] + sqrt(.5 * pad^2)
    }
  }

  idilb <- which(d == "lb")
  if (length(idilb) > 0) {
    if (idilb > 1) {
      px_aller[idilb - 1] <- px_aller[idilb - 1] - sqrt(.5 * pad^2)
    }
    if ((idilb + 1) <= length(d)) {
      px_aller[idilb + 2] <- px_aller[idilb + 2] - sqrt(.5 * pad^2)
    }
  }

  # pénultième ----
  py_pen <- if (d[1] %in% c("rb", "lb")) ys[1] - sqrt(.5 * pad^2) else
    ys[1] + pad

  # out ----
  list(
    px = c(px_1, px_aller, px_centre, px_retour, px_1[1], px_1[1]),
    py = c(py_1, py_aller, py_centre, py_retour, py_pen, py_1[1])
  )
}


qrtpad <- function(i, pad = .3) {
  qrt <- sqrt(.5 * pad^2)
  switch(
    i,
    `1` = c(qrt, -qrt),
    `2` = c(pad, 0),
    `3` = c(qrt, qrt),
    `4` = c(0, pad),
    `5` = c(-qrt, qrt),
    `6` = c(-pad, 0),
    `7` = c(-qrt, -qrt)
  )
}

# plot(x = 0:2, y = 0:2, pch = 19)
# for (i in 1:7) {
#   pd <- qrtpad(i)
#   points(x = 1 + pd[1], y = 1 + pd[2], pch = 2, col = "red")
# }
# sapply(1:7, qrtpad)

# npad: function to calculate circular offset of a point from the center of a region
# by dividing circle into n equal parts, beginning from the top
npad <- function(i, n = 12, pad = .3) {
  angle <- 2 * pi / n
  x <- sin(angle * i) * pad
  y <- cos(angle * i) * pad
  c(x, y)
}
