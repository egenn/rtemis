# dplot3_protein

#' Plot the primary amino acid sequence of a protein
#'
#' @param x Character vector of amino acid sequence (1-letter abbreviations)
#' @param group Named list of lists with indices of groups. These will be
#' highlighted by coloring the border of markers
#' @param motif Named list of lists with indices of motifs. These will be
#' highlighted by coloring the markers and lines of motifs using the
#' \code{palette} colors
#' @param n.per.row Integer: Number of amino acids to show per row
#' @param main Character: Main title
#' @param main.xy Numeric vector, length 2: x and y coordinates for title.
#' e.g. if \code{main.xref} and \code{main.yref} are \code{"paper"}:
#' \code{c(0.055, .975)} is top left, \code{c(.5, .975)} is top and
#' middle
#' @param main.xref Character: xref for title
#' @param main.yref Character: yref for title
#' @param main.xanchor Character: xanchor for title
#' @param main.yanchor Character: yanchor for title
#' @param layout Character: "1curve", "grid": type of layout to use
#' @param show.markers Logical: If TRUE, show amino acid markers
#' @param show.text Logical: If TRUE, annotate amino acids with elements
#' from input \code{x}
#'
#' @author E.D. Gennatas
#' @export
#' @examples
#' \dontrun{
#' tau <- seqinr::read.fasta("https://rest.uniprot.org/uniprotkb/P10636.fasta",
#'     seqtype = "AA"
#' )
#' dplot3_protein(as.character(tau[[1]]))
#' }
#' # or directly using the UniProt accession number:
#' dplot3_protein("P10636")
#'
dplot3_protein <- function(x,
                           group = NULL,
                           motif = NULL,
                           ptm = NULL,
                           n.per.row = NULL,
                           main = NULL,
                           main.xy = c(0.055, .975),
                           main.xref = "paper",
                           main.yref = "paper",
                           main.xanchor = "middle",
                           main.yanchor = "top",
                           layout = c("simple", "grid", "1curve", "2curve"),
                           show.markers = TRUE,
                           show.text = TRUE,
                           font.size = 11,
                           text.col = NULL,
                           scatter.mode = "markers+lines",
                           # AA marker
                           marker.size = 26,
                           marker.col = NULL, #"gray18",
                           marker.alpha = 1,
                           marker.symbol = "circle",
                           marker.hoverinfo = "text",
                           # AA line
                           line.col = NULL, #"gray18",
                           line.alpha = 1,
                           line.width = 2,
                           # Hover names
                           show.full.names = FALSE,
                           # Motifs
                           motif.scatter.mode = "markers+lines",
                           motif.style = 3,
                           motif.marker.size = marker.size,
                           motif.marker.alpha = .6,
                           motif.marker.symbol = "circle",
                           motif.line.dash = "solid",
                           motif.line.shape = "line",
                           motif.line.smoothing = 1,
                           motif.line.width = 1,
                           motif.line.alpha = .6,
                           theme = rtTheme,
                           motif.palette = rtPalette,
                           motif.outline.only = FALSE,
                           motif.outline.pad = 2, # for fake polys
                           motif.pad = .45, # for real polys
                           motif.fill.alpha = .1666666,
                           motif.fill.shape = "line",
                           motif.fill.smoothing = 1,
                           bpadcx = .5,
                           bpadcy = .5,
                           # Groups
                           group.marker.size = marker.size,
                           group.marker.symbol = marker.symbol,
                           group.marker.alpha = 1,
                           group.border.width = 1,
                           group.palette = rtPalette,
                           # Text groups
                           disease.variants = NULL,
                           text.group = NULL,
                           text.group.palette = c(theme$fg, "red"),
                           # PTMs
                           showlegend.ptm = TRUE,
                           ptm.col = 2:10,
                           ptm.symbol = "circle",
                           ptm.offset = .2,
                           ptm.pad = .1,
                           ptm.marker.size = marker.size/4,
                           # Position annotations
                           annotate.position.every = 10,
                           annotate.position.alpha = .5,
                           annotate.position.ay = -.4 * marker.size,
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
                           # config
                           displayModeBar = TRUE,
                           modeBar.file.format = "svg",
                           scrollZoom = TRUE,
                           # file out
                           filename = NULL,
                           file.width = 500,
                           file.height = 500,
                           file.scale = 1,
                           width = NULL,
                           height = NULL,
                           verbose = TRUE,
                           trace = 0, ...) {

    # Data ----
    if (length(x) == 1) {
        dat <- uniprot_get(x, verbose = verbose)
        x <- dat[["Sequence"]]
        if (is.null(main)) main <- dat[["Identifier"]]
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

    # add .5 to ends of lines
    # diff(ys)

    # Theme ----
    extraargs <- list(...)
    if (is.character(theme)) {
        theme <- do.call(paste0("theme_", theme), extraargs)
    } else {
        for (i in seq(extraargs)) {
            theme[[names(extraargs)[i]]] <- extraargs[[i]]
        }
    }
    if (is.null(text.col)) text.col <- theme$fg
    if (is.null(marker.col)) marker.col <- color_fade(theme$fg, theme$bg, .9)
    marker.col <- plotly::toRGB(marker.col, alpha = marker.alpha)
    if (is.null(line.col)) line.col <- color_fade(theme$fg, theme$bg, .9)
    line.col <- plotly::toRGB(line.col, alpha = marker.alpha)

    main.col <- plotly::toRGB(theme$main.col)
    labs.col <- plotly::toRGB(theme$labs.col)
    if (is.null(legend.col)) legend.col <- labs.col
    grid.col <- plotly::toRGB(theme$grid.col, theme$grid.alpha)

    # Palette ----
    if (is.character(motif.palette)) motif.palette <- rtpalette(motif.palette)
    if (is.character(group.palette)) group.palette <- rtpalette(group.palette)

    # Match abbreviations to full names ----
    if (show.full.names) {
        input <- switch(max(nchar(x)),
            "1" = "1",
            "3" = "3",
            "full"
        )

        if (input == "full") {
            xnames <- x
        } else {
            if (input == "1") {
                xnames <- factor(x,
                    levels = aa$Abbreviation1,
                    labels = aa$Name
                ) |> as.character()
            } else {
                xnames <- factor(x,
                    levels = toupper(aa$Abbreviation3),
                    labels = aa$Name
                ) |> as.character()
            }
        }
    } else {
        xnames <- x
    }

    # plotly ----
    plt <- plotly::plot_ly(
        width = width,
        height = height
    )

    # AA markers and lines ----
    if (show.markers) {
        plt |> plotly::add_trace(
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
            text = paste0(position, ": ", xnames),
            name = "",
            hoverinfo = marker.hoverinfo
        ) -> plt
    }

    # Motifs ----
    if (!is.null(motif)) {
        motifnames <- names(motif)
        if (is.null(motifnames)) {
            motifnames <- paste("Motif", seq_along(motif))
        }

        if (motif.style == 1) {
            # '- Motif style 1 ----
            # for overlapping sets within each motif
            for (i in seq_along(motif)) {
                for (j in seq_along(motif[[i]])) {
                    plt |> plotly::add_trace(
                        x = xs[motif[[i]][[j]]],
                        y = ys[motif[[i]][[j]]],
                        type = "scatter",
                        mode = motif.scatter.mode,
                        marker = list(
                            color = plotly::toRGB(
                                motif.palette[[i]],
                                alpha = motif.marker.alpha
                            ),
                            size = motif.marker.size,
                            symbol = motif.marker.symbol
                        ),
                        line = list(
                            color = plotly::toRGB(
                                motif.palette[[i]],
                                alpha = motif.line.alpha
                            ),
                            dash = motif.line.dash,
                            shape = motif.line.shape,
                            smoothing = motif.line.smoothing,
                            width = motif.line.width
                        ),
                        name = motifnames[i],
                        legendgroup = motifnames[i],
                        showlegend = j == 1
                    ) -> plt
                    if (motif.outline.only) {
                        # simulate rounded selection around AAs
                        # need motif.marker.size & line.width > marker.size
                        plt |> plotly::add_trace(
                            x = xs[motif[[i]][[j]]],
                            y = ys[motif[[i]][[j]]],
                            type = "scatter",
                            mode = motif.scatter.mode,
                            marker = list(
                                color = plotly::toRGB(
                                    # marker.col,
                                    theme$bg,
                                    alpha = marker.alpha
                                ),
                                size = motif.marker.size - motif.outline.pad,
                                symbol = motif.marker.symbol
                            ),
                            line = list(
                                color = plotly::toRGB(
                                    # line.col,
                                    theme$bg,
                                    alpha = line.alpha
                                ),
                                shape = motif.line.shape,
                                smoothing = motif.line.smoothing,
                                width = motif.line.width - motif.outline.pad
                            ),
                            name = NULL,
                            legendgroup = motifnames[i],
                            showlegend = F
                        ) -> plt
                        plt |> plotly::add_trace(
                            x = xs[motif[[i]][[j]]],
                            y = ys[motif[[i]][[j]]],
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
                            legendgroup = motifnames[i],
                            showlegend = F
                        ) -> plt
                    }
                }
            }
        } else if (motif.style == 2) {
            # '- Motif style 2 ----
            # for non-overlapping sets within each motif
            for (i in seq_along(motif)) {
                plt |> plotly::add_trace(
                    x = xs[unlist(motif[[i]])],
                    y = ys[unlist(motif[[i]])],
                    type = "scatter",
                    mode = "markers",
                    marker = list(
                        color = plotly::toRGB(
                            motif.palette[[i]],
                            alpha = motif.marker.alpha
                        ),
                        size = motif.marker.size,
                        symbol = motif.marker.symbol
                    ),
                    name = motifnames[i]
                ) -> plt
            }
        } else {
            # '- Motif style 3 ----
            # for 1curve only
            # motif polys: get marker direction and location:
            # left, leftborder, right, rightborder
            dl <- c(
                "r",
                rep(c("r", "l"), each = n.per.row - 1, length = n - 1)
            )
            dl[seq(n.per.row, n, n.per.row - 1)] <-
                paste0(dl[seq(n.per.row, n, n.per.row - 1)], "b")
            # i: IDI of motif group
            for (i in seq_along(motif)) {
                # each motif's directions
                motif_dl <- lapply(seq_along(motif[[i]]), \(j) {
                    dl[motif[[i]][[j]]]
                })

                motif_poly_xy <- lapply(seq_along(motif[[i]]), \(j) {
                    poly_xys(
                        xs = xs[motif[[i]][[j]]],
                        ys = ys[motif[[i]][[j]]],
                        d = motif_dl[[j]],
                        pad = motif.pad,
                        bpadcx = bpadcx,
                        bpadcy = bpadcy
                    )
                })
                
                for (j in seq_along(motif[[i]])) {
                    plt |> plotly::add_polygons(
                        x = motif_poly_xy[[j]]$px,
                        y = motif_poly_xy[[j]]$py,
                        line = list(
                            color = motif.palette[[i]], 
                            width = motif.line.width, 
                            shape = motif.fill.shape,
                            smoothing = motif.fill.smoothing),
                        fillcolor = plotly::toRGB(
                            motif.palette[[i]],
                            alpha = motif.fill.alpha
                        ),
                        name = motifnames[i],
                        legendgroup = motifnames[i],
                        showlegend = j == 1
                    ) -> plt
                }
            } # each motif's regions' coords
        }
    } # /motifs

    # Groups ----
    if (!is.null(group)) {
        groupnames <- names(group)
        if (is.null(groupnames)) {
            groupnames <- paste("Group", seq_along(group))
        }
        # for overlapping sets within each motif
        for (i in seq_along(group)) {
            for (j in seq_along(group[[i]])) {
                plt |> plotly::add_trace(
                    x = xs[group[[i]][[j]]],
                    y = ys[group[[i]][[j]]],
                    type = "scatter",
                    mode = "markers",
                    marker = list(
                        color = plotly::toRGB(
                            "#000000",
                            alpha = 0
                        ),
                        size = group.marker.size,
                        symbol = group.marker.symbol,
                        line = list(
                            color = plotly::toRGB(
                                group.palette[[i]],
                                alpha = group.marker.alpha
                            ),
                            width = group.border.width
                        )
                    ),
                    name = groupnames[i],
                    legendgroup = groupnames[i],
                    showlegend = j == 1
                ) -> plt
            }
        }
    } # /groups

    # PTMs ----
    if (!is.null(ptm)) {
        if (trace > 0) msg("Adding PTM markers...")
        ptm.ypad <- lapply(seq_along(ptm), \(i) rep(ptm.offset, n))
        ptm.ypad[[1]][ptm[[1]]] <- ptm.offset + ptm.pad
        for (i in 2:length(ptm)) {
            ptm.ypad[[i]][ptm[[i]]] <- ptm.ypad[[i-1]][ptm[[i]]] + ptm.pad
        }
        ptm.symbol <- recycle(ptm.symbol, ptm)
        ptm.names <- names(ptm)
        for (i in seq_along(ptm)) {
            plt |> plotly::add_trace(
                x = xs[ptm[[i]]],
                y = ys[ptm[[i]]] + ptm.ypad[[i]][ptm[[i]]],
                type = "scatter",
                mode = "markers",
                marker = list(
                    color = plotly::toRGB(ptm.col[[i]]),
                    size = ptm.marker.size,
                    symbol = ptm.symbol[i]
                ),
                name = ptm.names[i],
                showlegend = showlegend.ptm
            ) -> plt
        }
    }

    # AA labels ----
    if (show.text) {
        text.col.levels <- unique(text.col)
        n.text.groups <- length(text.col.levels)
        if (is.null(text.group) & n.text.groups > 1) {
            text.group <- factor(text.col)
        }
        if (is.null(text.group) & !is.null(disease.variants)) {
            text.group <- character(length(x))
            text.group[disease.variants] <- "Disease-Associated Variant"
            text.group <- factor(text.group)
        }
        if (is.null(text.group)) {
            plt |> plotly::add_annotations(
                xref = "x",
                yref = "y",
                x = xs,
                y = ys,
                text = x,
                font = list(
                    family = theme$font.family,
                    size = font.size,
                    color = text.col
                ),
                showarrow = FALSE
            ) -> plt
        } else {
            text.group <- factor(text.group)
            text.group.levels <- levels(text.group)
            for (i in seq_along(text.group.levels)) {
                idx <- text.group == text.group.levels[i]
                plt |> plotly::add_annotations(
                    xref = "x",
                    yref = "y",
                    x = xs[idx],
                    y = ys[idx],
                    text = x[idx],
                    font = list(
                        family = theme$font.family,
                        size = font.size,
                        color = text.group.palette[[i]]
                    ),
                    showarrow = FALSE
                    # name = text.group.levels[[i]],
                    # showlegend = nchar(text.group.levels[[i]]) > 0
                ) -> plt
            }
        }
    }

    # Position annotations ----
    if (!is.null(annotate.position.every) && length(x) > annotate.position.every) {
        idxpos <- seq(annotate.position.every, n, annotate.position.every)
        plt |> plotly::add_annotations(
            x = xs[idxpos],
            y = ys[idxpos],
            xref = "x",
            yref = "y",
            xanchor = "middle",
            yanchor = "bottom",
            ax = 0,
            ay = annotate.position.ay,
            text = idxpos,
            showarrow = T,
            arrowcolor = "#ffffff00",
            font = list(
                size = font.size,
                family = theme$font.family,
                color = plotly::toRGB(theme$fg, alpha = annotate.position.alpha)
            )
        ) -> plt
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

    plt <- plotly::layout(plt,
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
        legend = .legend
    )

    # Config
    plt <- plotly::config(plt,
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

    plt
} # rtemis::dplot3_protein

aa <- data.frame(
    Abbreviation1 = c(
        "A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
        "M", "F", "P", "S", "T", "W", "Y", "V", "B", "Z", "X", ""
    ),
    Abbreviation3 = c(
        "Ala", "Arg", "Asn", "Asp", "Cys", "Gln", "Glu", "Gly", "His",
        "Ile", "Leu", "Lys", "Met", "Phe", "Pro", "Ser", "Thr", "Trp",
        "Tyr", "Val", "Asx", "Glx", "Xaa", "TERM"
    ),
    Name = c(
        "Alanine", "Arginine", "Asparagine", "Aspartate",
        "Cysteine", "Glutamine", "Glutamate", "Glycine",
        "Histidine", "Isoleucine", "Leucine", "Lysine", "Methionine",
        "Phenylalanine", "Proline", "Serine", "Threonine", "Tryptophan",
        "Tyrosine", "Valine", "Aspartic acid or Asparagine",
        "Glutamine or Glutamic acid", "(Any)", "Termination codon"
    )
)


poly_xys <- function(xs, 
                     ys, 
                     d, 
                     pad = 1,
                     bpadcx = .5,
                     bpadcy = .5) {
    n <- length(xs)
    dk <- rep(1, n)
    kinks <- which("rb" == d | "lb" == d)
    for (i in kinks) {
        if ((i + 1) <= n) {
            dk[(i + 1):n] <- -dk[(i + 1):n]
        }
    }

    # dd <- c(0, diff(as.integer(factor(d))))
    # dd <- cumsum(abs(c(0, diff(as.integer(factor(d))))))
    # dn <- factor(d, levels = c("rb", "l", "r", "lb"), labels = c(-2, -1, 1, 2))
    # dd <- c(0, diff(dn))
    # dn <- factor(d, levels = c("rb", "l", "r", "lb"), labels = c(0, -1, 1, 0))

    # first ----
    px_1 <- switch(d[1],
        "r" = xs[1] - pad,
        "l" = xs[1] + pad,
        "rb" = c(xs[1] - pad, xs[1]),
        "lb" = c(xs[1] + pad, xs[1])
    )
    py_1 <- switch(d[1],
        "rb" = rep(ys[1] - pad, 2),
        "lb" = rep(ys[1] - pad, 2),
        ys[1] - pad
    )

    # aller ----
    # k: IDI of individual amino acid within region
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
        }) |> unlist()

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
        }) |> unlist()

    # centre ----
    dr <- rev(d)
    dkr <- rev(dk)
    xsr <- rev(xs)
    ysr <- rev(ys)
    px_centre <-
        switch(dr[1],
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
                if (k == 1 |k == length(dr)) {
                    rep(xsr[k] - pad, 2)
                } else {
                    rep(xsr[k] - sqrt(.5 * pad^2), 2)
                }
            } else if (dr[k] == "lb") {
                if (k == 1 | k == length(dr)) {
                    rep(xsr[k] + pad, 2)
                } else {
                    rep(xsr[k] + sqrt(.5 * pad^2), 2)
                }
            } else {
                xsr[k]
            }
        }) |> unlist()
    
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
        }) |> unlist()

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

    # penultimate ----
    py_pen <- if (d[1] %in% c("rb", "lb")) ys[1] - sqrt(.5 * pad^2) else ys[1] + pad

    # out ----
    list(
        px = c(px_1, px_aller, px_centre, px_retour, px_1[1], px_1[1]),
        py = c(py_1, py_aller, py_centre, py_retour, py_pen, py_1[1])
    )
}
