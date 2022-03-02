rectart <- function(col_lo = "#00ffff",
                    col_hi = "#ff00ff",
                    bg = "#121212",
                    k_h = 10, k_v = 10) {
  
  x <- seq(0, 100, length.out = k_h + 1)
  x[-c(1, k_h + 1)] <- x[-c(1, k_h + 1)] + rnorm(k_h - 1, sd = 10)
  y <- seq(0, 100, length.out = k_v + 1)
  y[-c(1, k_v + 1)] <- y[-c(1, k_v + 1)] + rnorm(k_v - 1, sd = 10)
  
  .col <- colorRampPalette(c(col_lo, col_hi))(k_h * k_v)
  
  # Plot
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4))
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100))
  for (i in seq(k_h)) {
    for (j in seq(k_v)) {
      polygon(x = c(x[i], x[i + 1], x[i + 1], x[i]),
              y = c(y[j], y[j], y[j + 1], y[j + 1]),
              col = .col[i*j], border = NA)
    }
  }
  
} # rtemis::rectart

polyart <- function(k_h = 20, k_v = 20,
                    jsd = 3,
                    col_lo = "#00ffff", 
                    col_mid = NULL,
                    col_hi = "#ff00ff",
                    bg = "#121212") {
  
  x <- matrix(rep(seq(0, 100, length.out = k_h + 1), k_v + 1), k_v + 1, byrow = TRUE)
  for (i in seq(k_v + 1)) {
    x[i, -c(1, k_h + 1)] <- x[i, -c(1, k_h + 1)] + rnorm(k_h - 1, sd = jsd)
  }
  
  y <- matrix(rep(seq(0, 100, length.out = k_v + 1), k_h + 1), k_v + 1)
  for (i in seq(k_h + 1)) {
    y[-c(1, k_v + 1), i] <- y[-c(1, k_v + 1), i] + rnorm(k_v - 1, sd = jsd)
  }
  
  .col <- colorRampPalette(c(col_lo, col_mid, col_hi))(k_h * k_v)
  
  
  # Plot
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4), xaxs = "i", yaxs = "i")
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100))
  for (i in seq(k_v)) {
    for (j in seq(k_h)) {
      polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1], x[i, j+1]),
              y = c(y[i, j], y[i+1, j], y[i+1, j+1], y[i, j+1]),
              col = .col[i*j], border = NA)
    }
  }
  
} # rtemis::polyart

triart <- function(k_h = 10, k_v = 6,
                   jsd = 3,
                   col_lo = "#00ffff",
                   col_hi = "#ff00ff",
                   bg = "#121212") {
  
  x <- matrix(rep(seq(0, 100, length.out = k_h + 1), k_v + 1), k_v + 1, byrow = TRUE)
  x
  for (i in seq(k_v + 1)) {
    x[i, -c(1, k_h + 1)] <- x[i, -c(1, k_h + 1)] + rnorm(k_h - 1, sd = jsd)
  }
  x
  
  y <- matrix(rep(seq(0, 100, length.out = k_v + 1), k_h + 1), k_v + 1)
  y
  for (i in seq(k_h + 1)) {
    y[-c(1, k_v + 1), i] <- y[-c(1, k_v + 1), i] + rnorm(k_v - 1, sd = jsd)
  }
  y
  
  .col <- colorRampPalette(c(col_lo, col_hi))(k_h * k_v)
  
  
  # Plot
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4), xaxs = "i", yaxs = "i")
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100))
  for (i in seq(k_v)) {
    for (j in seq(k_h)) {
      polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1]),
              y = c(y[i, j], y[i+1, j], y[i+1, j+1]),
              col = .col[i*j], border = NA)
    }
  }
  
} # rtemis::polyart

polyshadow0 <- function(k_h = 20, k_v = 20,
                        jsd = 1,
                        shadow = .95,
                        col_lo = "#00ffff", 
                        col_mid = NULL,
                        col_hi = "#ff00ff",
                        color.progression = c("product", "counter"),
                        bg = "#121212",
                        text = NULL,
                        text.x = 95,
                        text.y = 5, 
                        text.adj = c(1, 0),
                        text.col = "#ffffff",
                        text.cex = 2,
                        font.family = "") {
  
  color.progression <- match.arg(color.progression)
  
  x <- matrix(rep(seq(0, 100, length.out = k_h + 1), k_v + 1), k_v + 1, byrow = TRUE)
  for (i in seq(k_v + 1)) {
    x[i, -c(1, k_h + 1)] <- x[i, -c(1, k_h + 1)] + rnorm(k_h - 1, sd = jsd)
  }
  
  y <- matrix(rep(seq(0, 100, length.out = k_v + 1), k_h + 1), k_v + 1)
  for (i in seq(k_h + 1)) {
    y[-c(1, k_v + 1), i] <- y[-c(1, k_v + 1), i] + rnorm(k_v - 1, sd = jsd)
  }
  
  .col <- colorRampPalette(c(col_lo, col_mid, col_hi))(k_h * k_v)
  
  
  # Plot ====
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4), xaxs = "i", yaxs = "i")
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100))
  for (i in seq(k_v)) {
    for (j in seq(k_h)) {
      ind <- if (color.progression == "product") i*j else j + k_h*(i - 1)
      polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1]),
              y = c(y[i, j], y[i+1, j], y[i+1, j+1]),
              col = .col[ind], border = NA)
      polygon(x = c(x[i, j], x[i+1, j+1], x[i, j+1]),
              y = c(y[i, j], y[i+1, j+1], y[i, j+1]),
              col = adjustcolor(.col[ind], shadow), border = NA)
    }
  }
  
  # Text ====
  if (!is.null(text)) {
    text(x = text.x, 
         y = text.y, 
         labels = text, 
         adj = text.adj,
         col = text.col,
         cex = text.cex,
         family = font.family)
  }
  
} # rtemis::polyshadow

polyshadow1 <- function(k_h = 20, k_v = 20,
                      jsd = 1,
                      shadow = .95,
                      col_lo = "#00ffff", 
                      col_mid = NULL,
                      col_hi = "#ff00ff",
                      color.progression = c("product", "counter"),
                      shadow.direction = 1,
                      bg = "#121212",
                      text = NULL,
                      text.x = 95,
                      text.y = 5, 
                      text.adj = c(1, 0),
                      text.col = "#ffffff",
                      text.cex = 2,
                      font.family = "",
                      seed = NULL) {
  
  color.progression <- match.arg(color.progression)
  
  if (!is.null(seed)) set.seed(seed)
  x <- matrix(rep(seq(0, 100, length.out = k_h + 1), k_v + 1), k_v + 1, byrow = TRUE)
  for (i in seq(k_v + 1)) {
    x[i, -c(1, k_h + 1)] <- x[i, -c(1, k_h + 1)] + rnorm(k_h - 1, sd = jsd)
  }
  
  y <- matrix(rep(seq(0, 100, length.out = k_v + 1), k_h + 1), k_v + 1)
  for (i in seq(k_h + 1)) {
    y[-c(1, k_v + 1), i] <- y[-c(1, k_v + 1), i] + rnorm(k_v - 1, sd = jsd)
  }
  
  .col <- colorRampPalette(c(col_lo, col_mid, col_hi))(k_h * k_v)
  
  
  # Plot ====
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4), 
      xaxs = "i", yaxs = "i")
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100),
       axes = FALSE, ann = FALSE)
  for (i in seq(k_v)) {
    for (j in seq(k_h)) {
      ind <- if (color.progression == "product") i*j else j + k_h*(i - 1)
      if (shadow.direction == 1) {
        polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1]),
                y = c(y[i, j], y[i+1, j], y[i+1, j+1]),
                col = .col[ind], border = NA)
        polygon(x = c(x[i, j], x[i+1, j+1], x[i, j+1]),
                y = c(y[i, j], y[i+1, j+1], y[i, j+1]),
                col = adjustcolor(.col[ind], shadow), border = NA)
      } else {
        polygon(x = c(x[i, j], x[i+1, j], x[i, j+1]),
                y = c(y[i, j], y[i+1, j], y[i, j+1]),
                col = adjustcolor(.col[ind], shadow), border = NA)
        polygon(x = c(x[i+1, j], x[i+1, j+1], x[i, j+1]),
                y = c(y[i+1, j], y[i+1, j+1], y[i, j+1]),
                col = .col[ind], border = NA)
      }
      
    }
  }
  
  # Text ====
  if (!is.null(text)) {
    text(x = text.x, 
         y = text.y, 
         labels = text, 
         adj = text.adj,
         col = text.col,
         cex = text.cex,
         family = font.family)
  }
  
} # rtemis::polyshadow


polyshadow <- function(k_h = 50, k_v = 50,
                       jsd = 5,
                       shadow = 1, # seq(.98, .92, length.out = k_h*k_v)
                       col_lo = "#00000000",
                       col_mid = NULL,
                       col_hi = "#ff00ff",
                       space = c("rgb", "Lab"),
                       alpha = TRUE,
                       color.progression = c("product", "mean", "counter"),
                       shadow.direction = 1,
                       border.alpha = 1,
                       poly.border = NA,
                       bg = "#000000",
                       text = NULL,
                       text.x = 97,
                       text.y = 3,
                       text.adj = c(1, 0),
                       text.col = "#ffffff80",
                       text.cex = 2,
                       text.srt = 0,
                       font.family = "",
                       xpd = FALSE,
                       seed = NULL,
                       filename = NULL,
                       pdf.width = 6,
                       pdf.height = 6,
                       trace = 0,
                       verbose = TRUE,
                       par.reset = TRUE) {
  
  color.progression <- match.arg(color.progression)
  # space <- match.arg(space)
  shadow <- recycle(shadow, seq(k_h*k_v))
  
  if (!is.null(seed)) set.seed(seed)
  x <- matrix(rep(seq(0, 100, length.out = k_h + 1), k_v + 1), k_v + 1, byrow = TRUE)
  for (i in seq(k_v + 1)) {
    x[i, -c(1, k_h + 1)] <- x[i, -c(1, k_h + 1)] + rnorm(k_h - 1, sd = jsd)
  }
  
  y <- matrix(rep(seq(0, 100, length.out = k_v + 1), k_h + 1), k_v + 1)
  for (i in seq(k_h + 1)) {
    y[-c(1, k_v + 1), i] <- y[-c(1, k_v + 1), i] + rnorm(k_v - 1, sd = jsd)
  }
  
  .col <- colorRampPalette(c(col_lo, col_mid, col_hi),
                           alpha = alpha)(k_h * k_v)
  
  # Plot ====
  if (!is.null(filename)) pdf(filename, width = pdf.width, height = pdf.height)
  
  if (par.reset) {
    par.orig <- par(no.readonly = TRUE)
    on.exit(par(par.orig))
  }
  par(bg = bg, mai = rep(0, 4), mar = rep(0, 4), 
      xaxs = "i", yaxs = "i",
      xpd = xpd)
  plot(NULL, xlim = c(0, 100), ylim = c(0, 100),
       axes = FALSE, ann = FALSE, xpd = xpd)
  for (i in seq(k_v)) {
    for (j in seq(k_h)) {
      ind <- switch (color.progression,
                     product = i*j,
                     counter = j + k_h*(i - 1),
                     mean = round( ((.5*(i+j)) / (.5*(k_h+k_v))) * k_h*k_v )
      )
      if (trace > 0) cat("i=", i, "; j=", j, "; ind=", ind, "\n")
      if (all(shadow == 1)) {
        if (border.alpha == 1) {
          polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1], x[i, j+1]),
                  y = c(y[i, j], y[i+1, j], y[i+1, j+1], y[i, j+1]),
                  col = .col[ind], 
                  border = poly.border,
                  xpd = xpd)
        } else {
          col <- adjustcolor(.col[ind], 
                             if (x[i, j] == 0 | x[i+1, j+1] == 100 | y[i, j] == 0 | y[i+1, j+1] == 100) 
                               border.alpha else 1)
          polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1], x[i, j+1]),
                  y = c(y[i, j], y[i+1, j], y[i+1, j+1], y[i, j+1]),
                  col = col, 
                  border = poly.border,
                  xpd = xpd)
        }
        
      } else {
        if (shadow.direction == 1) {
          polygon(x = c(x[i, j], x[i+1, j], x[i+1, j+1]),
                  y = c(y[i, j], y[i+1, j], y[i+1, j+1]),
                  col = .col[ind], 
                  border = poly.border,
                  xpd = xpd)
          polygon(x = c(x[i, j], x[i+1, j+1], x[i, j+1]),
                  y = c(y[i, j], y[i+1, j+1], y[i, j+1]),
                  col = adjustcolor(.col[ind], shadow[ind]), 
                  border = poly.border,
                  xpd = xpd)
        } else {
          polygon(x = c(x[i, j], x[i+1, j], x[i, j+1]),
                  y = c(y[i, j], y[i+1, j], y[i, j+1]),
                  col = adjustcolor(.col[ind], shadow[ind]), 
                  border = poly.border,
                  xpd = xpd)
          polygon(x = c(x[i+1, j], x[i+1, j+1], x[i, j+1]),
                  y = c(y[i+1, j], y[i+1, j+1], y[i, j+1]),
                  col = .col[ind], 
                  border = poly.border,
                  xpd = xpd)
        }
      }
    }
  }
  
  # Text ====
  if (!is.null(text)) {
    text(x = text.x, 
         y = text.y, 
         labels = text, 
         adj = text.adj,
         col = text.col,
         cex = text.cex,
         family = font.family,
         srt = text.srt)
  }
  
  if (!is.null(filename)) {
    dev.off()
    if (verbose) {
      if (file.exists(filename)) {
        msg("Saved", filename)
      } else {
        rtWarning("Failed to save", filename)
      }
    }
  }
  
  invisible(.col)
  
} # rtemis::polyshadow