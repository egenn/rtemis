# rt_lle_calc_k
# ::rtemis::
# 2019 E.D. Gennatas www.lambdamd.org

#' \pkg{rtemis} internal: `lle::calc_k` function adapted to work with `pbapply`
#'
#' @keywords internal

rt_lle_calc_k <- function(x, m,
                          kmin = 1, kmax = 20,
                          plotres = TRUE,
                          iLLE = FALSE,
                          n.cores = rtCores,
                          verbose = TRUE) {
  N <- NROW(x)
  if (kmax >= N) {
    kmax <- N - 1
  }

  perform_calc <- function(k, x, m, iLLE = FALSE) {
    N <- NROW(x)
    Y <- lle::lle(x, m, k, 2, 0, iLLE = iLLE)$Y
    Dx <- as.matrix(dist(x))
    Dy <- as.matrix(dist(Y))
    rho <- c()
    for (j in seq(N)) rho <- c(rho, cor(Dx[j, ], Dy[j, ]))
    mean(1 - rho^2)
  } # rtemis::perform_calc

  rho <- pbapply::pbsapply(kmin:kmax, \(i) perform_calc(
    k = i,
    x = x,
    m = m,
    iLLE = iLLE
  ),
  cl = n.cores
  )

  res <- data.frame(k = c(kmin:kmax), rho = rho)
  if (plotres) {
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(res$k, res$rho, type = "b", xlab = "k", ylab = expression(1 -
      rho^2), main = "")
    abline(h = min(res$rho, na.rm = TRUE), col = "red")
    grid()
  }
  if (verbose) cat("best k:", head(res$k[order(res$rho)], 3), "\n\n")
  res
} # rtemis::rt_lle_calc_k
