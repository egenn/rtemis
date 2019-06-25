# rt_lle_calc_k
# ::rtemis::
# 2019 Efstathios D. Gennatas egenn.github.io

#' \pkg{rtemis} internal: \code{lle::calc_k} function adapted to work with \code{pbapply}
#'
#' @keywords internal

rt_lle_calc_k <- function(X, m,
                          kmin = 1, kmax = 20,
                          plotres = TRUE,
                          iLLE = FALSE,
                          n.cores = rtCores,
                          verbose = TRUE) {
  N <- dim(X)[1]
  if (kmax >= N)
    kmax <- N - 1
  # if (.Platform$OS.type == "windows") {
  #   dev <- "nul"
  # } else {
  #   dev <- "/dev/null"
  # }

  # if (parallel == TRUE)
  #   sfInit(parallel = TRUE, cpus = cpus)
  # else sfInit(parallel = FALSE)
  # options(warn = -1)
  # sfLibrary(lle)
  # options(warn = 0)
  perform_calc <- function(k, X, m, iLLE = FALSE) {
    N <- dim(X)[1]
    # sink(dev)
    Y <- lle::lle(X, m, k, 2, 0, iLLE = iLLE)$Y
    # sink()
    Dx <- as.matrix(dist(X))
    Dy <- as.matrix(dist(Y))
    rho <- c()
    for (i in 1:N) rho <- c(rho, cor(Dx[i, ], Dy[i, ]))
    return(mean(1 - rho^2))
  }
  # rho <- invisible(sfLapply(kmin:kmax, perform_calc, X, m,
  #                           iLLE))

  # rho <- pbapply::pblapply(kmin:kmax, function(i) perform_calc(k = i,
  #                                                              X = X,
  #                                                              m = m,
  #                                                              iLLE = iLLE),
  #                          cl = n.cores)
  rho <- pbapply::pbsapply(kmin:kmax, function(i) perform_calc(k = i,
                                                               X = X,
                                                               m = m,
                                                               iLLE = iLLE),
                           cl = n.cores)

  # rho <- unclass(unlist(rho))
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
