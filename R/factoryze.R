# factoryze.R
# ::rtemis::
# 2016 Efstathios D. Gennatas egenn.github.io

#' Factor Analysis
#'
#' Perform parallel analysis, factor analysis, bifactor analysis
#' and hierarchical clustering
#'
#' Consult \code{psych::fa} for more information on the parameters
#'
#' @param x Data. Will be coerced to data frame
#' @param n.factors Integer: If NULL, will be estimated using parallel analysis
#' @param method Character: Factor analysis method:
#' "minres": minimum residual (OLS), "wls": weighted least squares (WLS);
#' "gls": generalized weighted least squares (GLS); "pa": principal factor solution;
#' "ml": maximum likelihood;
#' "minchi": minimize the sample size weighted chi square when treating pairwise correlations with different
#' number of subjects per pair;
#' "minrank": minimum rank factor analysis.
#' Default = "minres"
#' @param rotation Character: Rotation methods.
#' No rotation: "none";
#' Orthogonal: "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor";
#' Oblique: "promax", "oblimin", "simplimax", "bentlerQ, "geominQ", "biquartimin", "cluster".
#' Default = "oblimin"
#' @param scores Character: Factor score estimation method. Options: "regression",
#' "Thurstone": simple regression, "tenBerge": correlation-preserving, "Anderson", "Barlett".
#' Default = "regression"
#' @param cor Character: Correlation method:
#' "cor": Pearson correlation, "cov": Covariance, "tet": tetrachoric, "poly": polychoric,
#' "mixed": mixed cor for a mixture of tetrachorics, polychorics, Pearsons, biserials, and polyserials,
#' "Yuleb": Yulebonett, "Yuleq" and "YuleY": Yule coefficients
#' @param fa.n.iter Integer: Number of iterations for factor analysis. Default = 100
#' @param omega.method Character: Factor analysis method for the bifactor analysis. Same options as \code{method}
#' Default = "minres"
#' @param omega.rotation Character: Rotation method for bifactor analysis:
#' "oblimin", "simplimax", "promax", "cluster", "target". Default = "oblimin"
#' @param omega.n.iter Integer: Number of iterations for bifactor analysis. Default = 1
#' @param x.name Character: Name your dataset. Used for plotting
#' @param print.plot Logical: If TRUE, print plots along the way. Default = TRUE
#' @param do.pa Logical: If TRUE, perform parallel analysis. Default = TRUE
#' @param do.fa Logical: If TRUE, perform factor analysis. Default = TRUE
#' @param do.bifactor Logical: If TRUE, perform bifactor analysis. Default = TRUE
#' @param do.hclust Logical: If TRUE, perform hierarchical cluster analysis. Default = TRUE
#' @param verbose Logical: If TRUE, print messages to output. Default = TRUE
#' @param ... Additional arguments to pass to \code{psych::fa}
#' @author Efstathios D. Gennatas
#' @export

factoryze <- function(x,
                      n.factors = NULL,
                      method = "minres",
                      rotation = "oblimin",
                      scores = "regression",
                      cor = "cor",
                      fa.n.iter = 100,
                      omega.method = "minres",
                      omega.rotation = c("oblimin", "simplimax", "promax", "cluster", "target"),
                      omega.n.iter = 1,
                      x.name = NULL,
                      print.plot = TRUE,
                      do.pa = TRUE,
                      do.fa = TRUE,
                      do.bifactor = TRUE,
                      do.hclust = FALSE,
                      verbose = TRUE, ...) {

  # [ DEPENDENCIES ] ====
  if (!depCheck("psych", verbose = FALSE)) {
    cat("\n"); stop("Please install dependencies and try again")
  }

  # [ ARGUMENTS ] ====
  if (is.null(x.name)) x.name <- deparse(substitute(x))
  if (is.null(n.factors)) do.pa <- TRUE
  omega.rotation <- match.arg(omega.rotation)

  # [ PARALLEL ANALYSIS - Estimate Number of Factors ] ====
  if (do.pa) {
    if (verbose) msg("Running Parallel Analysis...")
    parallel.analysis <- psych::fa.parallel(x, fm = method,
                                            main = "Parallel Analysis Scree Plot")
    if (is.null(n.factors)) {
      n.factors <- parallel.analysis$nfact
      if (n.factors == 0) {
        warning("Parallel analysis returned 0 factors; will use 2")
        n.factors <- 2
      }
    } else {
      if (n.factors != parallel.analysis$nfact) {
        msg(n.factors, "requested; Parallel analysis suggests", parallel.analysis$nfact, "factors.")
        msg("Check scree plot")
      }
    }
  } else {
    parallel.analysis <- NULL
  }
  if (verbose) msg("Using", n.factors, "factors")

  # [ FACTORS ANALYSIS ] ====
  if (do.fa) {
    if (verbose) msg("Running Exploratory Factor Analysis...")
    x.fa <- psych::fa(x, nfactors = n.factors,
                      fm = method,
                      rotate = rotation,
                      scores = scores,
                      cor = cor,
                      n.iter = fa.n.iter)
    # Plot factors
    if (print.plot) psych::fa.diagram(x.fa, main = paste(x.name, "Factor Analysis"))
    # Get factor scores
    fa.scores <- x.fa$scores
  } else {
    x.fa <- fa.scores <- NULL
  }

  # [ BIFACTOR ANALYSIS ] ====
  if (do.bifactor) {
    if (verbose) msg("Running Bifactor Analysis...")
    x.omega <- psych::omegaSem(x, nfactors = n.factors,
                               fm = omega.method,
                               n.iter = omega.n.iter,
                               rotate = omega.rotation,
                               plot = print.plot, title = paste(x.name, "Bifactor Analysis"))
    # Bifactor scores
    bifactor.scores <- x.omega$omegaSem$scores[, 1]
  } else {
    x.omega <- bifactor.scores <- NULL
  }

  # [ HIERARCHICAL CLUSTER ANALYSIS ] ====
  if (do.hclust) {
    if (verbose) msg("Performing hierarchical cluster analysis...")
    x.pvclust <- pvclust::pvclust(x)
    if (print.plot) plot(x.pvclust)
  } else {
    x.pvclust <- NULL
  }

  # [ OUTPUT ] ====
  s.out <- list(parallel.analysis = parallel.analysis,
                factor.analysis = x.fa,
                factor.scores = fa.scores,
                bifactor.analysis = x.omega,
                bifactor.scores = bifactor.scores,
                hclust = x.pvclust)
  s.out

} # rtemis::factoryze
