[![R-CMD-check](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtemis-org/rtemis/actions/workflows/R-CMD-check.yaml)

# rtemis: Advanced Machine Learning &amp; Visualization.

This is the new version of the rtemis R package and it is under active 
The original package is [here](https://github.com/egenn/rtemis).

The new verion (0.99+) features:

- Backend: A near-complete rewrite using the new [**S7** class system](https://github.com/RConsortium/S7), replacing all previous use of R6 and S3 classes.
- API: **Functional user-facing API**, to maintain a consistent, user-friendly interface.
- Expanded use of `**setup_()**` functions, to offer increased transparency & clear separation of parameters.
- Strict **type checking** and **condition validation** throughout to minimize user error and provide highly focused error messages & suggestions.
- Expanded transparent messaging through each step.

## Installation

Using `pak`:

```r
pak::pkg_install("rtemis-org/rtemis")
```

## Documentation

The documentation is available at [rdocs.rtemis.org](https://rdocs.rtemis.org).

[![](https://rdocs.rtemis.org/assets/rtemis-mlv-cover.webp)](https://rdocs.rtemis.org)
