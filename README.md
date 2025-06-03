[![R-CMD-check](https://github.com/egenn/rtemis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/egenn/rtemis/actions/workflows/R-CMD-check.yaml)
[![rtemis status badge](https://egenn.r-universe.dev/badges/rtemis)](https://egenn.r-universe.dev/rtemis)

***NEW VERSION HAS MOVED TO [RTEMIS-ORG/RTEMIS](https://github.com/rtemis-org/rtemis)***

# **_rtemis_** Machine Learning and Visualization

A platform for advanced Machine Learning research and applications.  
The goal of **rtemis** is to make data science efficient and accessible with no compromise on flexibility.

<div style="text-align:center">
<a href="https://rtemis.org">
<img align = "center" src="https://egenn.github.io/imgs/rtemis_logo.png"></a>
</div>

## What's new

rtemis version 0.99 is currently being developed and includes extensive rewriting of the core code,
focussing on a "functional OOP" design throughout. This will be the test release prior to CRAN 
submission of version 1.0.

## Documentation

* [**Documentation and vignettes**](https://rtemis.org/rtemis)  

## Requirements

R version 4.1 or higher

## Installation

You can install `rtemis` from `r-universe` or using `pak`, `remotes`, or `devtools`.

* `r-universe`:

  ```r
  install.packages('rtemis', repos = c('https://egenn.r-universe.dev', 'https://cloud.r-project.org'))
  ```

* `pak`:

  ```r
  pak::pkg_install("egenn/rtemis")
  ```

* `remotes`:

  ```r
  remotes::install_github("egenn/rtemis")
  ```

* `devtools`:
  
  ```r
  devtools::install_github("egenn/rtemis")
  ```

### Note about Fortran support in MacOS

To allow compilation from source of any dependencies that require Fortran, you
will need to install the GNU Fortran compiler. The easiest way to do this is
with [Homebrew](https://brew.sh/):

```bash
brew install gcc
```

Then, you will need to add the following to your `~/.R/Makevars` file:

```bash
FC      = usr/local/opt/gcc/bin/gfortran
F77     = /usr/local/opt/gcc/bin/gfortran
FLIBS   = -L/usr/local/opt/gcc/lib
```

### Note about using `d_UMAP()`

`d_UMAP()` requires the `uwot` package, which currently requires that the `Matrix` and
`irlba` dependencies be installed from source. See more in the `uwot` issue
[here](https://github.com/jlmelville/uwot/issues/115).

### More setup info

See [here](https://rtemis.org/rtemis/Setup.html) for more setup and
installation instructions.

**Note:** Make sure to keep your installation updated by running
`remotes::install_github("egenn/rtemis")` regularly: it will only proceed if
there are updates available.

## 30-second intro to **rtemis**

Install dependencies if they are not already installed:

```r
packages <- c("future.apply", "ranger")
.add <- !packages %in% installed.packages()
install.packages(packages[.add])
```

Get cross-validated random forest performance on the iris dataset:

```r
library(rtemisalpha)
mod <- train_cv(iris)
```

## What's new

We are working towards the 1.0 release, which will feature updates to the
API as well as the backend, and preparing for CRAN submission.
This will be accompanied by expansion of the [documentation](https://rtemis.org/rtemis).  
For all updates, please see the [NEWS](NEWS.md) file.

The Python and Julia ports, `rtemispy` and `Rtemis.jl`, which are not yet 
publicly available, are in parallel development. With the upcoming 1.0 release
of rtemis, the aim is to offer a unified API across all three languages.

## Features

* **Visualization**
  * Static: **_mplot3_** family (base graphics)
  * Dynamic: **_dplot3_** family ([plotly](https://plotly.com/r/))
* **Unsupervised Learning**
  * Clustering: **_c_\*_**
  * Decomposition: **_d_\*_**
* **Supervised Learning**
  * Classification, Regression, Survival Analysis: **_s_\*_**
* **Cross-Decomposition**
  * Sparse Canonical Correlation / Sparse Decomposition: **_x_\*_**
* **Meta-Models**  
  * Model Stacking: **_metaMod()_**
  * Modality Stacking: **_metaFeat()_**
  * Group-weighted Stacking: **_metaGroup()_**

  (metaFeat and metaGroup have been removed for updating)

---

<img align = "center" src="https://egenn.github.io/imgs/rtemis_vis_collage.png">
</br>
<img align = "center" src="https://egenn.github.io/imgs/rtemis_rstudio.png">
</br>  

---

## rtemislive

**rtemislive** is rtemis' web interface / GUI.
It makes advanced visualization and modeling instantly accessible by all.
It is currently available for beta testing at UCSF,
and will be made publicly available once funding is secured for a hosting server.

<img align = "center" src="https://egenn.github.io/imgs/rtemislive_0.92_dplot3_xyz.jpeg">
</br>

## VS Code theme

Get the [rtemis-dark VS Code theme](https://marketplace.visualstudio.com/items?itemName=egenn.rtemis-dark).

Recommended font is Fira Code with its pretty ligatures.

## Python & Julia APIs

Python and Julia APIs are in development. The goal is to delliver a unified API across
all three languages by the time of the 1.0 release.
