# **_rtemis_** Machine Learning and Visualization [![Build Status](https://travis-ci.com/egenn/rtemis.svg?branch=master)](https://travis-ci.com/egenn/rtemis)

A platform for advanced Machine Learning research and applications.  
The goal of __rtemis__ is to make data science efficient and accessible with no compromise on flexibility.

<div style="text-align:center">
<a href="https://rtemis.lambdamd.org">
<img align = "center" src="https://egenn.github.io/imgs/rtemis_logo.png"></a>
</div>

## Documentation

* [__Documentation and vignettes__](https://rtemis.lambdamd.org)  

## Installation

See [here](https://rtemis.lambdamd.org/setup.html) for more setup and installation instructions.

```r
install.packages("remotes")
remotes::install_github("egenn/rtemis")
```

**Note:** *Make sure to keep your installation updated by running* `remotes::install_github("egenn/rtemis")` *regularly: it will only proceed if there are updates available*

## 60-second intro to __rtemis__

Install dependencies if they are not already installed:

```r
packages <- c("pbapply", "ranger")
.add <- !packages %in% installed.packages()
install.packages(packages[.add])
```

Load __rtemis__ and get cross-validated random forest performance on the iris dataset:

```r
library(rtemis)
mod <- elevate(iris)
mod$plot()
```

## What's new

### __0.80.0 Beta__

An accumulation of updates and added functionality, algorithms, graphics.  
Majority of `mplot3` and `dplot3` functions now work with the new theme system provided by `theme_*` functions like `theme_lightgrid` and `theme_darkgrid`.

### __0.79__

07-02-2019: "Super Papaya" Release out

### __0.78__

04-02-2019: __rtemis__ moved to public repo

## Features

* __Visualization__
  * Static: **_mplot3_** family (base graphics)
  * Dynamic: **_dplot3_** family ([plotly](https://plot.ly/r/))
* __Unsupervised Learning__
  * Clustering: **_u.\*_**
  * Decomposition: **_d.\*_**
* __Supervised Learning__
  * Classification, Regression, Survival Analysis: **_s.\*_**
* __Cross-Decomposition__
  * Sparse Canonical Correlation / Sparse Decomposition: **_x.\*_**
* __Meta-Models__  
  * Model Stacking: **_metaMod()_**
  * Modality Stacking: **_metaFeat()_**
  * Group-weighted Stacking: **_metaGroup()_**

  (metaFeat and metaGroup have been removed for updating)

## Ongoing work

* __rtemis__ is under active development
* Novel algorithms developed in __rtemis__ will generally be added to this public repository around the publication of the corresponding papers.
* R Documentation is ongoing.

---

<img align = "center" src="https://egenn.github.io/imgs/rtemis_vis_collage.png">
</br>
<img align = "center" src="https://egenn.github.io/imgs/rtemis_rstudio.png">
</br>  

---  

<img align = "center" src="https://egenn.github.io/imgs/rtemis_hex_2020.png" width="205">  

---

[2020 Efstathios (Stathis) D. Gennatas MBBS AICSM PhD](https://egenn.github.io)
