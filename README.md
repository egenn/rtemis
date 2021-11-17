# **_rtemis_** Machine Learning and Visualization [![Build Status](https://travis-ci.com/egenn/rtemis.svg?branch=master)](https://travis-ci.com/egenn/rtemis)

A platform for advanced Machine Learning research and applications.  
The goal of **rtemis** is to make data science efficient and accessible with no compromise on flexibility.

<div style="text-align:center">
<a href="https://rtemis.lambdamd.org">
<img align = "center" src="https://egenn.github.io/imgs/rtemis_logo.png"></a>
</div>

## Documentation

* [**Documentation and vignettes**](https://rtemis.lambdamd.org)  

## Requirements

R version 4.1 or higher

## Installation

See [here](https://rtemis.lambdamd.org/setup.html) for more setup and installation instructions.

```r
install.packages("remotes")
remotes::install_github("egenn/rtemis")
```

**Note:** *Make sure to keep your installation updated by running* `remotes::install_github("egenn/rtemis")` *regularly: it will only proceed if there are updates available*

## 60-second intro to **rtemis**

Install dependencies if they are not already installed:

```r
packages <- c("pbapply", "ranger")
.add <- !packages %in% installed.packages()
install.packages(packages[.add])
```

Load **rtemis** and get cross-validated random forest performance on the iris dataset:

```r
library(rtemis)
mod <- elevate(iris)
mod$plot()
```

## What's new

### **0.82**

* Themes: New darkgray theme now always on whether you like it or not - jk:
it's the new default but you can can always set your own default using
e.g. `options(rt.theme = "lightgrid")`. Also, new `lightgray` theme.
* Many improvements / additions to `dplot3*` functions.
* Plenty more I haven't had a chance to document here

### **0.80.0**

An accumulation of updates and added functionality, algorithms, graphics.  
Majority of `mplot3` and `dplot3` functions now work with the new theme system provided by `theme_*` functions like `theme_lightgrid` and `theme_darkgrid`.

### **0.79**

07-02-2019: "Super Papaya" Release out

### **0.78**

04-02-2019: **rtemis** moved to public repo

## Features

* **Visualization**
  * Static: **_mplot3_** family (base graphics)
  * Dynamic: **_dplot3_** family ([plotly](https://plot.ly/r/))
* **Unsupervised Learning**
  * Clustering: **_u.\*_**
  * Decomposition: **_d.\*_**
* **Supervised Learning**
  * Classification, Regression, Survival Analysis: **_s.\*_**
* **Cross-Decomposition**
  * Sparse Canonical Correlation / Sparse Decomposition: **_x.\*_**
* **Meta-Models**  
  * Model Stacking: **_metaMod()_**
  * Modality Stacking: **_metaFeat()_**
  * Group-weighted Stacking: **_metaGroup()_**

  (metaFeat and metaGroup have been removed for updating)

## Ongoing work

* **rtemis** is under active development
* Novel algorithms developed in **rtemis** will generally be added to this public repository around the publication of the corresponding papers.
* R Documentation is ongoing.

---

<img align = "center" src="https://egenn.github.io/imgs/rtemis_vis_collage.png">
</br>
<img align = "center" src="https://egenn.github.io/imgs/rtemis_rstudio.png">
</br>  

---

[2021 Efstathios (Stathis) D. Gennatas MBBS AICSM PhD](https://egenn.lambdamd.org)
