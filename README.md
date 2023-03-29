# **_rtemis_** Machine Learning and Visualization

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

Install `rtemis` usikg `pak`, `remotes`, or `devtools`:

```r
pak::pkg_install("egenn/rtemis")
```

_or_

```r
remotes::install_github("egenn/rtemis")
```

_or_


```r
devtools::install_github("egenn/rtemis")
```

See [here](https://rtemis.lambdamd.org/Setup.html) for more setup and
installation instructions.

**Note:** Make sure to keep your installation updated by running
`remotes::install_github("egenn/rtemis")` regularly: it will only proceed if
there are updates available.

## 30-second intro to **rtemis**

Install dependencies if they are not already installed:

```r
packages <- c("future", "ranger")
.add <- !packages %in% installed.packages()
install.packages(packages[.add])
```

Get cross-validated random forest performance on the iris dataset:

```r
library(rtemis)
mod <- elevate(iris)
```

## What's new

### **0.91**

Switched the main parallelizable functions (resLearn for outer resamples;
gridSearchlearn for inner resamples) to use [`future`](https://github.com/HenrikBengtsson/future) for parallelization
and [`progressr`](https://github.com/HenrikBengtsson/progressr) for progress reporting.

### **0.90**

Multiple additions and updates.  
**Major change**: Renamed modeling and visualization functions to substitute 
dots with underscores:

* Supervised learning: `s.` => `s_`
* Decomposition: `d.` => `d_`
* Clustering: `u.` => `c_`
* Cross-decomposition: `x.` => `x_`
* Static graphics: `mplot3.` => `mplot3_`; `mplot.` => `mplot_`; `gplot3.` => `gplot3_`
* Interactive graphics: `dplot3.` => `dplot3_`

### **0.82**

* Themes: New darkgray theme now always on whether you like it or not - jk:
it's the new default but you can always set your own default using
e.g. `options(rt.theme = "lightgrid")`. Also, new `lightgray` theme.
* New option to set default plotting font: e.g. `options(rt.font = "Inter")`
* Many improvements / additions to `dplot3*` functions.
* Plenty more I haven't had a chance to document here

### **0.80.0**

An accumulation of updates and added functionality, algorithms, graphics.  
Majority of `mplot3` and `dplot3` functions now work with the new theme system
provided by `theme_*` functions like `theme_lightgrid` and `theme_darkgrid`.

### **0.79**

07-02-2019: "Super Papaya" Release out

### **0.78**

04-02-2019: **rtemis** moved to public repo

## Features

* **Visualization**
  * Static: **_mplot3_** family (base graphics)
  * Dynamic: **_dplot3_** family ([plotly](https://plot.ly/r/))
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

[&copy;2022 E.D. Gennatas MBBS AICSM PhD](https://egenn.lambdamd.org)
