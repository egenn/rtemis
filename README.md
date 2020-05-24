**_rtemis_** Machine Learning and Visualization [![Build Status](https://travis-ci.com/egenn/rtemis.svg?branch=master)](https://travis-ci.com/egenn/rtemis)
===============================================
A platform for advanced Machine Learning research and applications.  
The goal of __rtemis__ is to make data science efficient and accessible with no compromise on flexibility.

<div style="text-align:center">
<a href="https://rtemis.netlify.com">
<img align = "center" src="https://rtemis.netlify.com/rtemis_0.79_Release.jpeg">
</a>    
</div>

### Documentation
* [__Online documentation and vignettes__](https://rtemis.netlify.com)  
* [__R PDF manual__](https://egenn.github.io/docs/rtemis.pdf)

### Installation
See [here](https://rtemis.netlify.com/setup.html) for more setup and installation instructions.

```r
install.packages("remotes")
remotes::install_github("egenn/rtemis")
```

**Note:** *Make sure to keep your installation updated by running* `remotes::install_github("egenn/rtemis")` *regularly: it will only proceed if there are updates available*


### 10-second intro to __rtemis__
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

### What's new
* __v0.79__: 07-02-2019 "Super Papaya" Release out
* __v0.78__: 04-02-2019 __rtemis__ moved to public repo

### Features
* __Visualization__
     - Static: **_mplot3_** family (base graphics)
     - Dynamic: **_dplot3_** family ([plotly](https://plot.ly/r/))
* __Unsupervised Learning__
     - Clustering: **_u.\*_**
     - Decomposition: **_d.\*_**
* __Supervised Learning__
     - Classification, Regression, Survival Analysis: **_s.\*_**
* __Cross-Decomposition__
     - Sparse Canonical Correlation / Sparse Decomposition: **_x.\*_**
* __Meta-Models__  
     - Model Stacking: **_metaMod()_**
     - Modality Stacking: **_metaFeat()_**
     - Group-weighted Stacking: **_metaGroup()_**

  (metaFeat and metaGroup have been temporarily removed for updating)

### Ongoing work
* Novel algorithms developed in __rtemis__ will generally be added to this public repository as soon as the corresponding papers or preprints are published.
* R Documentation is ongoing and should be completed soon.
* __rtemis__ is under active development with many enhancements and extensions in the works
---
<img align = "center" src="https://egenn.github.io/imgs/rtemis_vis_collage.png">
<img align = "center" src="https://egenn.github.io/imgs/iris_CART.png">
<br>

---  

<img align = "center" src="https://egenn.github.io/imgs/rtemis_hex_2020.png" width="205">  

2019 Efstathios (Stathis) D. Gennatas MBBS AICSM PhD  
