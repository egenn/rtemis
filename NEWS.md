# Version 0.95.5

* Transition to `train()` complete; `elevate()` and `elevate1()`
  removed.
  
* Updated `s_LightGBM` and `s_LightRuleFit` for better handling of
  categorical variables.
  
* Updated tests.

* General cleanup in preparation for 1.0 release & CRAN submission.


# Version 0.91

* Switched the main parallelizable functions (resLearn for outer
  resamples; gridSearchlearn for inner resamples) to use [`future`]
  for parallelization and [`progressr`] for progress reporting.

# Version 0.90

* Multiple additions and updates.

* **Major change**: Renamed modeling and visualization functions to
  substitute dots with underscores:
  
  - Supervised learning: `s.` => `s_`
  - Decomposition: `d.` => `d_`
  - Clustering: `u.` => `c_`
  - Cross-decomposition: `x.` => `x_`
  - Static graphics: `mplot3.` => `mplot3_`; `mplot.` => `mplot_`; `gplot3.` => `gplot3_`
  - Interactive graphics: `dplot3.` => `dplot3_`

# Version 0.82

* Themes: New darkgray theme now always on whether you like it or
  not - jk: it's the new default but you can always set your own
  default using e.g. `options(rt.theme = "lightgrid")`. Also, new
  `lightgray` theme.

* New option to set default plotting font: e.g. `options(rt.font =
  "Inter")`
  
* Many improvements / additions to `dplot3*` functions.

* Plenty more I haven't had a chance to document here.


# Version 0.80.0

* An accumulation of updates and added functionality, algorithms, graphics.

* Majority of `mplot3` and `dplot3` functions now work with the new
  theme system provided by `theme_*` functions like `theme_lightgrid`
  and `theme_darkgrid`.


# Version 0.79 [2019-02-07]

* "Super Papaya" Release out.


# Version 0.78 [2019-02-04]

* **rtemis** moved to public repo.


[`future`]: https://github.com/HenrikBengtsson/future
[`progressr`]: https://github.com/HenrikBengtsson/progressr
