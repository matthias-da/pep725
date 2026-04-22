# pep725 1.1.0 (in development)

## Bug fixes

* `pheno_plot_timeseries()` no longer mutates the caller's data. Previously
  a `setDT(data)` call would convert a caller-supplied `data.frame` into a
  `data.table` by reference.
* `pheno_normals()` now derives quantile column names from the supplied
  `probs` levels. Previously custom `probs` were rejected unless exactly six
  values were supplied, and even then the output columns were hard-coded to
  `q05..q95` regardless of the actual levels — silently mislabelling
  quantiles. Default `probs` still produce `q05, q10, q25, q75, q90, q95`;
  custom `probs` now produce e.g. `q02_5, q97_5` for the 2.5% / 97.5%
  quantiles. The mapping is stored in `attr(result, "q_names")`, and
  `plot.pheno_normals()` falls back to the nearest available quantile pair
  when the default `q25/q75` are not present.

## New features and improvements

* *(pending — see improvement audit 2026-04-22)*

# pep725 1.0.2

* Reduced example sizes for `pep_simulate()` and `pep_coverage()` to avoid
  high CPU/elapsed time ratios on multi-core CRAN check machines

# pep725 1.0.1

* Fixed CRAN additional check: `pep_download()` no longer writes to user cache
  directory during `R CMD check` (uses `tempdir()` instead to avoid
  `_R_CHECK_THINGS_IN_OTHER_DIRS_` note)

# pep725 1.0.0

* Initial JOSS and CRAN submission on Valentin`s day, 14. February 2026
* Data access: `pep_download()`, `pep_import()`, `pep_simulate()`
* Core analysis: `pheno_normals()`, `pheno_anomaly()`, `pheno_gradient()`, `pheno_synchrony()`, `pheno_combine()`, `pheno_trend_turning()`
* Quality and validation: `pep_quality()`, `pep_completeness()`, `pep_check_phases()`, `pep_flag_outliers()`, `pep_second_events()`
* Advanced analysis: `pheno_pls()`, `calc_thermal_units()`, `calc_daylength()`
* Visualization: `plot()` methods for all result classes, `pheno_leaflet()`, `pheno_map()`
* Four vignettes covering data access, analysis, spatial patterns, and quality assessment
