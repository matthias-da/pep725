# pep725 1.1.0

## Bug fixes

* `pheno_plot_timeseries()` no longer mutates the caller's data. Previously
  a `setDT(data)` call would convert a caller-supplied `data.frame` into a
  `data.table` by reference.
* `pheno_trend_turning()` — the internal `sequential_mk()` helper now
  implements the published Sneyers (1975) sequential Mann-Kendall
  procedure: cumulative rank-count statistic
  \eqn{t_i = \sum_{k \le i} n_k} with
  \eqn{E[t_i] = i(i-1)/4} and
  \eqn{\mathrm{Var}[t_i] = i(i-1)(2i+5)/72}, plus turning-point
  detection as a sign change of \eqn{u(t) - u'(t)} inside the 95%
  non-significance band. Previous versions used a per-index
  (non-cumulative) \eqn{S} statistic with \eqn{\mathrm{Var}/18} and an
  ad-hoc \eqn{|u| > 0.5} threshold — neither matching Sneyers' method.
  Output of `pheno_trend_turning()` therefore changes numerically for
  v1.1.0. Vectorised rank counting replaces the previous O(n²) R loop.

* `calc_daylength()` gains a `method` argument. The default `"brock"` matches
  pep725 <= 1.0.2 (Brock 1981 / Spencer 1971 simple declination formula).
  A new `"cbm"` option implements the Forsythe et al. (1995) Climate-Budget-
  Model formulation, which includes the orbital-eccentricity correction
  and is typically more accurate near the poles (up to ~15 min/day
  difference at mid-latitudes). The references section now correctly
  attributes the default formula to Brock / Spencer; the Forsythe citation
  is retained for the optional CBM method.

* `calc_thermal_units(method = "single_sine")` now implements the correct
  Baskerville-Emin (1969) single-sine formula. Previous versions multiplied
  the day-above-base width factor incorrectly and returned GDD values that
  were roughly double the textbook reference for the symmetric-around-base
  case (e.g., `tmin=0, tmax=10, tbase=5` produced 3.18 where the textbook
  value is 1.59). Analyses that used the single-sine method on PEP725 data
  in pep725 <= 1.0.2 are affected and should be re-run. New pinned-numeric
  tests lock the formula against Snyder (1985) and Baskerville-Emin (1969)
  reference values, plus the `single_sine <= modified` invariant.

* `pheno_normals()` now derives quantile column names from the supplied
  `probs` levels. Previously custom `probs` were rejected unless exactly six
  values were supplied, and even then the output columns were hard-coded to
  `q05..q95` regardless of the actual levels — silently mislabelling
  quantiles. Default `probs` still produce `q05, q10, q25, q75, q90, q95`;
  custom `probs` now produce e.g. `q02_5, q97_5` for the 2.5% / 97.5%
  quantiles. The mapping is stored in `attr(result, "q_names")`, and
  `plot.pheno_normals()` falls back to the nearest available quantile pair
  when the default `q25/q75` are not present.

## Deprecations

* `kendall_tau()` is deprecated in favour of `mann_kendall_z()`. The old
  function was misnamed: it returned the Mann-Kendall Z-statistic, not
  Kendall's τ (which is bounded in [-1, 1]). `kendall_tau()` still works
  (now as a thin wrapper that emits a deprecation warning) and returns the
  same value it always did; please update callers to `mann_kendall_z()`.

## New features and improvements

* `pep_plot_outliers(type = "diagnostic")` — paper/vignette-ready 4-panel
  diagnostic figure (residuals vs fitted, Q-Q, |residual| vs covariate,
  per-station max-residual map). Works for any detection method.

* `pep_outliers_leaflet()` — new function. Interactive Leaflet visualisation
  of a `pep_outliers` object: stations as circle markers sized by number
  of flagged observations and coloured by their maximum absolute
  residual (or robust Mahalanobis distance for `method = "mahalanobis"`).
  Per-station popups list the top-N worst offenders. Complements
  `pheno_leaflet()` (which is a selection gadget); this is a
  visualisation you can embed in knitr reports.

* `pep_flag_outliers()` gains a multivariate detection method:
  `method = "mahalanobis"`. Treats each station-year as a vector of DOYs
  across phases and flags station-years whose **robust** Mahalanobis
  distance (MCD estimator via `robustbase::covMcd()`) exceeds a χ²-based
  cut-off (default `sqrt(qchisq(0.975, df = p))`). Catches joint
  inconsistencies across phases — e.g. a BBCH 60 and 65 reported only one
  day apart — that slip past every univariate detector because each
  marginal DOY looks fine. Falls back to the 30-day rule for small or
  singular groups.

* `pep_flag_outliers()` gains a model-based detection method:
  `method = "gam_residual"`. For each group (typically species × phase),
  fits a GAM of DOY on year, altitude, latitude, and a station random
  intercept (customisable via the new `formula` argument) and flags
  observations whose robust-z-scored residual exceeds `threshold`
  (default 3.5). Detects covariate-inconsistent anomalies — e.g. a
  lowland station reporting a high-altitude DOY, or a station-year
  report inconsistent with the global climate trend — that the
  univariate 30-day rule misses. Falls back to `"30day"` for groups
  smaller than the new `min_n_per_group` argument (default 50) or
  when the GAM fails to converge, with an informative message.

* New `mann_kendall_z()` function — the correctly named replacement for
  `kendall_tau()`. Adds a tie correction to `Var(S)` (so tied DOY values do
  not inflate the statistic) and the continuity correction
  `Z = (S - sign(S))/sqrt(Var(S))`, matching `Kendall::MannKendall()`. The
  vectorised implementation replaces the previous O(n²) R loop.

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
