## R CMD check results

0 errors | 0 warnings | 2 notes (both are local-machine environmental,
not package issues):

* "unable to verify current time" — local NTP reachability.
* "Skipping checking HTML validation: 'tidy' doesn't look like recent
  enough HTML Tidy." — local macOS `tidy` binary predates the CRAN
  build-farm version.

These notes do not appear under `R CMD check --as-cran` on win-builder
or rhub.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Release summary: 1.0.2 → 1.1.0

This release is the result of a strategic improvement audit and
addresses six scientific-correctness bugs plus a substantial
expansion of the outlier-detection toolkit. Details in NEWS.md;
headline items:

### Scientific-correctness fixes

1. `calc_thermal_units(method = "single_sine")` — corrected the
   Baskerville-Emin (1969) / Snyder (1985) mixed-case formula.
   Previous versions returned GDD values roughly double the textbook
   reference for the symmetric-around-base case. Published analyses
   using the single-sine method on pep725 <= 1.0.2 are affected and
   should be re-run. Pinned-numeric regression tests lock the formula
   against the reference.
2. `pheno_trend_turning()` — internal `sequential_mk()` now implements
   the published Sneyers (1975) sequential Mann-Kendall procedure
   (cumulative rank-count statistic with E[t_i] = i(i-1)/4,
   Var[t_i] = i(i-1)(2i+5)/72) instead of the previous ad-hoc per-index
   statistic. Output changes numerically; documented in NEWS.
3. `pheno_normals()` — custom `probs` now produce correctly labelled
   quantile columns; previously the six column names were hardcoded
   regardless of the requested levels, silently mislabelling any
   non-default quantiles.
4. `kendall_tau()` renamed to `mann_kendall_z()` (old name kept as a
   deprecated alias with a one-time warning). The function never
   returned Kendall's τ; it always returned the Mann-Kendall Z-statistic,
   and the name was misleading.
5. `calc_daylength()` cited Forsythe et al. (1995) but implemented the
   Brock / Spencer declination formula. Citation corrected; a new
   `method = "cbm"` option adds the actual Forsythe Climate-Budget-
   Model formulation.
6. `pheno_plot_timeseries()` no longer mutates the caller's data via
   `setDT(data)` (silent data.frame → data.table conversion by
   reference).

### New features

* `pep_flag_outliers()` gains two method options:
  - `"gam_residual"` — covariate-aware detection via `mgcv::gam`.
  - `"mahalanobis"` — multivariate detection with the **robust**
    MCD covariance estimator (`robustbase::covMcd`), catching joint
    inconsistencies across phases that univariate detectors miss.
* `pep_plot_outliers()` gains `"diagnostic"` (4-panel paper figure,
  method-aware) and `"profile"` (phase-profile parallel coordinates)
  plot types.
* New `pep_outliers_leaflet()` — interactive htmlwidget visualisation
  of flagged stations, embeddable in knitr reports.
* New `mann_kendall_z()` with tie and continuity corrections.

### No new dependency

All new functionality uses packages already in `Imports`/`Suggests`
(`mgcv`, `robustbase`, `leaflet`). `Kendall` added to `Suggests`
for the `mann_kendall_z` reference-value tests.

### Breaking change (documented)

`pheno_trend_turning()` numerical output changes as a consequence of
fix (2) above. This is a minor-version bump precisely because the
output is not backwards-compatible; the previous formulation was not
textbook.
