## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Note on leaflet.extras

The package `leaflet.extras` is listed in Suggests and declared via
`Additional_repositories: https://trafficonese.r-universe.dev` in DESCRIPTION.

`leaflet.extras` was recently archived from CRAN (2026-02-19) but remains
available from the r-universe repository above. It is used only in a single
interactive function (`pheno_leaflet()`) whose example is wrapped in
`\dontrun{}`. The package is checked at runtime via `requireNamespace()` and
the function provides a clear installation message pointing to the r-universe
if the package is not available. No other functionality in the package depends
on `leaflet.extras`.
