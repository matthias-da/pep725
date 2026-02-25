## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. In the previous submission we received:

> Suggests or Enhances not in mainstream repositories:
>   leaflet.extras
>
> but no declaration where to get this from?

The package `leaflet.extras` was recently archived from CRAN (2026-02-19) but
remains available from its r-universe repository. We have declared this in
DESCRIPTION via:

```
Additional_repositories: https://trafficonese.r-universe.dev
```

`leaflet.extras` is listed in Suggests only and is used in a single interactive
function (`pheno_leaflet()`), whose example is wrapped in `\dontrun{}`. The
dependency is guarded at runtime with `requireNamespace()` and provides a clear
installation message pointing users to the r-universe repository if the package
is not installed. No other functionality in the package depends on
`leaflet.extras`.
