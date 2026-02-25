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

We have removed `leaflet.extras` from Suggests entirely. It is no longer
declared as a dependency. The single function that uses it (`pheno_leaflet()`,
an interactive Shiny gadget) checks for its availability at runtime via
`requireNamespace()` and provides a clear installation message pointing to
the r-universe repository if the package is not installed.
