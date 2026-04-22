## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. Changes since the previous submission (1.0.1):

1. **Fixed `donttest` NOTE (CPU/elapsed ratio)**: The examples for
   `pep_simulate()` and `pep_coverage()` triggered a NOTE because
   CPU time exceeded 2.5x elapsed time on multi-core systems (implicit
   multi-threading in mgcv and data.table). Both examples now use
   smaller data subsets, reducing runtime from ~95s and ~45s to under 2s.
