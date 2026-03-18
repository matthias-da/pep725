## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. Changes since the previous submission:

1. **Fixed cache directory NOTE**: `pep_download()` caches synthetic data
   to the user cache directory via `tools::R_user_dir()`. During R CMD check
   this triggered a NOTE from `_R_CHECK_THINGS_IN_OTHER_DIRS_`. We now
   detect the check environment and use `tempdir()` instead, so no files
   are written outside the package/temp directories during checks.

2. **`leaflet.extras` removed from Suggests** (previous feedback):
   It is no longer declared as a dependency. The single function that uses
   it (`pheno_leaflet()`) checks availability at runtime via
   `requireNamespace()` and provides a clear installation message pointing
   to the r-universe repository.
