#' @importFrom utils data globalVariables
.onAttach <- function(libname, pkgname) {
  # Load small auxiliary datasets into global environment
 utils::data("agroscope", package = pkgname, envir = globalenv())
  utils::data("meteoSwiss", package = pkgname, envir = globalenv())
  utils::data("giss", package = pkgname, envir = globalenv())
  utils::data("hail", package = pkgname, envir = globalenv())

  packageStartupMessage("pep725 loaded.")
 packageStartupMessage("To get phenological data, use:")
  packageStartupMessage("  pep <- pep_download()        # Download synthetic data")
  packageStartupMessage("  data(pep_seed)               # Load small seed dataset")
  packageStartupMessage("  pep <- pep_import('path/')   # Import real PEP725 data")
}
