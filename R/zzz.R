#' @importFrom utils data globalVariables head tail txtProgressBar setTxtProgressBar
#' @importFrom stats sd quantile rnorm residuals lm pnorm as.formula complete.cases contr.sum coef predict
#' @importFrom data.table copy setDT fifelse rbindlist fread setcolorder setnames setorderv
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("pep725 loaded.")
 packageStartupMessage("To get phenological data, use:")
  packageStartupMessage("  pep <- pep_download()        # Download synthetic data")
  packageStartupMessage("  data(pep_seed)               # Load small seed dataset")
  packageStartupMessage("  pep <- pep_import('path/')   # Import real PEP725 data")
}
