#' Pan-European Phenology Data (PEP725) - Synthetic Version
#'
#' A synthetic version of phenological observations based on the PEP725 database
#' structure, used for analyzing crop responses to climate variables. The synthetic
#' data preserves the statistical properties and structure of real PEP725 data
#' while ensuring data privacy.
#'
#' @name pep_synth
#' @format A data.table with approximately 7 million rows and 18+ variables:
#' \describe{
#'   \item{s_id}{Station ID (factor)}
#'   \item{lon}{Longitude (numeric)}
#'   \item{lat}{Latitude (numeric)}
#'   \item{alt}{Altitude in meters (integer)}
#'   \item{genus}{Genus of the observed plant (factor)}
#'   \item{species}{Species (factor)}
#'   \item{phase_id}{Phenological phase code (integer)}
#'   \item{year}{Observation year (integer)}
#'   \item{day}{Day of year (DOY) of the phenological event (integer)}
#'   \item{country}{Country name (character)}
#' }
#'
#' @details
#' The full synthetic PEP dataset is not shipped with the package due to size
#' constraints. Use \code{\link{pep_download}} to download it from the package
#' repository. Alternatively, use \code{data(pep_seed)} for a small subset
#' suitable for testing and examples.
#'
#' @section Data Access:
#' \itemize{
#'   \item \code{pep_download()} - Download full synthetic dataset (~64 MB)
#'   \item \code{data(pep_seed)} - Load small seed dataset (1,319 rows)
#'   \item \code{simulate_pep()} - Generate your own synthetic data
#' }
#'
#' @source Structure based on \url{https://www.pep725.eu/}
#'
#' @seealso \code{\link{pep_download}}, \code{\link{pep_seed}}, \code{\link{simulate_pep}}
#'
#' @examples
#' \donttest{
#' # Download full synthetic dataset
#' pep <- pep_download()
#' summary(pep)
#'
#' # Or use the small seed dataset
#' data(pep_seed)
#' summary(pep_seed)
#' }
NULL

#' Historical Harvest and Heading Dates from MeteoSwiss
#'
#' This dataset contains manually observed heading and harvest dates for a crop (likely winter wheat) near Nyon/Changins, Switzerland, as archived by MeteoSwiss. The data includes day-of-year (DOY), year, and calendar dates for both phenophases over a 30-year period.
#'
#' @format A data.table with 30 rows and 8 variables:
#' \describe{
#'   \item{yearHd}{Year of heading observation (integer)}
#'   \item{monthHd}{Month of heading (integer)}
#'   \item{dayHd}{Day of heading (integer)}
#'   \item{DOYHd}{Day of year for heading (integer)}
#'   \item{yearHv}{Year of harvest observation (integer)}
#'   \item{monthHv}{Month of harvest (integer)}
#'   \item{dayHv}{Day of harvest (integer)}
#'   \item{DOYHv}{Day of year for harvest (integer)}
#' }
#'
#' @details These observational records are used to validate phenological models and assess long-term changes in crop development stages under historical climate. The data originates from the MeteoSwiss station network near Changins, which is part of Switzerland’s official agricultural phenology monitoring.
#'
#' @source MeteoSwiss (Bundesamt für Meteorologie und Klimatologie)
#'
#' @docType data
#' @keywords datasets
#' @name meteoSwiss
#' @usage data(meteoSwiss)
#'
#' @examples
#' data(meteoSwiss)
#' plot(meteoSwiss$yearHd, meteoSwiss$DOYHd, type = "b", ylab = "Heading DOY", xlab = "Year")
#' plot(meteoSwiss$yearHv, meteoSwiss$DOYHv, type = "b", ylab = "Harvest DOY", xlab = "Year")
"meteoSwiss"