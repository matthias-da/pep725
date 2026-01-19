#' Minimal Seed Dataset for Synthetic Data Generation
#'
#' A small subset of phenological data suitable for use with \code{\link{simulate_pep}}
#' to generate synthetic phenological datasets. This dataset contains real structure
#' but limited scope, making it suitable for package examples and testing.
#'
#' @format A data.table with 1,319 rows and 10 variables:
#' \describe{
#'   \item{s_id}{Station identifier (factor)}
#'   \item{lon}{Longitude in decimal degrees}
#'   \item{lat}{Latitude in decimal degrees}
#'   \item{alt}{Altitude in meters}
#'   \item{genus}{Plant genus (factor)}
#'   \item{species}{Full species name (factor)}
#'   \item{phase_id}{BBCH phenological phase code (integer)}
#'   \item{year}{Observation year}
#'   \item{day}{Day of year (DOY) of the phenological event}
#'   \item{country}{Country name}
#' }
#'
#' @details
#' This dataset includes observations for:
#' \itemize{
#'   \item \strong{Species}: Triticum aestivum (wheat), Vitis vinifera (grapevine),
#'     Malus domestica (apple)
#'   \item \strong{Phases}: BBCH 10 (emergence), 60 (flowering), 65 (full flowering)
#'   \item \strong{Countries}: Austria, Germany-South
#'   \item \strong{Years}: 1990-2015
#'   \item \strong{Stations}: 30 stations
#' }
#'
#' For a full synthetic dataset, use \code{\link{pep_download}} to download
#' pre-generated synthetic data from the package repository.
#'
#' @source
#' Derived from PEP725 Pan European Phenology Database \url{http://www.pep725.eu/}
#'
#' @examples
#' # Load the seed dataset
#' data(pep_seed)
#'
#' # Examine structure
#' str(pep_seed)
#'
#' # Use with simulate_pep to create synthetic data
#' \dontrun{
#' pep_synthetic <- simulate_pep(pep_seed)
#' }
#'
#' @seealso
#' \code{\link{simulate_pep}} for generating synthetic data,
#' \code{\link{pep_download}} for downloading full synthetic dataset
#'
#' @name pep_seed
#' @docType data
#' @keywords datasets
NULL
