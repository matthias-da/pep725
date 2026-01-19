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
#' \dontrun{
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
#' @source MeteoSwiss (Bundesamt für Meteorologie und Klimatologie), via Agroscope archives
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

#' Agroscope Winter Wheat Phenological Trials
#'
#' This dataset contains phenological observations from Agroscope winter wheat field trials in Switzerland. It includes sowing, heading, and harvest dates for different wheat varieties and locations (postal codes) over multiple years. These data are typically used for model calibration and validation of crop phenology under Swiss agroclimatic conditions.
#'
#' @format A data.table with 41,424 rows and 7 variables:
#' \describe{
#'   \item{Variety}{Wheat variety name (character), may contain missing values}
#'   \item{PLZ}{Swiss postal code of the trial location (integer)}
#'   \item{SowingYEAR}{Year of sowing (integer)}
#'   \item{SowingDOY}{Day of year of sowing (integer)}
#'   \item{HarvestYEAR}{Year of harvest (integer)}
#'   \item{HeadingDOY}{Day of year of heading (numeric)}
#'   \item{HarvestDOY}{Day of year of harvest (integer)}
#' }
#'
#' @details The dataset originates from Agroscope’s official winter wheat variety trials. Observations cover multiple varieties and locations, providing key phenological stages (sowing, heading, harvest) required for climate impact assessments, phenological model calibration, and sensitivity analyses. Missing values in the \code{Variety} column indicate unclassified entries but retain valid phenological dates.
#'
#' @source Agroscope (Swiss Federal Research Station for Agroecology and Agriculture)
#'
#' @docType data
#' @keywords datasets
#' @name agroscope
#' @usage data(agroscope)
#'
#' @examples
#' data(agroscope)
#' summary(agroscope)
#' with(agroscope, plot(HeadingDOY, HarvestDOY,
#'      xlab = "Heading DOY", ylab = "Harvest DOY", main = "Phenological stages"))
#' table(agroscope$Variety)
"agroscope"

#' Global GISS Temperature Anomalies (1880–2024)
#'
#' This dataset contains global annual surface air temperature anomalies
#' from the NASA GISS (Goddard Institute for Space Studies), relative to a 1951–1980 baseline.
#' It is used in climate–phenology comparisons and sensitivity analyses.
#'
#' @format A \code{data.table} with 145 rows and 3 variables:
#' \describe{
#'   \item{\code{year}}{Integer. Calendar year from 1880 to 2024.}
#'   \item{\code{dT}}{Numeric. Annual global mean surface temperature anomaly (°C) relative to the 1951–1980 baseline.}
#'   \item{\code{dT_sm}}{Numeric. Smoothed anomaly (°C), typically a 5- or 11-year running mean.}
#' }
#'
#' @source NASA GISS Surface Temperature Analysis (GISTEMP v4) via \url{https://data.giss.nasa.gov/gistemp/}
#'
#' @details
#' The anomaly values are computed relative to the 1951–1980 climatological average.
#' The smoothed version (\code{dT_sm}) is useful for visual trend analysis or for climate-driven modeling where year-to-year variability should be dampened.
#'
#' This dataset is automatically loaded with the package and used by the function \code{\link{regional_box_ts}}
#' to link phenological responses with temperature anomalies.
#'
#' @seealso \code{\link{regional_box_ts}}, \code{\link{pep}}, \code{\link{agroscope}}, \code{\link{meteoSwiss}}
#'
#' @examples
#' data(giss)
#' plot(giss$year, giss$dT, type = "l", main = "Global Temperature Anomalies (GISS)",
#'      ylab = "Anomaly (°C)", xlab = "Year")
"giss"

#' Daily Hail Probability Data for Austria, Germany, and Switzerland
#'
#' This dataset contains daily hail probability estimates derived from
#' convection-permitting climate simulations. It includes mean hail probability,
#' lower and upper uncertainty bounds, and spatial metadata (number of grid
#' cells and total area) for each day of the year (DOY), for each country and for
#' multiple climate scenarios.
#'
#' The dataset is provided as a `data.table` / `data.frame` with 4,392 rows
#' (365–366 DOY × countries × scenarios) and nine variables.
#'
#' @format A data frame with 4,392 observations and 9 variables:
#' \describe{
#'   \item{Subdomain}{Character. Geographic subdomain identifier
#'         (e.g., `"Austria"`, `"Switzerland"`, `"Germany"`).}
#'   \item{DOY}{Integer. Day of year (1–365 or 1–366).}
#'   \item{mean}{Numeric. Mean daily hail probability across all grid cells
#'         belonging to the subdomain and scenario.}
#'   \item{q5}{Numeric. 5th percentile of the daily hail probability distribution
#'         across grid cells (lower uncertainty bound).}
#'   \item{q95}{Numeric. 95th percentile of the daily hail probability distribution
#'         across grid cells (upper uncertainty bound).}
#'   \item{n_cells}{Integer. Number of model grid cells contributing to the
#'         aggregated hail probabilities for that subdomain.}
#'   \item{area}{Numeric. Total area (in km² or model units) covered by the
#'         grid cells included in the aggregation.}
#'   \item{Country}{Character. Country code or name used for grouping in
#'         analyses (e.g., `"Austria"`, `"Germany"`).}
#'   \item{Scenario}{Character. Climate scenario identifier, typically including
#'         `"ctrl"` (historical control simulation) and future scenarios.}
#' }
#'
#' @details
#' The hail probabilities are derived from climate simulations using a
#' convection-permitting model. The values represent **aggregated hail
#' occurrence probabilities**, averaged across all cells belonging to each
#' country’s subdomain.
#'
#' This dataset is typically used for:
#' * climate impact assessments,
#' * phenology–hail overlap studies,
#' * agricultural risk modelling,
#' * scenario comparisons (CTRL vs. PGW/SCEN).
#'
#' @seealso
#' \code{\link{integral}} for integrating hail probabilities over phenological windows.
#'
#' @examples
#' \dontrun{
#' # Inspect structure
#' str(hail)
#'
#' # Plot mean hail probability for Austria (CTRL)
#' library(ggplot2)
#' ggplot(hail[hail$Country == "Austria" & hail$Scenario == "ctrl", ],
#'        aes(DOY, mean)) +
#'   geom_line() +
#'   labs(y = "Mean hail probability", x = "Day of Year")
#' }
#'
#' @docType data
#' @keywords datasets
"hail"

