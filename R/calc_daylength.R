#' Calculate Daylength (Photoperiod)
#'
#' Computes the astronomical daylength (hours of daylight) for a given
#' day of year and latitude. Photoperiod is an important environmental
#' driver of plant phenology.
#'
#' @param doy Integer or numeric vector. Day of year (1-365/366).
#' @param lat Numeric. Latitude in decimal degrees. Positive for Northern
#'   Hemisphere, negative for Southern Hemisphere.
#'
#' @return A list with components:
#'   \describe{
#'     \item{daylength}{Daylength in hours}
#'     \item{declination}{Solar declination angle in degrees}
#'   }
#'   If \code{doy} is a vector, returns a data.frame with these columns.
#'
#' @details
#' The calculation uses standard astronomical formulas based on the
#' solar declination angle. The formula assumes a "flat horizon" and
#' does not account for atmospheric refraction, elevation, or local
#' topography.
#'
#' At polar latitudes (|lat| > 66.5), continuous daylight or darkness
#' may occur around solstices.
#'
#' @section Phenological Relevance:
#' Many phenological events are triggered by photoperiod thresholds:
#' \itemize{
#'   \item Spring bud burst often requires both temperature accumulation
#'     and minimum daylength
#'   \item Autumn leaf senescence may be triggered by shortening days
#'   \item Flowering in many species is photoperiod-dependent
#' }
#'
#' @examples
#' # Daylength at spring equinox (DOY 80) at 50°N
#' calc_daylength(80, 50)
#'
#' # Summer solstice at different latitudes
#' calc_daylength(172, c(30, 45, 60))
#'
#' # Daylength through the year at 45°N
#' yearly <- calc_daylength(1:365, 45)
#' plot(yearly$doy, yearly$daylength, type = "l",
#'      xlab = "Day of Year", ylab = "Daylength (hours)")
#'
#' @author Matthias Templ
#' @export
calc_daylength <- function(doy, lat) {
  # Input validation
  if (!is.numeric(doy) || !is.numeric(lat)) {
    stop("'doy' and 'lat' must be numeric", call. = FALSE)
  }

  if (any(doy < 1 | doy > 366, na.rm = TRUE)) {
    warning("Some DOY values outside 1-366 range", call. = FALSE)
  }

  if (any(abs(lat) > 90, na.rm = TRUE)) {
    stop("Latitude must be between -90 and 90 degrees", call. = FALSE)
  }

  # Convert latitude to radians
  lat_rad <- lat * pi / 180

  # Solar declination angle (radians)
  # Using simplified formula: delta = 23.45 * sin(2*pi*(284+doy)/365)
  declination_rad <- 23.45 * pi / 180 * sin(2 * pi * (284 + doy) / 365)

  # Hour angle at sunrise/sunset
  # cos(hour_angle) = -tan(lat) * tan(declination)
  cos_hour_angle <- -tan(lat_rad) * tan(declination_rad)

  # Handle polar day/night cases
  daylength <- numeric(length(cos_hour_angle))

  # Normal case: sun rises and sets
  normal <- abs(cos_hour_angle) <= 1
  hour_angle <- acos(pmin(pmax(cos_hour_angle, -1), 1))
  daylength[normal] <- 2 * hour_angle[normal] * 12 / pi

  # Polar day: sun never sets (cos_hour_angle < -1)
  polar_day <- cos_hour_angle < -1
  daylength[polar_day] <- 24

  # Polar night: sun never rises (cos_hour_angle > 1)
  polar_night <- cos_hour_angle > 1
  daylength[polar_night] <- 0

  # Convert declination to degrees for output
  declination_deg <- declination_rad * 180 / pi

  # Return format depends on input
  if (length(doy) == 1 && length(lat) == 1) {
    list(
      daylength = daylength,
      declination = declination_deg
    )
  } else {
    data.frame(
      doy = doy,
      lat = lat,
      daylength = daylength,
      declination = declination_deg
    )
  }
}


#' Maximum Daylength at a Latitude
#'
#' Calculates the maximum possible daylength (at summer solstice) for
#' a given latitude.
#'
#' @param lat Numeric. Latitude in decimal degrees.
#'
#' @return Numeric. Maximum daylength in hours.
#'
#' @examples
#' # Maximum daylength at different latitudes
#' calc_max_daylength(c(0, 30, 45, 60, 66.5))
#'
#' @author Matthias Templ
#' @export
calc_max_daylength <- function(lat) {
  # Summer solstice is approximately DOY 172 (June 21)
  result <- calc_daylength(172, lat)

  if (is.data.frame(result)) {
    result$daylength
  } else {
    result$daylength
  }
}


#' Add Daylength to Phenological Data
#'
#' Adds a daylength column to a phenological dataset based on the
#' observation day and station latitude.
#'
#' @param pep A \code{pep} object or data.table with \code{day} and
#'   \code{lat} columns.
#'
#' @return The input data with an added \code{daylength} column (hours).
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' pep <- add_daylength(pep)
#' head(pep[, .(day, lat, daylength)])
#' }
#'
#' @seealso \code{\link{calc_daylength}} for the underlying calculation
#' @author Matthias Templ
#' @export
add_daylength <- function(pep) {
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  if (!all(c("day", "lat") %in% names(pep))) {
    stop("Data must contain 'day' and 'lat' columns", call. = FALSE)
  }

  # Calculate daylength for each observation
  # Use set() to avoid issues with pep class's [ method
  daylength_vals <- calc_daylength(pep$day, pep$lat)$daylength
  data.table::set(pep, j = "daylength", value = daylength_vals)

  pep
}
