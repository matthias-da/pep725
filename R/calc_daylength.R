#' Calculate Daylength (Photoperiod)
#'
#' Computes the astronomical daylength (hours of daylight) for a given
#' day of year and latitude. Photoperiod is an important environmental
#' driver of plant phenology.
#'
#' @param doy Integer or numeric vector. Day of year (1-365/366).
#' @param lat Numeric. Latitude in decimal degrees. Positive for Northern
#'   Hemisphere, negative for Southern Hemisphere.
#' @param method Character. Daylength model:
#'   \describe{
#'     \item{\code{"brock"}}{(Default) Brock (1981) / Spencer (1971)
#'       simple declination model: \eqn{\delta = 23.45^\circ \cdot
#'       \sin(2\pi(284 + \mathrm{DOY})/365)}. Fast and adequate at
#'       mid-latitudes; differs from CBM by up to a few minutes/day
#'       near the poles.}
#'     \item{\code{"cbm"}}{Forsythe et al. (1995) Climate-Budget-Model
#'       formulation. Includes the eccentricity correction of the Earth's
#'       orbit and is generally the most accurate of the four models
#'       Forsythe et al. compared.}
#'   }
#'
#' @return A list with components:
#'   \describe{
#'     \item{daylength}{Daylength in hours}
#'     \item{declination}{Solar declination angle in degrees}
#'   }
#'   If \code{doy} is a vector, returns a data.frame with these columns.
#'
#' @details
#' Both methods assume a "flat horizon" (sunrise/sunset when the geometric
#' centre of the sun crosses the horizon) and do not account for
#' atmospheric refraction, elevation, or local topography.
#'
#' At polar latitudes (\eqn{|lat| > 66.5^\circ}), continuous daylight or
#' darkness may occur around solstices.
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
#' @references
#' Spencer, J. W. (1971). Fourier series representation of the position of
#' the sun. \emph{Search} 2(5):172. (Declination formula used by the
#' \code{"brock"} method.)
#'
#' Brock, T. D. (1981). Calculating solar radiation for ecological studies.
#' \emph{Ecological Modelling} 14(1-2):1-19.
#' \doi{10.1016/0304-3800(81)90011-9}
#'
#' Forsythe, W. C., Rykiel, E. J., Stahl, R. S., Wu, H., and
#' Schoolfield, R. M. (1995). A model comparison for daylength as a
#' function of latitude and day of year. \emph{Ecological Modelling}
#' 80(1):87-95. \doi{10.1016/0304-3800(94)00034-F}
#' (CBM daylength model used by the \code{"cbm"} method.)
#'
#' @author Matthias Templ
#' @export
calc_daylength <- function(doy, lat, method = c("brock", "cbm")) {
  method <- match.arg(method)
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

  # Solar declination (radians)
  if (method == "brock") {
    # Brock (1981) / Spencer (1971): delta = 23.45° * sin(2π(284+DOY)/365)
    declination_rad <- 23.45 * pi / 180 * sin(2 * pi * (284 + doy) / 365)
  } else {
    # Forsythe et al. (1995) CBM model: revolution-angle correction for
    # the Earth's elliptic orbit.
    #   theta = 0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (J - 186)))
    #   delta = asin(0.39795 * cos(theta))
    theta <- 0.2163108 +
      2 * atan(0.9671396 * tan(0.00860 * (doy - 186)))
    declination_rad <- asin(0.39795 * cos(theta))
  }

  # Convert latitude to radians
  lat_rad <- lat * pi / 180

  # Hour angle at sunrise/sunset: cos(H) = -tan(lat) * tan(delta)
  cos_hour_angle <- -tan(lat_rad) * tan(declination_rad)

  # Handle polar day/night and NA cases
  daylength <- rep(NA_real_, length(cos_hour_angle))
  not_na <- !is.na(cos_hour_angle)

  # Normal case: sun rises and sets
  normal <- not_na & abs(cos_hour_angle) <= 1
  hour_angle <- acos(pmin(pmax(cos_hour_angle, -1), 1))
  daylength[normal] <- 2 * hour_angle[normal] * 12 / pi

  # Polar day: sun never sets (cos_hour_angle < -1)
  polar_day <- not_na & cos_hour_angle < -1
  daylength[polar_day] <- 24

  # Polar night: sun never rises (cos_hour_angle > 1)
  polar_night <- not_na & cos_hour_angle > 1
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

  result$daylength
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
