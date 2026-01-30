#' Calculate Thermal Units (Growing Degree Days)
#'
#' Computes accumulated thermal units (growing degree days, GDD) from daily
#' temperature data. Thermal time is a fundamental concept in phenology,
#' representing the heat accumulation that drives plant development.
#'
#' @param tmin Numeric vector of daily minimum temperatures (°C).
#' @param tmax Numeric vector of daily maximum temperatures (°C). If \code{NULL},
#'   \code{tmin} is treated as daily mean temperature.
#' @param t_base Numeric. Base temperature (°C) below which no development occurs.
#'   Default is 5°C, typical for temperate crops.
#' @param t_cap Numeric. Optional upper temperature cap (°C). Temperatures above
#'   this are set to \code{t_cap} before GDD calculation. Default \code{NULL}
#'   (no cap). Use ~30°C for crops where high heat doesn't increase development.
#' @param method Character. Calculation method:
#'   \describe{
#'     \item{"average"}{(Default) GDD = max(0, (tmax + tmin)/2 - t_base)}
#'     \item{"modified"}{Sets negative daily GDD to 0 before averaging}
#'     \item{"single_sine"}{Sine wave approximation for more accurate heat units}
#'   }
#' @param cumulative Logical. If \code{TRUE} (default), returns cumulative sum.
#'   If \code{FALSE}, returns daily values.
#' @param na.rm Logical. Remove NA values in accumulation? Default \code{TRUE}.
#'
#' @return Numeric vector of thermal units (degree-days). Same length as input.
#'   If \code{cumulative = TRUE}, returns running sum.
#'
#' @details
#' Growing Degree Days (GDD) quantify heat accumulation above a base temperature.
#' Plants require a certain thermal sum to reach phenological stages (e.g.,
#' flowering typically requires 500-1500 GDD depending on species).
#'
#' The base temperature represents the minimum temperature for growth:
#' \itemize{
#'   \item Cool-season crops (wheat, barley): 0-5°C
#'   \item Warm-season crops (maize, soybean): 10°C
#'   \item Fruit trees (apple, cherry): 4-7°C
#'   \item Grapevine: 10°C
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{average}{Simple average: GDD = max(0, (Tmax + Tmin)/2 - Tbase).
#'     Most common method, used by many agricultural agencies.}
#'   \item{modified}{Like average, but Tmin and Tmax are first bounded to
#'     the range from Tbase to Tcap before averaging. Prevents negative contributions.}
#'   \item{single_sine}{Approximates the daily temperature curve as a sine
#'     wave, computing the area above Tbase. More accurate when temperatures
#'     cross the base threshold during the day.}
#' }
#'
#' @examples
#' # Daily temperatures for a week
#' tmin <- c(5, 7, 8, 10, 12, 11, 9)
#' tmax <- c(15, 18, 20, 22, 25, 23, 19)
#'
#' # Cumulative GDD with base 10°C
#' calc_thermal_units(tmin, tmax, t_base = 10)
#'
#' # Daily GDD values
#' calc_thermal_units(tmin, tmax, t_base = 10, cumulative = FALSE)
#'
#' # With upper cap at 30°C
#' calc_thermal_units(tmin, tmax, t_base = 10, t_cap = 30)
#'
#' # Using mean temperature only
#' tmean <- (tmin + tmax) / 2
#' calc_thermal_units(tmean, t_base = 10)
#'
#' @references
#' McMaster, G.S., Wilhelm, W.W. (1997). Growing degree-days: one equation,
#' two interpretations. Agricultural and Forest Meteorology 87:291-300.
#'
#' @seealso \code{\link{calc_thermal_sum}} for computing thermal sum at
#'   phenological observations
#'
#' @author Matthias Templ
#' @export
calc_thermal_units <- function(tmin,
                                tmax = NULL,
                                t_base = 5,
                                t_cap = NULL,
                                method = c("average", "modified", "single_sine"),
                                cumulative = TRUE,
                                na.rm = TRUE) {

  method <- match.arg(method)

  # Input validation

  if (!is.numeric(tmin)) {
    stop("'tmin' must be numeric", call. = FALSE)
  }

  # If tmax not provided, treat tmin as mean temperature
  if (is.null(tmax)) {
    tmean <- tmin
    tmax <- tmin
    tmin_orig <- tmin
  } else {
    if (!is.numeric(tmax)) {
      stop("'tmax' must be numeric", call. = FALSE)
    }
    if (length(tmin) != length(tmax)) {
      stop("'tmin' and 'tmax' must have same length", call. = FALSE)
    }
    tmean <- (tmin + tmax) / 2
    tmin_orig <- tmin
  }

  # Apply temperature cap if specified
  if (!is.null(t_cap)) {
    tmax <- pmin(tmax, t_cap)
    tmin <- pmin(tmin, t_cap)
    tmean <- (tmin + tmax) / 2
  }

  n <- length(tmin)
  gdd <- numeric(n)

  if (method == "average") {
    # Simple averaging method
    # GDD = max(0, (Tmax + Tmin)/2 - Tbase)
    gdd <- pmax(0, tmean - t_base)

  } else if (method == "modified") {
    # Modified method - bound temperatures first
    tmin_adj <- pmax(tmin, t_base)
    tmax_adj <- pmax(tmax, t_base)
    if (!is.null(t_cap)) {
      tmin_adj <- pmin(tmin_adj, t_cap)
      tmax_adj <- pmin(tmax_adj, t_cap)
    }
    gdd <- (tmax_adj + tmin_adj) / 2 - t_base

  } else if (method == "single_sine") {
    # Single sine wave approximation
    # More accurate when temperature crosses base during day
    for (i in seq_len(n)) {
      if (is.na(tmin_orig[i]) || is.na(tmax[i])) {
        gdd[i] <- NA
        next
      }

      t_min_i <- tmin_orig[i]
      t_max_i <- tmax[i]
      t_avg <- (t_min_i + t_max_i) / 2
      t_amp <- (t_max_i - t_min_i) / 2

      if (t_min_i >= t_base) {
        # Entire day above base
        gdd[i] <- t_avg - t_base
      } else if (t_max_i <= t_base) {
        # Entire day below base
        gdd[i] <- 0
      } else {
        # Temperature crosses base - use sine integration
        # theta = angle where temperature = t_base
        theta <- acos((t_base - t_avg) / t_amp)
        # GDD = integral of (T - Tbase) over portion above Tbase
        gdd[i] <- (1 / pi) * ((t_avg - t_base) * (pi - 2 * theta) +
                                2 * t_amp * sin(theta))
        gdd[i] <- max(0, gdd[i])
      }
    }
  }

  # Return cumulative or daily values
  if (cumulative) {
    if (na.rm) {
      gdd[is.na(gdd)] <- 0
    }
    gdd <- cumsum(gdd)
  }

  gdd
}


#' Calculate Thermal Sum at Phenological Events
#'
#' Computes the accumulated thermal units (GDD) from a start date to the
#' observed phenological event date. Useful for determining the thermal
#' requirements of different phenophases.
#'
#' @param pep A \code{pep} object or data.table with phenological observations.
#'   Must contain \code{year} and \code{day} columns.
#' @param temp_data A data.frame or data.table with daily temperature data.
#'   Must contain columns: \code{year}, \code{doy} (day of year),
#'   \code{tmin}, \code{tmax} (or \code{tmean}).
#' @param t_base Numeric. Base temperature (°C). Default 5.
#' @param t_start Integer. Start day of year for accumulation. Default 1 (Jan 1).
#'   Use 60 for March 1, etc.
#' @param by Character vector. Columns to match temperature data to phenology
#'   (e.g., \code{"s_id"} for station-specific temperatures). Default \code{NULL}
#'   uses same temperature for all observations in a year.
#' @param method Character. GDD calculation method (see \code{\link{calc_thermal_units}}).
#'
#' @return The input data with an additional \code{thermal_sum} column containing
#'   the accumulated GDD from \code{t_start} to the observed DOY.
#'
#' @details
#' This function joins phenological observations with temperature data and
#' calculates the thermal sum (accumulated GDD) at each observation date.
#' This is useful for:
#' \itemize{
#'   \item Determining thermal requirements of phenophases
#'   \item Comparing thermal sums across years, locations, or species
#'   \item Validating phenological models
#' }
#'
#' @examples
#' \dontrun{
#' # Load phenology data
#' data(pep_seed)
#'
#' # Create example temperature data (normally from weather stations/reanalysis)
#' temp <- data.frame(
#'   year = rep(2000:2015, each = 365),
#'   doy = rep(1:365, 16),
#'   tmin = rnorm(365 * 16, mean = 5, sd = 8),
#'   tmax = rnorm(365 * 16, mean = 15, sd = 8)
#' )
#'
#' # Add thermal sum to phenology data
#' pep_thermal <- calc_thermal_sum(pep_seed, temp, t_base = 5)
#' }
#'
#' @seealso \code{\link{calc_thermal_units}} for the underlying GDD calculation
#'
#' @author Matthias Templ
#' @export
calc_thermal_sum <- function(pep,
                              temp_data,
                              t_base = 5,
                              t_start = 1,
                              by = NULL,
                              method = "average") {

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }
  if (!inherits(temp_data, "data.frame")) {
    stop("'temp_data' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }
  if (!inherits(temp_data, "data.table")) {
    temp_data <- data.table::as.data.table(temp_data)
  }

  # Check required columns
  if (!all(c("year", "day") %in% names(pep))) {
    stop("'pep' must have 'year' and 'day' columns", call. = FALSE)
  }
  if (!all(c("year", "doy") %in% names(temp_data))) {
    stop("'temp_data' must have 'year' and 'doy' columns", call. = FALSE)
  }

  # Check for temperature columns
  has_minmax <- all(c("tmin", "tmax") %in% names(temp_data))
  has_mean <- "tmean" %in% names(temp_data)
  if (!has_minmax && !has_mean) {
    stop("'temp_data' must have either 'tmin'/'tmax' or 'tmean' columns", call. = FALSE)
  }

  # Get unique year-location combinations from pep
  pep <- data.table::copy(pep)
  pep[, .row_id := .I]

  # Calculate thermal sum for each observation
  thermal_sums <- numeric(nrow(pep))

  for (i in seq_len(nrow(pep))) {
    obs_year <- pep$year[i]
    obs_doy <- pep$day[i]

    if (is.na(obs_year) || is.na(obs_doy)) {
      thermal_sums[i] <- NA
      next
    }

    # Get temperature data for this year
    if (is.null(by)) {
      year_temp <- temp_data[year == obs_year & doy >= t_start & doy <= obs_doy]
    } else {
      # Match by additional columns (e.g., station)
      filter_expr <- list(year = obs_year)
      for (col in by) {
        if (col %in% names(pep) && col %in% names(temp_data)) {
          filter_expr[[col]] <- pep[[col]][i]
        }
      }
      year_temp <- temp_data[year == obs_year & doy >= t_start & doy <= obs_doy]
      for (col in by) {
        if (col %in% names(year_temp) && col %in% names(pep)) {
          year_temp <- year_temp[get(col) == pep[[col]][i]]
        }
      }
    }

    if (nrow(year_temp) == 0) {
      thermal_sums[i] <- NA
      next
    }

    # Order by DOY
    data.table::setorder(year_temp, doy)

    # Calculate GDD
    if (has_minmax) {
      gdd <- calc_thermal_units(year_temp$tmin, year_temp$tmax,
                                 t_base = t_base, method = method,
                                 cumulative = TRUE)
    } else {
      gdd <- calc_thermal_units(year_temp$tmean, t_base = t_base,
                                 method = method, cumulative = TRUE)
    }

    thermal_sums[i] <- tail(gdd, 1)
  }

  pep[, thermal_sum := thermal_sums]
  pep[, .row_id := NULL]

  pep
}
