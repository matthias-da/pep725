# Prevent R CMD check notes
utils::globalVariables(c("lon", "lat", "n", "n_species", "mean_doy", "trend",
                         "species_cv", "year", "day", "species", "s_id"))

#' Plot Phenology Station Maps with Google Maps
#'
#' Generates a map of PEP725 phenological station locations with optional coloring
#' by various statistics including mean phenological timing, trends, and species
#' variation.
#'
#' @param pep A `data.table` or `data.frame` with phenological observations.
#'   Required columns depend on \code{color_by}:
#'   \itemize{
#'     \item Always required: \code{lon}, \code{lat}
#'     \item For \code{"n_species"}, \code{"mean_doy"}, \code{"species_cv"}: \code{species}
#'     \item For \code{"mean_doy"}, \code{"trend"}: \code{year}, \code{day}
#'   }
#' @param location Named vector with center longitude and latitude for the map.
#'   Defaults to near Changins: \code{c(lon = 6.233, lat = 46.400)}.
#' @param zoom Zoom level for the map (default: 4 for Europe-wide, 7 for regional).
#' @param color_by Character. What to color stations by:
#'   \describe{
#'     \item{"none"}{Black points showing station locations only}
#'     \item{"n_obs"}{Number of observations per station}
#'     \item{"n_species"}{Number of species recorded per station}
#'     \item{"mean_doy"}{Mean day-of-year (phenological timing) per station}
#'     \item{"trend"}{Trend in days/year per station (positive = later, negative = earlier)}
#'     \item{"species_cv"}{Coefficient of variation in timing across species at each station
#'       (higher = more variation among species)}
#'   }
#' @param phase_id Integer. BBCH phase code to filter by. Recommended for
#'   \code{"mean_doy"} and \code{"trend"} to ensure meaningful comparisons.
#'   If \code{NULL} (default), all phases are included.
#' @param period Integer vector of years to include. Default is all years.
#'   For trend calculation, at least 10 years are recommended.
#' @param min_years Integer. Minimum years required for trend calculation.
#'   Stations with fewer years show as NA. Default is 10.
#' @param min_species Integer. Minimum species required for \code{species_cv}.
#'   Default is 3.
#' @param point_size Size of station points (default: 0.8).
#' @param output_file Optional file path to export the plot (e.g. \code{"map.pdf"}).
#' @param key Google Maps API key (you can set globally via \code{register_google()} instead).
#'
#' @return A ggplot map object.
#'
#' @details
#' The new color options provide insights into:
#' \itemize{
#'   \item \strong{mean_doy}: Spatial patterns in phenological timing. Earlier
#'     timing (lower DOY) typically in southern/lowland areas.
#'   \item \strong{trend}: Where phenology is advancing (negative = earlier over time,
#'     shown in blue) or delaying (positive, shown in red). Uses Kendall's tau
#'     normalized statistic for robustness.
#'   \item \strong{species_cv}: Stations where different species show similar timing
#'     (low CV) vs. divergent timing (high CV). High variation may indicate
#'     species-specific responses to local conditions.
#' }
#'
#' @section Interpreting Trends:
#' The trend is calculated as Kendall's normalized tau statistic, which ranges

#' roughly from -3 to +3 for typical data:
#' \itemize{
#'   \item Negative values (blue): Phenology is getting earlier over time
#'   \item Positive values (red): Phenology is getting later over time
#'   \item Values near 0: No clear trend
#'   \item |tau| > 1.96: Statistically significant at 95% level
#' }
#'
#' @seealso \code{\link{pheno_normals}} for detailed normal calculations,
#'   \code{\link{kendall_tau}} for trend calculation,
#'   \code{\link{pheno_synchrony}} for synchrony analysis
#'
#' @import ggplot2
#' @import ggmap
#' @importFrom dplyr group_by summarise n n_distinct
#' @importFrom ggmap register_google get_map
#' @importFrom stats sd coef lm na.omit
#' @examples
#' \dontrun{
#' # Register API key (or provide via argument)
#' ggmap::register_google(key = "your_api_key_here")
#' pep <- pep_download()
#'
#' # Plot all stations (Europe-wide)
#' map_pep(pep, color_by = "none", zoom = 4)
#'
#' # Plot with number of observations
#' map_pep(pep, color_by = "n_obs", zoom = 4)
#'
#' # Plot mean phenological timing for flowering (phase 60)
#' map_pep(pep, color_by = "mean_doy", phase_id = 60, zoom = 4)
#'
#' # Plot trends in flowering timing
#' map_pep(pep, color_by = "trend", phase_id = 60,
#'         period = 1990:2020, min_years = 10, zoom = 4)
#'
#' # Plot species variation at each station
#' map_pep(pep, color_by = "species_cv", phase_id = 60, zoom = 4)
#'
#' # Regional view near Changins with number of species per station
#' pep_sub <- pep[pep$lat > 44.7 & pep$lat < 48.1 & pep$lon > 4.2 & pep$lon < 8.1, ]
#' map_pep(pep_sub, color_by = "n_species", zoom = 7)
#' }
#' @author Matthias Templ
#' @export
map_pep <- function(
    pep,
    location = c(lon = 6.233, lat = 46.400),
    zoom = 4,
    color_by = c("none", "n_obs", "n_species", "mean_doy", "trend", "species_cv"),
    phase_id = NULL,
    period = NULL,
    min_years = 10,
    min_species = 3,
    point_size = 0.8,
    output_file = NULL,
    key = NULL
) {
  color_by <- match.arg(color_by)

  # Input validation
  if (!all(c("lon", "lat") %in% names(pep))) {
    stop("'pep' must contain 'lon' and 'lat' columns", call. = FALSE)
  }

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Make a working copy
  dt <- data.table::copy(pep)

  # Apply phase filter if specified

  if (!is.null(phase_id)) {
    if (!"phase_id" %in% names(dt)) {
      warning("'phase_id' column not found. Ignoring phase_id filter.", call. = FALSE)
    } else {
      dt <- dt[dt$phase_id %in% phase_id]
      if (nrow(dt) == 0) {
        stop("No observations found for phase_id: ", paste(phase_id, collapse = ", "),
             call. = FALSE)
      }
    }
  }

  # Apply period filter if specified
  if (!is.null(period)) {
    if (!"year" %in% names(dt)) {
      warning("'year' column not found. Ignoring period filter.", call. = FALSE)
    } else {
      dt <- dt[dt$year %in% period]
      if (nrow(dt) == 0) {
        stop("No observations found in specified period", call. = FALSE)
      }
    }
  }

  # Register Google Maps API key
  if (!is.null(key)) {
    register_google(key = key)
  }

  # Download map
  gmap <- get_map(location = location, zoom = zoom)
  g <- ggmap(gmap) +
    theme_minimal()

  # Build the plot based on color_by option
  if (color_by == "none") {
    # Just plot unique lon/lat
    unique_coords <- unique(dt[, c("lon", "lat")])
    g <- g + geom_point(data = unique_coords, aes(x = lon, y = lat),
                        size = point_size, color = "black")
    title <- "Phenology Stations"

  } else if (color_by == "n_obs") {
    nobs <- dt[, .(n = .N), by = .(lon, lat)]
    g <- g + geom_point(data = nobs, aes(x = lon, y = lat, color = n),
                        size = point_size) +
      scale_color_viridis_c(name = "No. obs.", option = "plasma")
    title <- "Number of Observations per Station"

  } else if (color_by == "n_species") {
    if (!"species" %in% names(dt)) {
      stop("'species' column required for color_by = 'n_species'", call. = FALSE)
    }
    nsp <- dt[, .(n_species = data.table::uniqueN(species)), by = .(lon, lat)]
    g <- g + geom_point(data = nsp, aes(x = lon, y = lat, color = n_species),
                        size = point_size) +
      scale_color_viridis_c(name = "No. species", option = "plasma")
    title <- "Number of Species per Station"

  } else if (color_by == "mean_doy") {
    # Check required columns
    if (!all(c("day", "year") %in% names(dt))) {
      stop("'day' and 'year' columns required for color_by = 'mean_doy'", call. = FALSE)
    }

    # Calculate mean DOY per station
    # First get annual means, then average across years for robustness
    station_means <- dt[, .(annual_mean = mean(day, na.rm = TRUE)),
                        by = .(lon, lat, year)]
    station_means <- station_means[, .(mean_doy = mean(annual_mean, na.rm = TRUE),
                                        n_years = .N),
                                    by = .(lon, lat)]

    g <- g + geom_point(data = station_means, aes(x = lon, y = lat, color = mean_doy),
                        size = point_size) +
      scale_color_viridis_c(name = "Mean DOY", option = "plasma",
                            direction = -1)  # Earlier = blue, later = yellow
    title <- paste0("Mean Phenological Timing",
                    if (!is.null(phase_id)) paste0(" (Phase ", phase_id, ")") else "")

  } else if (color_by == "trend") {
    # Check required columns
    if (!all(c("day", "year") %in% names(dt))) {
      stop("'day' and 'year' columns required for color_by = 'trend'", call. = FALSE)
    }

    # Calculate trend per station using Kendall's tau
    # First aggregate to annual means per station
    annual_data <- dt[, .(annual_mean = mean(day, na.rm = TRUE)),
                      by = .(lon, lat, year)][order(lon, lat, year)]

    # Calculate trend for each station
    station_trends <- annual_data[, {
      if (.N >= min_years) {
        # Use kendall_tau for robust trend estimation
        tau <- kendall_tau(annual_mean)
        list(trend = tau, n_years = .N)
      } else {
        list(trend = NA_real_, n_years = .N)
      }
    }, by = .(lon, lat)]

    # Remove stations with NA trends for cleaner plotting
    station_trends_valid <- station_trends[!is.na(trend)]

    if (nrow(station_trends_valid) == 0) {
      warning("No stations have enough years for trend calculation (min_years = ",
              min_years, ")", call. = FALSE)
      # Fall back to showing all stations in gray
      g <- g + geom_point(data = unique(dt[, .(lon, lat)]),
                          aes(x = lon, y = lat),
                          size = point_size, color = "gray50")
    } else {
      # Use diverging color scale: blue = earlier (negative), red = later (positive)
      max_abs <- max(abs(station_trends_valid$trend), na.rm = TRUE)
      g <- g + geom_point(data = station_trends_valid,
                          aes(x = lon, y = lat, color = trend),
                          size = point_size) +
        scale_color_gradient2(name = "Trend\n(Kendall tau)",
                              low = "blue", mid = "white", high = "red",
                              midpoint = 0,
                              limits = c(-max_abs, max_abs))
    }
    title <- paste0("Phenological Trends",
                    if (!is.null(phase_id)) paste0(" (Phase ", phase_id, ")") else "",
                    "\nBlue = earlier, Red = later")

  } else if (color_by == "species_cv") {
    # Check required columns
    if (!all(c("day", "species") %in% names(dt))) {
      stop("'day' and 'species' columns required for color_by = 'species_cv'",
           call. = FALSE)
    }

    # Calculate CV of mean DOY across species at each station
    # First get mean DOY per species per station
    species_means <- dt[, .(species_mean = mean(day, na.rm = TRUE)),
                        by = .(lon, lat, species)]

    # Then calculate CV across species at each station
    station_cv <- species_means[, {
      n_sp <- .N
      if (n_sp >= min_species) {
        mean_val <- mean(species_mean, na.rm = TRUE)
        sd_val <- sd(species_mean, na.rm = TRUE)
        cv <- if (!is.na(mean_val) && mean_val > 0) 100 * sd_val / mean_val else NA_real_
        list(species_cv = cv, n_species = n_sp)
      } else {
        list(species_cv = NA_real_, n_species = n_sp)
      }
    }, by = .(lon, lat)]

    station_cv_valid <- station_cv[!is.na(species_cv)]

    if (nrow(station_cv_valid) == 0) {
      warning("No stations have enough species for CV calculation (min_species = ",
              min_species, ")", call. = FALSE)
      g <- g + geom_point(data = unique(dt[, .(lon, lat)]),
                          aes(x = lon, y = lat),
                          size = point_size, color = "gray50")
    } else {
      g <- g + geom_point(data = station_cv_valid,
                          aes(x = lon, y = lat, color = species_cv),
                          size = point_size) +
        scale_color_viridis_c(name = "Species CV (%)", option = "magma")
    }
    title <- paste0("Species Variation in Timing",
                    if (!is.null(phase_id)) paste0(" (Phase ", phase_id, ")") else "",
                    "\nHigher CV = more variation among species")
  }

  g <- g + labs(title = title) +
    theme(legend.position = "right",
          plot.title = element_text(size = 11))

  # Save if requested
  if (!is.null(output_file)) {
    ggsave(output_file, plot = g, width = 10, height = 8)
  }

  return(g)
}
