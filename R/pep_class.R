#' @title PEP725 Phenological Data Class
#'
#' @description
#' S3 class for PEP725 phenological data that inherits from \code{data.table}.
#' Provides validation, informative print/summary methods, and convenient defaults.
#'
#' @name pep-class
#' @aliases pep-class
#'
#' @importFrom data.table as.data.table uniqueN .N .SD setDT copy fifelse rbindlist fread setcolorder setnames setorderv melt
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram geom_line labs theme_minimal coord_quickmap
#' @importFrom stats IQR median sd quantile rnorm residuals lm pnorm as.formula complete.cases contr.sum coef predict contrasts<-
#' @importFrom utils head tail txtProgressBar setTxtProgressBar
#' @importFrom patchwork wrap_plots
#' @import methods
NULL

# Prevent R CMD check notes for data.table columns and other non-standard evaluation
utils::globalVariables(c(
  # Core pep columns
  "s_id", "lon", "lat", "genus", "species", "phase_id", "year", "day",
  "n_obs", "country", "phase_name",
  # data.table special symbols
  ".N", ".I", ".SD", ":=", ".row_id", ".has_na",
  # Analysis variables
  "Country", "DOY", "DOYHd", "DOYHv", "HarvestDOY", "HarvestYEAR", "HeadingDOY",
  "Integral", "N", "PLZ", "Scenario", "Tgl", "alt", "altitude", "annual_mean_doy",
  "anomaly_days", "b", "baseline_doy", "baseline_spread", "bound", "colname",
  "dT", "dTgl", "d_mean", "daylength", "days_decade", "direction", "doy",
  "doy_ctrl", "doy_end", "doy_end_ctrl", "doy_end_scen", "doy_scen", "doy_start",
  "doy_start_ctrl", "doy_start_scen", "doy_synth", "elevation", "filter_type",
  "first_year", "geometry", "giss", "inside", "interpretation", "iqr_doy",
  "is_extreme", "label", "label_y", "last_year", "mad_doy", "mean_DOY", "mean_day",
  "mean_doy", "median_DOY", "median_day", "median_doy", "n_species", "n_stations",
  "observed_doy", "panel", "pct_outliers", "percentile", "phase", "pheno",
  "present", "q25", "q75", "qc_ori_flag", "r_squared", "s_ctrl", "s_scen",
  "sd_doy", "se", "series", "significant", "slope", "station_mean_doy",
  "subspecies", "tau", "thermal_sum", "tmp_end_ctrl", "tmp_end_scen",
  "tmp_start_ctrl", "tmp_start_scen", "total", "value", "var_value", "x", "y",
  "yearHd", "yearHv", "year_max", "year_min", "years_present", "z_score"
))

# =============================================================================
# Constructor
# =============================================================================

#' Create a PEP725 Phenological Data Object
#'
#' Constructor function that creates a validated \code{pep} object from a
#' data.frame or data.table. The object inherits from \code{data.table} and
#' retains all its functionality.
#'
#' @param x A data.frame or data.table containing PEP725 phenological data.
#' @param validate Logical. If \code{TRUE} (default), validates that required
#'   columns are present and have correct types.
#'
#' @return An object of class \code{c("pep", "data.table", "data.frame")}.
#'
#' @details
#' Required columns:
#' \itemize{
#'   \item \code{s_id} - Station ID
#'   \item \code{lon}, \code{lat} - Coordinates
#'   \item \code{genus}, \code{species} - Plant taxonomy
#'   \item \code{phase_id} - BBCH phenological phase code
#'   \item \code{year}, \code{day} - Observation timing
#' }
#'
#' @examples
#' \donttest{
#' # From imported data
#' dt <- pep_import("path/to/data")
#' pep_data <- new_pep(dt)
#'
#' # Validation will catch missing columns
#' bad_data <- data.table(x = 1:10)
#' new_pep(bad_data)  # Error: Missing required columns
#' }
#'
#' @author Matthias Templ
#' @export
new_pep <- function(x, validate = TRUE) {
  # Ensure it's a data.table


  if (!inherits(x, "data.table")) {
    x <- as.data.table(x)
  }

  if (validate) {
    validate_pep(x)
  }

  # Add class (prepend to preserve data.table inheritance)
  class(x) <- c("pep", class(x))

  x
}

#' Validate PEP725 Data Structure
#'
#' Internal function to validate that a data object has the required
#' structure for PEP725 phenological data.
#'
#' @param x A data.frame or data.table to validate.
#' @return Invisibly returns \code{TRUE} if valid; otherwise throws an error.
#' @keywords internal
validate_pep <- function(x) {
  required_cols <- c("s_id", "lon", "lat", "genus", "species", "phase_id", "year", "day")

  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns for pep object: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Type checks
  if (!is.numeric(x$lon) || !is.numeric(x$lat)) {
    stop("Columns 'lon' and 'lat' must be numeric", call. = FALSE)
  }

  if (!is.numeric(x$year) || !is.numeric(x$day)) {
    stop("Columns 'year' and 'day' must be numeric/integer", call. = FALSE)
  }

  if (!is.numeric(x$phase_id)) {
    stop("Column 'phase_id' must be numeric/integer (BBCH code)", call. = FALSE)
  }

  invisible(TRUE)
}

# =============================================================================
# Type checking and coercion
# =============================================================================

#' Test if Object is a PEP725 Data Object
#'
#' @param x An R object.
#' @return Logical; \code{TRUE} if \code{x} is of class \code{pep}.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' is.pep(pep)
#' is.pep(data.frame(x = 1))
#' }
#'
#' @author Matthias Templ
#' @export
is.pep <- function(x) {


  inherits(x, "pep")
}

#' Coerce to PEP725 Data Object
#'
#' @param x A data.frame or data.table to coerce.
#' @param ... Additional arguments (currently unused).
#' @return An object of class \code{pep}.
#'
#' @author Matthias Templ
#' @export
as.pep <- function(x, ...) {
  new_pep(x)
}

# =============================================================================
# Print method
# =============================================================================

#' Print Method for PEP725 Data
#'
#' Displays a concise summary of the PEP725 phenological dataset.
#'
#' @param x A \code{pep} object.
#' @param n Number of rows to display (default 5).
#' @param ... Additional arguments passed to print.
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' print(pep)
#' }
#'
#' @author Matthias Templ
#' @export
print.pep <- function(x, n = 5, ...) {
  # Header
  cat("PEP725 Phenological Data\n")
  cat(rep("-", 50), "\n", sep = "")

  # Key statistics
  n_obs <- format(nrow(x), big.mark = ",")
  n_stations <- format(length(unique(x$s_id)), big.mark = ",")
  n_species <- length(unique(paste(x$genus, x$species)))
  n_phases <- length(unique(x$phase_id))
  year_range <- range(x$year, na.rm = TRUE)

  cat(sprintf("Observations: %s\n", n_obs))
  cat(sprintf("Stations:     %s\n", n_stations))
  cat(sprintf("Species:      %d\n", n_species))
  phase_codes <- paste(sort(unique(x$phase_id))[1:min(5, n_phases)], collapse = ", ")
  if (n_phases > 5) phase_codes <- paste0(phase_codes, ", ...")
  cat(sprintf("Phases:       %d (BBCH codes: %s)\n", n_phases, phase_codes))
  cat(sprintf("Years:        %d - %d\n", year_range[1], year_range[2]))

  # Geographic extent
  lon_range <- range(x$lon, na.rm = TRUE)
  lat_range <- range(x$lat, na.rm = TRUE)
  cat(sprintf("Extent:       lon [%.2f, %.2f], lat [%.2f, %.2f]\n",
              lon_range[1], lon_range[2], lat_range[1], lat_range[2]))

  # Country info if available
  if ("country" %in% names(x)) {
    countries <- sort(unique(x$country))
    n_countries <- length(countries)
    cat(sprintf("Countries:    %d (%s%s)\n",
                n_countries,
                paste(head(countries, 5), collapse = ", "),
                if (n_countries > 5) ", ..." else ""))
  }

  cat(rep("-", 50), "\n", sep = "")

  # Show first few rows (using data.table print)
  cat(sprintf("First %d rows:\n", min(n, nrow(x))))
  print(as.data.table(x)[1:min(n, nrow(x)), .(s_id, lon, lat, genus, species, phase_id, year, day)])

  invisible(x)
}

# =============================================================================
# Summary method
# =============================================================================

#' Summary Method for PEP725 Data
#'
#' Provides a detailed phenological summary of the dataset, including
#' breakdowns by species, phase, country, and temporal coverage.
#'
#' @param object A \code{pep} object.
#' @param by Character. Summarize by \code{"species"}, \code{"phase"},
#'   \code{"country"}, or \code{"year"}. Default is \code{"species"}.
#' @param top Integer. Number of top entries to show (default 10).
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{pep_summary} object (list) containing summary tables,
#'   printed to console.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' summary(pep)
#' summary(pep, by = "phase")
#' summary(pep, by = "country")
#' }
#'
#' @author Matthias Templ
#' @export
summary.pep <- function(object, by = c("species", "phase", "country", "year"),
                        top = 10, ...) {
  by <- match.arg(by)
  x <- object

  cat("PEP725 Phenological Data Summary\n")
  cat(rep("=", 50), "\n\n", sep = "")

  # Overall statistics
  cat("OVERVIEW\n")
  cat(sprintf("  Total observations: %s\n", format(nrow(x), big.mark = ",")))
  cat(sprintf("  Unique stations:    %s\n", format(length(unique(x$s_id)), big.mark = ",")))
  cat(sprintf("  Year range:         %d - %d\n", min(x$year), max(x$year)))
  cat(sprintf("  DOY range:          %d - %d\n", min(x$day, na.rm = TRUE), max(x$day, na.rm = TRUE)))
  cat("\n")

  # Summary by selected dimension
  result <- switch(by,
    species = {
      cat(sprintf("TOP %d SPECIES (by observation count)\n", top))
      summary_dt <- x[, .(
        n_obs = .N,
        n_stations = uniqueN(s_id),
        year_min = min(year),
        year_max = max(year),
        median_doy = as.integer(median(day, na.rm = TRUE))
      ), by = .(genus, species)][order(-n_obs)][1:min(top, .N)]

      print(summary_dt)
      summary_dt
    },
    phase = {
      cat("OBSERVATIONS BY BBCH PHASE\n")
      # BBCH code descriptions
      bbch_names <- c(
        "10" = "Leaf development",
        "11" = "First leaf unfolded",
        "30" = "Stem elongation",
        "51" = "Inflorescence visible",
        "60" = "Flowering/Heading",
        "61" = "Beginning of flowering",
        "65" = "Full flowering/Anthesis",
        "69" = "End of flowering",
        "70" = "Fruit development",
        "80" = "Ripening",
        "85" = "Soft dough",
        "90" = "Maturity",
        "100" = "Harvest",
        "205" = "Autumn coloring >= 50%"
      )

      summary_dt <- x[, .(
        n_obs = .N,
        n_species = uniqueN(paste(genus, species)),
        median_doy = as.integer(median(day, na.rm = TRUE)),
        iqr_doy = as.integer(IQR(day, na.rm = TRUE))
      ), by = phase_id][order(phase_id)]

      summary_dt[, phase_name := bbch_names[as.character(phase_id)]]
      summary_dt[is.na(phase_name), phase_name := "Other"]

      print(summary_dt[, .(phase_id, phase_name, n_obs, n_species, median_doy, iqr_doy)])
      summary_dt
    },
    country = {
      if (!"country" %in% names(x)) {
        cat("  (No 'country' column - use add_country() first)\n")
        return(invisible(NULL))
      }
      cat(sprintf("TOP %d COUNTRIES (by observation count)\n", top))
      summary_dt <- x[, .(
        n_obs = .N,
        n_stations = uniqueN(s_id),
        n_species = uniqueN(paste(genus, species)),
        year_min = min(year),
        year_max = max(year)
      ), by = country][order(-n_obs)][1:min(top, .N)]

      print(summary_dt)
      summary_dt
    },
    year = {
      cat("TEMPORAL COVERAGE\n")
      summary_dt <- x[, .(
        n_obs = .N,
        n_stations = uniqueN(s_id),
        n_species = uniqueN(paste(genus, species))
      ), by = year][order(year)]

      # Show summary stats
      cat(sprintf("  Years with data:    %d\n", nrow(summary_dt)))
      cat(sprintf("  Median obs/year:    %s\n", format(median(summary_dt$n_obs), big.mark = ",")))
      cat(sprintf("  Peak year:          %d (%s obs)\n",
                  summary_dt[which.max(n_obs), year],
                  format(max(summary_dt$n_obs), big.mark = ",")))
      cat("\n  Last 10 years:\n")
      print(tail(summary_dt, 10))
      summary_dt
    }
  )

  # Return summary object invisibly
  out <- list(
    n_obs = nrow(x),
    n_stations = length(unique(x$s_id)),
    n_species = length(unique(paste(x$genus, x$species))),
    year_range = range(x$year),
    summary_table = result
  )
  class(out) <- "pep_summary"
  invisible(out)
}

# =============================================================================
# Plot method
# =============================================================================

#' Plot Method for PEP725 Data
#'
#' Provides default visualizations for PEP725 phenological data.
#'
#' @param x A \code{pep} object.
#' @param type Character. Type of plot: \code{"map"} for station locations,
#'   \code{"timeseries"} for temporal trends, \code{"histogram"} for DOY distribution.
#'   Default is \code{"map"}.
#' @param ... Additional arguments passed to the underlying plot function.
#'
#' @return A ggplot object (invisibly).
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' plot(pep)
#' plot(pep, type = "timeseries")
#' plot(pep, type = "histogram")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram geom_line
#' @importFrom ggplot2 labs theme_minimal coord_quickmap stat_summary
#' @author Matthias Templ
#' @export
plot.pep <- function(x, type = c("map", "timeseries", "histogram"), ...) {
  type <- match.arg(type)

  switch(type,
    map = {
      # Station map
      stations <- unique(x[, .(s_id, lon, lat)])
      p <- ggplot(stations, aes(x = lon, y = lat)) +
        geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
        coord_quickmap() +
        labs(
          title = "PEP725 Station Locations",
          subtitle = sprintf("%s stations", format(nrow(stations), big.mark = ",")),
          x = "Longitude", y = "Latitude"
        ) +
        theme_minimal()
    },
    timeseries = {
      # Annual observation counts and mean DOY
      annual <- x[, .(n_obs = .N, mean_doy = mean(day, na.rm = TRUE)), by = year]
      p <- ggplot(annual, aes(x = year, y = mean_doy)) +
        geom_line(color = "steelblue", linewidth = 0.8) +
        geom_point(aes(size = n_obs), alpha = 0.5, color = "steelblue") +
        labs(
          title = "Mean Phenological Day of Year Over Time",
          subtitle = "Point size = number of observations",
          x = "Year", y = "Mean DOY",
          size = "Observations"
        ) +
        theme_minimal()
    },
    histogram = {
      # DOY distribution
      p <- ggplot(x, aes(x = day)) +
        geom_histogram(bins = 52, fill = "steelblue", color = "white", alpha = 0.8) +
        labs(
          title = "Distribution of Phenological Events",
          subtitle = sprintf("%s observations", format(nrow(x), big.mark = ",")),
          x = "Day of Year (DOY)", y = "Count"
        ) +
        theme_minimal()
    }
  )

  print(p)
  invisible(p)
}

# =============================================================================
# Subsetting - preserve class
# =============================================================================

#' Subset PEP725 Data While Preserving Class
#'
#' Ensures that subsetting operations return a \code{pep} object
#' when the result still has the required structure.
#'
#' @param x A \code{pep} object.
#' @param ... Arguments passed to the data.table subset method.
#'
#' @return A \code{pep} object if structure is preserved, otherwise a data.table.
#'
#' @author Matthias Templ
#' @export
`[.pep` <- function(x, ...) {
  # Call the data.table method
  class(x) <- class(x)[-1]
  result <- NextMethod()

  # Try to preserve pep class if result is still valid
  if (is.data.frame(result) && nrow(result) > 0) {
    required_cols <- c("s_id", "lon", "lat", "genus", "species", "phase_id", "year", "day")
    if (all(required_cols %in% names(result))) {
      class(result) <- c("pep", class(result))
    }
  }

  result
}

# =============================================================================
# Helper: BBCH code lookup
# =============================================================================

#' Get BBCH Phase Description
#'
#' Returns human-readable descriptions for BBCH phenological codes.
#'
#' @param codes Integer vector of BBCH codes.
#' @param na.rm Logical. If \code{TRUE} (default), removes NA codes and codes
#'   without a known description from the output.
#' @param sort Logical. If \code{TRUE} (default), sorts the output by phase code.
#'
#' @return A data.frame with columns \code{phase_id} and \code{description},
#'   ordered by phase code (if \code{sort = TRUE}).
#'
#' @examples
#' bbch_description(c(60, 65, 100))
#' bbch_description(c(100, 60, 65, NA), na.rm = TRUE, sort = TRUE)
#'
#' @author Matthias Templ
#' @export
bbch_description <- function(codes, na.rm = TRUE, sort = TRUE) {
  bbch_lookup <- c(
    "0" = "Dry seed / Dormancy",
    "1" = "Seed imbibition",
    "5" = "Radicle emerged",
    "7" = "Coleoptile emerged",
    "9" = "Emergence",
    "10" = "First leaf through coleoptile",
    "11" = "First leaf unfolded",
    "12" = "2 leaves unfolded",
    "13" = "3 leaves unfolded",
    "19" = "9+ leaves unfolded",
    "21" = "Beginning of tillering",
    "29" = "End of tillering",
    "30" = "Beginning of stem elongation",
    "31" = "First node detectable",
    "37" = "Flag leaf just visible",
    "39" = "Flag leaf ligule visible",
    "41" = "Early boot",
    "45" = "Mid boot",
    "49" = "Late boot",
    "51" = "Beginning of heading",
    "55" = "Middle of heading",
    "59" = "End of heading",
    "60" = "Beginning of flowering / Heading complete",
    "61" = "Beginning of flowering",
    "65" = "Full flowering / Anthesis",
    "69" = "End of flowering",
    "70" = "Milk development",
    "71" = "Watery ripe",
    "73" = "Early milk",
    "75" = "Medium milk",
    "77" = "Late milk",
    "80" = "Dough development",
    "83" = "Early dough",
    "85" = "Soft dough",
    "87" = "Hard dough",
    "89" = "Fully ripe",
    "90" = "Over-ripe",
    "92" = "Over-ripe, grain hard",
    "97" = "Plant dead",
    "99" = "Harvested product",
    "100" = "Harvest",
    "205" = "Autumn coloring >= 50%"
  )

  # Remove NA codes if requested
  if (na.rm) {
    codes <- codes[!is.na(codes)]
  }

  # Get unique codes
  codes <- unique(codes)

  # Look up descriptions
  descriptions <- bbch_lookup[as.character(codes)]

  # Remove unknown phases if na.rm is TRUE

  if (na.rm) {
    known <- !is.na(descriptions)
    codes <- codes[known]
    descriptions <- descriptions[known]
  } else {
    descriptions[is.na(descriptions)] <- "Unknown phase"
  }

  # Create result data.frame
  result <- data.frame(
    phase_id = as.integer(codes),
    description = unname(descriptions),
    stringsAsFactors = FALSE
  )

  # Sort by phase_id if requested
  if (sort) {
    result <- result[order(result$phase_id), ]
    rownames(result) <- NULL
  }

  result
}

# =============================================================================
# Coverage function
# =============================================================================

#' Assess Data Coverage of PEP725 Phenological Data
#'
#' Provides comprehensive coverage statistics for phenological datasets,
#' including temporal, geographical, and species dimensions.
#'
#' @param x A \code{pep} object or data.frame with phenological data.
#' @param kind Character. Type of coverage to assess:
#'   \itemize{
#'     \item \code{"all"} (default): All coverage types combined
#'     \item \code{"temporal"}: Year range, gaps, observations per year
#'     \item \code{"geographical"}: Countries, stations, coordinate ranges
#'     \item \code{"species"}: Species and genus diversity
#'   }
#' @param top Integer. Number of top entries to show in tables (default: 10).
#' @param plot Logical. If \code{TRUE}, produces a visualization of the
#'   coverage. Default is \code{FALSE}.
#' @param by Character. Optional grouping variable for more detailed analysis
#'   (e.g., \code{"country"}, \code{"species"}). Only used for temporal coverage.
#'
#' @return A list of class \code{pep_coverage} containing coverage statistics.
#'   The structure depends on \code{kind}:
#'   \describe{
#'     \item{temporal}{Year range, number of years, gaps, observations per year}
#'     \item{geographical}{Countries, stations, coordinate bounds, altitude range}
#'     \item{species}{Genera, species, subspecies counts and details}
#'     \item{all}{All of the above combined}
#'   }
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Full coverage report
#' coverage(pep)
#'
#' # Temporal coverage only
#' coverage(pep, kind = "temporal")
#'
#' # Geographical coverage with plot
#' coverage(pep, kind = "geographical", plot = TRUE)
#'
#' # Species coverage, top 5
#' coverage(pep, kind = "species", top = 5)
#'
#' # Temporal coverage by country
#' coverage(pep, kind = "temporal", by = "country")
#' }
#'
#' @author Matthias Templ
#' @export
coverage <- function(x, kind = c("all", "temporal", "geographical", "species"),
                     top = 10, plot = FALSE, by = NULL) {

kind <- match.arg(kind)

  # Ensure data.table
  if (!inherits(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }

  result <- list()
  class(result) <- "pep_coverage"
  attr(result, "kind") <- kind

# ---------------------------------------------------------------------------
  # Temporal coverage
  # ---------------------------------------------------------------------------
  if (kind %in% c("all", "temporal")) {
    years <- sort(unique(x$year))
    year_range <- range(years)
    n_years <- length(years)

    # Find gaps (missing years in sequence)
    expected_years <- seq(min(years), max(years))
    missing_years <- setdiff(expected_years, years)

    # Observations per year
    obs_by_year <- x[, .(
      n_obs = .N,
      n_stations = uniqueN(s_id),
      n_species = uniqueN(paste(genus, species))
    ), by = year][order(year)]

    # By group if specified
    if (!is.null(by) && by %in% names(x)) {
      obs_by_year_group <- x[, .(
        n_obs = .N,
        n_stations = uniqueN(s_id),
        year_min = min(year),
        year_max = max(year),
        n_years = uniqueN(year)
      ), by = by][order(-n_obs)]
    } else {
      obs_by_year_group <- NULL
    }

    result$temporal <- list(
      year_range = year_range,
      n_years = n_years,
      missing_years = if (length(missing_years) > 0) missing_years else NULL,
      n_gaps = length(missing_years),
      obs_by_year = obs_by_year,
      obs_by_group = obs_by_year_group,
      median_obs_per_year = median(obs_by_year$n_obs),
      peak_year = obs_by_year[which.max(n_obs), year],
      peak_obs = max(obs_by_year$n_obs)
    )
  }

  # ---------------------------------------------------------------------------
  # Geographical coverage
  # ---------------------------------------------------------------------------
  if (kind %in% c("all", "geographical")) {
    # Countries
    if ("country" %in% names(x)) {
      countries <- x[, .(
        n_obs = .N,
        n_stations = uniqueN(s_id),
        n_species = uniqueN(paste(genus, species))
      ), by = country][order(-n_obs)]
    } else {
      countries <- NULL
    }

    # Stations
    stations <- x[, .(
      n_obs = .N,
      lon = lon[1],
      lat = lat[1],
      alt = if ("alt" %in% names(x)) alt[1] else NA_real_
    ), by = s_id]

    # Coordinate bounds
    lon_range <- range(x$lon, na.rm = TRUE)
    lat_range <- range(x$lat, na.rm = TRUE)

    # Altitude range
    if ("alt" %in% names(x)) {
      alt_range <- range(x$alt, na.rm = TRUE)
      alt_range[!is.finite(alt_range)] <- NA
    } else {
      alt_range <- c(NA, NA)
    }

    result$geographical <- list(
      n_countries = if (!is.null(countries)) nrow(countries) else NA,
      countries = if (!is.null(countries)) countries[1:min(top, nrow(countries))] else NULL,
      n_stations = nrow(stations),
      lon_range = lon_range,
      lat_range = lat_range,
      alt_range = alt_range,
      stations_sample = stations[sample(.N, min(top, .N))]
    )
  }

  # ---------------------------------------------------------------------------
  # Species coverage
  # ---------------------------------------------------------------------------
  if (kind %in% c("all", "species")) {
    # Genera
    genera <- x[, .(
      n_obs = .N,
      n_species = uniqueN(species),
      n_stations = uniqueN(s_id)
    ), by = genus][order(-n_obs)]

    # Species
    species_tbl <- x[, .(
      n_obs = .N,
      n_stations = uniqueN(s_id),
      year_min = min(year),
      year_max = max(year)
    ), by = .(genus, species)][order(-n_obs)]

    # Subspecies (if available)
    if ("subspecies" %in% names(x)) {
      subspecies_tbl <- x[!is.na(subspecies) & subspecies != "", .(
        n_obs = .N,
        n_stations = uniqueN(s_id)
      ), by = .(genus, species, subspecies)][order(-n_obs)]
    } else {
      subspecies_tbl <- NULL
    }

    result$species <- list(
      n_genera = nrow(genera),
      n_species = nrow(species_tbl),
      n_subspecies = if (!is.null(subspecies_tbl)) nrow(subspecies_tbl) else 0,
      genera = genera[1:min(top, nrow(genera))],
      species = species_tbl[1:min(top, nrow(species_tbl))],
      subspecies = if (!is.null(subspecies_tbl) && nrow(subspecies_tbl) > 0)
        subspecies_tbl[1:min(top, nrow(subspecies_tbl))] else NULL
    )
  }

  # ---------------------------------------------------------------------------
  # Plot if requested
  # ---------------------------------------------------------------------------
  if (plot) {
    p <- plot.pep_coverage(result)
    print(p)
  }

  result
}


#' Print Method for PEP Coverage
#'
#' @param x A \code{pep_coverage} object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#' @author Matthias Templ
#' @export
print.pep_coverage <- function(x, ...) {
  kind <- attr(x, "kind")

  cat("PEP725 Data Coverage Report\n")
  cat(rep("=", 50), "\n\n", sep = "")

  # Temporal
  if (!is.null(x$temporal)) {
    cat("TEMPORAL COVERAGE\n")
    cat(rep("-", 30), "\n", sep = "")
    cat(sprintf("  Year range:      %d - %d (%d years)\n",
                x$temporal$year_range[1], x$temporal$year_range[2],
                x$temporal$n_years))
    cat(sprintf("  Missing years:   %d gaps\n", x$temporal$n_gaps))
    if (x$temporal$n_gaps > 0 && x$temporal$n_gaps <= 10) {
      cat(sprintf("                   (%s)\n",
                  paste(x$temporal$missing_years, collapse = ", ")))
    }
    cat(sprintf("  Median obs/year: %s\n",
                format(x$temporal$median_obs_per_year, big.mark = ",")))
    cat(sprintf("  Peak year:       %d (%s obs)\n",
                x$temporal$peak_year,
                format(x$temporal$peak_obs, big.mark = ",")))
    cat("\n")

    if (!is.null(x$temporal$obs_by_group)) {
      cat("  Coverage by group:\n")
      print(x$temporal$obs_by_group[1:min(5, nrow(x$temporal$obs_by_group))])
      cat("\n")
    }
  }

  # Geographical
  if (!is.null(x$geographical)) {
    cat("GEOGRAPHICAL COVERAGE\n")
    cat(rep("-", 30), "\n", sep = "")
    if (!is.na(x$geographical$n_countries)) {
      cat(sprintf("  Countries:       %d\n", x$geographical$n_countries))
    }
    cat(sprintf("  Stations:        %s\n",
                format(x$geographical$n_stations, big.mark = ",")))
    cat(sprintf("  Longitude:       %.2f to %.2f\n",
                x$geographical$lon_range[1], x$geographical$lon_range[2]))
    cat(sprintf("  Latitude:        %.2f to %.2f\n",
                x$geographical$lat_range[1], x$geographical$lat_range[2]))
    if (!all(is.na(x$geographical$alt_range))) {
      cat(sprintf("  Altitude:        %d to %d m\n",
                  as.integer(x$geographical$alt_range[1]),
                  as.integer(x$geographical$alt_range[2])))
    }
    cat("\n")

    if (!is.null(x$geographical$countries)) {
      cat("  Top countries:\n")
      print(x$geographical$countries)
      cat("\n")
    }
  }

  # Species
  if (!is.null(x$species)) {
    cat("SPECIES COVERAGE\n")
    cat(rep("-", 30), "\n", sep = "")
    cat(sprintf("  Genera:          %d\n", x$species$n_genera))
    cat(sprintf("  Species:         %d\n", x$species$n_species))
    if (x$species$n_subspecies > 0) {
      cat(sprintf("  Subspecies:      %d\n", x$species$n_subspecies))
    }
    cat("\n  Top genera:\n")
    print(x$species$genera)
    cat("\n  Top species:\n")
    print(x$species$species)
    cat("\n")
  }

  invisible(x)
}


#' Plot Method for PEP Coverage
#'
#' @param x A \code{pep_coverage} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object (or list of ggplot objects).
#' @author Matthias Templ
#' @export
plot.pep_coverage <- function(x, ...) {
  kind <- attr(x, "kind")
  plots <- list()

  # Temporal plot
  if (!is.null(x$temporal)) {
    p_temporal <- ggplot2::ggplot(x$temporal$obs_by_year,
                                   ggplot2::aes(x = year, y = n_obs)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(color = "steelblue", size = 1) +
      ggplot2::labs(title = "Temporal Coverage",
                    x = "Year", y = "Number of Observations") +
      ggplot2::theme_minimal()
    plots$temporal <- p_temporal
  }

  # Geographical plot (simple: stations by country)
  if (!is.null(x$geographical) && !is.null(x$geographical$countries)) {
    countries_top <- x$geographical$countries[1:min(10, nrow(x$geographical$countries))]
    countries_top$country <- factor(countries_top$country,
                                     levels = rev(countries_top$country))
    p_geo <- ggplot2::ggplot(countries_top,
                              ggplot2::aes(x = n_stations, y = country)) +
      ggplot2::geom_col(fill = "forestgreen", alpha = 0.7) +
      ggplot2::labs(title = "Geographical Coverage",
                    x = "Number of Stations", y = "") +
      ggplot2::theme_minimal()
    plots$geographical <- p_geo
  }

  # Species plot
  if (!is.null(x$species)) {
    species_top <- x$species$species[1:min(10, nrow(x$species$species))]
    species_top$label <- paste(species_top$genus, species_top$species)
    species_top$label <- factor(species_top$label, levels = rev(species_top$label))
    p_species <- ggplot2::ggplot(species_top,
                                  ggplot2::aes(x = n_obs, y = label)) +
      ggplot2::geom_col(fill = "coral", alpha = 0.7) +
      ggplot2::labs(title = "Species Coverage",
                    x = "Number of Observations", y = "") +
      ggplot2::theme_minimal()
    plots$species <- p_species
  }

  # Combine plots if multiple
  if (length(plots) > 1) {
    combined <- patchwork::wrap_plots(plots, ncol = 1)
    return(combined)
  } else if (length(plots) == 1) {
    return(plots[[1]])
  }

  invisible(NULL)
}
