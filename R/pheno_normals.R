#' Calculate Phenological Normals (Climatology)
#'
#' Computes reference "normal" phenology for a specified period, analogous to
#' WMO climate normals. Returns central tendency, spread, and percentile
#' statistics for phenological day-of-year (DOY) values.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and grouping variables.
#' @param period Integer vector specifying the years to include in the normal
#'   calculation. Default is \code{1991:2020} (current WMO standard period).
#' @param by Character vector of column names to group by. Common choices:
#'   \itemize{
#'     \item \code{c("country", "genus", "phase_id")} - Regional normals by genus/phase
#'     \item \code{c("s_id", "species", "phase_id")} - Station-level normals
#'     \item \code{c("genus", "phase_id")} - Overall normals ignoring geography
#'   }
#'   Default is \code{c("country", "genus", "phase_id")}.
#' @param species Optional character string to filter by species column.
#'   Can be genus name (e.g., "Triticum") or full species (e.g., "Triticum aestivum").
#'   If \code{NULL} (default), all species in the data are included.
#' @param phase_id Optional integer vector to filter by BBCH phase codes.
#'   If \code{NULL} (default
#' ), all phases in the data are included.
#' @param min_years Minimum number of years required to calculate valid normals.
#'   Default is 20 (WMO standard). Groups with fewer years return \code{NA}.
#' @param probs Numeric vector of probabilities for percentile calculation.
#'   Default is \code{c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95)}.
#' @param na.rm Logical. Should missing values be removed? Default \code{TRUE}.
#'
#' @return A \code{data.table} with the following columns:
#'   \describe{
#'     \item{<by variables>}{Grouping variables as specified}
#'     \item{n_years}{Number of years with data in the period}
#'     \item{n_obs}{Total number of observations}
#'     \item{mean_doy}{Arithmetic mean DOY}
#'     \item{median_doy}{Median DOY (more robust to outliers)}
#'     \item{sd_doy}{Standard deviation}
#'     \item{iqr_doy}{Interquartile range}
#'     \item{mad_doy}{Median absolute deviation (robust spread)}
#'     \item{q05, q10, q25, q75, q90, q95}{Percentiles (or as specified in \code{probs})}
#'     \item{period}{Character string describing the reference period}
#'   }
#'
#' @details
#' Phenological normals provide a baseline for comparing individual years or
#' detecting trends. This function calculates both classical statistics (mean, SD)
#' and robust alternatives (median, MAD, IQR) that are less sensitive to outliers.
#'
#' The function can be used in two ways:
#' \enumerate{
#'   \item \strong{Full dataset}: Pass the complete \code{pep} object and use
#'     \code{species} and \code{phase_id} parameters to filter internally.
#'   \item \strong{Pre-filtered subset}: Filter the data first using data.table
#'     syntax, then pass the subset to the function.
#' }
#'
#' @section Standard Reference Periods:
#' \itemize{
#'   \item \strong{1961-1990}: Historical reference (pre-acceleration of warming)
#'   \item \strong{1991-2020}: Current WMO standard normal period
#' }
#'
#' @examples
#' \dontrun{
#' # Download synthetic data first
#' pep <- pep_download()
#'
#' # Calculate normals for all species and phases by country
#' normals_all <- pheno_normals(pep)
#'
#' # Normals for wheat (Triticum) only, heading phase (60)
#' wheat_normals <- pheno_normals(pep,
#'                                species = "Triticum",
#'                                phase_id = 60)
#'
#' # Using a pre-filtered subset
#' wheat <- pep[genus == "Triticum" & phase_id %in% c(60, 100)]
#' wheat_normals <- pheno_normals(wheat,
#'                                by = c("country", "phase_id"))
#'
#' # Station-level normals for detailed analysis
#' station_normals <- pheno_normals(pep,
#'                                  species = "Triticum",
#'                                  phase_id = 60,
#'                                  by = c("s_id", "country"))
#'
#' # Historical reference period
#' historical <- pheno_normals(pep,
#'                             period = 1961:1990,
#'                             species = "Triticum")
#'
#' # Compare two periods
#' period1 <- pheno_normals(pep, period = 1961:1990, species = "Triticum")
#' period2 <- pheno_normals(pep, period = 1991:2020, species = "Triticum")
#' shift <- period2$mean_doy - period1$mean_doy
#' }
#'
#' @seealso
#' \code{\link{pheno_anomaly}} for calculating anomalies relative to normals,
#' \code{\link{pep_download}} for obtaining the main dataset
#'
#' @author Matthias Templ
#' @export
pheno_normals <- function(pep,
                          period = 1991:2020,
                          by = c("country", "genus", "phase_id"),
                          species = NULL,
                          phase_id = NULL,
                          min_years = 20,
                          probs = c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95),
                          na.rm = TRUE) {

 # Input validation
 if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- as.data.table(pep)
  }

  # Check required columns
  required_cols <- c("year", "day")
  missing_cols <- setdiff(required_cols, names(pep))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

 # Check that 'by' columns exist
  missing_by <- setdiff(by, names(pep))
  if (length(missing_by) > 0) {
    stop("Column(s) specified in 'by' not found: ",
         paste(missing_by, collapse = ", "),
         call. = FALSE)
  }

  # Validate period
  if (!is.numeric(period) || length(period) < 2) {
    stop("'period' must be a numeric vector with at least 2 years", call. = FALSE)
  }

  # Validate probs
  if (!is.numeric(probs) || any(probs < 0) || any(probs > 1)) {
    stop("'probs' must be numeric values between 0 and 1", call. = FALSE)
  }

 # Make a copy to avoid modifying original
  dt <- copy(pep)

  # Apply species filter if specified
  if (!is.null(species)) {
    species_filter <- species  # Avoid name collision with column

    # Check which column to filter on (genus or species)
    if ("genus" %in% names(dt)) {
      # Try matching against genus first, then species
      # Convert to character to handle factors
      genus_match <- as.character(dt$genus) == species_filter
      species_match <- if ("species" %in% names(dt)) {
        as.character(dt$species) == species_filter
      } else {
        FALSE
      }

      dt <- dt[genus_match | species_match]

      if (nrow(dt) == 0) {
        stop("No observations found for species '", species_filter, "'", call. = FALSE)
      }
    } else if ("species" %in% names(dt)) {
      dt <- dt[as.character(dt$species) == species_filter]
      if (nrow(dt) == 0) {
        stop("No observations found for species '", species_filter, "'", call. = FALSE)
      }
    } else {
      warning("Neither 'genus' nor 'species' column found. Ignoring species filter.",
              call. = FALSE)
    }
  }

  # Apply phase_id filter if specified
  if (!is.null(phase_id)) {
    if (!"phase_id" %in% names(dt)) {
      warning("'phase_id' column not found. Ignoring phase_id filter.", call. = FALSE)
    } else {
      phase_filter <- phase_id  # Avoid name collision with column
      dt <- dt[dt$phase_id %in% phase_filter]
      if (nrow(dt) == 0) {
        stop("No observations found for phase_id: ",
             paste(phase_id, collapse = ", "), call. = FALSE)
      }
    }
  }

  # Filter to specified period
  dt <- dt[year %in% period]

  if (nrow(dt) == 0) {
    stop("No observations found in the specified period: ",
         min(period), "-", max(period), call. = FALSE)
  }

  # Create period label
  period_label <- paste0(min(period), "-", max(period))

  # Calculate normals by group
  # First aggregate to annual means per group to avoid double-counting
  # multiple observations in the same year
  annual_means <- dt[, .(
    annual_mean_doy = mean(day, na.rm = na.rm)
  ), by = c(by, "year")]

  # Now calculate statistics across years
  result <- annual_means[, {
    doy_values <- annual_mean_doy
    n_yrs <- sum(!is.na(doy_values))

    if (n_yrs >= min_years) {
      # Calculate percentiles
      quantiles <- quantile(doy_values, probs = probs, na.rm = na.rm)

      list(
        n_years = n_yrs,
        n_obs = .N,
        mean_doy = mean(doy_values, na.rm = na.rm),
        median_doy = median(doy_values, na.rm = na.rm),
        sd_doy = sd(doy_values, na.rm = na.rm),
        iqr_doy = IQR(doy_values, na.rm = na.rm),
        mad_doy = mad(doy_values, na.rm = na.rm),
        q05 = quantiles[1],
        q10 = quantiles[2],
        q25 = quantiles[3],
        q75 = quantiles[4],
        q90 = quantiles[5],
        q95 = quantiles[6],
        period = period_label
      )
    } else {
      # Return NA for groups with insufficient data
      list(
        n_years = n_yrs,
        n_obs = .N,
        mean_doy = NA_real_,
        median_doy = NA_real_,
        sd_doy = NA_real_,
        iqr_doy = NA_real_,
        mad_doy = NA_real_,
        q05 = NA_real_,
        q10 = NA_real_,
        q25 = NA_real_,
        q75 = NA_real_,
        q90 = NA_real_,
        q95 = NA_real_,
        period = period_label
      )
    }
  }, by = by]

  # Add informative message about groups with insufficient data
  n_insufficient <- sum(result$n_years < min_years)
  if (n_insufficient > 0) {
    message(sprintf(
      "Note: %d group(s) have fewer than %d years of data and return NA values.",
      n_insufficient, min_years
    ))
  }

  # Order by grouping variables
  setorderv(result, by)

  # Add class for potential method dispatch
  class(result) <- c("pheno_normals", class(result))

  # Store parameters as attributes
  attr(result, "period") <- period
  attr(result, "min_years") <- min_years
  attr(result, "species_filter") <- species
  attr(result, "phase_filter") <- phase_id

  result
}


#' Print Method for Phenological Normals
#'
#' @param x A \code{pheno_normals} object
#' @param n Number of rows to display
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pheno_normals <- function(x, n = 10, ...) {
  period <- attr(x, "period")
  min_years <- attr(x, "min_years")
  species_filter <- attr(x, "species_filter")
  phase_filter <- attr(x, "phase_filter")

  cat("Phenological Normals\n")
  cat(strrep("-", 50), "\n")
  cat(sprintf("Reference period: %d-%d\n", min(period), max(period)))
  cat(sprintf("Minimum years required: %d\n", min_years))

  if (!is.null(species_filter)) {
    cat(sprintf("Species filter: %s\n", species_filter))
  }
  if (!is.null(phase_filter)) {
    cat(sprintf("Phase filter: %s\n", paste(phase_filter, collapse = ", ")))
  }

  cat(sprintf("Groups with valid normals: %d / %d\n",
              sum(!is.na(x$mean_doy)), nrow(x)))
  cat(strrep("-", 50), "\n\n")

  # Convert to data.table for printing

  dt <- as.data.table(x)

  # Identify grouping columns (non-statistic columns)
  stat_cols <- c("n_years", "n_obs", "mean_doy", "median_doy", "sd_doy",
                 "iqr_doy", "mad_doy", "q05", "q10", "q25", "q75", "q90",
                 "q95", "period")
  group_cols <- setdiff(names(dt), stat_cols)

  # Sort so that rows with NA in grouping columns appear last
  if (length(group_cols) > 0) {
    # Create indicator: TRUE if any grouping column has NA
    dt[, .has_na := Reduce(`|`, lapply(.SD, is.na)), .SDcols = group_cols]
    setorderv(dt, c(".has_na", group_cols), na.last = TRUE)
    dt[, .has_na := NULL]
  }

  # Print first n rows
  print(dt[1:min(n, nrow(dt))])

  if (nrow(x) > n) {
    cat(sprintf("\n... and %d more rows\n", nrow(x) - n))
  }

  invisible(x)
}


#' Summary Method for Phenological Normals
#'
#' @param object A \code{pheno_normals} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @author Matthias Templ
#' @export
summary.pheno_normals <- function(object, ...) {
  period <- attr(object, "period")

  cat("Phenological Normals Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat(sprintf("Reference period: %d-%d\n", min(period), max(period)))
  cat(sprintf("Total groups: %d\n", nrow(object)))
  cat(sprintf("Groups with valid normals: %d (%.1f%%)\n",
              sum(!is.na(object$mean_doy)),
              100 * sum(!is.na(object$mean_doy)) / nrow(object)))
  cat("\n")

  # Summary of DOY statistics (for valid normals only)
  valid <- object[!is.na(mean_doy)]

  if (nrow(valid) > 0) {
    cat("DOY Statistics (across all groups with valid normals):\n")
    cat(sprintf("  Mean DOY range: %.0f - %.0f\n",
                min(valid$mean_doy), max(valid$mean_doy)))
    cat(sprintf("  Median DOY range: %.0f - %.0f\n",
                min(valid$median_doy), max(valid$median_doy)))
    cat(sprintf("  Typical SD: %.1f days\n",
                median(valid$sd_doy, na.rm = TRUE)))
    cat(sprintf("  Typical IQR: %.1f days\n",
                median(valid$iqr_doy, na.rm = TRUE)))
  }

  invisible(list(
    n_groups = nrow(object),
    n_valid = sum(!is.na(object$mean_doy)),
    period = period
  ))
}
