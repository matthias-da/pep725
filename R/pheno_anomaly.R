#' Calculate Phenological Anomalies
#'
#' Computes deviations from long-term phenological baselines to identify
#' extreme years and detect climate signals. Returns both raw anomalies
#' (in days) and standardized anomalies for cross-species comparison.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and grouping variables.
#' @param baseline_period Integer vector specifying the years to use for
#'   baseline calculation. Default is \code{1961:1990} (pre-acceleration
#'   warming reference period).
#' @param target_years Integer vector specifying years to calculate anomalies
#'   for. If \code{NULL} (default), calculates for all years in the data.
#' @param by Character vector of column names to group by. Must match columns
#'   in the data. Default is \code{c("country", "genus", "phase_id")}.
#' @param species Optional character string to filter by species/genus.
#'   If \code{NULL} (default), all species are included.
#' @param phase_id Optional integer vector to filter by BBCH phase codes.
#'   If \code{NULL} (default), all phases are included.
#' @param robust Logical. If \code{TRUE} (default), uses median and MAD for
#'   baseline and standardization. If \code{FALSE}, uses mean and SD.
#' @param min_years Minimum number of years required in the baseline period
#'   to calculate valid anomalies. Default is 15.
#' @param extreme_threshold Numeric. Z-score threshold for flagging extreme
#'   events. Default is 2 (approximately 95th percentile for normal distribution).
#' @param normals Optional pre-computed \code{pheno_normals} object. If provided,
#'   baseline statistics are taken from this object instead of being computed.
#' @param na.rm Logical. Should missing values be removed? Default \code{TRUE}.
#'
#' @return A \code{data.table} with the following columns:
#'   \describe{
#'     \item{<by variables>}{Grouping variables as specified}
#'     \item{year}{Year of observation}
#'     \item{observed_doy}{Observed mean day-of-year for the group/year}
#'     \item{baseline_doy}{Long-term baseline (median if robust, mean otherwise)}
#'     \item{baseline_spread}{Baseline spread (MAD if robust, SD otherwise)}
#'     \item{anomaly_days}{Deviation in days (negative = early, positive = late)}
#'     \item{z_score}{Standardized anomaly (anomaly / spread)}
#'     \item{percentile}{Percentile rank relative to baseline distribution}
#'     \item{is_extreme}{Logical flag for extreme events (|z_score| > threshold)}
#'     \item{direction}{Character: "early", "late", or "normal"}
#'   }
#'
#' @details
#' Phenological anomalies quantify how much a given year deviates from
#' long-term expectations. This is crucial for:
#' \itemize{
#'   \item Detecting climate change signals in phenology
#'   \item Identifying extreme phenological years
#'   \item Comparing anomalies across different species/regions
#' }
#'
#' The function supports two approaches:
#' \itemize{
#'   \item \strong{Robust} (default): Uses median for central tendency and MAD
#'     (Median Absolute Deviation) for spread. More resistant to outliers.
#'   \item \strong{Classical}: Uses mean and standard deviation. More sensitive
#'     to extreme values but provides interpretable z-scores under normality.
#' }
#'
#' @section Interpretation:
#' \itemize{
#'   \item \strong{anomaly_days}: Direct interpretation - "X days earlier/later
#'     than normal"
#'   \item \strong{z_score}: Standardized - values > 2 or < -2 are typically
#'     considered extreme
#'   \item \strong{percentile}: Non-parametric - e.g., 5th percentile means
#'     "earlier than 95% of baseline years"
#' }
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' # Calculate anomalies relative to 1961-1990 baseline
#' anomalies <- pheno_anomaly(pep,
#'                            baseline_period = 1961:1990,
#'                            species = "Triticum",
#'                            phase_id = 60)
#'
#' # Find extreme early years
#' extreme_early <- anomalies[is_extreme == TRUE & direction == "early"]
#'
#' # Using pre-computed normals
#' normals <- pheno_normals(pep, period = 1961:1990, species = "Triticum")
#' anomalies <- pheno_anomaly(pep, species = "Triticum", normals = normals)
#'
#' # Classical (mean/SD) approach
#' anomalies_classical <- pheno_anomaly(pep,
#'                                      species = "Triticum",
#'                                      robust = FALSE)
#'
#' # Anomalies for specific target years
#' recent <- pheno_anomaly(pep,
#'                         baseline_period = 1961:1990,
#'                         target_years = 2010:2020,
#'                         species = "Triticum")
#' }
#'
#' @seealso
#' \code{\link{pheno_normals}} for calculating baseline statistics,
#' \code{\link{pep_download}} for obtaining the main dataset
#'
#' @author Matthias Templ
#' @export
pheno_anomaly <- function(pep,
                          baseline_period = 1961:1990,
                          target_years = NULL,
                          by = c("country", "genus", "phase_id"),
                          species = NULL,
                          phase_id = NULL,
                          robust = TRUE,
                          min_years = 15,
                          extreme_threshold = 2,
                          normals = NULL,
                          na.rm = TRUE) {

  # Input validation
 if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
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

  # Validate parameters
  if (!is.numeric(baseline_period) || length(baseline_period) < 2) {
    stop("'baseline_period' must be a numeric vector with at least 2 years",
         call. = FALSE)
  }

  if (!is.null(target_years) && !is.numeric(target_years)) {
    stop("'target_years' must be NULL or a numeric vector", call. = FALSE)
  }

  if (!is.numeric(extreme_threshold) || extreme_threshold <= 0) {
    stop("'extreme_threshold' must be a positive number", call. = FALSE)
  }

  # Make a copy to avoid modifying original
  dt <- data.table::copy(pep)

  # Apply species filter if specified
  if (!is.null(species)) {
    species_filter <- species  # Avoid name collision with column

    if ("genus" %in% names(dt)) {
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
      phase_filter <- phase_id
      dt <- dt[dt$phase_id %in% phase_filter]
      if (nrow(dt) == 0) {
        stop("No observations found for phase_id: ",
             paste(phase_id, collapse = ", "), call. = FALSE)
      }
    }
  }

  # Calculate or extract baseline statistics
  if (!is.null(normals)) {
    # Use provided normals
    if (!inherits(normals, "pheno_normals")) {
      warning("'normals' should be a pheno_normals object. Attempting to use anyway.",
              call. = FALSE)
    }

    # Extract baseline period from normals if available
    normals_period <- attr(normals, "period")
    if (!is.null(normals_period)) {
      baseline_period <- normals_period
    }

    baseline_stats <- data.table::as.data.table(normals)

    # Check that normals have compatible grouping
    normals_by <- intersect(by, names(baseline_stats))
    if (length(normals_by) < length(by)) {
      warning("Some grouping variables not found in normals: ",
              paste(setdiff(by, normals_by), collapse = ", "),
              call. = FALSE)
    }

    # Select appropriate baseline columns
    if (robust) {
      baseline_stats[, baseline_doy := median_doy]
      baseline_stats[, baseline_spread := mad_doy]
    } else {
      baseline_stats[, baseline_doy := mean_doy]
      baseline_stats[, baseline_spread := sd_doy]
    }

  } else {
    # Calculate baseline from data
    baseline_data <- dt[year %in% baseline_period]

    if (nrow(baseline_data) == 0) {
      stop("No observations found in the baseline period: ",
           min(baseline_period), "-", max(baseline_period), call. = FALSE)
    }

    # First aggregate to annual means per group
    annual_means <- baseline_data[, .(
      annual_mean_doy = mean(day, na.rm = na.rm)
    ), by = c(by, "year")]

    # Calculate baseline statistics
    baseline_stats <- annual_means[, {
      doy_values <- annual_mean_doy
      n_yrs <- sum(!is.na(doy_values))

      if (n_yrs >= min_years) {
        if (robust) {
          list(
            n_baseline_years = n_yrs,
            baseline_doy = median(doy_values, na.rm = na.rm),
            baseline_spread = mad(doy_values, na.rm = na.rm),
            baseline_q05 = quantile(doy_values, 0.05, na.rm = na.rm),
            baseline_q95 = quantile(doy_values, 0.95, na.rm = na.rm)
          )
        } else {
          list(
            n_baseline_years = n_yrs,
            baseline_doy = mean(doy_values, na.rm = na.rm),
            baseline_spread = sd(doy_values, na.rm = na.rm),
            baseline_q05 = quantile(doy_values, 0.05, na.rm = na.rm),
            baseline_q95 = quantile(doy_values, 0.95, na.rm = na.rm)
          )
        }
      } else {
        list(
          n_baseline_years = n_yrs,
          baseline_doy = NA_real_,
          baseline_spread = NA_real_,
          baseline_q05 = NA_real_,
          baseline_q95 = NA_real_
        )
      }
    }, by = by]
  }

  # Filter target years if specified
  if (!is.null(target_years)) {
    dt <- dt[year %in% target_years]
    if (nrow(dt) == 0) {
      stop("No observations found for target years: ",
           paste(range(target_years), collapse = "-"), call. = FALSE)
    }
  }

  # Calculate annual observed DOY per group
  observed_annual <- dt[, .(
    observed_doy = mean(day, na.rm = na.rm),
    n_obs = .N
  ), by = c(by, "year")]

  # Merge with baseline statistics
  result <- merge(observed_annual, baseline_stats, by = by, all.x = TRUE)

  # Calculate anomalies
  result[, anomaly_days := observed_doy - baseline_doy]

  # Calculate z-score (handle zero spread)
  result[, z_score := ifelse(
    is.na(baseline_spread) | baseline_spread == 0,
    NA_real_,
    anomaly_days / baseline_spread
  )]

  # Calculate percentile rank from z-score (assumes approximately normal distribution)
  # This is a reasonable approximation for phenological data
  result[, percentile := ifelse(
    is.na(z_score),
    NA_real_,
    100 * pnorm(z_score)
  )]

  # Flag extreme events
  result[, is_extreme := abs(z_score) > extreme_threshold]

  # Add direction
  result[, direction := ifelse(
    is.na(anomaly_days),
    NA_character_,
    ifelse(anomaly_days < -extreme_threshold * baseline_spread, "early",
           ifelse(anomaly_days > extreme_threshold * baseline_spread, "late", "normal"))
  )]

  # Reorder columns for readability
  col_order <- c(by, "year", "observed_doy", "baseline_doy", "baseline_spread",
                 "anomaly_days", "z_score", "percentile", "is_extreme", "direction",
                 "n_obs")

  # Only include columns that exist
  col_order <- intersect(col_order, names(result))
  if ("n_baseline_years" %in% names(result)) {
    col_order <- c(col_order, "n_baseline_years")
  }

  data.table::setcolorder(result, col_order)

  # Order by grouping variables and year
  data.table::setorderv(result, c(by, "year"))

  # Add informative message about groups with insufficient data
  n_insufficient <- sum(is.na(result$baseline_doy))
  if (n_insufficient > 0) {
    message(sprintf(
      "Note: %d observation(s) have insufficient baseline data and return NA anomalies.",
      n_insufficient
    ))
  }

  # Add class for potential method dispatch
  class(result) <- c("pheno_anomaly", class(result))

  # Store parameters as attributes
  attr(result, "baseline_period") <- baseline_period
  attr(result, "target_years") <- target_years
  attr(result, "robust") <- robust
  attr(result, "extreme_threshold") <- extreme_threshold
  attr(result, "species_filter") <- species
  attr(result, "phase_filter") <- phase_id

  result
}


#' Print Method for Phenological Anomalies
#'
#' @param x A \code{pheno_anomaly} object
#' @param n Number of rows to display
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pheno_anomaly <- function(x, n = 10, ...) {
  baseline <- attr(x, "baseline_period")
  robust <- attr(x, "robust")
  threshold <- attr(x, "extreme_threshold")
  species_filter <- attr(x, "species_filter")
  phase_filter <- attr(x, "phase_filter")

  cat("Phenological Anomalies\n")
  cat(strrep("-", 50), "\n")

  if (!is.null(baseline) && length(baseline) > 0) {
    cat(sprintf("Baseline period: %d-%d\n", min(baseline), max(baseline)))
  } else {
    cat("Baseline period: (from pre-computed normals)\n")
  }

  method_str <- if (is.null(robust)) "Unknown" else ifelse(robust, "Robust (median/MAD)", "Classical (mean/SD)")
  cat(sprintf("Method: %s\n", method_str))

  if (!is.null(threshold)) {
    cat(sprintf("Extreme threshold: |z| > %.1f\n", threshold))
  }

  if (!is.null(species_filter)) {
    cat(sprintf("Species filter: %s\n", species_filter))
  }
  if (!is.null(phase_filter)) {
    cat(sprintf("Phase filter: %s\n", paste(phase_filter, collapse = ", ")))
  }

  # Summary statistics
  valid <- x[!is.na(anomaly_days)]
  if (nrow(valid) > 0) {
    n_extreme <- sum(valid$is_extreme, na.rm = TRUE)
    n_early <- sum(valid$direction == "early", na.rm = TRUE)
    n_late <- sum(valid$direction == "late", na.rm = TRUE)

    cat(sprintf("\nTotal observations: %d (valid: %d)\n", nrow(x), nrow(valid)))
    cat(sprintf("Extreme events: %d (%.1f%%) - %d early, %d late\n",
                n_extreme, 100 * n_extreme / nrow(valid), n_early, n_late))
    cat(sprintf("Anomaly range: %.1f to %.1f days\n",
                min(valid$anomaly_days), max(valid$anomaly_days)))
  }

  cat(strrep("-", 50), "\n\n")

  # Print as data.table
  print(data.table::as.data.table(x)[1:min(n, nrow(x))])

  if (nrow(x) > n) {
    cat(sprintf("\n... and %d more rows\n", nrow(x) - n))
  }

  invisible(x)
}


#' Summary Method for Phenological Anomalies
#'
#' @param object A \code{pheno_anomaly} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @author Matthias Templ
#' @export
summary.pheno_anomaly <- function(object, ...) {
  baseline <- attr(object, "baseline_period")

  cat("Phenological Anomalies Summary\n")
  cat(strrep("=", 50), "\n\n")

  if (!is.null(baseline) && length(baseline) > 0) {
    cat(sprintf("Baseline period: %d-%d\n", min(baseline), max(baseline)))
  } else {
    cat("Baseline period: (from pre-computed normals)\n")
  }
  cat(sprintf("Total observations: %d\n", nrow(object)))

  valid <- object[!is.na(anomaly_days)]

  if (nrow(valid) > 0) {
    cat(sprintf("Valid anomalies: %d (%.1f%%)\n\n",
                nrow(valid), 100 * nrow(valid) / nrow(object)))

    cat("Anomaly Statistics:\n")
    cat(sprintf("  Mean anomaly: %.1f days\n", mean(valid$anomaly_days)))
    cat(sprintf("  Median anomaly: %.1f days\n", median(valid$anomaly_days)))
    cat(sprintf("  SD of anomalies: %.1f days\n", sd(valid$anomaly_days)))
    cat(sprintf("  Range: %.1f to %.1f days\n",
                min(valid$anomaly_days), max(valid$anomaly_days)))

    cat("\nExtreme Events:\n")
    n_extreme <- sum(valid$is_extreme, na.rm = TRUE)
    n_early <- sum(valid$direction == "early", na.rm = TRUE)
    n_late <- sum(valid$direction == "late", na.rm = TRUE)

    cat(sprintf("  Total extreme: %d (%.1f%%)\n",
                n_extreme, 100 * n_extreme / nrow(valid)))
    cat(sprintf("  Extreme early: %d\n", n_early))
    cat(sprintf("  Extreme late: %d\n", n_late))

    # Most extreme years
    if (nrow(valid) > 0) {
      cat("\nMost Extreme Observations:\n")
      extreme_sorted <- valid[order(-abs(z_score))][1:min(5, nrow(valid))]
      for (i in seq_len(nrow(extreme_sorted))) {
        row <- extreme_sorted[i]
        cat(sprintf("  %d: %.1f days (%s, z=%.2f)\n",
                    row$year,
                    row$anomaly_days,
                    ifelse(row$anomaly_days < 0, "early", "late"),
                    row$z_score))
      }
    }
  } else {
    cat("\nNo valid anomalies calculated (insufficient baseline data).\n")
  }

  invisible(list(
    n_total = nrow(object),
    n_valid = nrow(valid),
    mean_anomaly = if (nrow(valid) > 0) mean(valid$anomaly_days) else NA,
    n_extreme = if (nrow(valid) > 0) sum(valid$is_extreme, na.rm = TRUE) else 0,
    baseline_period = baseline
  ))
}
