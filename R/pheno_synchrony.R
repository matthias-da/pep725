#' Analyze Phenological Synchrony Across Stations
#'
#' Measures how synchronized phenological events are across stations within
#' regions and whether synchrony is changing over time. Higher synchrony
#' indicates more uniform timing of phenological events.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and \code{s_id} (station ID).
#' @param species Optional character string to filter by species/genus.
#' @param phase_id Optional integer to filter by a single BBCH phase code.
#' @param by Character vector of column names to group by for synchrony calculation.
#'   Default is \code{c("country", "year")} for annual synchrony by country.
#' @param min_stations Minimum number of stations required to calculate synchrony.
#'   Default is 5. Groups with fewer stations return NA.
#' @param compute_trend Logical. If \code{TRUE} (default), computes trend in
#'   synchrony over time using robust regression.
#' @param na.rm Logical. Should missing values be removed? Default \code{TRUE}.
#'
#' @return A \code{pheno_synchrony} object (list) containing:
#'   \describe{
#'     \item{data}{A \code{data.table} with synchrony metrics per group:
#'       n_stations, mean_doy, sd_doy, cv_pct, and quality indicators}
#'     \item{trend}{If \code{compute_trend = TRUE}, a data.table with trend
#'       analysis results per region (slope, p-value, direction)}
#'     \item{overall}{Summary statistics across all groups}
#'   }
#'
#' @details
#' Synchrony measures how similar phenological timing is across different
#' stations within the same region and year. This is important for:
#' \itemize{
#'   \item Understanding spatial coherence of phenological signals
#'   \item Detecting changes in spatial variability over time
#'   \item Assessing network representativeness
#' }
#'
#' @section Synchrony Metrics:
#' \describe{
#'   \item{sd_doy}{Standard deviation across stations - lower values indicate
#'     higher synchrony}
#'   \item{cv_pct}{Coefficient of variation (SD/mean * 100) - relative measure
#'     that allows comparison across phases with different mean timing}
#'   \item{range_doy}{Range of DOY values across stations}
#' }
#'
#' @section Trend Interpretation:
#' \itemize{
#'   \item Negative trend in SD: Increasing synchrony over time
#'   \item Positive trend in SD: Decreasing synchrony (more variable)
#' }
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Subset to two countries for speed
#' pep_subset <- pep[country %in% c("Germany", "Switzerland")]
#'
#' # Calculate synchrony for wheat heading by country and year
#' sync <- pheno_synchrony(pep_subset,
#'                         species = "Triticum",
#'                         phase_id = 60)
#' print(sync)
#'
#' # Get trend results (robust regression per country)
#' sync$trend
#'
#' # Synchrony without trend analysis (faster)
#' sync_simple <- pheno_synchrony(pep_subset,
#'                                species = "Triticum",
#'                                phase_id = 60,
#'                                compute_trend = FALSE)
#'
#' # Custom grouping variables
#' sync_detailed <- pheno_synchrony(pep_subset,
#'                                  species = "Triticum",
#'                                  phase_id = 60,
#'                                  by = c("country", "year"))
#' }
#'
#' @seealso
#' \code{\link{pheno_normals}} for climatological statistics,
#' \code{\link{pheno_anomaly}} for anomaly detection
#'
#' @author Matthias Templ
#' @export
pheno_synchrony <- function(pep,
                            species = NULL,
                            phase_id = NULL,
                            by = c("country", "year"),
                            min_stations = 5,
                            compute_trend = TRUE,
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
  required_cols <- c("year", "day", "s_id")
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

  # Validate min_stations
  if (!is.numeric(min_stations) || min_stations < 2) {
    stop("'min_stations' must be a numeric value >= 2", call. = FALSE)
  }

  # Make a copy to avoid modifying original
  dt <- data.table::copy(pep)

  # Apply species filter
  if (!is.null(species)) {
    if ("genus" %in% names(dt)) {
      genus_match <- dt$genus == species
      species_match <- if ("species" %in% names(dt)) dt$species == species else FALSE
      dt <- dt[genus_match | species_match]
      if (nrow(dt) == 0) {
        stop("No observations found for species '", species, "'", call. = FALSE)
      }
    } else if ("species" %in% names(dt)) {
      dt <- dt[dt$species == species]
    } else {
      warning("Neither 'genus' nor 'species' column found.", call. = FALSE)
    }
  }

  # Apply phase_id filter
  if (!is.null(phase_id)) {
    if (!"phase_id" %in% names(dt)) {
      warning("'phase_id' column not found.", call. = FALSE)
    } else {
      if (length(phase_id) > 1) {
        warning("Multiple phase_ids provided. Using first one only.",
                call. = FALSE)
        phase_id <- phase_id[1]
      }
      dt <- dt[dt$phase_id == phase_id]
      if (nrow(dt) == 0) {
        stop("No observations found for phase_id: ", phase_id, call. = FALSE)
      }
    }
  }

  # Remove missing values
  dt <- dt[!is.na(day)]

  # First aggregate to station-level means within each group
  # This handles multiple observations per station-year
  station_means <- dt[, .(
    station_mean_doy = mean(day, na.rm = na.rm)
  ), by = c(by, "s_id")]

  # Calculate synchrony metrics by group
  sync_data <- station_means[, {
    doy_values <- station_mean_doy
    n_stations <- .N

    if (n_stations >= min_stations) {
      mean_doy <- mean(doy_values, na.rm = na.rm)
      sd_doy <- sd(doy_values, na.rm = na.rm)
      cv_pct <- if (!is.na(mean_doy) && mean_doy != 0) {
        100 * sd_doy / mean_doy
      } else {
        NA_real_
      }
      range_doy <- max(doy_values, na.rm = na.rm) - min(doy_values, na.rm = na.rm)
      iqr_doy <- IQR(doy_values, na.rm = na.rm)

      list(
        n_stations = as.integer(n_stations),
        mean_doy = round(mean_doy, 1),
        sd_doy = round(sd_doy, 2),
        cv_pct = round(cv_pct, 2),
        range_doy = as.integer(range_doy),
        iqr_doy = round(iqr_doy, 1),
        min_doy = as.integer(min(doy_values, na.rm = na.rm)),
        max_doy = as.integer(max(doy_values, na.rm = na.rm))
      )
    } else {
      list(
        n_stations = as.integer(n_stations),
        mean_doy = NA_real_,
        sd_doy = NA_real_,
        cv_pct = NA_real_,
        range_doy = NA_integer_,
        iqr_doy = NA_real_,
        min_doy = NA_integer_,
        max_doy = NA_integer_
      )
    }
  }, by = by]

  # Order results
  data.table::setorderv(sync_data, by)

  # Calculate trend in synchrony if requested
  trend_data <- NULL
  if (compute_trend && "year" %in% by) {
    # Identify grouping variables other than year
    region_vars <- setdiff(by, "year")

    if (length(region_vars) > 0) {
      # Calculate trend for each region
      trend_data <- sync_data[!is.na(sd_doy), {
        if (.N >= 10) {  # Need at least 10 years for trend
          tryCatch({
            # Use robust regression
            model <- robustbase::lmrob(sd_doy ~ year, data = .SD)
            coefs <- coef(model)
            summ <- summary(model)

            list(
              n_years = as.integer(.N),
              year_min = as.integer(min(year)),
              year_max = as.integer(max(year)),
              slope = as.numeric(round(coefs[2], 4)),
              intercept = as.numeric(round(coefs[1], 2)),
              r_squared = as.numeric(round(summ$r.squared, 3)),
              p_value = as.numeric(round(summ$coefficients[2, 4], 4)),
              direction = as.character(ifelse(coefs[2] < 0,
                                              "increasing synchrony",
                                              "decreasing synchrony")),
              significant = as.logical(summ$coefficients[2, 4] < 0.05)
            )
          }, error = function(e) {
            list(
              n_years = as.integer(.N),
              year_min = as.integer(min(year)),
              year_max = as.integer(max(year)),
              slope = NA_real_,
              intercept = NA_real_,
              r_squared = NA_real_,
              p_value = NA_real_,
              direction = NA_character_,
              significant = NA
            )
          })
        } else {
          list(
            n_years = as.integer(.N),
            year_min = as.integer(min(year)),
            year_max = as.integer(max(year)),
            slope = NA_real_,
            intercept = NA_real_,
            r_squared = NA_real_,
            p_value = NA_real_,
            direction = NA_character_,
            significant = NA
          )
        }
      }, by = region_vars]
    } else {
      # Single region (no grouping besides year)
      if (sum(!is.na(sync_data$sd_doy)) >= 10) {
        valid_data <- sync_data[!is.na(sd_doy)]
        tryCatch({
          model <- robustbase::lmrob(sd_doy ~ year, data = valid_data)
          coefs <- coef(model)
          summ <- summary(model)

          trend_data <- data.table::data.table(
            n_years = nrow(valid_data),
            year_min = min(valid_data$year),
            year_max = max(valid_data$year),
            slope = round(coefs[2], 4),
            intercept = round(coefs[1], 2),
            r_squared = round(summ$r.squared, 3),
            p_value = round(summ$coefficients[2, 4], 4),
            direction = ifelse(coefs[2] < 0, "increasing synchrony", "decreasing synchrony"),
            significant = summ$coefficients[2, 4] < 0.05
          )
        }, error = function(e) {
          trend_data <- NULL
        })
      }
    }
  }

  # Calculate overall statistics
  valid_sync <- sync_data[!is.na(sd_doy)]
  overall <- list(
    n_groups = nrow(sync_data),
    n_valid_groups = nrow(valid_sync),
    mean_sd_doy = if (nrow(valid_sync) > 0) mean(valid_sync$sd_doy, na.rm = TRUE) else NA,
    median_sd_doy = if (nrow(valid_sync) > 0) median(valid_sync$sd_doy, na.rm = TRUE) else NA,
    mean_cv_pct = if (nrow(valid_sync) > 0) mean(valid_sync$cv_pct, na.rm = TRUE) else NA,
    mean_n_stations = if (nrow(valid_sync) > 0) mean(valid_sync$n_stations, na.rm = TRUE) else NA
  )

  # Build result
  result <- list(
    data = sync_data,
    trend = trend_data,
    overall = overall,
    by = by,
    min_stations = min_stations,
    species = species,
    phase_id = phase_id
  )

  class(result) <- c("pheno_synchrony", "list")
  result
}


#' Print Method for Phenological Synchrony Analysis
#'
#' @param x A \code{pheno_synchrony} object
#' @param n Number of rows to display
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pheno_synchrony <- function(x, n = 10, ...) {
  cat("Phenological Synchrony Analysis\n")
  cat(strrep("-", 50), "\n")

  if (!is.null(x$species)) {
    cat(sprintf("Species: %s\n", x$species))
  }
  if (!is.null(x$phase_id)) {
    cat(sprintf("Phase ID: %d\n", x$phase_id))
  }
  cat(sprintf("Grouping: %s\n", paste(x$by, collapse = ", ")))
  cat(sprintf("Min stations required: %d\n", x$min_stations))

  cat(strrep("-", 50), "\n\n")

  # Overall statistics
  cat("Overall Statistics:\n")
  cat(sprintf("  Total groups: %d (valid: %d)\n",
              x$overall$n_groups, x$overall$n_valid_groups))
  cat(sprintf("  Mean SD across stations: %.1f days\n", x$overall$mean_sd_doy))
  cat(sprintf("  Mean CV: %.1f%%\n", x$overall$mean_cv_pct))
  cat(sprintf("  Mean stations per group: %.1f\n", x$overall$mean_n_stations))

  cat("\n")

  # Print data
  cat("Synchrony Data:\n")
  print(x$data[1:min(n, nrow(x$data))])

  if (nrow(x$data) > n) {
    cat(sprintf("\n... and %d more rows\n", nrow(x$data) - n))
  }

  # Print trend if available
  if (!is.null(x$trend) && nrow(x$trend) > 0) {
    cat("\nTrend Analysis (change in SD over time):\n")
    print(x$trend)

    # Interpretation
    sig_trends <- x$trend[significant == TRUE]
    if (nrow(sig_trends) > 0) {
      cat("\nSignificant trends detected:\n")
      for (i in seq_len(nrow(sig_trends))) {
        row <- sig_trends[i]
        if (length(setdiff(x$by, "year")) > 0) {
          region_name <- paste(as.character(row[, setdiff(x$by, "year"), with = FALSE]),
                               collapse = ", ")
        } else {
          region_name <- "Overall"
        }
        cat(sprintf("  %s: %s (p = %.4f)\n",
                    region_name, row$direction, row$p_value))
      }
    }
  }

  invisible(x)
}


#' Summary Method for Phenological Synchrony Analysis
#'
#' @param object A \code{pheno_synchrony} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @author Matthias Templ
#' @export
summary.pheno_synchrony <- function(object, ...) {
  cat("Phenological Synchrony Summary\n")
  cat(strrep("=", 50), "\n\n")

  if (!is.null(object$species)) {
    cat(sprintf("Species: %s\n", object$species))
  }
  if (!is.null(object$phase_id)) {
    cat(sprintf("Phase ID: %d\n", object$phase_id))
  }

  cat(sprintf("\nGroups analyzed: %d\n", object$overall$n_groups))
  cat(sprintf("Groups with sufficient stations: %d (%.1f%%)\n",
              object$overall$n_valid_groups,
              100 * object$overall$n_valid_groups / object$overall$n_groups))

  cat("\nSynchrony Statistics (across all valid groups):\n")
  cat(sprintf("  Mean SD: %.1f days\n", object$overall$mean_sd_doy))
  cat(sprintf("  Median SD: %.1f days\n", object$overall$median_sd_doy))
  cat(sprintf("  Mean CV: %.1f%%\n", object$overall$mean_cv_pct))

  # SD distribution
  valid_data <- object$data[!is.na(sd_doy)]
  if (nrow(valid_data) > 0) {
    cat("\nSD Distribution:\n")
    cat(sprintf("  Min: %.1f days\n", min(valid_data$sd_doy)))
    cat(sprintf("  25th percentile: %.1f days\n", quantile(valid_data$sd_doy, 0.25)))
    cat(sprintf("  Median: %.1f days\n", median(valid_data$sd_doy)))
    cat(sprintf("  75th percentile: %.1f days\n", quantile(valid_data$sd_doy, 0.75)))
    cat(sprintf("  Max: %.1f days\n", max(valid_data$sd_doy)))
  }

  # Trend summary
  if (!is.null(object$trend) && nrow(object$trend) > 0) {
    cat("\nTrend Summary:\n")
    n_sig <- sum(object$trend$significant, na.rm = TRUE)
    n_increasing <- sum(object$trend$direction == "increasing synchrony" &
                        object$trend$significant, na.rm = TRUE)
    n_decreasing <- sum(object$trend$direction == "decreasing synchrony" &
                        object$trend$significant, na.rm = TRUE)

    cat(sprintf("  Regions with significant trends: %d of %d\n",
                n_sig, nrow(object$trend)))
    cat(sprintf("  Increasing synchrony: %d\n", n_increasing))
    cat(sprintf("  Decreasing synchrony: %d\n", n_decreasing))
  }

  invisible(object$overall)
}


#' Plot Method for Phenological Synchrony Analysis
#'
#' Creates a time series plot of synchrony (SD) over years.
#'
#' @param x A \code{pheno_synchrony} object
#' @param region Optional region to plot (for multi-region analyses)
#' @param ... Additional arguments passed to ggplot
#'
#' @return A ggplot object
#' @author Matthias Templ
#' @export
plot.pheno_synchrony <- function(x, region = NULL, ...) {
  if (!"year" %in% x$by) {
    stop("Plot requires 'year' in grouping variables", call. = FALSE)
  }

  plot_data <- x$data[!is.na(sd_doy)]

  if (nrow(plot_data) == 0) {
    stop("No valid data for plotting", call. = FALSE)
  }

  # Filter to specific region if requested
  region_vars <- setdiff(x$by, "year")
  if (!is.null(region) && length(region_vars) > 0) {
    # Simple filter for single region variable
    if (length(region_vars) == 1) {
      plot_data <- plot_data[get(region_vars) == region]
    }
  }

  # Create base plot
  if (length(region_vars) > 0 && is.null(region)) {
    # Multiple regions - faceted plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = year, y = sd_doy)) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::geom_point(ggplot2::aes(size = n_stations), alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.8) +
      ggplot2::facet_wrap(as.formula(paste("~", region_vars[1])), scales = "free_y") +
      ggplot2::labs(
        x = "Year",
        y = "SD across stations (days)",
        title = "Phenological Synchrony Over Time",
        subtitle = "Lower SD = Higher synchrony",
        size = "N stations"
      ) +
      ggplot2::theme_minimal()
  } else {
    # Single region or overall
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = year, y = sd_doy)) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::geom_point(ggplot2::aes(size = n_stations), alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.8) +
      ggplot2::labs(
        x = "Year",
        y = "SD across stations (days)",
        title = "Phenological Synchrony Over Time",
        subtitle = ifelse(!is.null(region),
                          paste("Region:", region),
                          "Lower SD = Higher synchrony"),
        size = "N stations"
      ) +
      ggplot2::theme_minimal()
  }

  p
}
