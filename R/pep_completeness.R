# Prevent R CMD check notes for data.table columns
utils::globalVariables(c("n_obs", "completeness_pct", ".data"))

#' Assess Species and Phase Completeness of Phenological Data
#'
#' Provides comprehensive assessment of data completeness across species,
#' phases, years, and optionally countries. Useful for identifying data gaps
#' and planning analyses.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, \code{genus},
#'   \code{species}, and \code{phase_id}.
#' @param by Character vector specifying grouping dimensions. Options include
#'   \code{"species"}, \code{"genus"}, \code{"phase_id"}, \code{"year"},
#'   \code{"country"}, and \code{"s_id"}. Default is
#'   \code{c("genus", "species", "phase_id")}.
#' @param year_range Optional integer vector of length 2 specifying the start
#'   and end years for assessment. If \code{NULL} (default), uses the full
#'   range of years in the data.
#' @param min_obs Integer. Minimum number of observations required to include

#'   a group in the output. Default is 1.
#' @param include_years Logical. If \code{TRUE}, includes year-level detail
#'   in the output (creates a row per group × year). Default is \code{FALSE}
#'   for summary mode.
#'
#' @return A \code{data.table} of class \code{pep_completeness} with columns:
#'   \describe{
#'     \item{by variables}{Grouping variables as specified in \code{by}}
#'     \item{n_obs}{Total number of observations}
#'     \item{n_stations}{Number of unique stations}
#'     \item{n_years}{Number of years with data}
#'     \item{year_min}{First year of observations}
#'     \item{year_max}{Last year of observations}
#'     \item{year_span}{Total span in years}
#'     \item{completeness_pct}{Percentage of years with data within span}
#'     \item{median_doy}{Median day of year}
#'     \item{iqr_doy}{Interquartile range of day of year}
#'   }
#'
#' @details
#' This function assesses data coverage across multiple dimensions, helping
#' users understand where data is available and where gaps exist. It is
#' particularly useful for:
#' \itemize{
#'   \item Identifying which species × phase combinations have sufficient data
#'   \item Finding temporal gaps in time series
#'   \item Comparing coverage across countries or regions
#'   \item Planning analyses that require minimum observation counts
#' }
#'
#' @section Completeness Calculation:
#' Completeness is calculated as the percentage of years with at least one
#' observation within the year span (year_max - year_min + 1). A completeness
#' of 100% means every year in the span has data.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Use Swiss subset for faster computation
#' pep_ch <- pep[country == "Switzerland"]
#'
#' # Basic completeness by species and phase
#' comp <- pep_completeness(pep_ch)
#' print(comp)
#'
#' # Filter to well-observed combinations
#' comp_good <- comp[n_obs >= 100 & completeness_pct >= 80]
#'
#' # Completeness by country (use Alpine countries)
#' pep_alpine <- pep[country %in% c("Switzerland", "Austria")]
#' comp_country <- pep_completeness(pep_alpine,
#'                                  by = c("country", "genus", "phase_id"))
#'
#' # Year-level detail for a specific period
#' comp_yearly <- pep_completeness(pep_ch,
#'                                  year_range = c(1991, 2020),
#'                                  include_years = TRUE)
#'
#' # Visualize completeness
#' plot(comp)
#' }
#'
#' @seealso
#' \code{\link{pep_check_phases}} for validating expected phases exist,
#' \code{\link{pep_quality}} for quality grading,
#' \code{\link{pep_coverage}} for overall data coverage
#'
#' @author Matthias Templ
#' @export
pep_completeness <- function(pep,
                              by = c("genus", "species", "phase_id"),
                              year_range = NULL,
                              min_obs = 1,
                              include_years = FALSE) {

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

  # Make a copy to avoid modifying original
  dt <- data.table::copy(pep)

  # Apply year range filter if specified
  if (!is.null(year_range)) {
    if (!is.numeric(year_range) || length(year_range) != 2) {
      stop("'year_range' must be a numeric vector of length 2", call. = FALSE)
    }
    dt <- dt[year >= year_range[1] & year <= year_range[2]]
    if (nrow(dt) == 0) {
      stop("No observations found in year range: ",
           year_range[1], "-", year_range[2], call. = FALSE)
    }
  }

  # Determine if s_id exists for station counting
  has_stations <- "s_id" %in% names(dt)

  if (include_years) {
    # Year-level detail mode
    by_with_year <- c(by, "year")

    result <- dt[, {
      list(
        n_obs = as.integer(.N),
        n_stations = if (has_stations) data.table::uniqueN(s_id) else NA_integer_,
        median_doy = as.numeric(round(median(day, na.rm = TRUE), 1)),
        iqr_doy = as.numeric(round(stats::IQR(day, na.rm = TRUE), 1))
      )
    }, by = by_with_year]

  } else {
    # Summary mode (default)
    result <- dt[, {
      years_present <- unique(year)
      n_years_present <- length(years_present)
      year_min <- min(year, na.rm = TRUE)
      year_max <- max(year, na.rm = TRUE)
      year_span <- year_max - year_min + 1
      completeness <- if (year_span > 0) 100 * n_years_present / year_span else 0

      list(
        n_obs = as.integer(.N),
        n_stations = if (has_stations) data.table::uniqueN(s_id) else NA_integer_,
        n_years = as.integer(n_years_present),
        year_min = as.integer(year_min),
        year_max = as.integer(year_max),
        year_span = as.integer(year_span),
        completeness_pct = as.numeric(round(completeness, 1)),
        median_doy = as.numeric(round(median(day, na.rm = TRUE), 1)),
        iqr_doy = as.numeric(round(stats::IQR(day, na.rm = TRUE), 1))
      )
    }, by = by]
  }

  # Apply minimum observation filter
  result <- result[n_obs >= min_obs]

  # Order by grouping variables
  data.table::setorderv(result, by)

  # Add class for method dispatch
  class(result) <- c("pep_completeness", class(result))

  # Store parameters as attributes
  attr(result, "by") <- by
  attr(result, "year_range") <- year_range
  attr(result, "min_obs") <- min_obs
  attr(result, "include_years") <- include_years

  result
}


#' Print Method for Completeness Assessment
#'
#' @param x A \code{pep_completeness} object
#' @param n Number of rows to display
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pep_completeness <- function(x, n = 15, ...) {
  by <- attr(x, "by")
  year_range <- attr(x, "year_range")
  min_obs <- attr(x, "min_obs")
  include_years <- attr(x, "include_years")

  cat("Phenological Data Completeness Assessment\n")
  cat(strrep("-", 50), "\n")

  cat(sprintf("Grouping by: %s\n", paste(by, collapse = ", ")))

  if (!is.null(year_range) && length(year_range) == 2) {
    cat(sprintf("Year range: %d-%d\n", year_range[1], year_range[2]))
  }

  if (min_obs > 1) {
    cat(sprintf("Minimum observations: %d\n", min_obs))
  }

  cat(sprintf("Total groups: %d\n", nrow(x)))

  if (!include_years && "completeness_pct" %in% names(x)) {
    # Summary statistics
    cat(sprintf("Mean completeness: %.1f%%\n", mean(x$completeness_pct, na.rm = TRUE)))
    cat(sprintf("Groups with >80%% completeness: %d (%.1f%%)\n",
                sum(x$completeness_pct >= 80, na.rm = TRUE),
                100 * sum(x$completeness_pct >= 80, na.rm = TRUE) / nrow(x)))
  }

  cat(strrep("-", 50), "\n\n")

  # Print as data.table
  print(data.table::as.data.table(x)[1:min(n, nrow(x))])

  if (nrow(x) > n) {
    cat(sprintf("\n... and %d more rows\n", nrow(x) - n))
  }

  invisible(x)
}


#' Summary Method for Completeness Assessment
#'
#' @param object A \code{pep_completeness} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @author Matthias Templ
#' @export
summary.pep_completeness <- function(object, ...) {
  by <- attr(object, "by")
  include_years <- attr(object, "include_years")

  cat("Phenological Data Completeness Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat(sprintf("Total groups: %d\n", nrow(object)))
  cat(sprintf("Total observations: %s\n", format(sum(object$n_obs), big.mark = ",")))

  if ("n_stations" %in% names(object) && !all(is.na(object$n_stations))) {
    cat(sprintf("Total unique stations: %s\n",
                format(sum(object$n_stations, na.rm = TRUE), big.mark = ",")))
  }

  if (!include_years && "completeness_pct" %in% names(object)) {
    cat("\nCompleteness Distribution:\n")
    breaks <- c(0, 20, 40, 60, 80, 100)
    labels <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")
    bins <- cut(object$completeness_pct, breaks = breaks, labels = labels,
                include.lowest = TRUE, right = TRUE)
    bin_table <- table(bins)
    for (i in seq_along(bin_table)) {
      pct <- 100 * bin_table[i] / nrow(object)
      bar <- paste(rep("*", round(pct / 5)), collapse = "")
      cat(sprintf("  %s: %4d (%5.1f%%) %s\n", labels[i], bin_table[i], pct, bar))
    }

    cat("\nYear Coverage Statistics:\n")
    cat(sprintf("  Mean year span: %.1f years\n", mean(object$year_span, na.rm = TRUE)))
    cat(sprintf("  Mean years with data: %.1f\n", mean(object$n_years, na.rm = TRUE)))
    cat(sprintf("  Earliest data: %d\n", min(object$year_min, na.rm = TRUE)))
    cat(sprintf("  Latest data: %d\n", max(object$year_max, na.rm = TRUE)))
  }

  cat("\nObservation Statistics:\n")
  cat(sprintf("  Mean observations per group: %.1f\n", mean(object$n_obs, na.rm = TRUE)))
  cat(sprintf("  Median observations per group: %.0f\n", median(object$n_obs, na.rm = TRUE)))
  cat(sprintf("  Max observations: %s\n", format(max(object$n_obs), big.mark = ",")))

  if ("median_doy" %in% names(object)) {
    cat("\nPhenology Statistics:\n")
    cat(sprintf("  DOY range: %.0f - %.0f\n",
                min(object$median_doy, na.rm = TRUE),
                max(object$median_doy, na.rm = TRUE)))
  }

  invisible(list(
    n_groups = nrow(object),
    total_obs = sum(object$n_obs),
    mean_completeness = if ("completeness_pct" %in% names(object))
      mean(object$completeness_pct, na.rm = TRUE) else NA
  ))
}


#' Plot Method for Completeness Assessment
#'
#' Creates visualizations of data completeness, including heatmaps and
#' bar charts.
#'
#' @param x A \code{pep_completeness} object
#' @param type Character. Type of plot: \code{"heatmap"} for completeness
#'   heatmap (requires 2 grouping variables), \code{"bar"} for observation
#'   counts, \code{"timeline"} for year coverage. Default is \code{"heatmap"}.
#' @param top Integer. Number of top groups to display. Default is 20.
#' @param ... Additional arguments (unused)
#'
#' @return A ggplot object (invisibly)
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' pep_ch <- pep[country == "Switzerland"]
#' comp <- pep_completeness(pep_ch, by = c("genus", "phase_id"))
#' plot(comp, type = "heatmap")
#' plot(comp, type = "bar", top = 15)
#' }
#'
#' @author Matthias Templ
#' @export
plot.pep_completeness <- function(x, type = c("heatmap", "bar", "timeline"),
                                   top = 20, ...) {
  type <- match.arg(type)
  by <- attr(x, "by")
  include_years <- attr(x, "include_years")

  if (type == "heatmap") {
    # Need at least 2 grouping variables for heatmap
    if (length(by) < 2) {
      warning("Heatmap requires at least 2 grouping variables. Switching to 'bar' plot.",
              call. = FALSE)
      type <- "bar"
    }
  }

  if (type == "heatmap" && !include_years) {
    # Create species × phase heatmap
    dt <- data.table::as.data.table(x)

    # Use first two 'by' variables for axes
    x_var <- by[1]
    y_var <- by[2]

    # Get top groups by observation count
    if (nrow(dt) > top * top) {
      top_x <- dt[, .(total = sum(n_obs)), by = x_var][order(-total)][1:min(top, .N)][[x_var]]
      top_y <- dt[, .(total = sum(n_obs)), by = y_var][order(-total)][1:min(top, .N)][[y_var]]
      dt <- dt[get(x_var) %in% top_x & get(y_var) %in% top_y]
    }

    # Aggregate if there are more grouping variables
    if (length(by) > 2) {
      dt <- dt[, .(
        n_obs = sum(n_obs),
        completeness_pct = mean(completeness_pct, na.rm = TRUE)
      ), by = c(x_var, y_var)]
    }

    fill_var <- if ("completeness_pct" %in% names(dt)) "completeness_pct" else "n_obs"
    fill_label <- if (fill_var == "completeness_pct") "Completeness (%)" else "Observations"

    p <- ggplot2::ggplot(dt, ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[fill_var]]
    )) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey90") +
      ggplot2::labs(
        title = "Data Completeness Heatmap",
        x = x_var,
        y = y_var,
        fill = fill_label
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid = ggplot2::element_blank()
      )

  } else if (type == "bar") {
    dt <- data.table::as.data.table(x)[order(-n_obs)][1:min(top, nrow(x))]

    # Create label from grouping variables
    if (length(by) == 1) {
      dt$label <- as.character(dt[[by[1]]])
    } else {
      dt$label <- do.call(paste, c(dt[, ..by], sep = " | "))
    }
    dt$label <- factor(dt$label, levels = rev(dt$label))

    p <- ggplot2::ggplot(dt, ggplot2::aes(x = n_obs, y = label)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
      ggplot2::labs(
        title = sprintf("Top %d Groups by Observation Count", min(top, nrow(x))),
        x = "Number of Observations",
        y = ""
      ) +
      ggplot2::theme_minimal(base_size = 11)

  } else if (type == "timeline") {
    if (!include_years && "year_min" %in% names(x) && "year_max" %in% names(x)) {
      dt <- data.table::as.data.table(x)[order(-n_obs)][1:min(top, nrow(x))]

      # Create label
      if (length(by) == 1) {
        dt$label <- as.character(dt[[by[1]]])
      } else {
        dt$label <- do.call(paste, c(dt[, ..by], sep = " | "))
      }
      dt$label <- factor(dt$label, levels = rev(dt$label))

      p <- ggplot2::ggplot(dt, ggplot2::aes(y = label)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = year_min, xend = year_max, yend = label),
          linewidth = 3, color = "steelblue", alpha = 0.7
        ) +
        ggplot2::geom_point(ggplot2::aes(x = year_min), size = 2, color = "steelblue") +
        ggplot2::geom_point(ggplot2::aes(x = year_max), size = 2, color = "steelblue") +
        ggplot2::labs(
          title = "Temporal Coverage by Group",
          x = "Year",
          y = ""
        ) +
        ggplot2::theme_minimal(base_size = 11)
    } else {
      warning("Timeline plot requires summary mode with year_min/year_max columns.",
              call. = FALSE)
      return(invisible(NULL))
    }
  }

  print(p)
  invisible(p)
}
