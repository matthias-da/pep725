#' Assess Data Quality of Phenological Observations
#'
#' Provides a comprehensive assessment of data completeness, temporal coverage,
#' outlier prevalence, and assigns quality grades to phenological time series.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and grouping variables.
#' @param by Character vector of column names to group by for quality assessment.
#'   Default is \code{c("s_id", "genus", "phase_id")} for station-level assessment.
#' @param year_range Optional integer vector of length 2 specifying the start and
#'   end years for assessment. If \code{NULL} (default), uses the full range
#'   of years in the data.
#' @param species Optional character string to filter by species/genus.
#' @param phase_id Optional integer vector to filter by BBCH phase codes.
#' @param outlier_method Character string specifying outlier detection method.
#'   Either \code{"tukey"} (default, uses 1.5*IQR rule) or \code{"zscore"}
#'   (flags values with |z| > 3).
#' @param na.rm Logical. Should missing values be removed? Default \code{TRUE}.
#'
#' @return A \code{data.table} with the following columns:
#'   \describe{
#'     \item{<by variables>}{Grouping variables as specified}
#'     \item{n_obs}{Total number of observations}
#'     \item{year_min}{First year of observations}
#'     \item{year_max}{Last year of observations}
#'     \item{year_span}{Total span in years (year_max - year_min + 1)}
#'     \item{n_years}{Number of distinct years with data}
#'     \item{completeness_pct}{Percentage of years with data within the span}
#'     \item{max_gap_years}{Length of the longest gap (consecutive missing years)}
#'     \item{n_gaps}{Number of gaps in the time series}
#'     \item{n_outliers}{Number of statistical outliers detected}
#'     \item{outlier_pct}{Percentage of observations that are outliers}
#'     \item{doy_mean}{Mean day-of-year}
#'     \item{doy_sd}{Standard deviation of day-of-year}
#'     \item{doy_range}{Range of day-of-year values}
#'     \item{quality_grade}{Overall quality grade (A, B, C, or D)}
#'   }
#'
#' @details
#' This function evaluates phenological time series quality based on multiple
#' criteria important for trend analysis and climatological calculations.
#'
#' @section Quality Grading Criteria:
#' \describe{
#'   \item{Grade A}{Completeness >= 80%, no gaps > 2 years, outliers < 2%}
#'   \item{Grade B}{Completeness >= 60%, no gaps > 5 years, outliers < 5%}
#'   \item{Grade C}{Completeness >= 40%, gaps <= 10 years, outliers < 10%}
#'   \item{Grade D}{Below grade C thresholds}
#' }
#'
#' @section Outlier Detection:
#' The Tukey method (default) flags observations outside the interquartile
#' fences: \code{[Q1 - 1.5*IQR, Q3 + 1.5*IQR]}. This is robust to non-normal
#' distributions common in phenological data.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Subset to one country for speed
#' pep_ch <- pep[country == "Switzerland"]
#'
#' # Assess quality for stations in Switzerland
#' quality <- pep_quality(pep_ch)
#' print(quality)
#'
#' # Filter to high-quality stations only
#' high_quality <- quality[quality_grade %in% c("A", "B")]
#' high_quality
#'
#' # Assess specific species and phase
#' apple_quality <- pep_quality(pep_ch,
#'                              species = "Malus",
#'                              phase_id = 60)
#' apple_quality
#'
#' # Country-level assessment (use subset of countries)
#' pep_subset <- pep[country %in% c("Switzerland", "Austria")]
#' country_quality <- pep_quality(pep_subset,
#'                                by = c("country", "genus", "phase_id"))
#' country_quality
#'
#' # Assess quality within a specific time window
#' modern_quality <- pep_quality(pep_ch,
#'                               year_range = c(1991, 2020))
#' modern_quality
#' }
#'
#' @seealso
#' \code{\link{pheno_normals}} for calculating climatological normals,
#' \code{\link{pheno_anomaly}} for anomaly detection
#'
#' @author Matthias Templ
#' @export
pep_quality <- function(pep,
                        by = c("s_id", "genus", "phase_id"),
                        year_range = NULL,
                        species = NULL,
                        phase_id = NULL,
                        outlier_method = c("tukey", "zscore"),
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

  # Validate outlier_method
  outlier_method <- match.arg(outlier_method)

  # Make a copy to avoid modifying original
  dt <- data.table::copy(pep)

  # Apply species filter if specified
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
      if (nrow(dt) == 0) {
        stop("No observations found for species '", species, "'", call. = FALSE)
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

  # Define outlier detection function
  detect_outliers <- function(x) {
    if (length(x) < 4) return(rep(FALSE, length(x)))

    if (outlier_method == "tukey") {
      q1 <- quantile(x, 0.25, na.rm = na.rm)
      q3 <- quantile(x, 0.75, na.rm = na.rm)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      return(x < lower | x > upper)
    } else {
      # zscore method
      z <- (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
      return(abs(z) > 3)
    }
  }

  # Function to find maximum gap in years
  find_max_gap <- function(years, min_year, max_year) {
    if (length(years) <= 1) return(0)

    all_years <- min_year:max_year
    present <- all_years %in% years

    if (all(present)) return(0)

    # Find runs of missing years
    runs <- rle(!present)
    gaps <- runs$lengths[runs$values]

    if (length(gaps) == 0) return(0)
    return(max(gaps))
  }

  # Function to count gaps
  count_gaps <- function(years, min_year, max_year) {
    if (length(years) <= 1) return(0)

    all_years <- min_year:max_year
    present <- all_years %in% years

    if (all(present)) return(0)

    runs <- rle(!present)
    return(sum(runs$values))
  }

  # Calculate quality metrics by group
  result <- dt[, {
    doy_values <- day
    years_present <- unique(year)
    n_years_present <- length(years_present)

    year_min <- min(year, na.rm = na.rm)
    year_max <- max(year, na.rm = na.rm)
    year_span <- year_max - year_min + 1

    # Completeness
    completeness <- if (year_span > 0) 100 * n_years_present / year_span else 0

    # Gap analysis
    max_gap <- find_max_gap(years_present, year_min, year_max)
    n_gaps <- count_gaps(years_present, year_min, year_max)

    # Outlier detection
    is_outlier <- detect_outliers(doy_values)
    n_outliers <- sum(is_outlier, na.rm = TRUE)
    outlier_pct <- if (length(doy_values) > 0) 100 * n_outliers / length(doy_values) else 0

    # DOY statistics
    doy_mean <- mean(doy_values, na.rm = na.rm)
    doy_sd <- sd(doy_values, na.rm = na.rm)
    doy_range <- max(doy_values, na.rm = na.rm) - min(doy_values, na.rm = na.rm)

    # Quality grading
    grade <- if (completeness >= 80 && max_gap <= 2 && outlier_pct < 2) {
      "A"
    } else if (completeness >= 60 && max_gap <= 5 && outlier_pct < 5) {
      "B"
    } else if (completeness >= 40 && max_gap <= 10 && outlier_pct < 10) {
      "C"
    } else {
      "D"
    }

    list(
      n_obs = as.integer(.N),
      year_min = as.integer(year_min),
      year_max = as.integer(year_max),
      year_span = as.integer(year_span),
      n_years = as.integer(n_years_present),
      completeness_pct = as.numeric(round(completeness, 1)),
      max_gap_years = as.integer(max_gap),
      n_gaps = as.integer(n_gaps),
      n_outliers = as.integer(n_outliers),
      outlier_pct = as.numeric(round(outlier_pct, 1)),
      doy_mean = as.numeric(round(doy_mean, 1)),
      doy_sd = as.numeric(round(doy_sd, 1)),
      doy_range = as.integer(doy_range),
      quality_grade = as.character(grade)
    )
  }, by = by]

  # Order by grouping variables
  data.table::setorderv(result, by)

  # Add class for potential method dispatch
  class(result) <- c("pep_quality", class(result))

  # Store parameters as attributes
  attr(result, "year_range") <- year_range
  attr(result, "outlier_method") <- outlier_method
  attr(result, "species_filter") <- species
  attr(result, "phase_filter") <- phase_id

  result
}


#' Print Method for Data Quality Assessment
#'
#' @param x A \code{pep_quality} object
#' @param n Number of rows to display
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pep_quality <- function(x, n = 10, ...) {
  year_range <- attr(x, "year_range")
  outlier_method <- attr(x, "outlier_method")
  species_filter <- attr(x, "species_filter")
  phase_filter <- attr(x, "phase_filter")

  cat("Phenological Data Quality Assessment\n")
  cat(strrep("-", 50), "\n")

  if (!is.null(year_range) && length(year_range) == 2) {
    cat(sprintf("Assessment period: %d-%d\n", year_range[1], year_range[2]))
  }

  if (!is.null(outlier_method)) {
    cat(sprintf("Outlier method: %s\n", outlier_method))
  }

  if (!is.null(species_filter)) {
    cat(sprintf("Species filter: %s\n", species_filter))
  }
  if (!is.null(phase_filter)) {
    cat(sprintf("Phase filter: %s\n", paste(phase_filter, collapse = ", ")))
  }

  # Grade distribution
  grade_table <- table(x$quality_grade)
  cat("\nQuality Grade Distribution:\n")
  for (g in c("A", "B", "C", "D")) {
    count <- if (g %in% names(grade_table)) grade_table[[g]] else 0
    pct <- 100 * count / nrow(x)
    cat(sprintf("  Grade %s: %d (%.1f%%)\n", g, count, pct))
  }

  cat(sprintf("\nTotal groups assessed: %d\n", nrow(x)))
  cat(strrep("-", 50), "\n\n")

  # Print as data.table
  print(data.table::as.data.table(x)[1:min(n, nrow(x))])

  if (nrow(x) > n) {
    cat(sprintf("\n... and %d more rows\n", nrow(x) - n))
  }

  invisible(x)
}


#' Summary Method for Data Quality Assessment
#'
#' @param object A \code{pep_quality} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @author Matthias Templ
#' @export
summary.pep_quality <- function(object, ...) {
  cat("Phenological Data Quality Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat(sprintf("Total groups assessed: %d\n\n", nrow(object)))

  # Grade distribution
  grade_table <- table(object$quality_grade)
  cat("Quality Grade Distribution:\n")
  for (g in c("A", "B", "C", "D")) {
    count <- if (g %in% names(grade_table)) grade_table[[g]] else 0
    pct <- 100 * count / nrow(object)
    bar <- paste(rep("*", round(pct / 5)), collapse = "")
    cat(sprintf("  Grade %s: %4d (%5.1f%%) %s\n", g, count, pct, bar))
  }

  cat("\nCompleteness Statistics:\n")
  cat(sprintf("  Mean completeness: %.1f%%\n", mean(object$completeness_pct, na.rm = TRUE)))
  cat(sprintf("  Median completeness: %.1f%%\n", median(object$completeness_pct, na.rm = TRUE)))
  cat(sprintf("  Range: %.1f%% - %.1f%%\n",
              min(object$completeness_pct, na.rm = TRUE),
              max(object$completeness_pct, na.rm = TRUE)))

  cat("\nTemporal Coverage:\n")
  cat(sprintf("  Mean years with data: %.1f\n", mean(object$n_years, na.rm = TRUE)))
  cat(sprintf("  Mean year span: %.1f\n", mean(object$year_span, na.rm = TRUE)))
  cat(sprintf("  Mean max gap: %.1f years\n", mean(object$max_gap_years, na.rm = TRUE)))

  cat("\nOutlier Statistics:\n")
  cat(sprintf("  Mean outlier percentage: %.2f%%\n", mean(object$outlier_pct, na.rm = TRUE)))
  cat(sprintf("  Groups with no outliers: %d (%.1f%%)\n",
              sum(object$n_outliers == 0),
              100 * sum(object$n_outliers == 0) / nrow(object)))

  invisible(list(
    n_groups = nrow(object),
    grade_distribution = grade_table,
    mean_completeness = mean(object$completeness_pct, na.rm = TRUE),
    mean_max_gap = mean(object$max_gap_years, na.rm = TRUE),
    mean_outlier_pct = mean(object$outlier_pct, na.rm = TRUE)
  ))
}


#' Plot Method for Data Quality Assessment
#'
#' Creates visualizations of phenological data quality metrics, including
#' grade distribution and a map of station quality.
#'
#' @param x A \code{pep_quality} object.
#' @param which Character. Type of plot to produce:
#'   \itemize{
#'     \item \code{"overview"} (default): Two-panel overview with grade
#'       distribution and station quality map (requires \code{pep} argument)
#'     \item \code{"grades"}: Bar chart of quality grade distribution only
#'     \item \code{"map"}: Map of station locations colored by quality grade
#'       (requires \code{pep} argument for coordinates)
#'   }
#' @param pep Optional \code{pep} object providing station coordinates (lon, lat).
#'   Required for \code{which = "map"} or \code{which = "overview"}.
#' @param title Optional character string for the plot title.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object (invisibly).
#'
#' @details
#' For the map visualization, the function:
#' \itemize{
#'   \item Aggregates quality to one grade per station (worst grade if multiple phases)
#'   \item Uses country borders from the \code{rnaturalearth} package
#'   \item Colors stations by quality grade (A=green, B=light green, C=orange, D=red)
#' }
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Assess quality for Swiss stations
#' pep_ch <- pep[country == "Switzerland"]
#' quality <- pep_quality(pep_ch, by = c("s_id", "phase_id"))
#'
#' # Grade distribution only (no pep data needed)
#' plot(quality, which = "grades")
#'
#' # Map of station quality (requires pep for coordinates)
#' plot(quality, which = "map", pep = pep_ch)
#'
#' # Overview: grades + map
#' plot(quality, pep = pep_ch)
#' }
#'
#' @author Matthias Templ
#' @export
plot.pep_quality <- function(x, which = c("overview", "grades", "map"),
                              pep = NULL, title = NULL, ...) {
  which <- match.arg(which)

  # Define grade colors (green to red spectrum)
  grade_colors <- c("A" = "#2E7D32", "B" = "#7CB342", "C" = "#FFA726", "D" = "#D32F2F")

  # Ensure quality_grade is a factor with correct order
  plot_data <- data.table::as.data.table(x)
  plot_data[, quality_grade := factor(quality_grade, levels = c("A", "B", "C", "D"))]

  # Grade distribution bar chart
  plot_grades <- function() {
    grade_counts <- plot_data[, .N, by = quality_grade]
    grade_counts[, pct := 100 * N / sum(N)]

    ggplot2::ggplot(grade_counts, ggplot2::aes(x = quality_grade, y = N, fill = quality_grade)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f%%)", N, pct)),
                         vjust = -0.2, size = 3) +
      ggplot2::scale_fill_manual(values = grade_colors) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::labs(x = "Quality Grade", y = "Number of Groups",
                    title = "Quality Grade Distribution") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }

  # Map of station quality
  plot_map <- function() {
    if (is.null(pep)) {
      stop("'pep' argument required for map visualization (provides station coordinates)",
           call. = FALSE)
    }

    # Check for required columns
    if (!all(c("s_id", "lon", "lat") %in% names(pep))) {
      stop("'pep' must contain columns: s_id, lon, lat", call. = FALSE)
    }

    # Get unique station coordinates
    stations <- unique(data.table::as.data.table(pep)[, .(s_id, lon, lat)])

    # Aggregate quality to station level (worst grade per station)
    # Convert grade to numeric for aggregation, then back
    grade_numeric <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4)
    plot_data[, grade_num := grade_numeric[as.character(quality_grade)]]

    station_quality <- plot_data[, .(
      worst_grade_num = max(grade_num, na.rm = TRUE),
      n_phases = .N
    ), by = s_id]

    station_quality[, quality_grade := factor(
      names(grade_numeric)[worst_grade_num],
      levels = c("A", "B", "C", "D")
    )]

    # Merge with coordinates
    map_data <- merge(station_quality, stations, by = "s_id", all.x = TRUE)
    map_data <- map_data[!is.na(lon) & !is.na(lat)]

    if (nrow(map_data) == 0) {
      stop("No stations with valid coordinates found", call. = FALSE)
    }

    # Get country borders from rnaturalearth
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    # Determine map extent from data (with padding)
    lon_range <- range(map_data$lon, na.rm = TRUE)
    lat_range <- range(map_data$lat, na.rm = TRUE)
    lon_pad <- diff(lon_range) * 0.1
    lat_pad <- diff(lat_range) * 0.1

    # Create map
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
      ggplot2::geom_point(
        data = map_data,
        ggplot2::aes(x = lon, y = lat, color = quality_grade),
        size = 1.5, alpha = 0.7
      ) +
      ggplot2::scale_color_manual(values = grade_colors, name = "Grade", drop = FALSE) +
      ggplot2::coord_sf(
        xlim = c(lon_range[1] - lon_pad, lon_range[2] + lon_pad),
        ylim = c(lat_range[1] - lat_pad, lat_range[2] + lat_pad),
        expand = FALSE
      ) +
      ggplot2::labs(
        title = "Station Quality Map",
        subtitle = sprintf("%d stations", nrow(map_data)),
        x = "Longitude", y = "Latitude"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_line(color = "gray90"),
        legend.position = "right"
      )
  }

  # Generate requested plot type
  if (which == "grades") {
    p <- plot_grades()
    if (!is.null(title)) p <- p + ggplot2::labs(title = title)
    print(p)
    return(invisible(p))
  }

  if (which == "map") {
    p <- plot_map()
    if (!is.null(title)) p <- p + ggplot2::labs(title = title)
    print(p)
    return(invisible(p))
  }

  # Overview: grades + map side by side
  if (is.null(pep)) {
    message("Note: 'pep' argument not provided. Showing grades only.")
    message("For overview with map, use: plot(quality, pep = your_pep_data)")
    p <- plot_grades()
    if (!is.null(title)) p <- p + ggplot2::labs(title = title)
    print(p)
    return(invisible(p))
  }

  p1 <- plot_grades()
  p2 <- plot_map()

  # Use patchwork to combine
  combined <- p1 + p2 +
    patchwork::plot_annotation(
      title = if (!is.null(title)) title else "Data Quality Overview",
      subtitle = sprintf("Total groups: %d | Stations: %d",
                         nrow(x), length(unique(plot_data$s_id)))
    ) +
    patchwork::plot_layout(widths = c(1, 2))

  print(combined)
  invisible(combined)
}
