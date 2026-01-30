# Prevent R CMD check notes
utils::globalVariables(c("day", "year", "s_id", "phase_id", "genus", "species",
                         "lon", "lat", "country", "n_events", "event_type",
                         "doy_1", "doy_2", "gap_days", "is_second_event",
                         "median_doy", "mad_doy", "z_score", "month",
                         "expected_window_start", "expected_window_end",
                         "..cols_keep", "n_total", "proportion",
                         "q05", "q95", "doy_max", "doy_min", "month_name",
                         "month_label"))

#' Detect Second Flowering and Other Repeated Phenological Events
#'
#' Identifies potential second flowering events and other irregular repeated
#' phenological observations. These may represent biologically meaningful
#' responses to climate disruption rather than data errors.
#'
#' @param pep A \code{pep} object or data.table with phenological observations.
#' @param phases Integer vector of phase IDs to analyze. Default is
#'   \code{c(60, 65)} for flowering phases. Use \code{NULL} to analyze all phases.
#' @param method Character. Detection method:
#'   \describe{
#'     \item{"late_season"}{(Default) Flag observations occurring after the
#'       expected phenological window (DOY > late_threshold).}
#'     \item{"multiple_per_year"}{Detect multiple observations of the same
#'       phase at the same station within a single year.}
#'     \item{"both"}{Apply both methods.}
#'   }
#' @param late_threshold Integer. DOY threshold for "late season" events.
#'   Default is 250 (~September 7). Events after this date for spring phases
#'   are flagged as potential second events.
#' @param gap_threshold Integer. For "multiple_per_year" method, minimum days
#'   between observations to consider them separate events. Default is 60.
#' @param reference_period Integer vector of years to calculate expected
#'   phenological windows. Default is \code{1991:2020}.
#' @param z_threshold Numeric. Z-score threshold for identifying late events
#'   relative to the reference period. Default is 3.
#' @param min_obs Integer. Minimum observations in reference period to calculate
#'   expected windows. Default is 10.
#'
#' @return A list of class \code{second_events} containing:
#'   \describe{
#'     \item{events}{Data.table of detected second/repeated events with columns:
#'       s_id, species, phase_id, year, day, event_type, expected timing info}
#'     \item{summary}{Summary statistics by species, phase, and event type}
#'     \item{by_year}{Annual counts of detected events}
#'     \item{by_month}{Monthly distribution of events}
#'     \item{total_by_year}{Total observations per year (for relative scaling in plots)}
#'     \item{method}{Detection method used}
#'     \item{params}{Parameters used for detection}
#'   }
#'
#' @details
#' Second flowering and other repeated phenological events are increasingly
#' reported as climate patterns become more irregular. These events may indicate:
#' \itemize{
#'   \item Disrupted coordination between environmental cues and development
#'   \item Response to unusual autumn warmth after summer dormancy
#'   \item Failure of normal seasonal inhibition mechanisms
#' }
#'
#' The function uses two complementary approaches:
#' \enumerate{
#'   \item \strong{Late season detection}: Identifies observations occurring
#'     well after the normal phenological window (e.g., flowering in autumn)
#'   \item \strong{Multiple events}: Finds stations reporting the same phase
#'     multiple times in one year with sufficient gap between observations
#' }
#'
#' @section Interpreting Results:
#' Not all detected events are necessarily "true" second flowering. Consider:
#' \itemize{
#'   \item \strong{High confidence}: Same station reports phase twice with
#'     60+ day gap; late observation is clearly outside normal window
#'   \item \strong{Medium confidence}: Single late observation at a station;
#'     could be second event or data entry error
#'   \item \strong{Requires verification}: Check original data sources,
#'     geographic context, species biology
#' }
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#'
#' # Detect late-season flowering events
#' second <- detect_second_events(pep, phases = c(60, 65))
#' print(second)
#' summary(second)
#' plot(second)
#'
#' # Focus on very late events (October onwards)
#' second_late <- detect_second_events(pep, late_threshold = 275)
#'
#' # Detect actual repeated observations at same station
#' repeated <- detect_second_events(pep, method = "multiple_per_year")
#'
#' # Combine both methods
#' all_events <- detect_second_events(pep, method = "both")
#'
#' # Analyze specific species
#' apple_second <- detect_second_events(
#'   pep[species == "Malus domestica"],
#'   phases = c(60, 65)
#' )
#' }
#'
#' @seealso \code{\link{flag_outliers}} for general outlier detection,
#'   \code{\link{plot_outliers}} for outlier visualization
#'
#' @references
#' Related to emerging research on phenological irregularity and climate
#' disruption of seasonal timing.
#'
#' @author Matthias Templ
#' @export
detect_second_events <- function(pep,
                                  phases = c(60, 65),
                                  method = c("late_season", "multiple_per_year", "both"),
                                  late_threshold = 250,
                                  gap_threshold = 60,
                                  reference_period = 1991:2020,
                                  z_threshold = 3,
                                  min_obs = 10) {

 method <- match.arg(method)

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table and remove pep class to avoid subsetting issues
  dt <- data.table::as.data.table(pep)
  class(dt) <- c("data.table", "data.frame")

  # Check required columns
  required <- c("s_id", "year", "day", "phase_id")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Filter to specified phases
  if (!is.null(phases)) {
    dt <- dt[phase_id %in% phases]
    if (nrow(dt) == 0) {
      stop("No observations found for specified phases", call. = FALSE)
    }
  }

  # Calculate reference statistics for each group
  ref_stats <- calculate_reference_windows(dt, reference_period, min_obs)

  # Initialize results
  events_late <- NULL
  events_multiple <- NULL

  # ============================================================
  # Method 1: Late season detection
  # ============================================================
  if (method %in% c("late_season", "both")) {
    events_late <- detect_late_season_events(dt, ref_stats, late_threshold, z_threshold)
  }

  # ============================================================
  # Method 2: Multiple observations per year
  # ============================================================
  if (method %in% c("multiple_per_year", "both")) {
    events_multiple <- detect_multiple_events(dt, gap_threshold)
  }

  # Combine results
  if (method == "both") {
    # Combine and deduplicate
    all_events <- data.table::rbindlist(list(events_late, events_multiple), fill = TRUE)
    # Remove duplicates (same observation flagged by both methods)
    all_events <- unique(all_events, by = c("s_id", "year", "day", "phase_id"))
  } else if (method == "late_season") {
    all_events <- events_late
  } else {
    all_events <- events_multiple
  }

  # Add month column
  if (nrow(all_events) > 0) {
    all_events[, month := ceiling(day / 30.44)]
    all_events[month > 12, month := 12]
  }

  # Create summary statistics
  summary_stats <- create_event_summary(all_events)

  # Calculate total observations per year for relative scaling
  total_by_year <- dt[, .(n_total = .N), by = year][order(year)]

  # Create result object
  result <- list(
    events = all_events,
    summary = summary_stats$by_species,
    by_year = summary_stats$by_year,
    by_month = summary_stats$by_month,
    total_by_year = total_by_year,
    reference_stats = ref_stats,
    method = method,
    params = list(
      phases = phases,
      late_threshold = late_threshold,
      gap_threshold = gap_threshold,
      reference_period = reference_period,
      z_threshold = z_threshold
    )
  )

  class(result) <- c("second_events", "list")
  result
}


#' @keywords internal
calculate_reference_windows <- function(dt, reference_period, min_obs) {
  # Calculate expected DOY statistics from reference period
  ref_data <- dt[year %in% reference_period]

  # Group by species and phase
  group_cols <- intersect(c("genus", "species", "phase_id"), names(dt))

  if (length(group_cols) == 0) {
    group_cols <- "phase_id"
  }

  ref_stats <- ref_data[, .(
    n_obs = .N,
    median_doy = as.numeric(median(day, na.rm = TRUE)),
    mad_doy = as.numeric(mad(day, na.rm = TRUE, constant = 1.4826)),
    q05 = as.numeric(quantile(day, 0.05, na.rm = TRUE)),
    q95 = as.numeric(quantile(day, 0.95, na.rm = TRUE)),
    min_doy = as.numeric(min(day, na.rm = TRUE)),
    max_doy = as.numeric(max(day, na.rm = TRUE))
  ), by = group_cols]

  # Only keep groups with sufficient observations
  ref_stats <- ref_stats[n_obs >= min_obs]

  # Calculate expected window (use median +/- 3*MAD or q05-q95)
  ref_stats[, expected_window_start := pmax(q05, median_doy - 3 * mad_doy)]
  ref_stats[, expected_window_end := pmin(q95, median_doy + 3 * mad_doy)]

  ref_stats
}


#' @keywords internal
detect_late_season_events <- function(dt, ref_stats, late_threshold, z_threshold) {
  # Merge reference statistics
  group_cols <- intersect(c("genus", "species", "phase_id"), names(dt))
  if (length(group_cols) == 0) group_cols <- "phase_id"

  # Merge with reference stats
  dt_merged <- merge(dt, ref_stats, by = group_cols, all.x = TRUE)

  # Calculate z-score relative to reference
  dt_merged[, z_score := (day - median_doy) / mad_doy]

  # Flag late season events:
  # 1. DOY > late_threshold AND
  # 2. (z_score > z_threshold OR DOY > expected_window_end)
  dt_merged[, is_second_event :=
              !is.na(median_doy) &
              day > late_threshold &
              (z_score > z_threshold | day > expected_window_end)]

  # Extract flagged events
  events <- dt_merged[is_second_event == TRUE]

  if (nrow(events) > 0) {
    events[, event_type := "late_season"]

    # Keep relevant columns
    cols_keep <- c("s_id", "year", "day", "phase_id", "event_type",
                   "median_doy", "mad_doy", "z_score",
                   "expected_window_start", "expected_window_end")
    cols_keep <- intersect(cols_keep, names(events))

    # Add species info if available
    if ("species" %in% names(events)) cols_keep <- c(cols_keep, "species")
    if ("genus" %in% names(events)) cols_keep <- c(cols_keep, "genus")
    if ("lon" %in% names(events)) cols_keep <- c(cols_keep, "lon")
    if ("lat" %in% names(events)) cols_keep <- c(cols_keep, "lat")
    if ("country" %in% names(events)) cols_keep <- c(cols_keep, "country")

    events <- events[, ..cols_keep]
  }

  events
}


#' @keywords internal
detect_multiple_events <- function(dt, gap_threshold) {
  # Find stations with multiple observations of same phase in same year
  group_cols <- c("s_id", "phase_id", "year")
  if ("species" %in% names(dt)) group_cols <- c(group_cols, "species")

  # Count observations per station-phase-year
  multi <- dt[, .(
    n_events = .N,
    doy_min = min(day, na.rm = TRUE),
    doy_max = max(day, na.rm = TRUE),
    doy_list = list(sort(day))
  ), by = group_cols]

  # Filter to those with multiple observations
  multi <- multi[n_events >= 2]

  # Check if gap between observations exceeds threshold
  multi[, gap_days := doy_max - doy_min]
  multi <- multi[gap_days >= gap_threshold]

  if (nrow(multi) == 0) {
    return(data.table::data.table())
  }

  # Expand back to individual observations and mark as second events
  # Get the later observations (second, third, etc.)
  result_list <- list()

  for (i in seq_len(nrow(multi))) {
    row <- multi[i]
    doys <- unlist(row$doy_list)

    # All observations after the first one are "second events"
    if (length(doys) > 1) {
      second_doys <- doys[-1]  # All but the first

      for (d in second_doys) {
        # Find the original observation
        filter_expr <- dt$s_id == row$s_id &
          dt$phase_id == row$phase_id &
          dt$year == row$year &
          dt$day == d

        if ("species" %in% names(dt)) {
          filter_expr <- filter_expr & dt$species == row$species
        }

        obs <- dt[filter_expr][1]  # Take first match if duplicates

        if (nrow(obs) > 0) {
          obs[, event_type := "multiple_per_year"]
          obs[, doy_1 := doys[1]]  # First (normal) observation
          obs[, doy_2 := d]        # This (second) observation
          obs[, gap_days := d - doys[1]]
          obs[, n_events := length(doys)]
          result_list[[length(result_list) + 1]] <- obs
        }
      }
    }
  }

  if (length(result_list) > 0) {
    events <- data.table::rbindlist(result_list, fill = TRUE)
  } else {
    events <- data.table::data.table()
  }

  events
}


#' @keywords internal
create_event_summary <- function(events) {
  if (nrow(events) == 0) {
    return(list(
      by_species = data.table::data.table(),
      by_year = data.table::data.table(),
      by_month = data.table::data.table()
    ))
  }

  # Summary by species
  group_cols <- intersect(c("species", "phase_id", "event_type"), names(events))
  if (length(group_cols) > 0) {
    by_species <- events[, .(
      n_events = .N,
      n_stations = data.table::uniqueN(s_id),
      n_years = data.table::uniqueN(year),
      mean_doy = mean(day, na.rm = TRUE),
      min_doy = min(day, na.rm = TRUE),
      max_doy = max(day, na.rm = TRUE)
    ), by = group_cols][order(-n_events)]
  } else {
    by_species <- data.table::data.table()
  }

  # Summary by year
  by_year <- events[, .(n_events = .N), by = year][order(year)]

  # Summary by month
  if ("month" %in% names(events)) {
    by_month <- events[, .(n_events = .N), by = month][order(month)]
  } else {
    by_month <- data.table::data.table()
  }

  list(
    by_species = by_species,
    by_year = by_year,
    by_month = by_month
  )
}


#' Print Method for Second Events Detection
#'
#' @param x A \code{second_events} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns \code{x}
#' @export
print.second_events <- function(x, ...) {
  cat("Second/Repeated Phenological Events Detection\n")
  cat(strrep("=", 50), "\n\n")

  cat("Method:", x$method, "\n")
  cat("Phases analyzed:", paste(x$params$phases, collapse = ", "), "\n")

  if (x$method %in% c("late_season", "both")) {
    cat("Late threshold: DOY", x$params$late_threshold,
        sprintf("(~%s)", format(as.Date(x$params$late_threshold - 1, origin = "2000-01-01"), "%b %d")), "\n")
  }
  if (x$method %in% c("multiple_per_year", "both")) {
    cat("Gap threshold:", x$params$gap_threshold, "days\n")
  }

  cat("\n")
  n_events <- nrow(x$events)
  cat(sprintf("Total events detected: %d\n", n_events))

  if (n_events > 0) {
    cat(sprintf("Unique stations: %d\n", data.table::uniqueN(x$events$s_id)))
    cat(sprintf("Year range: %d - %d\n", min(x$events$year), max(x$events$year)))

    if ("event_type" %in% names(x$events)) {
      cat("\nBy event type:\n")
      print(table(x$events$event_type))
    }

    if (nrow(x$by_month) > 0) {
      cat("\nBy month:\n")
      month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      month_tbl <- x$by_month
      if (all(month_tbl$month %in% 1:12)) {
        month_tbl$month_name <- month_names[month_tbl$month]
        print(month_tbl[, .(month_name, n_events)])
      }
    }

    if ("species" %in% names(x$events)) {
      cat("\nTop species:\n")
      species_counts <- x$events[, .(n = .N), by = species][order(-n)]
      print(head(species_counts, 10))
    }
  }

  invisible(x)
}


#' Summary Method for Second Events Detection
#'
#' @param object A \code{second_events} object
#' @param ... Additional arguments (unused)
#' @return The summary data.table (invisibly)
#' @export
summary.second_events <- function(object, ...) {
  cat("Second/Repeated Phenological Events - Summary\n")
  cat(strrep("=", 50), "\n\n")

  if (nrow(object$events) == 0) {
    cat("No events detected.\n")
    return(invisible(NULL))
  }

  cat("Events by species and phase:\n\n")
  print(object$summary)

  cat("\n\nAnnual trend:\n")
  print(object$by_year)

  invisible(object$summary)
}


#' Plot Method for Second Events Detection
#'
#' @param x A \code{second_events} object
#' @param type Character. Plot type: "overview", "timeline", "seasonal", "map"
#' @param scale Character. Scale for y-axis in timeline/overview plots:
#'   \describe{
#'     \item{"absolute"}{(Default) Show raw counts of second events per year.}
#'     \item{"relative"}{Show proportion of second events relative to total
#'       observations per year. This accounts for varying data availability
#'       over time and reveals whether second events are becoming proportionally
#'       more common.}
#'   }
#' @param ... Additional arguments
#' @return A ggplot object (invisibly)
#' @export
#' @import ggplot2
plot.second_events <- function(x, type = c("overview", "timeline", "seasonal", "map"),
                               scale = c("absolute", "relative"), ...) {
  type <- match.arg(type)
  scale <- match.arg(scale)

  events <- x$events

  if (nrow(events) == 0) {
    message("No events to plot")
    return(invisible(NULL))
  }

  if (type == "overview") {
    # Multi-panel overview
    p1 <- ggplot(events, aes(x = day)) +
      geom_histogram(bins = 50, fill = "darkred", alpha = 0.7) +
      geom_vline(xintercept = x$params$late_threshold, linetype = "dashed", color = "black") +
      labs(title = "DOY Distribution of Second Events",
           x = "Day of Year", y = "Count") +
      theme_minimal()

    # Prepare data for timeline panel based on scale
    if (scale == "relative" && !is.null(x$total_by_year)) {
      # Merge with total observations to calculate proportion
      plot_data <- merge(x$by_year, x$total_by_year, by = "year", all.x = TRUE)
      plot_data[, proportion := n_events / n_total * 100]  # as percentage
      p2 <- ggplot(plot_data, aes(x = year, y = proportion)) +
        geom_col(fill = "darkred", alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
        labs(title = "Relative Second Events Over Time",
             x = "", y = "% of Total Observations") +
        theme_minimal()
    } else {
      p2 <- ggplot(x$by_year, aes(x = year, y = n_events)) +
        geom_col(fill = "darkred", alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
        labs(title = "Second Events Over Time",
             x = "", y = "Number of Events") +
        theme_minimal()
    }

    if (nrow(x$by_month) > 0) {
      month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      plot_month <- data.table::copy(x$by_month)
      plot_month[, month_label := month_abbr[month]]
      plot_month[, month_label := factor(month_label, levels = month_abbr)]
      p3 <- ggplot(plot_month, aes(x = month_label, y = n_events)) +
        geom_col(fill = "darkred", alpha = 0.7) +
        scale_x_discrete(drop = TRUE) +
        labs(title = "Monthly Distribution",
             x = "", y = "Count") +
        theme_minimal()
    } else {
      p3 <- ggplot() + theme_void()
    }

    if ("species" %in% names(events)) {
      top_species <- events[, .(n = .N), by = species][order(-n)][1:min(10, .N)]
      top_species[, species := factor(species, levels = rev(species))]
      p4 <- ggplot(top_species, aes(x = n, y = species)) +
        geom_col(fill = "darkred", alpha = 0.7) +
        labs(title = "Top Species",
             x = "Number of Events", y = "") +
        theme_minimal()
    } else {
      p4 <- ggplot() + theme_void()
    }

    combined <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
    print(combined)
    return(invisible(combined))
  }

  if (type == "timeline") {
    if (scale == "relative" && !is.null(x$total_by_year)) {
      # Merge with total observations to calculate proportion
      plot_data <- merge(x$by_year, x$total_by_year, by = "year", all.x = TRUE)
      plot_data[, proportion := n_events / n_total * 100]  # as percentage
      p <- ggplot(plot_data, aes(x = year, y = proportion)) +
        geom_line(color = "darkred", linewidth = 1) +
        geom_point(color = "darkred", size = 2) +
        geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "black") +
        labs(title = "Trend in Second Flowering Events (Relative)",
             subtitle = sprintf("Method: %s, Late threshold: DOY %d | Proportion of total observations",
                                x$method, x$params$late_threshold),
             x = "Year", y = "% of Total Observations") +
        theme_minimal()
    } else {
      p <- ggplot(x$by_year, aes(x = year, y = n_events)) +
        geom_line(color = "darkred", linewidth = 1) +
        geom_point(color = "darkred", size = 2) +
        geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "black") +
        labs(title = "Trend in Second Flowering Events",
             subtitle = sprintf("Method: %s, Late threshold: DOY %d",
                                x$method, x$params$late_threshold),
             x = "Year", y = "Number of Events") +
        theme_minimal()
    }

    print(p)
    return(invisible(p))
  }

  if (type == "seasonal") {
    if (!"month" %in% names(events)) {
      events[, month := ceiling(day / 30.44)]
    }
    month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    events[, month_label := month_abbr[month]]
    events[, month_label := factor(month_label, levels = month_abbr)]

    p <- ggplot(events, aes(x = month_label)) +
      geom_bar(fill = "darkred", alpha = 0.7) +
      scale_x_discrete(drop = TRUE) +
      labs(title = "Seasonal Distribution of Second Events",
           subtitle = "When do 'second flowering' events occur?",
           x = "Month", y = "Number of Events") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if ("species" %in% names(events) && length(unique(events$species)) <= 6) {
      p <- p + facet_wrap(~species)
    }

    print(p)
    return(invisible(p))
  }

  if (type == "map") {
    if (!all(c("lon", "lat") %in% names(events))) {
      stop("Map plot requires 'lon' and 'lat' columns", call. = FALSE)
    }

    station_summary <- events[, .(
      n_events = .N,
      years = paste(sort(unique(year)), collapse = ", "),
      lon = lon[1],
      lat = lat[1]
    ), by = s_id]

    p <- ggplot(station_summary, aes(x = lon, y = lat)) +
      geom_point(aes(size = n_events, color = n_events), alpha = 0.6) +
      scale_color_viridis_c(option = "plasma") +
      scale_size_continuous(range = c(2, 8)) +
      coord_quickmap() +
      labs(title = "Geographic Distribution of Second Events",
           subtitle = sprintf("%d stations with %d total events",
                              nrow(station_summary), sum(station_summary$n_events)),
           x = "Longitude", y = "Latitude",
           size = "Events", color = "Events") +
      theme_minimal()

    print(p)
    return(invisible(p))
  }
}
