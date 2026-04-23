# Prevent R CMD check notes
utils::globalVariables(c("day", "year", "s_id", "is_outlier", "deviation",
                         "expected_doy", "outlier_type", "lon", "lat",
                         "phase_id", "genus", "species", "month", "season",
                         "n_outliers", "pct_outliers", "n_obs", "country",
                         "outlier_category", "..density..",
                         "mean_deviation", "total_outliers",
                         "alt", "theoretical", "sample", "max_abs_dev",
                         "abs_dev",
                         # diagnostic (Mahalanobis branch)
                         "md", "flagged", "mean_md", "max_md", "z",
                         # profile
                         "sy_flagged", "sy_key", "phase_f"))

#' Visualize Phenological Outliers for Inspection
#'
#' Creates diagnostic plots to help distinguish between data errors and
#' biologically meaningful extreme events (e.g., second flowering). Multiple
#' plot types reveal different aspects of outlier patterns.
#'
#' @param x A \code{pep_outliers} object from \code{\link{pep_flag_outliers}}.
#' @param type Character. Type of plot to produce:
#'   \describe{
#'     \item{"overview"}{(Default) Multi-panel overview with distribution,
#'       seasonal pattern, and summary statistics.}
#'     \item{"seasonal"}{Distribution of outliers by month/season, useful for
#'       detecting second flowering (late-season outliers for spring phases).}
#'     \item{"map"}{Geographic distribution of outliers (requires lon/lat).}
#'     \item{"detail"}{Detailed view of individual outlier events with context.}
#'     \item{"station"}{Station-level outlier patterns over time.}
#'     \item{"doy_context"}{Shows outliers in context of full DOY distribution
#'       per phase, highlighting potential second events.}
#'     \item{"diagnostic"}{Paper- and vignette-ready 4-panel figure of
#'       model fit quality: (A) residuals vs fitted; (B) Q-Q plot of
#'       residuals; (C) |residual| vs altitude (if available) or year;
#'       (D) spatial map of maximum |residual| per station. Designed to
#'       accompany \code{method = "gam_residual"} but usable for every
#'       method (uses the \code{deviation} column as the residual). For
#'       \code{method = "mahalanobis"} the panels automatically switch
#'       to MD-specific diagnostics (sorted MD with chi-square
#'       threshold; Q-Q against \eqn{\chi^2_p}; mean / max MD over time;
#'       per-station worst-case MD map).}
#'     \item{"profile"}{Parallel-coordinates plot of the phase profile
#'       (one line per station-year across phases), with flagged
#'       station-years highlighted and the robust central profile
#'       overlaid. Designed for \code{method = "mahalanobis"} where the
#'       outlierness lives in the *shape* of a station-year across
#'       phases, not in any single DOY. The primary paper figure for
#'       the multivariate method.}
#'   }
#' @param phase_id Optional integer vector to filter specific phases for plotting.
#' @param outlier_only Logical. If TRUE (default for some types), show only
#'   outlier observations. If FALSE, show all data with outliers highlighted.
#' @param late_threshold Integer. DOY threshold for classifying "late" outliers
#'   as potential second events (default 250, ~September 7).
#' @param n_top Integer. For detail view, number of most extreme outliers to show.
#'   Default 20.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A \code{ggplot} object (or list of ggplots for "overview").
#'
#' @details
#' This function is designed to support quality control workflows where the

#' goal is to distinguish:
#' \enumerate{
#'   \item \strong{Data errors}: Typos, wrong dates, mis-coded phases
#'   \item \strong{Observer errors}: Misidentified species or phases
#'   \item \strong{Biologically meaningful extremes}: Second flowering, delayed
#'     development, climate-driven anomalies
#' }
#'
#' For detecting potential \strong{second flowering events}:
#' \itemize{
#'   \item Use \code{type = "seasonal"} to see if late-season outliers cluster
#'     in autumn months
#'   \item Use \code{type = "doy_context"} to visualize outliers relative to
#'     the main flowering distribution
#'   \item Focus on flowering phases (60, 65) with late DOY values (>250)
#' }
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Use Swiss subset for faster outlier detection
#' pep_ch <- pep[country == "Switzerland"]
#' pep_flagged <- pep_flag_outliers(pep_ch)
#'
#' # Overview of all outliers
#' pep_plot_outliers(pep_flagged, type = "overview")
#'
#' # Seasonal distribution - look for autumn outliers in flowering phases
#' pep_plot_outliers(pep_flagged, type = "seasonal", phase_id = c(60, 65))
#'
#' # Geographic pattern
#' pep_plot_outliers(pep_flagged, type = "map")
#'
#' # Detailed view of most extreme outliers
#' pep_plot_outliers(pep_flagged, type = "detail", n_top = 30)
#'
#' # DOY context for flowering phase - spot potential second flowering
#' pep_plot_outliers(pep_flagged, type = "doy_context", phase_id = 65)
#' }
#'
#' @seealso \code{\link{pep_flag_outliers}} for detecting outliers,
#'   \code{\link{pep_quality}} for comprehensive quality assessment
#'
#' @author Matthias Templ
#' @export
#' @import ggplot2
pep_plot_outliers <- function(x,
                          type = c("overview", "seasonal", "map", "detail",
                                   "station", "doy_context", "diagnostic",
                                   "profile"),
                          phase_id = NULL,
                          outlier_only = NULL,
                          late_threshold = 250,
                          n_top = 20,
                          ...) {

  type <- match.arg(type)

  # Validate input

  if (!inherits(x, "pep_outliers")) {
    stop("'x' must be a pep_outliers object from pep_flag_outliers()", call. = FALSE)
  }

  # Get attributes
  method <- attr(x, "method")
  threshold <- attr(x, "threshold")

  # Filter by phase if requested
  if (!is.null(phase_id)) {
    if (!"phase_id" %in% names(x)) {
      warning("'phase_id' column not found, ignoring filter", call. = FALSE
      )
    } else {
      x <- x[x$phase_id %in% phase_id, ]
    }
  }

  # Add helper columns
  dt <- data.table::copy(x)

  # Add month from DOY (approximate)
  dt[, month := ceiling(day / 30.44)]
  dt[month > 12, month := 12]
  dt[month < 1, month := 1]

  # Add season
  dt[, season := data.table::fcase(
    month %in% c(12, 1, 2), "Winter",
    month %in% 3:5, "Spring",
    month %in% 6:8, "Summer",
    month %in% 9:11, "Autumn"
  )]
  dt[, season := factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))]

  # Categorize outliers for potential second events
  dt[, outlier_category := data.table::fcase(
    !is_outlier | is.na(is_outlier), "Normal",
    is_outlier & deviation < 0, "Early outlier",
    is_outlier & deviation > 0 & day < late_threshold, "Late outlier",
    is_outlier & deviation > 0 & day >= late_threshold, "Very late (potential 2nd event)"
  )]
  dt[, outlier_category := factor(outlier_category,
                                   levels = c("Normal", "Early outlier",
                                              "Late outlier", "Very late (potential 2nd event)"))]

  # ============================================================
  # Plot type: OVERVIEW
  # ============================================================
  if (type == "overview") {
    return(pep_plot_outliers_overview(dt, method, threshold, late_threshold))
  }

  # ============================================================
  # Plot type: SEASONAL
  # ============================================================
  if (type == "seasonal") {
    return(pep_plot_outliers_seasonal(dt, late_threshold))
  }

  # ============================================================
  # Plot type: MAP
  # ============================================================
  if (type == "map") {
    return(pep_plot_outliers_map(dt))
  }

  # ============================================================
  # Plot type: DETAIL
  # ============================================================
  if (type == "detail") {
    return(pep_plot_outliers_detail(dt, n_top))
  }

  # ============================================================
  # Plot type: STATION
  # ============================================================
  if (type == "station") {
    return(pep_plot_outliers_station(dt))
  }

  # ============================================================
  # Plot type: DOY_CONTEXT
  # ============================================================
  if (type == "doy_context") {
    return(pep_plot_outliers_doy_context(dt, late_threshold))
  }

  # ============================================================
  # Plot type: DIAGNOSTIC (paper/vignette figure)
  # ============================================================
  if (type == "diagnostic") {
    return(pep_plot_outliers_diagnostic(dt, method, threshold))
  }

  # ============================================================
  # Plot type: PROFILE (phase-profile parallel coordinates)
  # ============================================================
  if (type == "profile") {
    return(pep_plot_outliers_profile(dt, method, threshold))
  }
}


#' @keywords internal
pep_plot_outliers_profile <- function(dt, method, threshold) {
  # Parallel-coordinates plot of phase profiles. Each station-year
  # contributes one line across the BBCH phases; flagged station-years
  # (by any detector, but this plot is designed for Mahalanobis) are
  # highlighted in red.
  if (!all(c("phase_id", "day", "s_id", "year") %in% names(dt))) {
    stop("Profile plot requires 's_id', 'year', 'phase_id', 'day' columns.",
         call. = FALSE)
  }

  dt <- data.table::copy(data.table::as.data.table(dt))
  # is_outlier may be NA for rows the detector didn't reach; treat as FALSE
  # for the plot (we want a single flag per station-year).
  dt[is.na(is_outlier), is_outlier := FALSE]

  # Per station-year: flagged if ANY phase row is flagged.
  dt[, sy_flagged := any(is_outlier), by = .(s_id, year)]
  dt[, sy_key := paste(s_id, year, sep = "_")]
  dt[, phase_f := factor(phase_id)]

  # Robust per-phase centre (median across all station-years) as reference
  # — this is what the MCD estimator is (approximately) targeting for the
  # centre of the ellipsoid.
  ref <- dt[, .(day = stats::median(day, na.rm = TRUE)), by = phase_f]
  ref[, sy_key := "__reference__"]
  ref[, sy_flagged := FALSE]
  ref[, is_outlier := FALSE]

  # Draw flagged on top of non-flagged; reference as a thick black line.
  non_flag <- dt[sy_flagged == FALSE]
  flag     <- dt[sy_flagged == TRUE]

  p <- ggplot(non_flag,
              aes(x = phase_f, y = day, group = sy_key)) +
    geom_line(color = "gray70", alpha = 0.35, linewidth = 0.3) +
    geom_line(data = flag,
              aes(x = phase_f, y = day, group = sy_key),
              color = "red", alpha = 0.8, linewidth = 0.7) +
    geom_line(data = ref,
              aes(x = phase_f, y = day, group = sy_key),
              color = "black", linewidth = 1.2, alpha = 0.9) +
    geom_point(data = ref,
               aes(x = phase_f, y = day),
               color = "black", size = 2.5) +
    labs(
      x = "BBCH phase",
      y = "Day of year (DOY)",
      title = "Phase-profile parallel coordinates",
      subtitle = sprintf(
        paste0("%d station-years (red = flagged%s; black = robust ",
               "median profile)"),
        data.table::uniqueN(dt[, .(s_id, year)]),
        if (identical(method, "mahalanobis"))
          sprintf(", MD threshold = %s",
                  format(threshold, digits = 3)) else "")
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray30", size = 9))

  # Facet by species if available and > 1 species.
  if ("species" %in% names(dt) &&
      data.table::uniqueN(dt$species) > 1) {
    p <- p + facet_wrap(~species, scales = "free_y")
  }
  p
}


#' @keywords internal
pep_plot_outliers_diagnostic <- function(dt, method, threshold) {
  # Paper-ready 4-panel diagnostic. For the Mahalanobis method the
  # `deviation` column holds unitless distances (not residuals in days)
  # and `expected_doy` is NA, so the residual/fitted/Q-Q axes don't
  # apply — we replace them with panels tailored to the MD output.
  is_mahal <- identical(method, "mahalanobis")
  dt <- data.table::copy(dt)
  dt[, abs_dev := abs(deviation)]

  if (is_mahal) {
    # --- Mahalanobis-specific diagnostic ---------------------------------
    # Panel A: sorted MD with the chi-square-based threshold highlighted.
    sy <- dt[, .(md = deviation[1],
                 flagged = any(is_outlier, na.rm = TRUE)),
             by = .(s_id, year)]
    sy <- sy[order(md)]
    sy[, rank := seq_len(.N)]
    pA <- ggplot(sy, aes(x = rank, y = md, color = flagged)) +
      geom_point(alpha = 0.6, size = 0.8) +
      geom_hline(yintercept = threshold, linetype = "dashed",
                 color = "red", linewidth = 0.5) +
      scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray50"),
                         na.value = "gray80") +
      labs(x = "Station-year rank (sorted)",
           y = "Robust Mahalanobis distance",
           title = "A. Sorted MD with chi-sq threshold") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 10))

    # Panel B: MD^2 vs chi-square quantiles (Q-Q plot of squared MD).
    # Under multivariate normality MD^2 ~ chi^2_p. We don't know p for
    # sure here; infer it from the number of distinct phases.
    p_dim <- data.table::uniqueN(dt$phase_id)
    md_sq <- sort(sy$md[is.finite(sy$md)]^2)
    qq_df <- data.frame(
      sample = md_sq,
      theoretical = stats::qchisq(stats::ppoints(length(md_sq)), df = p_dim)
    )
    pB <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
      geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "gray40") +
      labs(x = sprintf("Theoretical chi^2_%d quantiles", p_dim),
           y = "Observed MD^2",
           title = sprintf("B. Q-Q against chi^2_%d", p_dim)) +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", size = 10))

    # Panel C: MD over time, aggregated to per-year mean (highlights
    # epochs with anomalous observations across the network).
    if ("year" %in% names(dt)) {
      yr <- sy[, .(mean_md = mean(md, na.rm = TRUE),
                   max_md  = max(md, na.rm = TRUE),
                   n_flag  = sum(flagged)), by = year][order(year)]
      pC <- ggplot(yr, aes(x = year)) +
        geom_line(aes(y = mean_md), color = "steelblue", linewidth = 0.6) +
        geom_line(aes(y = max_md),  color = "red",
                  linewidth = 0.4, linetype = "dotted") +
        geom_hline(yintercept = threshold, linetype = "dashed",
                   color = "red", linewidth = 0.3) +
        labs(x = "Year", y = "MD (mean / max per year)",
             title = "C. Mean and max MD over time") +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(face = "bold", size = 10))
    } else {
      pC <- ggplot() + theme_void() + labs(title = "C. year unavailable")
    }
  } else {
    # --- Residual-based methods (default path) --------------------------
    # Panel A: residual vs fitted (expected DOY).
    if (all(!is.na(dt$expected_doy))) {
      pA <- ggplot(dt, aes(x = expected_doy, y = deviation)) +
        geom_point(aes(color = is_outlier), alpha = 0.35, size = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
        ggplot2::geom_smooth(method = "loess", se = FALSE,
                             color = "steelblue", linewidth = 0.6) +
        scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "red"),
                           labels = c("Normal", "Flagged"),
                           na.value = "gray80") +
        labs(x = "Fitted / expected DOY", y = "Residual (days)",
             color = NULL,
             title = "A. Residuals vs fitted") +
        theme_minimal(base_size = 10) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", size = 10))
    } else {
      pA <- ggplot() + theme_void() +
        labs(title = "A. Residuals vs fitted: not available")
    }

    # Panel B: Q-Q plot of standardised residuals against Normal.
    dev_std <- (dt$deviation - stats::median(dt$deviation, na.rm = TRUE)) /
      stats::mad(dt$deviation, na.rm = TRUE)
    qq_df <- data.frame(z = sort(dev_std[is.finite(dev_std)]))
    qq_df$theoretical <- stats::qnorm(stats::ppoints(nrow(qq_df)))
    pB <- ggplot(qq_df, aes(x = theoretical, y = z)) +
      geom_point(alpha = 0.3, size = 0.7, color = "steelblue") +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "gray40") +
      labs(x = "Theoretical N(0,1) quantiles",
           y = "Robust-z of residuals",
           title = "B. Q-Q: residual tail behaviour") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", size = 10))

    # Panel C: |residual| vs altitude (if present) or year (fallback).
    covariate <- if ("alt" %in% names(dt) && any(!is.na(dt$alt))) "alt" else
      if ("year" %in% names(dt)) "year" else NULL
    if (!is.null(covariate)) {
      pC <- ggplot(dt, aes(x = .data[[covariate]], y = abs_dev)) +
        geom_point(aes(color = is_outlier), alpha = 0.3, size = 0.7) +
        ggplot2::geom_smooth(method = "loess", se = FALSE,
                             color = "steelblue", linewidth = 0.6) +
        scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "red"),
                           na.value = "gray80") +
        labs(x = switch(covariate,
                        alt = "Altitude (m)",
                        year = "Year"),
             y = "|Residual| (days)",
             title = sprintf("C. |residual| vs %s",
                             switch(covariate, alt = "altitude", year = "year"))) +
        theme_minimal(base_size = 10) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", size = 10))
    } else {
      pC <- ggplot() + theme_void() +
        labs(title = "C. covariate unavailable")
    }
  }

  # Panel D: spatial map of max |residual| (or max MD) per station.
  d_label <- if (is_mahal) "Max robust MD" else "Max |residual|"
  d_title <- if (is_mahal) "D. Per-station worst-case MD"
             else         "D. Per-station worst-case residual"
  if (all(c("lon", "lat") %in% names(dt))) {
    station_max <- dt[, .(max_abs_dev = max(abs_dev, na.rm = TRUE),
                          lon = lon[1], lat = lat[1]), by = s_id]
    pD <- ggplot(station_max, aes(x = lon, y = lat)) +
      geom_point(aes(color = max_abs_dev), size = 1.6, alpha = 0.7) +
      ggplot2::scale_color_viridis_c(option = "magma", direction = -1,
                                     name = d_label) +
      coord_quickmap() +
      labs(x = "Longitude", y = "Latitude",
           title = d_title) +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", size = 10),
            legend.position = "right")
  } else {
    pD <- ggplot() + theme_void() +
      labs(title = "D. spatial map unavailable (no lon/lat)")
  }

  subtitle <- sprintf("method = %s, threshold = %s",
                      method, format(threshold, digits = 3))
  combined <- patchwork::wrap_plots(pA, pB, pC, pD, ncol = 2) +
    patchwork::plot_annotation(
      title = "Outlier-detection diagnostic",
      subtitle = subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        plot.subtitle = ggplot2::element_text(color = "gray30", size = 9)
      )
    )
  combined
}


# ============================================================
# Internal helper functions
# ============================================================

#' @keywords internal
pep_plot_outliers_overview <- function(dt, method, threshold, late_threshold) {

  # Calculate summary statistics
  n_total <- nrow(dt)
  n_outliers <- sum(dt$is_outlier, na.rm = TRUE)
  n_early <- sum(dt$outlier_category == "Early outlier", na.rm = TRUE)
  n_late <- sum(dt$outlier_category == "Late outlier", na.rm = TRUE)
  n_very_late <- sum(dt$outlier_category == "Very late (potential 2nd event)", na.rm = TRUE)

  # Panel 1: Deviation histogram
  p1 <- ggplot(dt, aes(x = deviation, fill = outlier_category)) +
    geom_histogram(bins = 60, alpha = 0.8) +
    scale_fill_manual(
      values = c("Normal" = "gray70",
                 "Early outlier" = "steelblue",
                 "Late outlier" = "orange",
                 "Very late (potential 2nd event)" = "red"),
      drop = FALSE
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = "Deviation from Expected DOY",
      x = "Deviation (days)",
      y = "Count",
      fill = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))

  # Panel 2: Seasonal distribution of outliers only
  outliers_only <- dt[is_outlier == TRUE]
  if (nrow(outliers_only) > 0) {
    p2 <- ggplot(outliers_only, aes(x = month, fill = outlier_category)) +
      geom_bar(alpha = 0.8) +
      scale_x_continuous(breaks = 1:12,
                         labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
      scale_fill_manual(
        values = c("Early outlier" = "steelblue",
                   "Late outlier" = "orange",
                   "Very late (potential 2nd event)" = "red"),
        drop = FALSE
      ) +
      labs(
        title = "Outliers by Month",
        subtitle = "Red = potential second events",
        x = "Month",
        y = "Count",
        fill = ""
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  } else {
    p2 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No outliers found") +
      theme_void()
  }

  # Panel 3: DOY distribution with outliers highlighted
  p3 <- ggplot(dt, aes(x = day, fill = is_outlier)) +
    geom_histogram(bins = 73, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("FALSE" = "gray60", "TRUE" = "red"),
                      labels = c("Normal", "Outlier"),
                      na.value = "gray80") +
    geom_vline(xintercept = late_threshold, linetype = "dashed",
               color = "darkred", linewidth = 0.8) +
    annotate("text", x = late_threshold + 5, y = Inf, vjust = 2,
             label = paste0("Late threshold (DOY ", late_threshold, ")"),
             color = "darkred", size = 3, hjust = 0) +
    labs(
      title = "Day of Year Distribution",
      x = "Day of Year",
      y = "Count",
      fill = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Panel 4: Summary text
  summary_text <- sprintf(
    "OUTLIER SUMMARY\n\nTotal observations: %s\nTotal outliers: %d (%.1f%%)\n\nEarly outliers: %d\nLate outliers: %d\nVery late (DOY >= %d): %d\n\nMethod: %s\nThreshold: %s",
    format(n_total, big.mark = ","),
    n_outliers,
    100 * n_outliers / n_total,
    n_early,
    n_late,
    late_threshold,
    n_very_late,
    method,
    threshold
  )

  p4 <- ggplot() +
    annotate("text", x = 0, y = 0.5, label = summary_text,
             hjust = 0, vjust = 0.5, size = 3.5, family = "mono") +
    xlim(-0.1, 1) + ylim(0, 1) +
    theme_void() +
    labs(title = "Summary Statistics")

  # Combine panels
  combined <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
  print(combined)
  invisible(list(deviation = p1, monthly = p2, doy = p3, summary = p4))
}


#' @keywords internal
pep_plot_outliers_seasonal <- function(dt, late_threshold) {

  outliers_only <- dt[is_outlier == TRUE]

  if (nrow(outliers_only) == 0) {
    message("No outliers to plot")
    return(invisible(NULL))
  }

  # By month and category
  p1 <- ggplot(outliers_only, aes(x = factor(month), fill = outlier_category)) +
    geom_bar(position = "stack", alpha = 0.8) +
    scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")) +
    scale_fill_manual(
      values = c("Early outlier" = "steelblue",
                 "Late outlier" = "orange",
                 "Very late (potential 2nd event)" = "red")
    ) +
    labs(
      title = "Seasonal Distribution of Outliers",
      subtitle = sprintf("Red bars (DOY >= %d) may indicate second flowering or other repeated events", late_threshold),
      x = "Month",
      y = "Number of Outliers",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")

  # If phase_id available, facet by phase
  if ("phase_id" %in% names(outliers_only) && length(unique(outliers_only$phase_id)) > 1) {
    p1 <- p1 + facet_wrap(~phase_id, scales = "free_y")
  }

  print(p1)
  invisible(p1)
}


#' @keywords internal
pep_plot_outliers_map <- function(dt) {

  if (!all(c("lon", "lat") %in% names(dt))) {
    stop("Map plot requires 'lon' and 'lat' columns", call. = FALSE)
  }

  outliers_only <- dt[is_outlier == TRUE]

  if (nrow(outliers_only) == 0) {
    message("No outliers to plot on map")
    return(invisible(NULL))
  }

  # Aggregate by station
  station_summary <- outliers_only[, .(
    n_outliers = .N,
    mean_deviation = mean(abs(deviation), na.rm = TRUE),
    lon = lon[1],
    lat = lat[1]
  ), by = s_id]

  p <- ggplot(station_summary, aes(x = lon, y = lat)) +
    geom_point(aes(size = n_outliers, color = mean_deviation), alpha = 0.6) +
    scale_color_viridis_c(option = "plasma", name = "Mean |deviation|") +
    scale_size_continuous(range = c(1, 8), name = "N outliers") +
    coord_quickmap() +
    labs(
      title = "Geographic Distribution of Outliers",
      subtitle = sprintf("Showing %d stations with outliers", nrow(station_summary)),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal()

  print(p)
  invisible(p)
}


#' @keywords internal
pep_plot_outliers_detail <- function(dt, n_top) {

  outliers_only <- dt[is_outlier == TRUE]

  if (nrow(outliers_only) == 0) {
    message("No outliers to show in detail")
    return(invisible(NULL))
  }

  # Get most extreme outliers
  outliers_only <- outliers_only[order(-abs(deviation))]
  top_outliers <- head(outliers_only, n_top)

  # Create label for each observation
  if ("species" %in% names(top_outliers)) {
    top_outliers[, label := sprintf("%s\n%s (phase %s)\nYear: %d",
                                     s_id, species, phase_id, year)]
  } else {
    top_outliers[, label := sprintf("%s (phase %s)\nYear: %d",
                                     s_id, phase_id, year)]
  }

  # Order by deviation
  top_outliers[, label := factor(label, levels = rev(label))]

  p <- ggplot(top_outliers, aes(x = deviation, y = label, fill = outlier_category)) +
    geom_col(alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    scale_fill_manual(
      values = c("Early outlier" = "steelblue",
                 "Late outlier" = "orange",
                 "Very late (potential 2nd event)" = "red")
    ) +
    geom_text(aes(label = sprintf("DOY: %d", day)),
              hjust = ifelse(top_outliers$deviation > 0, -0.1, 1.1),
              size = 2.5) +
    labs(
      title = sprintf("Top %d Most Extreme Outliers", n_top),
      subtitle = "Deviation from expected DOY for station-species-phase combination",
      x = "Deviation (days)",
      y = "",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 7),
          legend.position = "bottom")

  print(p)
  invisible(p)
}


#' @keywords internal
pep_plot_outliers_station <- function(dt) {

  if (!all(c("year", "s_id") %in% names(dt))) {
    stop("Station plot requires 'year' and 's_id' columns", call. = FALSE)
  }

  # Summarize by station and year
  station_year <- dt[, .(
    n_obs = .N,
    n_outliers = sum(is_outlier, na.rm = TRUE),
    pct_outliers = 100 * sum(is_outlier, na.rm = TRUE) / .N
  ), by = .(s_id, year)]

  # Find stations with most outliers
  station_totals <- station_year[, .(total_outliers = sum(n_outliers)), by = s_id]
  top_stations <- head(station_totals[order(-total_outliers)], 20)$s_id

  station_year_top <- station_year[s_id %in% top_stations]

  p <- ggplot(station_year_top, aes(x = year, y = s_id, fill = pct_outliers)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", name = "Outliers (%)") +
    labs(
      title = "Outlier Patterns by Station Over Time",
      subtitle = "Top 20 stations by total outliers",
      x = "Year",
      y = "Station ID"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6))

  print(p)
  invisible(p)
}


#' @keywords internal
pep_plot_outliers_doy_context <- function(dt, late_threshold) {

  # Show full DOY distribution with outliers marked
  p <- ggplot(dt, aes(x = day)) +
    geom_density(fill = "gray70", alpha = 0.5) +
    geom_rug(data = dt[is_outlier == TRUE],
             aes(color = outlier_category), alpha = 0.7, linewidth = 1) +
    scale_color_manual(
      values = c("Early outlier" = "steelblue",
                 "Late outlier" = "orange",
                 "Very late (potential 2nd event)" = "red")
    ) +
    geom_vline(xintercept = late_threshold, linetype = "dashed",
               color = "darkred", linewidth = 0.8) +
    annotate("text", x = late_threshold, y = Inf,
             label = sprintf(" DOY %d\n (potential 2nd events)", late_threshold),
             hjust = 0, vjust = 1.5, color = "darkred", size = 3) +
    labs(
      title = "DOY Distribution with Outliers",
      subtitle = "Density shows normal observations; rug marks show outlier positions",
      x = "Day of Year",
      y = "Density",
      color = "Outlier type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Facet by phase if multiple phases
  if ("phase_id" %in% names(dt) && length(unique(dt$phase_id)) > 1) {
    p <- p + facet_wrap(~phase_id, scales = "free_y")
  }

  print(p)
  invisible(p)
}
