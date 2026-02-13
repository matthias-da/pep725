# Prevent R CMD check notes
utils::globalVariables(c("year", "day", "tau_prog", "tau_retr", "is_turning"))

#' Detect Trend Turning Points in Phenological Time Series
#'
#' Applies a sequential Mann-Kendall test to detect approximate trend turning
#' points in phenological time series. This identifies years where trends
#' may have changed direction (e.g., when spring advancement accelerated or
#' reversed).
#'
#' @param pep A \code{pep} object, data.table, or numeric vector of DOY values.
#'   If a data.frame/data.table, must contain \code{year} and \code{day} columns.
#' @param by Character vector of column names to group by before analysis.
#'   Default is \code{NULL} (analyze all data as one series). Common choices:
#'   \code{c("genus", "species", "phase_id")} or \code{c("country", "phase_id")}.
#' @param min_years Integer. Minimum number of years required for analysis.
#'   Default is 10. Shorter series may produce unreliable results.
#' @param aggregate Logical. If \code{TRUE} (default), aggregates multiple
#'   observations per year (e.g., from multiple stations) using the median.
#'   If \code{FALSE}, expects exactly one value per year.
#'
#' @return An object of class \code{pheno_turning} containing:
#'   \describe{
#'     \item{results}{Data.table with columns: year, day (or median_day),
#'       tau_prog (progressive tau), tau_retr (retrograde tau),
#'       is_turning (logical indicating turning points)}
#'     \item{turning_points}{Years identified as potential turning points}
#'     \item{n_years}{Number of years in the series}
#'     \item{by}{Grouping variables used (if any)}
#'     \item{group_results}{If \code{by} is specified, list of results per group}
#'   }
#'
#' @details
#' The sequential Mann-Kendall test calculates two series:
#' \itemize{
#'   \item \strong{Progressive}: Kendall's tau computed from the start forward
#'   \item \strong{Retrograde}: Kendall's tau computed from the end backward
#' }
#'
#' Points where these series cross indicate potential trend turning points.
#' When either series exceeds confidence thresholds (|tau| > 1.96 for 95%,
#' |tau| > 2.58 for 99%) before and after a crossing, the turning point
#' is considered statistically significant.
#'
#' @section Interpretation:
#' \itemize{
#'   \item Positive tau indicates an increasing trend (later DOY = delayed phenology)
#'   \item Negative tau indicates a decreasing trend (earlier DOY = advanced phenology)
#'   \item Crossing points suggest the trend direction may have changed
#' }
#'
#' @examples
#' # Simple vector input (fast)
#' doy_series <- c(120, 118, 122, 115, 110, 108, 112, 105, 102, 100)
#' turning <- pheno_trend_turning(doy_series)
#' print(turning)
#'
#' # Using pep_seed data (no grouping for speed)
#' data(pep_seed)
#' vine <- pep_seed[pep_seed$species == "Vitis vinifera" &
#'                  pep_seed$phase_id == 65, ]
#' if (nrow(vine) > 0) {
#'   turning <- pheno_trend_turning(vine)
#' }
#'
#' @references
#' Sneyers R (1990). On statistical analysis of series of observations.
#' Technical Note No 143. World Meteorological Organization.
#'
#' @seealso
#' \code{\link{pheno_gradient}} for spatial trend analysis,
#' \code{\link{pheno_normals}} for baseline calculations
#'
#' @author Matthias Templ
#' @export
pheno_trend_turning <- function(pep,
                                 by = NULL,
                                 min_years = 10,
                                 aggregate = TRUE) {

 # Handle numeric vector input
 if (is.numeric(pep) && is.null(dim(pep))) {
    x <- pep
    if (length(x) < min_years) {
      stop(sprintf("Time series too short: %d values (minimum %d required)",
                   length(x), min_years), call. = FALSE)
    }
    result <- sequential_mk(x)
    result$year <- seq_along(x)
    class(result) <- c("pheno_turning", "list")
    attr(result, "n_years") <- length(x)
    attr(result, "by") <- NULL
    return(result)
  }

  # Input validation for data.frame
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame, data.table, or numeric vector", call. = FALSE)
  }

  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Check required columns
  if (!all(c("year", "day") %in% names(pep))) {
    stop("Data must contain 'year' and 'day' columns", call. = FALSE)
  }

  # Check grouping columns
  if (!is.null(by)) {
    missing_by <- setdiff(by, names(pep))
    if (length(missing_by) > 0) {
      stop("Column(s) not found: ", paste(missing_by, collapse = ", "), call. = FALSE)
    }
  }

  # Function to analyze a single group
  analyze_group <- function(dt) {
    # Aggregate by year if needed
    if (aggregate) {
      yearly <- dt[, .(median_day = median(day, na.rm = TRUE),
                       n_obs = .N), by = year][order(year)]
    } else {
      yearly <- dt[, .(median_day = day[1], n_obs = 1), by = year][order(year)]
    }

    # Remove NA values
    yearly <- yearly[!is.na(median_day)]

    if (nrow(yearly) < min_years) {
      return(list(
        results = yearly,
        turning_points = integer(0),
        n_years = nrow(yearly),
        error = sprintf("Insufficient years: %d (minimum %d)", nrow(yearly), min_years)
      ))
    }

    # Apply sequential Mann-Kendall
    mk_result <- sequential_mk(yearly$median_day)

    # Combine results
    yearly[, tau_prog := mk_result$progressive]
    yearly[, tau_retr := mk_result$retrograde]
    yearly[, is_turning := mk_result$turning_points]

    list(
      results = yearly,
      turning_points = yearly[is_turning == TRUE, year],
      n_years = nrow(yearly),
      error = NULL
    )
  }

  # Process data
  if (is.null(by)) {
    # Single group analysis
    result <- analyze_group(pep)
    result$by <- NULL
    result$group_results <- NULL
  } else {
    # Grouped analysis
    groups <- unique(pep[, ..by])
    group_results <- list()

    for (i in seq_len(nrow(groups))) {
      # Build filter expression
      filter_expr <- paste(
        sapply(by, function(col) {
          val <- groups[[col]][i]
          if (is.character(val) || is.factor(val)) {
            sprintf("%s == '%s'", col, as.character(val))
          } else {
            sprintf("%s == %s", col, val)
          }
        }),
        collapse = " & "
      )

      group_data <- pep[eval(parse(text = filter_expr))]
      group_label <- paste(sapply(by, function(col) as.character(groups[[col]][i])),
                           collapse = " | ")

      group_results[[group_label]] <- analyze_group(group_data)
      group_results[[group_label]]$group <- groups[i, ]
    }

    # Summarize turning points across groups
    all_turning <- lapply(group_results, function(g) {
      if (length(g$turning_points) > 0) {
        data.table::data.table(
          group = names(group_results)[which(sapply(group_results, identical, g))],
          turning_year = g$turning_points
        )
      }
    })
    all_turning <- data.table::rbindlist(all_turning[!sapply(all_turning, is.null)])

    result <- list(
      results = all_turning,
      turning_points = unique(all_turning$turning_year),
      n_groups = nrow(groups),
      by = by,
      group_results = group_results
    )
  }

  class(result) <- c("pheno_turning", "list")
  result
}


#' Sequential Mann-Kendall Test (Internal)
#'
#' Computes progressive and retrograde normalized Kendall tau series
#' and identifies potential trend turning points.
#'
#' @param x Numeric vector (time series)
#' @return List with progressive, retrograde, and turning_points
#' @keywords internal
sequential_mk <- function(x) {
  n <- length(x)
  if (n < 4) {
    return(list(
      progressive = rep(NA_real_, n),
      retrograde = rep(NA_real_, n),
      turning_points = rep(FALSE, n)
    ))
  }

  # Compute progressive series (forward)
  prog <- numeric(n)
  for (i in 2:n) {
    s <- 0
    for (j in 1:(i - 1)) {
      s <- s + sign(x[i] - x[j])
    }
    # Normalize: E[S] = 0, Var[S] = i*(i-1)*(2i+5)/18
    var_s <- i * (i - 1) * (2 * i + 5) / 18
    prog[i] <- s / sqrt(var_s)
  }
  prog[1] <- 0

  # Compute retrograde series (backward on reversed series)
  x_rev <- rev(x)
  retr_rev <- numeric(n)
  for (i in 2:n) {
    s <- 0
    for (j in 1:(i - 1)) {
      s <- s + sign(x_rev[i] - x_rev[j])
    }
    var_s <- i * (i - 1) * (2 * i + 5) / 18
    retr_rev[i] <- s / sqrt(var_s)
  }
  retr_rev[1] <- 0
  # Reverse back and negate (retrograde should be negative of reversed progressive)
  retr <- -rev(retr_rev)

  # Find turning points (where lines cross)
  turning <- rep(FALSE, n)
  for (i in 2:(n - 1)) {
    # Check for sign change in the difference
    diff_prev <- prog[i - 1] - retr[i - 1]
    diff_curr <- prog[i] - retr[i]
    diff_next <- prog[i + 1] - retr[i + 1]

    # Crossing occurs when sign changes
    if ((diff_prev * diff_curr <= 0) || (diff_curr * diff_next <= 0)) {
      # Additional check: both series should be non-trivial
      if (abs(prog[i]) > 0.5 || abs(retr[i]) > 0.5) {
        turning[i] <- TRUE
      }
    }
  }

  list(
    progressive = prog,
    retrograde = retr,
    turning_points = turning
  )
}


#' Kendall's Normalized Tau
#'
#' Computes Kendall's normalized tau statistic for a time series,
#' providing a non-parametric measure of monotonic trend.
#'
#' @param x Numeric vector (time series assumed to be equidistant)
#'
#' @return Numeric. Kendall's normalized tau statistic.
#'   Values near +1 indicate increasing trend, -1 decreasing trend,
#'   0 no trend. Approximately standard normal under no-trend null.
#'
#' @examples
#' # Decreasing trend (earlier phenology)
#' kendall_tau(c(120, 118, 115, 112, 110, 108, 105))
#'
#' # No clear trend
#' kendall_tau(c(120, 115, 122, 118, 121, 116, 119))
#'
#' @author Matthias Templ
#' @export
kendall_tau <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) {
    return(NA_real_)
  }

  # Calculate Kendall's S
  s <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      s <- s + sign(x[j] - x[i])
    }
  }

  # Variance under null hypothesis
  var_s <- n * (n - 1) * (2 * n + 5) / 18

  # Normalized tau
  tau <- s / sqrt(var_s)

  tau
}


#' Print Method for Trend Turning Analysis
#'
#' @param x A \code{pheno_turning} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns \code{x}
#' @export
print.pheno_turning <- function(x, ...) {
  cat("Phenological Trend Turning Point Analysis\n")
  cat(strrep("=", 50), "\n")

  if (!is.null(x$by)) {
    cat("Grouping: ", paste(x$by, collapse = ", "), "\n")
    cat("Number of groups: ", x$n_groups, "\n\n")

    # Summarize groups with turning points
    groups_with_tp <- sum(sapply(x$group_results, function(g) length(g$turning_points) > 0))
    cat(sprintf("Groups with turning points: %d / %d\n", groups_with_tp, x$n_groups))

    if (nrow(x$results) > 0) {
      cat("\nTurning points detected:\n")
      print(x$results)
    } else {
      cat("\nNo turning points detected.\n")
    }
  } else {
    n_years <- attr(x, "n_years")
    if (is.null(n_years)) n_years <- x$n_years

    cat(sprintf("Years analyzed: %d\n", n_years))
    cat(sprintf("Turning points found: %d\n", length(x$turning_points)))

    if (length(x$turning_points) > 0) {
      cat("Turning point years: ", paste(x$turning_points, collapse = ", "), "\n")
    }

    cat("\nTau statistics:\n")
    cat(sprintf("  Final progressive tau: %.3f\n",
                tail(x$results$tau_prog[!is.na(x$results$tau_prog)], 1)))
    cat(sprintf("  Final retrograde tau: %.3f\n",
                tail(x$results$tau_retr[!is.na(x$results$tau_retr)], 1)))
    cat("\nSignificance thresholds: |tau| > 1.96 (95%), |tau| > 2.58 (99%)\n")
  }

  invisible(x)
}


#' Plot Method for Trend Turning Analysis
#'
#' @param x A \code{pheno_turning} object
#' @param group Character. For grouped analysis, which group to plot.
#'   If \code{NULL} (default), plots the first group or ungrouped results.
#' @param show_thresholds Logical. Show significance threshold lines?
#'   Default \code{TRUE}.
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot object (invisibly)
#' @export
plot.pheno_turning <- function(x, group = NULL, show_thresholds = TRUE, ...) {
  # Get the results to plot
  if (!is.null(x$by) && !is.null(x$group_results)) {
    if (is.null(group)) {
      group <- names(x$group_results)[1]
      message("Plotting first group: ", group)
    }
    if (!group %in% names(x$group_results)) {
      stop("Group '", group, "' not found", call. = FALSE)
    }
    results <- x$group_results[[group]]$results
    title_suffix <- paste0(" (", group, ")")
  } else {
    results <- x$results
    title_suffix <- ""
  }

  if (is.null(results) || nrow(results) == 0) {
    stop("No results to plot", call. = FALSE)
  }

  # Prepare data for plotting
  plot_data <- data.table::melt(
    results,
    id.vars = "year",
    measure.vars = c("tau_prog", "tau_retr"),
    variable.name = "series",
    value.name = "tau"
  )
  plot_data[, series := ifelse(series == "tau_prog", "Progressive", "Retrograde")]

  # Mark turning points
  tp_data <- results[is_turning == TRUE]

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = year, y = tau, color = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.5)

  # Add threshold lines
  if (show_thresholds) {
    p <- p +
      ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                          color = "gray50", alpha = 0.7) +
      ggplot2::geom_hline(yintercept = c(-2.58, 2.58), linetype = "dotted",
                          color = "gray50", alpha = 0.7)
  }

  # Mark turning points
  if (nrow(tp_data) > 0) {
    p <- p +
      ggplot2::geom_vline(xintercept = tp_data$year, linetype = "dashed",
                          color = "red", alpha = 0.5) +
      ggplot2::annotate("point", x = tp_data$year, y = 0,
                        shape = 18, size = 4, color = "red")
  }

  p <- p +
    ggplot2::scale_color_manual(values = c("Progressive" = "steelblue",
                                            "Retrograde" = "darkorange")) +
    ggplot2::labs(
      title = paste0("Sequential Mann-Kendall Trend Analysis", title_suffix),
      subtitle = if (nrow(tp_data) > 0)
        paste("Turning points:", paste(tp_data$year, collapse = ", "))
      else "No turning points detected",
      x = "Year",
      y = "Normalized Kendall Tau",
      color = "Series"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  print(p)
  invisible(p)
}
