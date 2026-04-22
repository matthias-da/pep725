# Prevent R CMD check notes
utils::globalVariables(c("year", "day", "tau_prog", "tau_retr", "is_turning", "..by"))

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
    mk_result <- sequential_mk(x)
    yearly <- data.table::data.table(
      year = seq_along(x),
      median_day = x,
      n_obs = 1L,
      tau_prog = mk_result$progressive,
      tau_retr = mk_result$retrograde,
      is_turning = mk_result$turning_points
    )
    result <- list(
      results = yearly,
      turning_points = yearly[is_turning == TRUE, year],
      n_years = length(x),
      error = NULL,
      by = NULL,
      group_results = NULL
    )
    class(result) <- c("pheno_turning", "list")
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
      group_data <- pep[groups[i, , drop = FALSE], on = by, nomatch = NULL]
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


#' Sequential Mann-Kendall Test (Sneyers 1975, internal)
#'
#' Computes the Sneyers (1975) progressive and retrograde standardised
#' rank-count statistics \eqn{u(t)} and \eqn{u'(t)} for a time series, and
#' identifies potential trend turning points as intersections of the two
#' curves inside the non-significance band.
#'
#' Let \eqn{n_i} denote the number of \eqn{j < i} with \eqn{x_i > x_j},
#' and \eqn{t_i = \sum_{k=1}^{i} n_k}. Under the null hypothesis of no
#' trend, \eqn{E[t_i] = i(i-1)/4} and \eqn{\mathrm{Var}[t_i] = i(i-1)(2i+5)/72}.
#' The progressive statistic is
#' \eqn{u(t_i) = (t_i - E[t_i])/\sqrt{\mathrm{Var}[t_i]}}.
#' The retrograde \eqn{u'(t)} is the same procedure applied to the
#' reversed series, then reversed and negated so both curves fluctuate
#' around zero under stationarity.
#'
#' A turning point is flagged where the sign of \eqn{u - u'} changes
#' between two consecutive indices and both curves are within the 95\%
#' non-significance band \eqn{|z| < 1.96} at the crossing.
#'
#' Previous versions of this function (pep725 <= 1.0.2) used
#' \eqn{\mathrm{Var} = i(i-1)(2i+5)/18} with a per-index (non-cumulative)
#' \eqn{S} statistic and an ad-hoc \eqn{|u| > 0.5} threshold for the
#' turning-point filter. That formulation did not match Sneyers' procedure.
#'
#' @param x Numeric vector (time series assumed to be equidistant;
#'   callers should drop or impute NAs beforehand).
#' @return List with \code{progressive}, \code{retrograde} (both length
#'   \code{length(x)}), and \code{turning_points} (logical).
#' @references
#' Sneyers, R. (1975). Sur l'analyse statistique des séries d'observations.
#' WMO Technical Note No. 143, WMO No. 415. Geneva: WMO.
#'
#' Moraes, J. M. et al. (1998). Trends in hydrological parameters of a
#' Southern Brazilian watershed. \emph{Ambio} 27:302-307.
#' @keywords internal
sequential_mk <- function(x) {
  n <- length(x)
  if (n < 4) {
    return(list(
      progressive = rep(NA_real_, n),
      retrograde  = rep(NA_real_, n),
      turning_points = rep(FALSE, n)
    ))
  }

  # Rank-count helper: n_i = number of j < i with x_i > x_j.
  rank_counts <- function(v) {
    m <- length(v)
    out <- integer(m)
    for (i in seq_len(m)) {
      if (i == 1L) next
      out[i] <- sum(v[i] > v[seq_len(i - 1L)])
    }
    out
  }

  i_seq <- seq_len(n)
  E_t <- i_seq * (i_seq - 1) / 4
  V_t <- i_seq * (i_seq - 1) * (2 * i_seq + 5) / 72

  # Progressive series
  t_prog <- cumsum(rank_counts(x))
  prog <- (t_prog - E_t) / sqrt(V_t)
  prog[1] <- 0  # Var is zero at i = 1; by convention the curve starts at 0

  # Retrograde: compute u on the reversed series, then reverse the
  # resulting curve (no negation). Under H0 both u and u' fluctuate
  # around zero; under a monotone trend they diverge in opposite
  # directions and do not cross; at a trend change point (transition
  # from stationary to trending, or vice versa) they intersect inside
  # the 95% confidence band.
  t_retr_rev <- cumsum(rank_counts(rev(x)))
  retr_rev <- (t_retr_rev - E_t) / sqrt(V_t)
  retr_rev[1] <- 0
  retr <- rev(retr_rev)

  # Turning points: u and u' cross inside the 95% non-significance band.
  turning <- rep(FALSE, n)
  diff_pr <- prog - retr
  for (i in 2:(n - 1)) {
    if (!is.finite(diff_pr[i]) || !is.finite(diff_pr[i + 1])) next
    if (diff_pr[i] * diff_pr[i + 1] <= 0 &&
        abs(prog[i]) < 1.96 && abs(retr[i]) < 1.96) {
      turning[i] <- TRUE
    }
  }

  list(
    progressive = prog,
    retrograde  = retr,
    turning_points = turning
  )
}


#' Mann-Kendall Z-Statistic
#'
#' Computes the Mann-Kendall Z-statistic for a time series, a non-parametric
#' test of monotonic trend. The Z-statistic is approximately standard normal
#' under the no-trend null hypothesis.
#'
#' The implementation uses the standard tie correction for \code{Var(S)} (so
#' that tied observations do not inflate the test) and the continuity
#' correction \code{Z = (S - sign(S))/sqrt(Var(S))}, matching
#' \code{Kendall::MannKendall()}.
#'
#' @param x Numeric vector (time series assumed to be equidistant).
#'   \code{NA} values are silently removed.
#'
#' @return Numeric. The Mann-Kendall Z-statistic. Positive values indicate an
#'   increasing trend, negative values decreasing. Compare against standard
#'   normal quantiles (e.g., \code{|Z| > 1.96} for \code{p < 0.05}).
#'   Returns \code{NA} if fewer than three non-missing values.
#'
#' @section Note on \code{kendall_tau()}:
#' Earlier versions of pep725 exported a function called \code{kendall_tau()}
#' that actually returned this Mann-Kendall Z-statistic (not the true
#' Kendall's \eqn{\tau} = \eqn{S / (n(n-1)/2)}). \code{kendall_tau()} is now
#' a deprecated alias of \code{mann_kendall_z()}; please update callers.
#'
#' @references
#' Mann, H.B. (1945). Nonparametric tests against trend. \emph{Econometrica}
#' 13, 245-259.
#'
#' Kendall, M.G. (1975). \emph{Rank Correlation Methods}. 4th ed. London:
#' Charles Griffin.
#'
#' @examples
#' # Decreasing trend (earlier phenology)
#' mann_kendall_z(c(120, 118, 115, 112, 110, 108, 105))
#'
#' # No clear trend
#' mann_kendall_z(c(120, 115, 122, 118, 121, 116, 119))
#'
#' @seealso \code{\link{kendall_tau}} (deprecated alias);
#'   \code{\link{pheno_trend_turning}} for a full sequential Mann-Kendall
#'   analysis.
#' @author Matthias Templ
#' @export
mann_kendall_z <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) {
    return(NA_real_)
  }

  # Vectorised S = sum over i<j of sign(x_j - x_i).
  #   outer(x, x, "-")[i, j] = x[i] - x[j]
  # The *lower* triangle holds entries with i > j, i.e. sign(x[i] - x[j])
  # for i > j, which after re-labelling is the same multiset of pairs we
  # want (sign(x[j] - x[i]) for i < j).
  d <- outer(x, x, "-")
  s <- sum(sign(d[lower.tri(d)]))

  # Tie correction: if ties are present, reduce Var(S) by
  #   sum over tie groups g of t_g (t_g - 1) (2 t_g + 5) / 18.
  tie_counts <- tabulate(match(x, unique(x)))
  tie_counts <- tie_counts[tie_counts > 1]
  tie_term <- sum(tie_counts * (tie_counts - 1) * (2 * tie_counts + 5)) / 18

  var_s <- (n * (n - 1) * (2 * n + 5) - tie_term * 18) / 18

  # Continuity correction: subtract/add 1 from S before standardising.
  if (s > 0) {
    z <- (s - 1) / sqrt(var_s)
  } else if (s < 0) {
    z <- (s + 1) / sqrt(var_s)
  } else {
    z <- 0
  }

  z
}


#' Mann-Kendall Z-Statistic (deprecated alias for \code{mann_kendall_z})
#'
#' This function was misnamed in pep725 <= 1.0.2: it returned the
#' Mann-Kendall Z-statistic, not Kendall's \eqn{\tau = S / (n(n-1)/2)}.
#' Please call \code{\link{mann_kendall_z}} instead for identical (and
#' better: tie-corrected, continuity-corrected) behaviour.
#'
#' Note that this function is now a thin wrapper and will continue to
#' return the Z-statistic until a future major release.
#'
#' @param x Numeric vector (time series assumed to be equidistant).
#'   \code{NA} values are silently removed.
#' @return Numeric. Identical output shape to \code{mann_kendall_z()}.
#'
#' @examples
#' suppressWarnings(kendall_tau(c(120, 118, 115, 112, 110, 108, 105)))
#'
#' @seealso \code{\link{mann_kendall_z}}
#' @author Matthias Templ
#' @export
kendall_tau <- function(x) {
  .Deprecated(
    new = "mann_kendall_z",
    package = "pep725",
    msg = paste(
      "kendall_tau() is deprecated because it is misnamed: it returns the",
      "Mann-Kendall Z-statistic, not Kendall's tau. Use mann_kendall_z()",
      "for identical (and improved: tie-corrected) behaviour."
    )
  )
  mann_kendall_z(x)
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
