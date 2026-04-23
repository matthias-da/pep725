# Prevent R CMD check notes
utils::globalVariables(c("day", "year", "s_id", "expected_doy", "deviation",
                         "is_outlier", "outlier_type", "group_median", "group_mad",
                         "..by", "..group_cols"))

#' Flag Phenological Outliers
#'
#' Identifies outlier observations in phenological data using the 30-day rule
#' or statistical methods. Based on the approach described by Schaber & Badeck
#' (2002) for quality control of phenological observations.
#'
#' @param pep A \code{pep} object or data.table with phenological observations.
#'   Must contain \code{year}, \code{day}, and grouping columns.
#' @param by Character vector of columns defining groups for outlier detection.
#'   Default \code{c("s_id", "genus", "species", "phase_id")} detects outliers
#'   within each station-species-phase combination.
#' @param method Character. Outlier detection method:
#'   \describe{
#'     \item{"30day"}{(Default) Flag observations >30 days from group mean/median.
#'       The classic Schaber-Badeck approach for phenological data.}
#'     \item{"mad"}{Use Median Absolute Deviation. Flag if |deviation| > k * MAD,
#'       where k is set by \code{threshold}.}
#'     \item{"iqr"}{Use Interquartile Range. Flag if outside (Q1 - k*IQR, Q3 + k*IQR).}
#'     \item{"zscore"}{Flag if |z-score| > threshold (default 3).}
#'     \item{"gam_residual"}{Fit a GAM (via \code{mgcv::gam()}) per group
#'       using the supplied \code{formula} — by default accounting for year
#'       trend, elevation, latitude, and a station random intercept — and
#'       flag observations whose robust-z-scored residual exceeds
#'       \code{threshold}. Detects covariate-inconsistent anomalies that
#'       the within-group \code{"30day"} rule misses. Falls back to
#'       \code{"30day"} on groups with fewer than \code{min_n_per_group}
#'       observations or when the model fails to converge.}
#'     \item{"mahalanobis"}{Treat each station-year as a vector of DOYs
#'       across phases and flag station-years whose **robust** Mahalanobis
#'       distance exceeds the \code{threshold}. The centre and covariance
#'       are estimated by the Minimum Covariance Determinant (MCD)
#'       estimator of Rousseeuw (1984), via
#'       \code{robustbase::covMcd()} — using the classical, non-robust
#'       covariance here would let the outliers contaminate the
#'       estimate and mask themselves. Under MCD the squared distance is
#'       still approximately \eqn{\chi^2_p}, so the default threshold
#'       is \eqn{\sqrt{\chi^2_{0.975, p}}}, where \eqn{p} is the number
#'       of phases present in the group. Detects station-years whose
#'       pattern across phases is jointly inconsistent (e.g.\ BBCH 60
#'       and 65 impossibly close) even when each marginal DOY looks
#'       fine. All rows within a flagged station-year are marked; the
#'       \code{deviation} column stores the robust Mahalanobis distance.
#'       Groups with too few complete station-years (fewer than
#'       \eqn{\max(p + 5, 15)}) or a singular MCD fit fall back to the
#'       30-day rule.}
#'   }
#' @param threshold Numeric. Threshold for outlier detection.
#'   \describe{
#'     \item{For "30day":}{Days deviation to flag as outlier. Default 30.}
#'     \item{For "mad":}{Number of MADs. Default 3 (~2.5 SD equivalent).}
#'     \item{For "iqr":}{IQR multiplier. Default 1.5 (standard Tukey rule).}
#'     \item{For "zscore":}{Z-score threshold. Default 3.}
#'     \item{For "gam_residual":}{Robust-z threshold on residuals. Default 3.5.}
#'   }
#' @param center Character. Central tendency measure: \code{"median"} (default,
#'   more robust) or \code{"mean"}.
#' @param min_obs Integer. Minimum observations per group required for outlier
#'   detection. Groups with fewer observations are skipped. Default 5.
#' @param formula Optional \code{formula} for \code{method = "gam_residual"}.
#'   Default is
#'   \code{day ~ s(year) + s(alt) + s(lat) + s(s_id, bs = "re")}; any
#'   smooth term referencing a column missing from \code{pep} is silently
#'   dropped.
#' @param min_n_per_group Integer. Minimum group size required before fitting
#'   the GAM in \code{method = "gam_residual"}. Groups below this threshold
#'   fall back to the \code{"30day"} rule. Default 50.
#' @param flag_only Logical. If \code{TRUE} (default), adds outlier flag columns
#'   to data. If \code{FALSE}, removes flagged outliers from data.
#'
#' @return If \code{flag_only = TRUE}: The input data with additional columns:
#'   \describe{
#'     \item{is_outlier}{Logical. TRUE for flagged outliers.}
#'     \item{deviation}{Numeric. Days deviation from expected value.}
#'     \item{expected_doy}{Numeric. Expected DOY for the group.}
#'   }
#'   If \code{flag_only = FALSE}: Data with outliers removed.
#'
#' @details
#' The 30-day rule (Schaber & Badeck 2002) is widely used for phenological data
#' quality control. It flags observations that deviate more than 30 days from
#' the expected value for that station-species-phase combination. This threshold
#' is based on the observation that legitimate phenological variation rarely
#' exceeds one month.
#'
#' @section When to Use Each Method:
#' \describe{
#'   \item{30day}{Standard for phenological QC. Use when the 30-day biological
#'     threshold is meaningful.}
#'   \item{mad}{Robust to existing outliers. Good for initial screening of
#'     potentially contaminated data.}
#'   \item{iqr}{Standard statistical approach. Useful when comparing with
#'     other quality metrics.}
#'   \item{zscore}{Parametric approach. Use when data is approximately normal.}
#' }
#'
#' @examples
#' \donttest{
#' data(pep_seed)
#'
#' # Flag outliers using 30-day rule
#' pep_flagged <- pep_flag_outliers(pep_seed)
#' table(pep_flagged$is_outlier)
#'
#' # View flagged observations
#' pep_flagged[is_outlier == TRUE]
#'
#' # Remove outliers instead of flagging
#' pep_clean <- pep_flag_outliers(pep_seed, flag_only = FALSE)
#'
#' # Use MAD method with stricter threshold
#' pep_flagged <- pep_flag_outliers(pep_seed, method = "mad", threshold = 2.5)
#'
#' # Group by country instead of station
#' pep_flagged <- pep_flag_outliers(pep_seed,
#'                               by = c("country", "genus", "phase_id"))
#' }
#'
#' @references
#' Schaber J, Badeck F-W (2002). Evaluation of methods for the combination of
#' phenological time series and outlier detection. Tree Physiology 22:973-982.
#'
#' Rousseeuw P J (1984). Least median of squares regression.
#' \emph{Journal of the American Statistical Association} 79(388):871-880.
#' (Minimum Covariance Determinant estimator used by
#' \code{method = "mahalanobis"}.)
#'
#' @seealso \code{\link{pep_quality}} for comprehensive quality assessment,
#'   \code{\link{pheno_combine}} which uses residuals for outlier detection
#'
#' @author Matthias Templ
#' @export
pep_flag_outliers <- function(pep,
                           by = c("s_id", "genus", "species", "phase_id"),
                           method = c("30day", "mad", "iqr", "zscore",
                                      "gam_residual", "mahalanobis"),
                           threshold = NULL,
                           center = c("median", "mean"),
                           min_obs = 5,
                           formula = day ~ s(year) + s(alt) + s(lat) +
                             s(s_id, bs = "re"),
                           min_n_per_group = 50,
                           flag_only = TRUE) {

  method <- match.arg(method)
  center <- match.arg(center)

  # Set default thresholds by method
  if (is.null(threshold)) {
    threshold <- switch(method,
                        "30day" = 30,
                        "mad" = 3,
                        "iqr" = 1.5,
                        "zscore" = 3,
                        "gam_residual" = 3.5,
                        # Mahalanobis default: set per-group based on the
                        # number of phases found in the data; handled below.
                        "mahalanobis" = NA_real_)
  }

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Check required columns
  if (!"day" %in% names(pep)) {
    stop("'pep' must have a 'day' column", call. = FALSE)
  }

  # Check grouping columns exist
  by <- intersect(by, names(pep))
  if (length(by) == 0) {
    warning("No grouping columns found. Using all data as one group.", call. = FALSE)
    by <- NULL
  }


  # Make a copy and remove pep class to avoid [.pep interference
  dt <- data.table::copy(pep)
  class(dt) <- c("data.table", "data.frame")

  # Initialize outlier columns
  data.table::set(dt, j = "is_outlier", value = FALSE)
  data.table::set(dt, j = "deviation", value = NA_real_)
  data.table::set(dt, j = "expected_doy", value = NA_real_)

  # Function to detect outliers in a group
  detect_in_group <- function(doy_values) {
    n <- sum(!is.na(doy_values))
    if (n < min_obs) {
      return(list(
        is_outlier = rep(FALSE, length(doy_values)),
        deviation = rep(NA_real_, length(doy_values)),
        expected = rep(NA_real_, length(doy_values))
      ))
    }

    # Calculate center
    if (center == "median") {
      center_val <- median(doy_values, na.rm = TRUE)
    } else {
      center_val <- mean(doy_values, na.rm = TRUE)
    }

    # Calculate deviations
    dev <- doy_values - center_val

    # Detect outliers based on method
    outliers <- rep(FALSE, length(doy_values))

    if (method == "30day") {
      # Simple threshold: |deviation| > 30 days
      outliers <- abs(dev) > threshold

    } else if (method == "mad") {
      # MAD-based detection
      mad_val <- mad(doy_values, na.rm = TRUE, constant = 1.4826)
      if (mad_val > 0) {
        outliers <- abs(dev) > threshold * mad_val
      }

    } else if (method == "iqr") {
      # IQR-based detection (Tukey)
      q <- quantile(doy_values, c(0.25, 0.75), na.rm = TRUE)
      iqr_val <- q[2] - q[1]
      lower <- q[1] - threshold * iqr_val
      upper <- q[2] + threshold * iqr_val
      outliers <- doy_values < lower | doy_values > upper

    } else if (method == "zscore") {
      # Z-score based detection
      sd_val <- sd(doy_values, na.rm = TRUE)
      if (sd_val > 0) {
        z <- dev / sd_val
        outliers <- abs(z) > threshold
      }
    }

    # Handle NAs
    outliers[is.na(doy_values)] <- NA

    list(
      is_outlier = outliers,
      deviation = dev,
      expected = rep(center_val, length(doy_values))
    )
  }

  # GAM-residual detection operates on whole groups (with all covariates
  # visible at once), so we can't use the scalar detect_in_group() helper.
  # Dispatch here; other methods use the per-group univariate path below.
  if (method == "gam_residual") {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      stop("Package 'mgcv' is required for method = 'gam_residual'. ",
           "Install it with: install.packages('mgcv').",
           call. = FALSE)
    }
    .fit_group_gam <- function(sub, threshold, min_n_per_group, formula) {
      n_sub <- nrow(sub)
      fallback <- function() {
        center_val <- stats::median(sub$day, na.rm = TRUE)
        dev <- sub$day - center_val
        list(is_outlier = abs(dev) > 30,
             deviation  = dev,
             expected   = rep(center_val, n_sub))
      }
      if (n_sub < min_n_per_group) return(fallback())
      # Drop smooth terms referencing absent columns so the caller can
      # pass a generic default formula even when e.g. alt is missing.
      vars_needed <- all.vars(formula)
      missing_vars <- setdiff(vars_needed, names(sub))
      if (length(missing_vars) > 0) {
        f_terms <- attr(stats::terms(formula), "term.labels")
        keep <- vapply(f_terms,
                       function(tm) !any(missing_vars %in% all.vars(str2lang(tm))),
                       logical(1))
        f_kept <- paste(f_terms[keep], collapse = " + ")
        if (nchar(f_kept) == 0) return(fallback())
        formula <- stats::as.formula(paste("day ~", f_kept))
      }
      # s_id must be a factor for the random-intercept smooth term; silent
      # coercion is safe because it's not used as numeric elsewhere here.
      if ("s_id" %in% names(sub)) {
        sub$s_id <- factor(sub$s_id)
      }
      fit <- tryCatch(
        suppressWarnings(mgcv::gam(formula, data = sub, method = "REML")),
        error = function(e) NULL
      )
      if (is.null(fit)) return(fallback())
      res   <- as.numeric(stats::residuals(fit, type = "response"))
      fitted_vals <- as.numeric(stats::fitted(fit))
      scale <- stats::mad(res, constant = 1.4826)
      if (is.na(scale) || scale == 0) return(fallback())
      z <- res / scale
      list(is_outlier = abs(z) > threshold,
           deviation  = res,
           expected   = fitted_vals)
    }

    if (is.null(by) || length(by) == 0) {
      g <- .fit_group_gam(dt, threshold, min_n_per_group, formula)
      dt[, is_outlier := g$is_outlier]
      dt[, deviation := g$deviation]
      dt[, expected_doy := g$expected]
    } else {
      n_fallback <- 0L
      total_groups <- 0L
      dt[, c("is_outlier", "deviation", "expected_doy") := {
        total_groups <<- total_groups + 1L
        res_g <- .fit_group_gam(.SD, threshold, min_n_per_group, formula)
        if (.N < min_n_per_group) n_fallback <<- n_fallback + 1L
        list(res_g$is_outlier, res_g$deviation, res_g$expected)
      }, by = by]
      if (n_fallback > 0L) {
        message(sprintf(
          paste0("pep_flag_outliers(method = 'gam_residual'): %d of %d ",
                 "group(s) had fewer than min_n_per_group = %d observations ",
                 "and fell back to the 30-day rule."),
          n_fallback, total_groups, min_n_per_group
        ))
      }
    }
  } else if (method == "mahalanobis") {
    if (!requireNamespace("robustbase", quietly = TRUE)) {
      stop("Package 'robustbase' is required for method = 'mahalanobis'. ",
           "Install it with: install.packages('robustbase').",
           call. = FALSE)
    }
    if (!all(c("s_id", "year", "phase_id") %in% names(dt))) {
      stop("method = 'mahalanobis' requires 's_id', 'year', 'phase_id', ",
           "and 'day' columns in pep.",
           call. = FALSE)
    }

    .fit_group_mahalanobis <- function(sub, threshold) {
      n_sub <- nrow(sub)
      fallback <- function() {
        center_val <- stats::median(sub$day, na.rm = TRUE)
        dev <- sub$day - center_val
        list(is_outlier = abs(dev) > 30,
             deviation  = dev,
             expected   = rep(center_val, n_sub))
      }
      # Pivot to a (station-year) x phase wide matrix. Rows with any NA
      # phase are dropped from the covariance fit but still returned in
      # the long-format output (with NA MD).
      wide <- data.table::dcast(
        data.table::as.data.table(sub),
        s_id + year ~ phase_id,
        value.var = "day",
        fun.aggregate = mean, na.rm = TRUE
      )
      phase_cols <- setdiff(names(wide), c("s_id", "year"))
      phase_cols <- phase_cols[!grepl("^V[0-9]+$", phase_cols)]
      p <- length(phase_cols)
      if (p < 2) return(fallback())
      M <- as.matrix(wide[, phase_cols, with = FALSE])
      M[is.nan(M)] <- NA_real_
      complete <- stats::complete.cases(M)
      if (sum(complete) < max(p + 5, 15)) return(fallback())
      if (is.na(threshold)) {
        threshold <- sqrt(stats::qchisq(0.975, df = p))
      }
      fit <- tryCatch(
        robustbase::covMcd(M[complete, , drop = FALSE]),
        error = function(e) NULL
      )
      if (is.null(fit) || any(!is.finite(diag(fit$cov)))) {
        return(fallback())
      }
      diffs <- sweep(M, 2, fit$center, "-")
      inv_cov <- tryCatch(solve(fit$cov), error = function(e) NULL)
      if (is.null(inv_cov)) return(fallback())
      md <- sqrt(rowSums((diffs %*% inv_cov) * diffs))
      wide[, md := md]
      # Join the (s_id, year) MD back onto the long-format sub.
      long <- merge(
        data.table::as.data.table(sub)[, .I, by = .(s_id, year)],
        wide[, .(s_id, year, md)],
        by = c("s_id", "year"),
        all.x = TRUE
      )
      long <- long[order(I)]
      list(is_outlier = !is.na(long$md) & long$md > threshold,
           deviation  = long$md,
           expected   = rep(NA_real_, n_sub))
    }

    if (is.null(by) || length(by) == 0) {
      g <- .fit_group_mahalanobis(dt, threshold)
      dt[, is_outlier := g$is_outlier]
      dt[, deviation := g$deviation]
      dt[, expected_doy := g$expected]
    } else {
      dt[, c("is_outlier", "deviation", "expected_doy") := {
        res_g <- .fit_group_mahalanobis(.SD, threshold)
        list(res_g$is_outlier, res_g$deviation, res_g$expected)
      }, by = by]
    }
    # Record a representative threshold on the attribute (the chi-square
    # default depends on the number of phases; if user passed an explicit
    # threshold keep that, otherwise use the default for the actual
    # number of phases in the data).
    if (is.na(threshold)) {
      p_phases <- data.table::uniqueN(dt$phase_id)
      threshold <- sqrt(stats::qchisq(0.975, df = p_phases))
    }
  } else if (is.null(by) || length(by) == 0) {
    # Single group
    result <- detect_in_group(dt$day)
    dt[, is_outlier := result$is_outlier]
    dt[, deviation := result$deviation]
    dt[, expected_doy := result$expected]
  } else {
    # Process each group
    group_cols <- by

    # Use data.table grouping
    dt[, c("is_outlier", "deviation", "expected_doy") := {
      result <- detect_in_group(day)
      list(result$is_outlier, result$deviation, result$expected)
    }, by = group_cols]
  }

  # Add outlier type label
  data.table::set(dt, j = "outlier_type", value = data.table::fifelse(
    dt$is_outlier & !is.na(dt$is_outlier),
    data.table::fifelse(dt$deviation > 0, "late", "early"),
    NA_character_
  ))

  # Return flagged data or cleaned data
  if (flag_only) {
    # Add class for potential method dispatch
    class(dt) <- c("pep_outliers", class(dt))
    attr(dt, "method") <- method
    attr(dt, "threshold") <- threshold
    attr(dt, "by") <- by
    return(dt)
  } else {
    # Remove outliers
    dt_clean <- dt[is_outlier == FALSE | is.na(is_outlier)]
    dt_clean[, c("is_outlier", "deviation", "expected_doy", "outlier_type") := NULL]
    return(dt_clean)
  }
}


#' Print Method for Outlier Detection Results
#'
#' @param x A \code{pep_outliers} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns \code{x}
#' @export
print.pep_outliers <- function(x, ...) {
  method <- attr(x, "method")
  threshold <- attr(x, "threshold")
  by <- attr(x, "by")

  cat("Phenological Outlier Detection Results\n")
  cat(strrep("=", 45), "\n")

  if (!is.null(method) && length(method) == 1) {
    cat("Method:", method, "\n")
    cat("Threshold:", threshold,
        switch(method,
               "30day" = "days",
               "mad" = "MADs",
               "iqr" = "IQRs",
               "zscore" = "SDs"), "\n")
  }
  if (!is.null(by)) {
    cat("Grouping:", paste(by, collapse = ", "), "\n")
  }

  cat("\n")
  n_total <- nrow(x)
  n_outliers <- sum(x$is_outlier, na.rm = TRUE)
  n_early <- sum(x$outlier_type == "early", na.rm = TRUE)
  n_late <- sum(x$outlier_type == "late", na.rm = TRUE)

  cat(sprintf("Total observations: %s\n", format(n_total, big.mark = ",")))
  cat(sprintf("Outliers flagged: %d (%.2f%%)\n", n_outliers, 100 * n_outliers / n_total))
  cat(sprintf("  - Early outliers: %d\n", n_early))
  cat(sprintf("  - Late outliers: %d\n", n_late))

  if (n_outliers > 0) {
    cat("\nDeviation summary for outliers:\n")
    outlier_dev <- x$deviation[x$is_outlier == TRUE]
    cat(sprintf("  Min: %.1f days\n", min(outlier_dev, na.rm = TRUE)))
    cat(sprintf("  Max: %.1f days\n", max(outlier_dev, na.rm = TRUE)))
    cat(sprintf("  Mean |deviation|: %.1f days\n", mean(abs(outlier_dev), na.rm = TRUE)))
  }

  invisible(x)
}


#' Summary Method for Outlier Detection Results
#'
#' @param object A \code{pep_outliers} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns a summary data.table
#' @export
summary.pep_outliers <- function(object, ...) {
  by <- attr(object, "by")

  cat("Phenological Outlier Summary\n")
  cat(strrep("=", 45), "\n\n")

  # Overall summary
  n_total <- nrow(object)
  n_outliers <- sum(object$is_outlier, na.rm = TRUE)

  cat(sprintf("Total observations: %s\n", format(n_total, big.mark = ",")))
  cat(sprintf("Total outliers: %d (%.2f%%)\n\n", n_outliers, 100 * n_outliers / n_total))

  # Summary by group if grouping was used
  if (!is.null(by) && length(by) > 0) {
    group_summary <- object[, .(
      n_obs = .N,
      n_outliers = sum(is_outlier, na.rm = TRUE),
      pct_outliers = round(100 * sum(is_outlier, na.rm = TRUE) / .N, 2),
      mean_dev = round(mean(abs(deviation), na.rm = TRUE), 1)
    ), by = by][order(-n_outliers)]

    # Remove pep_outliers class to avoid recursion in print
    class(group_summary) <- class(group_summary)[class(group_summary) != "pep_outliers"]

    cat("Groups with most outliers:\n")
    print(head(group_summary[n_outliers > 0], 10))
  }

  # Distribution of deviations
  cat("\nDeviation distribution (all data):\n")
  dev_quantiles <- quantile(object$deviation, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
                            na.rm = TRUE)
  print(round(dev_quantiles, 1))

  invisible(object)
}


#' Plot Method for Outlier Detection Results
#'
#' @param x A \code{pep_outliers} object
#' @param type Character. Plot type: "histogram" for deviation distribution,
#'   "scatter" for DOY vs expected, "timeline" for outliers over time.
#' @param ... Additional arguments
#' @return A ggplot object (invisibly)
#' @export
plot.pep_outliers <- function(x, type = c("histogram", "scatter", "timeline"), ...) {
  type <- match.arg(type)
  threshold <- attr(x, "threshold")
  method <- attr(x, "method")

  if (type == "histogram") {
    # Deviation distribution
    p <- ggplot2::ggplot(x, ggplot2::aes(x = deviation, fill = is_outlier)) +
      ggplot2::geom_histogram(bins = 50, alpha = 0.9, position = "stack") +
      ggplot2::scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                                  labels = c("Normal", "Outlier"),
                                  na.value = "gray50") +
      ggplot2::labs(
        title = "Distribution of Deviations from Expected DOY",
        subtitle = sprintf("Method: %s, Threshold: %s", method, threshold),
        x = "Deviation (days)",
        y = "Count",
        fill = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    if (method == "30day") {
      p <- p +
        ggplot2::geom_vline(xintercept = c(-threshold, threshold),
                            linetype = "dashed", color = "red", alpha = 0.7)
    }

  } else if (type == "scatter") {
    # Observed vs expected DOY
    p <- ggplot2::ggplot(x, ggplot2::aes(x = expected_doy, y = day, color = is_outlier)) +
      ggplot2::geom_point(alpha = 0.5, size = 1) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                                   labels = c("Normal", "Outlier"),
                                   na.value = "gray50") +
      ggplot2::labs(
        title = "Observed vs Expected Day of Year",
        x = "Expected DOY (group median/mean)",
        y = "Observed DOY",
        color = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

  } else if (type == "timeline") {
    # Outliers over time
    if (!"year" %in% names(x)) {
      stop("'year' column required for timeline plot", call. = FALSE)
    }

    year_summary <- x[, .(
      n_obs = .N,
      n_outliers = sum(is_outlier, na.rm = TRUE),
      pct_outliers = 100 * sum(is_outlier, na.rm = TRUE) / .N
    ), by = year][order(year)]

    p <- ggplot2::ggplot(year_summary, ggplot2::aes(x = year, y = pct_outliers)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(size = n_obs), color = "steelblue", alpha = 0.7) +
      ggplot2::labs(
        title = "Outlier Percentage Over Time",
        x = "Year",
        y = "Outliers (%)",
        size = "Observations"
      ) +
      ggplot2::theme_minimal()
  }

  print(p)
  invisible(p)
}
