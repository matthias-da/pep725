# Prevent R CMD check notes
utils::globalVariables(c("day", "year", "JDay", "Tmean", "Tmin", "Tmax",
                         "pheno_year", ".", "VIP", "coefficient", "period"))

#' Partial Least Squares Analysis for Phenology-Temperature Relationships
#'
#' Performs PLS regression to identify temperature-sensitive periods affecting
#' phenological timing. This is useful for understanding which parts of the
#' year have the strongest influence on phenological events.
#'
#' @param pep A \code{pep} object or data.frame with phenological observations.
#'   Must contain \code{year} and \code{day} columns.
#' @param temp_data A data.frame with daily temperature data. Must contain
#'   columns \code{year}, \code{doy} (day of year), and either \code{Tmean} or

#'   both \code{Tmin} and \code{Tmax}.
#' @param by Character vector of grouping columns to match phenology and
#'   temperature data (e.g., \code{"s_id"} for station-specific analysis).
#'   Default \code{NULL} uses the same temperature for all observations per year.
#' @param method Character. PLS method to use:
#'   \describe{
#'     \item{"robust"}{(Default) Iteratively reweighted PLS using bisquare weights.
#'       Resistant to outliers in both phenology and temperature data.}
#'     \item{"standard"}{Classical PLS regression without robust weighting.}
#'   }
#' @param split_month Integer (1-12). The last month to include in the
#'   phenological year. Default 6 (June) means July-June phenological years.
#'   This is important for spring phenology where relevant temperatures
#'   span the previous autumn/winter.
#' @param runn_mean Integer (odd). Window size for running mean smoothing of
#'   temperatures. Default 11 days. Larger values produce smoother results.
#' @param ncomp Integer or NULL. Number of PLS components. If NULL (default),
#'   automatically determined to explain at least \code{expl_var} percent
#'   of variance.
#' @param expl_var Numeric. Minimum percentage of variance to explain when
#'   automatically selecting components. Default 30.
#' @param max_iter Integer. Maximum iterations for robust method. Default 20.
#' @param tol Numeric. Convergence tolerance for robust method. Default 1e-4.
#'
#' @return An object of class \code{pheno_pls} containing:
#'   \describe{
#'     \item{vip}{Data.frame with Variable Importance in Projection scores
#'       for each day of the phenological year.}
#'     \item{coefficients}{PLS regression coefficients for each day.}
#'     \item{model}{The fitted PLS model object.}
#'     \item{weights}{Observation weights (all 1 for standard method).}
#'     \item{r_squared}{Model R-squared.}
#'     \item{ncomp}{Number of components used.}
#'     \item{method}{Method used ("robust" or "standard").}
#'     \item{n_obs}{Number of observations.}
#'     \item{n_years}{Number of years analyzed.}
#'   }
#'
#' @details
#' PLS regression is particularly useful for phenology-temperature analysis
#' because it handles the high collinearity between daily temperatures
#' (adjacent days are highly correlated) while identifying which periods
#' have the strongest influence on phenological timing.
#'
#' The robust method uses iteratively reweighted PLS with bisquare weights,
#' which downweights observations with large residuals. This is important
#' for phenological data which may contain outliers from observation errors
#' or unusual weather events.
#'
#' @section Interpreting Results:
#' \describe{
#'   \item{VIP scores}{Values > 0.8 indicate important variables. Days with
#'     high VIP scores have strong influence on phenology timing.}
#'   \item{Coefficients}{Negative coefficients indicate that warmer temperatures
#'     during that period lead to earlier phenology (lower DOY).}
#' }
#'
#' @section Phenological Year:
#' Spring phenology (e.g., flowering) is influenced by temperatures from the
#' previous autumn through spring. The \code{split_month} parameter defines
#' where to split the calendar year. With \code{split_month = 6}, a flowering
#' event in April 2020 is analyzed against temperatures from July 2019
#' through June 2020.
#'
#' @examples
#' \donttest{
#' data(pep_seed)
#'
#' # Create example temperature data (fewer years for speed)
#' years <- 2005:2012
#' temp <- data.frame(
#'   year = rep(years, each = 365),
#'   doy = rep(1:365, length(years)),
#'   Tmin = rnorm(365 * length(years), mean = 5, sd = 8),
#'   Tmax = rnorm(365 * length(years), mean = 15, sd = 8)
#' )
#'
#' # Run standard PLS (fast: no CV, fixed components)
#' result <- pheno_pls(pep_seed, temp, method = "standard", ncomp = 2)
#' print(result)
#' }
#'
#' @references
#' Luedeling E, Gassner A (2012). Partial Least Squares Regression for
#' analyzing walnut phenology in California. Agricultural and Forest
#' Meteorology 158-159:104-113.
#'
#' Wold S, Sjostrom M, Eriksson L (2001). PLS-regression: a basic tool of
#' chemometrics. Chemometrics and Intelligent Laboratory Systems 58:109-130.
#'
#' @seealso \code{\link{pheno_trend_turning}} for trend analysis,
#'   \code{\link{pheno_anomaly}} for anomaly detection
#'
#' @author Matthias Templ
#' @importFrom stats mad weighted.mean median coef predict
#' @export
pheno_pls <- function(pep,
                          temp_data,
                          by = NULL,
                          method = c("robust", "standard"),
                          split_month = 6,
                          runn_mean = 11,
                          ncomp = NULL,
                          expl_var = 30,
                          max_iter = 20,
                          tol = 1e-4) {

  method <- match.arg(method)

  # Check for pls package

  if (!requireNamespace("pls", quietly = TRUE)) {
    stop("Package 'pls' is required for PLS analysis. Install with: install.packages('pls')",
         call. = FALSE)
  }

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }
  if (!inherits(temp_data, "data.frame")) {
    stop("'temp_data' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }
  if (!inherits(temp_data, "data.table")) {
    temp_data <- data.table::as.data.table(temp_data)
  }

  # Check required columns
  if (!all(c("year", "day") %in% names(pep))) {
    stop("'pep' must have 'year' and 'day' columns", call. = FALSE)
  }
  if (!all(c("year", "doy") %in% names(temp_data))) {
    stop("'temp_data' must have 'year' and 'doy' columns", call. = FALSE)
  }

  # Check temperature columns
  has_tmean <- "Tmean" %in% names(temp_data)
  has_minmax <- all(c("Tmin", "Tmax") %in% names(temp_data))
  if (!has_tmean && !has_minmax) {
    stop("'temp_data' must have either 'Tmean' or both 'Tmin' and 'Tmax'",
         call. = FALSE)
  }

  # Calculate Tmean if not present
  if (!has_tmean) {
    temp_data[, Tmean := (Tmin + Tmax) / 2]
  }

  # Validate runn_mean is odd

  if (runn_mean %% 2 == 0) {
    runn_mean <- runn_mean + 1
    warning("runn_mean must be odd; using ", runn_mean, call. = FALSE)
  }

  # Aggregate phenology by year (take median DOY per year for analysis)
  pep_agg <- pep[, .(pheno = median(day, na.rm = TRUE)), by = year]
  pep_agg <- pep_agg[!is.na(pheno)]

  # Determine phenological year for each observation
  # For split_month = 6, pheno year 2020 includes Jul 2019 - Jun 2020
  pep_agg[, pheno_year := year]

  # Get unique years with phenology data
  pheno_years <- sort(unique(pep_agg$pheno_year))

  # Build temperature matrix for PLS
  # Each row = one phenological year
  # Each column = one day of the phenological year (365 days)
  n_days <- 365
  temp_matrix <- matrix(NA, nrow = length(pheno_years), ncol = n_days)
  pheno_vector <- numeric(length(pheno_years))

  for (i in seq_along(pheno_years)) {
    py <- pheno_years[i]

    # Get phenology DOY for this year
    pheno_doy <- pep_agg[pheno_year == py, pheno]
    if (length(pheno_doy) == 0 || is.na(pheno_doy)) next
    pheno_vector[i] <- pheno_doy

    # Get temperatures for this phenological year
    # From (split_month + 1) of previous year to split_month of current year
    start_year <- py - 1
    end_year <- py

    # Days from previous year: from July 1 (if split_month = 6) to Dec 31
    days_prev_year <- temp_data[year == start_year & doy > split_month * 30.5]
    days_curr_year <- temp_data[year == end_year & doy <= split_month * 30.5]

    # Combine and order
    year_temps <- rbind(days_prev_year, days_curr_year)
    data.table::setorder(year_temps, year, doy)

    if (nrow(year_temps) < 300) next  # Skip years with insufficient data

    # Take first 365 days and extract Tmean
    temps <- head(year_temps$Tmean, n_days)
    if (length(temps) < n_days) {
      temps <- c(temps, rep(NA, n_days - length(temps)))
    }
    temp_matrix[i, ] <- temps
  }

  # Remove rows with missing data
  valid_rows <- complete.cases(temp_matrix) & !is.na(pheno_vector)
  temp_matrix <- temp_matrix[valid_rows, , drop = FALSE]
  pheno_vector <- pheno_vector[valid_rows]
  pheno_years <- pheno_years[valid_rows]

  if (nrow(temp_matrix) < 5) {
    stop("Insufficient data: need at least 5 complete phenological years",
         call. = FALSE)
  }

  # Apply running mean smoothing
  temp_matrix_smooth <- apply(temp_matrix, 1, function(row) {
    stats::filter(row, rep(1/runn_mean, runn_mean), sides = 2)
  })
  temp_matrix_smooth <- t(temp_matrix_smooth)

  # Handle NAs from smoothing at edges
  half_win <- floor(runn_mean / 2)
  temp_matrix_smooth[, 1:half_win] <- temp_matrix[, 1:half_win]
  temp_matrix_smooth[, (n_days - half_win + 1):n_days] <-
    temp_matrix[, (n_days - half_win + 1):n_days]

  # Create data frame for PLS
  pls_data <- data.frame(pheno = pheno_vector, temp_matrix_smooth)
  colnames(pls_data) <- c("pheno", paste0("day_", 1:n_days))

  # Scale predictors
  temp_scaled <- scale(temp_matrix_smooth)
  temp_center <- attr(temp_scaled, "scaled:center")
  temp_scale <- attr(temp_scaled, "scaled:scale")

  # Initialize weights
  n_obs <- nrow(temp_matrix_smooth)
  weights <- rep(1, n_obs)

  # Fit PLS model
  if (method == "standard") {
    # Standard PLS
    pls_fit <- fit_pls(pheno_vector, temp_scaled, ncomp, expl_var, weights)

  } else {
    # Robust PLS using iteratively reweighted approach
    pls_fit <- fit_pls(pheno_vector, temp_scaled, ncomp, expl_var, weights)

    for (iter in seq_len(max_iter)) {
      # Get residuals from training predictions
      pred <- pls_fit$predictions
      resid <- pheno_vector - pred

      # Calculate robust weights using bisquare function
      weights_new <- bisquare_weights(resid)

      # Check convergence
      weight_change <- max(abs(weights_new - weights))
      weights <- weights_new

      if (weight_change < tol) break

      # Refit with new weights
      pls_fit <- fit_pls(pheno_vector, temp_scaled, pls_fit$ncomp, expl_var, weights)
    }
  }

  # Calculate VIP scores
  vip_scores <- calculate_vip(pls_fit)

  # Get coefficients
  coefs <- pls_fit$coefficients

  # Calculate R-squared using predictions from the pls model
  pred_final <- pls_fit$predictions
  ss_res <- sum(weights * (pheno_vector - pred_final)^2)
  ss_tot <- sum(weights * (pheno_vector - weighted.mean(pheno_vector, weights))^2)
  r_squared <- 1 - ss_res / ss_tot

  # Create day labels for output
  # Days are numbered from start of phenological year
  day_labels <- create_day_labels(split_month, n_days)

  # Prepare output
  result <- list(
    vip = data.frame(
      day = 1:n_days,
      period = day_labels,
      VIP = vip_scores
    ),
    coefficients = data.frame(
      day = 1:n_days,
      period = day_labels,
      coefficient = coefs
    ),
    model = pls_fit,
    weights = weights,
    r_squared = r_squared,
    ncomp = pls_fit$ncomp,
    method = method,
    n_obs = n_obs,
    n_years = length(pheno_years),
    pheno_years = pheno_years,
    split_month = split_month,
    runn_mean = runn_mean
  )

  class(result) <- c("pheno_pls", "list")
  result
}


#' Fit PLS model (internal)
#' @noRd
fit_pls <- function(y, X, ncomp = NULL, expl_var = 30, weights = NULL) {
  n <- length(y)
  p <- ncol(X)

  if (is.null(weights)) weights <- rep(1, n)

  # Create data frame for pls
  pls_df <- data.frame(y = y, X = I(X))

  # Determine number of components if not specified
  max_comp <- min(n - 2, p, 15)

  if (is.null(ncomp)) {
    # Use cross-validation to select components
    suppressWarnings({
      pls_full <- pls::plsr(y ~ X, data = pls_df, ncomp = max_comp,
                            validation = "CV", weights = weights)
    })
    explvar <- cumsum(pls::explvar(pls_full))

    # Find first component that exceeds expl_var threshold
    ncomp <- which(explvar >= expl_var)[1]
    if (is.na(ncomp)) ncomp <- max_comp
    ncomp <- max(1, min(ncomp, max_comp))
  }

  # Fit final model
  pls_fit <- pls::plsr(y ~ X, data = pls_df, ncomp = ncomp, weights = weights)

  # Extract coefficients (on scaled X)
  coefs <- as.vector(coef(pls_fit, ncomp = ncomp))

  # Get predictions on training data
  predictions <- as.vector(predict(pls_fit, newdata = pls_df, ncomp = ncomp))

  list(
    pls_object = pls_fit,
    coefficients = coefs,
    ncomp = ncomp,
    weights = weights,
    predictions = predictions,
    X = X,
    y = y
  )
}


#' Predict from PLS model (internal)
#' @noRd
predict_pls <- function(pls_fit, X = NULL) {
  if (is.null(X)) {
    # Return training predictions
    return(pls_fit$predictions)
  }
  # Predict on new data
  pls_df <- data.frame(X = I(X))
  as.vector(predict(pls_fit$pls_object, newdata = pls_df, ncomp = pls_fit$ncomp))
}


#' Calculate VIP scores (internal)
#' @noRd
calculate_vip <- function(pls_fit) {
  # Extract components from pls object
  pls_obj <- pls_fit$pls_object
  ncomp <- pls_fit$ncomp

  # Get scores and loadings
  W <- pls_obj$loading.weights[, 1:ncomp, drop = FALSE]
  Q <- pls_obj$Yloadings[, 1:ncomp, drop = FALSE]
  T_scores <- pls_obj$scores[, 1:ncomp, drop = FALSE]

  # Calculate explained variance for each component
  SS <- colSums(T_scores^2) * as.vector(Q)^2

  # VIP calculation
  p <- nrow(W)
  vip <- numeric(p)

  for (j in seq_len(p)) {
    ss_j <- sum((W[j, ]^2) * SS)
    vip[j] <- sqrt(p * ss_j / sum(SS))
  }

  vip
}


#' Bisquare weight function for robust estimation (internal)
#' @noRd
bisquare_weights <- function(resid, c = 4.685) {
  # Robust scale estimate using MAD
  s <- mad(resid, center = median(resid), constant = 1.4826)
  if (s < 1e-10) s <- 1

  # Standardize residuals
  u <- resid / s

  # Bisquare weights
  weights <- ifelse(abs(u) <= c,
                    (1 - (u/c)^2)^2,
                    0)

  # Ensure no zero weights (minimum weight)
  weights <- pmax(weights, 0.01)
  weights
}


#' Create day labels for phenological year (internal)
#' @noRd
create_day_labels <- function(split_month, n_days) {
  # Create month-day labels starting from split_month + 1
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Reorder starting from split_month + 1
  start_idx <- split_month + 1
  month_order <- c(start_idx:12, 1:split_month)

  labels <- character(n_days)
  day_counter <- 1

  for (m in month_order) {
    n_days_month <- days_per_month[m]
    for (d in seq_len(n_days_month)) {
      if (day_counter > n_days) break
      labels[day_counter] <- paste0(months[m], "-", d)
      day_counter <- day_counter + 1
    }
    if (day_counter > n_days) break
  }

  labels
}


#' Print Method for PLS Phenology Results
#'
#' @param x A \code{pheno_pls} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns \code{x}
#' @export
print.pheno_pls <- function(x, ...) {
  cat("PLS Phenology-Temperature Analysis\n")
  cat(strrep("=", 45), "\n")
  cat("Method:", x$method, "\n")
  cat("Components:", x$ncomp, "\n")
  cat("R-squared:", sprintf("%.3f", x$r_squared), "\n")
  cat("Years analyzed:", x$n_years, "\n")
  cat("Running mean window:", x$runn_mean, "days\n")
  cat("Split month:", x$split_month, "\n\n")

  # Show top VIP periods
  vip_sorted <- x$vip[order(-x$vip$VIP), ]
  cat("Top 10 most important temperature periods:\n")
  print(head(vip_sorted, 10), row.names = FALSE)

  if (x$method == "robust") {
    n_downweighted <- sum(x$weights < 0.5)
    cat("\nRobust fitting: ", n_downweighted, " observations downweighted\n", sep = "")
  }

  invisible(x)
}


#' Summary Method for PLS Phenology Results
#'
#' @param object A \code{pheno_pls} object
#' @param vip_threshold Numeric. VIP threshold for identifying important
#'   periods. Default 0.8.
#' @param ... Additional arguments (unused)
#' @return Invisibly returns a summary data.frame
#' @export
summary.pheno_pls <- function(object, vip_threshold = 0.8, ...) {
  cat("PLS Phenology-Temperature Analysis Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Model fit:\n")
  cat("  Method:", object$method, "\n")
  cat("  Components:", object$ncomp, "\n")
  cat("  R-squared:", sprintf("%.3f", object$r_squared), "\n")
  cat("  Years:", object$n_years, "(", min(object$pheno_years), "-",
      max(object$pheno_years), ")\n\n")

  # Identify important periods (VIP > threshold)
  important <- object$vip[object$vip$VIP >= vip_threshold, ]

  if (nrow(important) > 0) {
    cat("Important temperature periods (VIP >=", vip_threshold, "):\n")

    # Group consecutive days into periods
    important_days <- important$day
    if (length(important_days) > 0) {
      # Find runs of consecutive days
      runs <- rle(diff(important_days) == 1)
      periods <- data.frame(
        start = integer(),
        end = integer(),
        mean_vip = numeric(),
        mean_coef = numeric()
      )

      pos <- 1
      for (i in seq_along(runs$lengths)) {
        if (i == 1 || !runs$values[i-1]) {
          start_day <- important_days[pos]
        }
        if (runs$values[i]) {
          end_day <- important_days[pos + runs$lengths[i]]
        } else {
          end_day <- important_days[pos]
        }
        if (!runs$values[i] || i == length(runs$lengths)) {
          days_in_period <- start_day:end_day
          mean_vip <- mean(object$vip$VIP[days_in_period])
          mean_coef <- mean(object$coefficients$coefficient[days_in_period])
          periods <- rbind(periods, data.frame(
            start = start_day,
            end = end_day,
            mean_vip = mean_vip,
            mean_coef = mean_coef
          ))
        }
        pos <- pos + runs$lengths[i]
      }

      # Add period labels
      periods$start_period <- object$vip$period[periods$start]
      periods$end_period <- object$vip$period[periods$end]
      periods$effect <- ifelse(periods$mean_coef < 0, "advancing", "delaying")

      cat("\n")
      for (i in seq_len(nrow(periods))) {
        cat(sprintf("  %s to %s: VIP=%.2f, %s effect\n",
                    periods$start_period[i],
                    periods$end_period[i],
                    periods$mean_vip[i],
                    periods$effect[i]))
      }
    }
  } else {
    cat("No periods with VIP >=", vip_threshold, "found.\n")
  }

  cat("\n")
  invisible(object$vip)
}


#' Plot Method for PLS Phenology Results
#'
#' @param x A \code{pheno_pls} object
#' @param type Character. Plot type: "vip" for VIP scores (default),
#'   "coef" for coefficients, "both" for combined plot.
#' @param vip_threshold Numeric. Threshold line for VIP plot. Default 0.8.
#' @param ... Additional arguments passed to plotting functions
#' @return A ggplot object (invisibly)
#' @export
plot.pheno_pls <- function(x, type = c("vip", "coef", "both"),
                                vip_threshold = 0.8, ...) {
  type <- match.arg(type)

  # Create month breaks for x-axis
  split_month <- x$split_month
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  month_order <- c((split_month + 1):12, 1:split_month)
  month_starts <- cumsum(c(1, days_per_month[month_order[-length(month_order)]]))
  month_labels <- months[month_order]

  if (type == "vip") {
    p <- ggplot2::ggplot(x$vip, ggplot2::aes(x = day, y = VIP)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = vip_threshold, linetype = "dashed",
                          color = "red", alpha = 0.7) +
      ggplot2::scale_x_continuous(breaks = month_starts, labels = month_labels) +
      ggplot2::labs(
        title = "Variable Importance in Projection (VIP) Scores",
        subtitle = sprintf("Method: %s, R-sq = %.3f", x$method, x$r_squared),
        x = "Month",
        y = "VIP Score"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  } else if (type == "coef") {
    p <- ggplot2::ggplot(x$coefficients, ggplot2::aes(x = day, y = coefficient)) +
      ggplot2::geom_line(color = "darkgreen", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      ggplot2::scale_x_continuous(breaks = month_starts, labels = month_labels) +
      ggplot2::labs(
        title = "PLS Regression Coefficients",
        subtitle = "Negative = warmer temps lead to earlier phenology",
        x = "Month",
        y = "Coefficient"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  } else {
    # Both plots combined
    p1 <- ggplot2::ggplot(x$vip, ggplot2::aes(x = day, y = VIP)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = vip_threshold, linetype = "dashed",
                          color = "red", alpha = 0.7) +
      ggplot2::scale_x_continuous(breaks = month_starts, labels = month_labels) +
      ggplot2::labs(title = "VIP Scores", x = NULL, y = "VIP") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())

    p2 <- ggplot2::ggplot(x$coefficients, ggplot2::aes(x = day, y = coefficient)) +
      ggplot2::geom_line(color = "darkgreen", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      ggplot2::scale_x_continuous(breaks = month_starts, labels = month_labels) +
      ggplot2::labs(title = "Coefficients", x = "Month", y = "Coefficient") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    p <- patchwork::wrap_plots(p1, p2, ncol = 1)
  }

  print(p)
  invisible(p)
}
