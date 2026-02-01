#' Analyze Phenological Gradients with Elevation or Latitude
#'
#' Quantifies how phenological timing shifts with altitude or latitude,
#' calculating the phenological lapse rate using robust regression methods.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and either \code{alt}
#'   (altitude) or \code{lat} (latitude).
#' @param variable Character string specifying the gradient variable.
#'   Either \code{"alt"} (altitude/elevation) or \code{"lat"} (latitude).
#'   Default is \code{"alt"}.
#' @param species Optional character string to filter by species/genus.
#' @param phase_id Optional integer to filter by a single BBCH phase code.
#' @param year_range Optional integer vector of length 2 specifying years to include.
#' @param by Optional character vector of grouping variables (e.g., "country").
#'   If provided, separate gradients are calculated for each group.
#' @param method Character string specifying regression method:
#'   \itemize{
#'     \item \code{"robust"} (default): Robust regression using \code{robustbase::lmrob}
#'     \item \code{"ols"}: Ordinary least squares regression
#'     \item \code{"quantile"}: Quantile regression for median using \code{quantreg::rq}
#'   }
#' @param aggregate Logical. If \code{TRUE} (default), first aggregate to station
#'   means before fitting. If \code{FALSE}, use all individual observations.
#' @param na.rm Logical. Should missing values be removed? Default \code{TRUE}.
#'
#' @return A list with class \code{pheno_gradient} containing:
#'   \describe{
#'     \item{summary}{A \code{data.table} with slope, intercept, R-squared,
#'       RMSE, n_stations, and interpretation}
#'     \item{model}{The fitted model object(s)}
#'     \item{data}{The data used for fitting}
#'     \item{variable}{The gradient variable used}
#'     \item{method}{The regression method used}
#'   }
#'
#' @details
#' Phenological gradients describe how the timing of biological events changes
#' with environmental factors. This function calculates:
#'
#' \itemize{
#'   \item \strong{Altitude gradient}: Typically 2-4 days delay per 100m elevation
#'     gain in temperate regions
#'   \item \strong{Latitude gradient}: Typically 2-5 days delay per degree north
#'     in Europe
#' }
#'
#' The robust regression method (default) is recommended because phenological
#' data often contains outliers from observation errors or microclimate effects.
#'
#' @section Interpretation:
#' The slope represents:
#' \itemize{
#'   \item For altitude: days delay per 100 meters elevation increase
#'   \item For latitude: days delay per degree latitude increase
#' }
#' Positive slopes indicate later phenology at higher elevations/latitudes.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Subset to fewer countries for speed
#' pep_subset <- pep[country %in% c("Germany", "Switzerland", "Austria")]
#'
#' # Altitude gradient for wheat heading
#' grad_alt <- pheno_gradient(pep_subset,
#'                            variable = "alt",
#'                            species = "Triticum",
#'                            phase_id = 60)
#' print(grad_alt)
#'
#' # Latitude gradient
#' grad_lat <- pheno_gradient(pep_subset,
#'                            variable = "lat",
#'                            species = "Triticum",
#'                            phase_id = 60)
#'
#' # Compare regression methods
#' grad_ols <- pheno_gradient(pep_subset, species = "Triticum",
#'                            phase_id = 60, method = "ols")
#' grad_robust <- pheno_gradient(pep_subset, species = "Triticum",
#'                               phase_id = 60, method = "robust")
#'
#' # Gradient by country (separate regression per country)
#' grad_by_country <- pheno_gradient(pep_subset,
#'                                   variable = "alt",
#'                                   species = "Triticum",
#'                                   phase_id = 60,
#'                                   by = "country")
#' }
#'
#' @seealso
#' \code{\link{pheno_normals}} for calculating climatological normals,
#' \code{\link[robustbase]{lmrob}} for robust regression details
#'
#' @author Matthias Templ
#' @export
#' @importFrom robustbase lmrob
pheno_gradient <- function(pep,
                           variable = c("alt", "lat"),
                           species = NULL,
                           phase_id = NULL,
                           year_range = NULL,
                           by = NULL,
                           method = c("robust", "ols", "quantile"),
                           aggregate = TRUE,
                           na.rm = TRUE) {

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Match arguments
  variable <- match.arg(variable)
  method <- match.arg(method)

  # Check required columns
  required_cols <- c("year", "day", variable)
  if (variable == "alt" && !"alt" %in% names(pep)) {
    # Try alternative column names
    if ("altitude" %in% names(pep)) {
      pep[, alt := altitude]
    } else if ("elevation" %in% names(pep)) {
      pep[, alt := elevation]
    } else {
      stop("Altitude column not found. Expected 'alt', 'altitude', or 'elevation'.",
           call. = FALSE)
    }
  }

  missing_cols <- setdiff(required_cols, names(pep))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Check quantreg availability for quantile method
  if (method == "quantile" && !requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for quantile regression. ",
         "Install it with: install.packages('quantreg')", call. = FALSE)
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
        warning("Multiple phase_ids provided. Using first one only for gradient analysis.",
                call. = FALSE)
        phase_id <- phase_id[1]
      }
      dt <- dt[dt$phase_id == phase_id]
      if (nrow(dt) == 0) {
        stop("No observations found for phase_id: ", phase_id, call. = FALSE)
      }
    }
  }

  # Apply year range filter
  if (!is.null(year_range)) {
    if (!is.numeric(year_range) || length(year_range) != 2) {
      stop("'year_range' must be a numeric vector of length 2", call. = FALSE)
    }
    dt <- dt[year >= year_range[1] & year <= year_range[2]]
    if (nrow(dt) == 0) {
      stop("No observations found in year range.", call. = FALSE)
    }
  }

  # Remove rows with missing gradient variable
  dt <- dt[!is.na(get(variable)) & !is.na(day)]

  if (nrow(dt) == 0) {
    stop("No valid observations after removing missing values.", call. = FALSE)
  }

  # Function to fit gradient model
  fit_gradient <- function(data, var_name, method_name) {
    # Prepare data for modeling
    if (aggregate && "s_id" %in% names(data)) {
      # Aggregate to station means
      agg_data <- data[, .(
        mean_doy = mean(day, na.rm = na.rm),
        var_value = mean(get(var_name), na.rm = na.rm),
        n_obs = .N
      ), by = "s_id"]
    } else {
      agg_data <- data.table::data.table(
        mean_doy = data$day,
        var_value = data[[var_name]],
        n_obs = 1L
      )
    }

    # Remove any remaining NAs
    agg_data <- agg_data[!is.na(mean_doy) & !is.na(var_value)]

    if (nrow(agg_data) < 3) {
      return(list(
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = NA_real_,
        rmse = NA_real_,
        n_points = nrow(agg_data),
        model = NULL,
        data = agg_data,
        error = "Insufficient data points"
      ))
    }

    # Fit model based on method
    tryCatch({
      if (method_name == "robust") {
        model <- robustbase::lmrob(mean_doy ~ var_value, data = agg_data)
        coefs <- coef(model)
        r_sq <- summary(model)$r.squared
      } else if (method_name == "ols") {
        model <- lm(mean_doy ~ var_value, data = agg_data)
        coefs <- coef(model)
        r_sq <- summary(model)$r.squared
      } else {
        # quantile regression
        model <- quantreg::rq(mean_doy ~ var_value, data = agg_data, tau = 0.5)
        coefs <- coef(model)
        # Pseudo R-squared for quantile regression
        null_model <- quantreg::rq(mean_doy ~ 1, data = agg_data, tau = 0.5)
        r_sq <- 1 - sum(abs(residuals(model))) / sum(abs(residuals(null_model)))
      }

      # Calculate RMSE
      rmse <- sqrt(mean(residuals(model)^2))

      # Scale slope appropriately
      if (var_name == "alt") {
        # Report as days per 100m
        slope_scaled <- coefs[2] * 100
        unit <- "days per 100m"
      } else {
        # Report as days per degree
        slope_scaled <- coefs[2]
        unit <- "days per degree"
      }

      list(
        slope = as.numeric(slope_scaled),
        slope_raw = as.numeric(coefs[2]),
        intercept = as.numeric(coefs[1]),
        r_squared = as.numeric(r_sq),
        rmse = as.numeric(rmse),
        n_points = nrow(agg_data),
        unit = unit,
        model = model,
        data = agg_data,
        error = NULL
      )
    }, error = function(e) {
      list(
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = NA_real_,
        rmse = NA_real_,
        n_points = nrow(agg_data),
        model = NULL,
        data = agg_data,
        error = as.character(e$message)
      )
    })
  }

  # Create interpretation text (vectorized)
  create_interpretation <- function(slope, var_name, r_sq) {
    # Handle single value case
    if (length(slope) == 1) {
      if (is.na(slope)) return(NA_character_)

      direction <- if (slope > 0) "later" else "earlier"
      abs_slope <- abs(slope)

      if (var_name == "alt") {
        return(sprintf("Phenology is %.1f days %s per 100m elevation increase (R-sq = %.2f)",
                abs_slope, direction, r_sq))
      } else {
        return(sprintf("Phenology is %.1f days %s per degree latitude increase (R-sq = %.2f)",
                abs_slope, direction, r_sq))
      }
    }

    # Handle vector case
    mapply(function(s, r) {
      if (is.na(s)) return(NA_character_)

      direction <- if (s > 0) "later" else "earlier"
      abs_slope <- abs(s)

      if (var_name == "alt") {
        sprintf("Phenology is %.1f days %s per 100m elevation increase (R-sq = %.2f)",
                abs_slope, direction, r)
      } else {
        sprintf("Phenology is %.1f days %s per degree latitude increase (R-sq = %.2f)",
                abs_slope, direction, r)
      }
    }, slope, r_sq, USE.NAMES = FALSE)
  }

  # Fit models
  if (is.null(by)) {
    # Single gradient for all data
    result_fit <- fit_gradient(dt, variable, method)

    summary_dt <- data.table::data.table(
      variable = variable,
      method = method,
      slope = result_fit$slope,
      intercept = result_fit$intercept,
      r_squared = result_fit$r_squared,
      rmse = result_fit$rmse,
      n_points = result_fit$n_points,
      interpretation = create_interpretation(result_fit$slope, variable, result_fit$r_squared)
    )

    result <- list(
      summary = summary_dt,
      model = result_fit$model,
      data = result_fit$data,
      variable = variable,
      method = method,
      species = species,
      phase_id = phase_id
    )

  } else {
    # Gradient by groups
    if (!all(by %in% names(dt))) {
      stop("Some grouping variables not found in data: ",
           paste(setdiff(by, names(dt)), collapse = ", "), call. = FALSE)
    }

    # Fit gradient for each group
    group_results <- dt[, {
      fit <- fit_gradient(.SD, variable, method)
      list(
        slope = fit$slope,
        intercept = fit$intercept,
        r_squared = fit$r_squared,
        rmse = fit$rmse,
        n_points = fit$n_points
      )
    }, by = by]

    # Add interpretation
    group_results[, interpretation := create_interpretation(slope, variable, r_squared)]

    # Add metadata columns
    group_results[, variable := variable]
    group_results[, method := method]

    result <- list(
      summary = group_results,
      model = NULL,  # Multiple models, not stored individually
      data = dt,
      variable = variable,
      method = method,
      by = by,
      species = species,
      phase_id = phase_id
    )
  }

  class(result) <- c("pheno_gradient", "list")
  result
}


#' Print Method for Phenological Gradient Analysis
#'
#' @param x A \code{pheno_gradient} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.pheno_gradient <- function(x, ...) {
  cat("Phenological Gradient Analysis\n")
  cat(strrep("-", 50), "\n")
  cat(sprintf("Gradient variable: %s\n", x$variable))
  cat(sprintf("Regression method: %s\n", x$method))

  if (!is.null(x$species)) {
    cat(sprintf("Species: %s\n", x$species))
  }
  if (!is.null(x$phase_id)) {
    cat(sprintf("Phase ID: %d\n", x$phase_id))
  }

  cat(strrep("-", 50), "\n\n")

  # Print summary table
  print(x$summary)

  # Print interpretation for single gradient
  if (nrow(x$summary) == 1 && !is.na(x$summary$interpretation[1])) {
    cat("\nInterpretation:\n")
    cat("  ", x$summary$interpretation[1], "\n")
  }

  invisible(x)
}


#' Plot Method for Phenological Gradient Analysis
#'
#' @param x A \code{pheno_gradient} object
#' @param ... Additional arguments passed to ggplot
#'
#' @return A ggplot object
#' @author Matthias Templ
#' @export
plot.pheno_gradient <- function(x, ...) {
  if (is.null(x$data) || nrow(x$data) == 0) {
    stop("No data available for plotting", call. = FALSE)
  }

  # Prepare plot data
  plot_data <- x$data

  # Determine x-axis label
  if (x$variable == "alt") {
    x_lab <- "Altitude (m)"
    x_var <- "var_value"
  } else {
    x_lab <- "Latitude (degrees N)"
    x_var <- "var_value"
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = var_value, y = mean_doy)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
    ggplot2::labs(
      x = x_lab,
      y = "Day of Year",
      title = sprintf("Phenological %s Gradient",
                      ifelse(x$variable == "alt", "Elevation", "Latitude")),
      subtitle = sprintf("Method: %s | R-sq = %.3f",
                         x$method,
                         ifelse(!is.null(x$summary$r_squared[1]),
                                x$summary$r_squared[1], NA))
    ) +
    ggplot2::theme_minimal()

  p
}
