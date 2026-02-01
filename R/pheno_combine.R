# Prevent R CMD check notes
utils::globalVariables(c("year", "day", "s_id", "station", "year_effect",
                         "station_effect", "residual", "fitted", "set_id",
                         "n_sets", "set_size", "..by"))

#' Create Combined Phenological Time Series
#'
#' Estimates a combined (regional/national) phenological time series from
#' multi-station observations by separating year effects from station effects.
#' This is essential for creating representative time series from networks
#' with varying station coverage over time.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with columns \code{year}, \code{day}, and \code{s_id}.
#' @param by Character vector of grouping variables (e.g., \code{c("genus",
#'   "species", "phase_id")}). A separate combined series is created for each
#'   group. Default is \code{NULL} (treat all data as one group).
#' @param method Character. Estimation method:
#'   \describe{
#'     \item{"robust"}{(Default) Least Absolute Deviations (LAD/L1) regression.
#'       Minimizes sum of absolute residuals, robust to outliers.}
#'     \item{"mixed"}{Mixed-effects model with year as fixed effect and station
#'       as random effect. Uses REML estimation.}
#'     \item{"ols"}{Ordinary Least Squares. Sensitive to outliers but provides
#'       standard errors and confidence intervals.}
#'   }
#' @param min_years Integer. Minimum years of overlap required. Default 5.
#' @param min_stations Integer. Minimum stations required. Default 3.
#' @param check_connectivity Logical. If \code{TRUE} (default), checks whether
#'   station-year combinations form a connected set. Warns if data is
#'   disconnected (which prevents valid combined series estimation).
#'
#' @return An object of class \code{pheno_combined} containing:
#'   \describe{
#'     \item{combined}{Data.table with the combined time series: year,
#'       year_effect (the combined DOY), se (standard error, if available)}
#'     \item{station_effects}{Data.table of station effects: s_id, effect, se}
#'     \item{residuals}{Data.table with original data plus fitted values and
#'       residuals for outlier detection}
#'     \item{method}{Estimation method used}
#'     \item{connectivity}{Connectivity check results (if performed)}
#'     \item{by}{Grouping variables used}
#'     \item{group_results}{For grouped analysis, results per group}
#'   }
#'
#' @details
#' The model fitted is: \deqn{DOY_{ij} = \mu_i + \sigma_j + \epsilon_{ij}}
#' where \eqn{\mu_i} is the year effect (combined series), \eqn{\sigma_j} is
#' the station effect, and \eqn{\epsilon_{ij}} is the residual.
#'
#' Station effects represent systematic differences between stations (e.g.,
#' due to altitude, microclimate, or observer differences). The combined
#' series removes these effects to reveal the underlying temporal trend.
#'
#' @section Connectivity:
#' A prerequisite for valid estimation is that station-year combinations
#' form a "connected" set - meaning there must be overlapping years between
#' stations to separate year and station effects. The function checks this
#' automatically and warns if data is disconnected.
#'
#' @section Outlier Detection:
#' With \code{method = "robust"}, large residuals (e.g., > 30 days) indicate
#' potential outliers or data errors. The residuals table can be used to
#' identify and investigate these.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Apple flowering in Switzerland (small, fast subset)
#' apple_ch <- pep[species == "Malus domestica" &
#'                 phase_id == 60 & country == "Switzerland"]
#'
#' # Create combined series using OLS
#' combined <- pheno_combine(apple_ch, method = "ols")
#' print(combined)
#'
#' # Identify potential outliers (residuals > 30 days)
#' outliers <- combined$residuals[abs(residual) > 30]
#' outliers
#' }
#'
#' @references
#' Schaber J, Badeck F-W (2002). Evaluation of methods for the combination
#' of phenological time series and outlier detection. Tree Physiology 22:973-982.
#'
#' @seealso
#' \code{\link{pheno_normals}} for climatological baselines,
#' \code{\link{pheno_trend_turning}} for trend turning point detection,
#' \code{\link{check_connectivity}} for connectivity assessment
#'
#' @author Matthias Templ
#' @export
pheno_combine <- function(pep,
                           by = NULL,
                           method = c("robust", "mixed", "ols"),
                           min_years = 5,
                           min_stations = 3,
                           check_connectivity = TRUE) {

  method <- match.arg(method)

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Check required columns
  required <- c("year", "day", "s_id")
  missing <- setdiff(required, names(pep))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Check grouping columns
  if (!is.null(by)) {
    missing_by <- setdiff(by, names(pep))
    if (length(missing_by) > 0) {
      stop("Column(s) not found: ", paste(missing_by, collapse = ", "), call. = FALSE)
    }
  }

  # Function to fit combined model for one group
  fit_combined <- function(dt, group_label = NULL) {
    # Remove missing values
    dt <- dt[!is.na(day) & !is.na(year) & !is.na(s_id)]

    # Check minimum requirements
    n_years <- data.table::uniqueN(dt$year)
    n_stations <- data.table::uniqueN(dt$s_id)

    if (n_years < min_years || n_stations < min_stations) {
      return(list(
        combined = NULL,
        station_effects = NULL,
        residuals = NULL,
        connectivity = NULL,
        error = sprintf("Insufficient data: %d years, %d stations (need %d years, %d stations)",
                        n_years, n_stations, min_years, min_stations)
      ))
    }

    # Check connectivity if requested
    conn <- NULL
    if (check_connectivity) {
      conn <- check_connectivity_internal(dt)
      if (!conn$is_connected) {
        warning(sprintf("%sData is not fully connected (%d disconnected sets). ",
                        if (!is.null(group_label)) paste0(group_label, ": ") else "",
                        conn$n_sets),
                "Using largest connected subset.", call. = FALSE)
        # Use largest connected set
        dt <- dt[s_id %in% conn$largest_set$stations &
                   year %in% conn$largest_set$years]
      }
    }

    # Prepare factors (drop unused levels to avoid contrast errors)
    dt <- data.table::copy(dt)
    # Remove pep class to avoid interference with := assignment
    class(dt) <- c("data.table", "data.frame")
    dt[, year := factor(year)]
    dt[, station := factor(as.character(s_id))]

    # Fit model based on method
    result <- tryCatch({
      if (method == "robust") {
        fit_lad(dt)
      } else if (method == "mixed") {
        fit_mixed(dt)
      } else {
        fit_ols(dt)
      }
    }, error = function(e) {
      list(error = e$message)
    })

    if (!is.null(result$error)) {
      return(list(
        combined = NULL,
        station_effects = NULL,
        residuals = NULL,
        connectivity = conn,
        error = result$error
      ))
    }

    result$connectivity <- conn
    result$n_obs <- nrow(dt)
    result$n_years <- n_years
    result$n_stations <- n_stations
    result
  }

  # Process data
  if (is.null(by)) {
    result <- fit_combined(pep)
    result$by <- NULL
    result$method <- method
    result$group_results <- NULL
  } else {
    # Grouped analysis
    groups <- unique(pep[, ..by])
    group_results <- list()

    for (i in seq_len(nrow(groups))) {
      # Build filter
      filter_list <- lapply(by, function(col) pep[[col]] == groups[[col]][i])
      filter_idx <- Reduce(`&`, filter_list)
      group_data <- pep[filter_idx]

      group_label <- paste(sapply(by, function(col) as.character(groups[[col]][i])),
                           collapse = " | ")

      group_results[[group_label]] <- fit_combined(group_data, group_label)
    }

    # Combine results
    all_combined <- data.table::rbindlist(
      lapply(names(group_results), function(nm) {
        res <- group_results[[nm]]
        if (!is.null(res$combined)) {
          cbind(group = nm, res$combined)
        }
      }),
      fill = TRUE
    )

    result <- list(
      combined = all_combined,
      method = method,
      by = by,
      n_groups = nrow(groups),
      group_results = group_results
    )
  }

  class(result) <- c("pheno_combined", "list")
  result
}


#' Fit LAD (Robust) Model
#' @keywords internal
fit_lad <- function(dt) {
  # Use quantreg for LAD regression
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' required for robust estimation", call. = FALSE)
  }

  # Fit model: day ~ year + station - 1 (no intercept, with sum-to-zero constraint)
  # Using treatment contrasts and adjusting afterwards for efficiency
  fit <- quantreg::rq(day ~ year + station, data = dt, tau = 0.5)

  coefs <- coef(fit)
  year_levels <- levels(dt$year)
  station_levels <- levels(dt$station)

  # Extract year effects (combined series)
  year_idx <- grep("^year", names(coefs))
  year_effects <- coefs[year_idx]
  names(year_effects) <- gsub("^year", "", names(year_effects))

  # Add intercept to first year and create full series
  intercept <- coefs["(Intercept)"]
  if (!is.na(intercept)) {
    year_effects <- year_effects + intercept
    # First year level is in the intercept
    year_effects <- c(intercept, year_effects)
    names(year_effects)[1] <- year_levels[1]
  }

  # Extract station effects
  station_idx <- grep("^station", names(coefs))
  station_effects <- coefs[station_idx]
  names(station_effects) <- gsub("^station", "", names(station_effects))

  # First station is reference (effect = 0), adjust to sum-to-zero
  station_effects <- c(0, station_effects)
  names(station_effects)[1] <- station_levels[1]
  station_mean <- mean(station_effects)
  station_effects <- station_effects - station_mean

  # Compute residuals
  dt[, fitted := predict(fit)]
  dt[, residual := day - fitted]

  list(
    combined = data.table::data.table(
      year = as.integer(names(year_effects)),
      year_effect = as.numeric(year_effects)
    )[order(year)],
    station_effects = data.table::data.table(
      s_id = names(station_effects),
      station_effect = as.numeric(station_effects)
    ),
    residuals = dt[, .(s_id, year = as.integer(as.character(year)),
                       day, fitted, residual)],
    fit = fit,
    error = NULL
  )
}


#' Fit Mixed-Effects Model
#' @keywords internal
fit_mixed <- function(dt) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("Package 'nlme' required for mixed-effects estimation", call. = FALSE)
  }

  # Fit: day ~ year (fixed) with station as random effect
  fit <- nlme::lme(day ~ year - 1, random = ~ 1 | station, data = dt,
                   method = "REML")

  # Extract fixed effects (year effects = combined series)
  fixed_effects <- nlme::fixef(fit)
  year_levels <- levels(dt$year)
  names(fixed_effects) <- year_levels

  # Get standard errors from model summary
  fit_summary <- summary(fit)
  se_fixed <- fit_summary$tTable[, "Std.Error"]

  # Extract random effects (station effects)
  random_effects <- nlme::ranef(fit)
  if (is.data.frame(random_effects)) {
    station_levels <- rownames(random_effects)
    random_vals <- random_effects[, 1]
  } else {
    station_levels <- rownames(random_effects[[1]])
    random_vals <- random_effects[[1]][, 1]
  }
  names(random_vals) <- station_levels

  # Compute residuals
  dt[, fitted := predict(fit)]
  dt[, residual := day - fitted]

  combined_dt <- data.table::data.table(
    year = as.integer(names(fixed_effects)),
    year_effect = as.numeric(fixed_effects),
    se = as.numeric(se_fixed)
  )

  # Extract variance components
  vc <- nlme::VarCorr(fit)
  var_station <- as.numeric(vc[1, "Variance"])
  var_residual <- as.numeric(vc[2, "Variance"])

  list(
    combined = combined_dt[order(year)],
    station_effects = data.table::data.table(
      s_id = names(random_vals),
      station_effect = as.numeric(random_vals)
    ),
    residuals = dt[, .(s_id, year = as.integer(as.character(year)),
                       day, fitted, residual)],
    fit = fit,
    var_station = var_station,
    var_residual = var_residual,
    error = NULL
  )
}


#' Fit OLS Model
#' @keywords internal
fit_ols <- function(dt) {
  # Sum-to-zero contrasts for station
  contrasts(dt$station) <- contr.sum(levels(dt$station))

  fit <- lm(day ~ year + station - 1, data = dt)
  coefs <- coef(fit)
  se <- summary(fit)$coefficients[, "Std. Error"]

  year_levels <- levels(dt$year)
  station_levels <- levels(dt$station)

  # Extract year effects
  year_idx <- grep("^year", names(coefs))
  year_effects <- coefs[year_idx]
  year_se <- se[year_idx]
  names(year_effects) <- gsub("^year", "", names(year_effects))
  names(year_se) <- names(year_effects)

  # Extract station effects (sum-to-zero, last level is -sum(others))
  station_idx <- grep("^station", names(coefs))
  station_effects <- coefs[station_idx]
  names(station_effects) <- station_levels[-length(station_levels)]
  # Last station effect is negative sum of others
  station_effects <- c(station_effects, -sum(station_effects))
  names(station_effects)[length(station_effects)] <- station_levels[length(station_levels)]

  # Compute residuals
  dt[, fitted := predict(fit)]
  dt[, residual := day - fitted]

  list(
    combined = data.table::data.table(
      year = as.integer(names(year_effects)),
      year_effect = as.numeric(year_effects),
      se = as.numeric(year_se)
    )[order(year)],
    station_effects = data.table::data.table(
      s_id = names(station_effects),
      station_effect = as.numeric(station_effects)
    ),
    residuals = dt[, .(s_id, year = as.integer(as.character(year)),
                       day, fitted, residual)],
    fit = fit,
    r_squared = summary(fit)$r.squared,
    error = NULL
  )
}


#' Check Connectivity of Station-Year Data (Internal)
#' @keywords internal
check_connectivity_internal <- function(dt) {
  # Create station-year matrix
  stations <- unique(dt$s_id)
  years <- sort(unique(dt$year))

  # Create incidence matrix (1 if station has data in year, 0 otherwise)
  present <- dt[, .(present = 1), by = .(s_id, year)]
  mat <- matrix(0, nrow = length(stations), ncol = length(years),
                dimnames = list(stations, years))

  for (i in seq_len(nrow(present))) {
    mat[as.character(present$s_id[i]), as.character(present$year[i])] <- 1
  }

  # Find connected components using breadth-first search
  n_stations <- length(stations)
  n_years <- length(years)

  station_set <- rep(0, n_stations)
  year_set <- rep(0, n_years)
  current_set <- 0

  for (s in seq_len(n_stations)) {
    if (station_set[s] == 0 && any(mat[s, ] > 0)) {
      current_set <- current_set + 1

      # BFS from this station
      station_queue <- s
      while (length(station_queue) > 0) {
        curr_s <- station_queue[1]
        station_queue <- station_queue[-1]

        if (station_set[curr_s] != 0) next
        station_set[curr_s] <- current_set

        # Find years this station has data
        connected_years <- which(mat[curr_s, ] > 0)
        for (y in connected_years) {
          if (year_set[y] == 0) {
            year_set[y] <- current_set
            # Find other stations with data in this year
            connected_stations <- which(mat[, y] > 0)
            station_queue <- c(station_queue,
                               connected_stations[station_set[connected_stations] == 0])
          }
        }
      }
    }
  }

  # Count sets and their sizes
  n_sets <- max(station_set)
  set_sizes <- sapply(seq_len(n_sets), function(i) {
    sum(mat[station_set == i, year_set == i, drop = FALSE] > 0)
  })

  # Find largest set
  largest_idx <- which.max(set_sizes)

  list(
    is_connected = n_sets == 1,
    n_sets = n_sets,
    set_sizes = set_sizes,
    largest_set = list(
      stations = stations[station_set == largest_idx],
      years = years[year_set == largest_idx],
      n_obs = set_sizes[largest_idx]
    ),
    station_sets = station_set,
    year_sets = year_set
  )
}


#' Check Station-Year Connectivity
#'
#' Checks whether station-year combinations in phenological data form a
#' connected set, which is required for valid combined time series estimation.
#'
#' @param pep A \code{pep} object or data.table with \code{year}, \code{day},
#'   and \code{s_id} columns.
#' @param by Optional grouping variables to check connectivity within groups.
#'
#' @return A list with connectivity information:
#'   \describe{
#'     \item{is_connected}{Logical. TRUE if all data forms one connected set.}
#'     \item{n_sets}{Number of disconnected sets found.}
#'     \item{set_sizes}{Number of observations in each set.}
#'     \item{largest_set}{Stations and years in the largest connected set.}
#'   }
#'
#' @details
#' Connectivity means that there exist overlapping years between stations,
#' allowing year effects to be separated from station effects. If data is
#' disconnected, the combined series cannot be uniquely estimated across
#' the disconnected parts.
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' conn <- check_connectivity(pep)
#' if (!conn$is_connected) {
#'   warning("Data has ", conn$n_sets, " disconnected sets")
#' }
#' }
#'
#' @seealso \code{\link{pheno_combine}} which uses this internally
#' @author Matthias Templ
#' @export
check_connectivity <- function(pep, by = NULL) {
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  required <- c("year", "day", "s_id")
  missing <- setdiff(required, names(pep))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (is.null(by)) {
    result <- check_connectivity_internal(pep)
    class(result) <- c("pep_connectivity", "list")
    return(result)
  }

  # Grouped analysis
  groups <- unique(pep[, ..by])
  results <- list()

  for (i in seq_len(nrow(groups))) {
    filter_list <- lapply(by, function(col) pep[[col]] == groups[[col]][i])
    filter_idx <- Reduce(`&`, filter_list)
    group_data <- pep[filter_idx]

    group_label <- paste(sapply(by, function(col) as.character(groups[[col]][i])),
                         collapse = " | ")
    results[[group_label]] <- check_connectivity_internal(group_data)
  }

  summary_dt <- data.table::rbindlist(lapply(names(results), function(nm) {
    data.table::data.table(
      group = nm,
      is_connected = results[[nm]]$is_connected,
      n_sets = results[[nm]]$n_sets,
      largest_set_size = results[[nm]]$largest_set$n_obs
    )
  }))

  result <- list(
    summary = summary_dt,
    group_results = results,
    by = by
  )
  class(result) <- c("pep_connectivity", "list")
  result
}


#' Print Method for Combined Time Series
#' @param x A \code{pheno_combined} object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns \code{x}
#' @export
print.pheno_combined <- function(x, ...) {
  cat("Combined Phenological Time Series\n")
  cat(strrep("=", 50), "\n")
  cat("Method:", x$method, "\n")

  if (!is.null(x$by)) {
    cat("Grouping:", paste(x$by, collapse = ", "), "\n")
    cat("Number of groups:", x$n_groups, "\n\n")

    # Summary of groups
    successful <- sum(sapply(x$group_results, function(g) is.null(g$error)))
    cat(sprintf("Successfully estimated: %d / %d groups\n", successful, x$n_groups))

    if (!is.null(x$combined) && nrow(x$combined) > 0) {
      cat("\nCombined series (first 10 rows):\n")
      print(head(x$combined, 10))
    }
  } else {
    if (!is.null(x$error)) {
      cat("\nError:", x$error, "\n")
    } else {
      cat(sprintf("Observations: %d\n", x$n_obs))
      cat(sprintf("Years: %d\n", x$n_years))
      cat(sprintf("Stations: %d\n", x$n_stations))

      if (!is.null(x$connectivity) && !x$connectivity$is_connected) {
        cat(sprintf("Warning: Data had %d disconnected sets (used largest)\n",
                    x$connectivity$n_sets))
      }

      cat("\nCombined time series:\n")
      print(x$combined)

      cat("\nStation effects (first 10):\n")
      print(head(x$station_effects[order(-abs(station_effect))], 10))

      cat("\nResidual summary:\n")
      cat(sprintf("  Mean: %.2f days\n", mean(x$residuals$residual, na.rm = TRUE)))
      cat(sprintf("  SD: %.2f days\n", sd(x$residuals$residual, na.rm = TRUE)))
      cat(sprintf("  |Residual| > 30 days: %d observations\n",
                  sum(abs(x$residuals$residual) > 30, na.rm = TRUE)))
    }
  }

  invisible(x)
}


#' Plot Method for Combined Time Series
#' @param x A \code{pheno_combined} object
#' @param type Character. "series" for combined time series, "stations" for
#'   station effects, "residuals" for residual distribution.
#' @param group For grouped analysis, which group to plot.
#' @param ... Additional arguments
#' @return A ggplot object (invisibly)
#' @export
plot.pheno_combined <- function(x, type = c("series", "stations", "residuals"),
                                 group = NULL, ...) {
  type <- match.arg(type)

  # Get data to plot
  if (!is.null(x$by) && !is.null(x$group_results)) {
    if (is.null(group)) {
      # Find first successful group
      successful <- names(which(sapply(x$group_results, function(g) is.null(g$error))))
      if (length(successful) == 0) {
        stop("No successfully estimated groups to plot", call. = FALSE)
      }
      group <- successful[1]
      message("Plotting group: ", group)
    }
    data <- x$group_results[[group]]
    title_suffix <- paste0("\n(", group, ")")
  } else {
    data <- x
    title_suffix <- ""
  }

  if (is.null(data) || !is.null(data$error)) {
    stop("No valid results to plot", call. = FALSE)
  }

  if (type == "series") {
    p <- ggplot2::ggplot(data$combined, ggplot2::aes(x = year, y = year_effect)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 1) +
      ggplot2::geom_point(color = "steelblue", size = 2)

    if ("se" %in% names(data$combined)) {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = year_effect - 1.96 * se,
                     ymax = year_effect + 1.96 * se),
        alpha = 0.2, fill = "steelblue"
      )
    }

    p <- p +
      ggplot2::labs(
        title = paste0("Combined Phenological Time Series", title_suffix),
        subtitle = paste("Method:", x$method),
        x = "Year",
        y = "Day of Year (DOY)"
      ) +
      ggplot2::theme_minimal()

  } else if (type == "stations") {
    effects <- data$station_effects[order(station_effect)]
    effects$s_id <- factor(effects$s_id, levels = effects$s_id)

    if (nrow(effects) > 30) {
      # Show only most extreme stations
      effects <- rbind(head(effects, 15), tail(effects, 15))
    }

    p <- ggplot2::ggplot(effects, ggplot2::aes(x = station_effect, y = s_id)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::labs(
        title = paste0("Station Effects", title_suffix),
        subtitle = "Deviation from combined series (days)",
        x = "Station Effect (days)",
        y = "Station ID"
      ) +
      ggplot2::theme_minimal()

  } else {
    p <- ggplot2::ggplot(data$residuals, ggplot2::aes(x = residual)) +
      ggplot2::geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = c(-30, 30), linetype = "dashed",
                          color = "red", alpha = 0.7) +
      ggplot2::labs(
        title = paste0("Residual Distribution", title_suffix),
        subtitle = "Red lines at +/-30 days (potential outliers)",
        x = "Residual (days)",
        y = "Count"
      ) +
      ggplot2::theme_minimal()
  }

  print(p)
  invisible(p)
}
