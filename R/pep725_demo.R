#' Demonstrate the pep725 Package Functions
#'
#' This function provides an interactive demonstration of the main features
#' of the pep725 package for PEP725 phenological data analysis. It showcases
#' data exploration, filtering, summarization, and visualization capabilities.
#'
#' @param which Character vector specifying which demos to run. Options are:
#'   \itemize{
#'     \item \code{"class"} - Demonstrate the pep class (print, summary)
#'     \item \code{"filter"} - Show data filtering and selection
#'     \item \code{"plot"} - Display visualization capabilities
#'     \item \code{"analysis"} - Show analysis functions (normals, anomalies)
#'     \item \code{"all"} - Run all demonstrations (default)
#'   }
#' @param pause Logical. If \code{TRUE} (default), pauses between demos
#'   waiting for user input. Set to \code{FALSE} for non-interactive use.
#'
#' @return Invisibly returns a list containing example outputs from each demo.
#'
#' @details
#' The demonstration downloads synthetic PEP725 data using \code{pep_download()}
#' and walks through typical workflows for phenological data analysis:
#'
#' \enumerate{
#'   \item \strong{Class demo}: Shows the enhanced print and summary methods
#'     for the pep class, demonstrating how to quickly understand the data.
#'   \item \strong{Filter demo}: Demonstrates selecting specific species,
#'     phases, and time periods using data.table syntax.
#'   \item \strong{Plot demo}: Creates visualizations including station maps,
#'     time series, and DOY distributions.
#'   \item \strong{Analysis demo}: Shows phenological normals calculation,
#'     anomaly detection, and data quality assessment.
#' }
#'
#' @examples
#' \donttest{
#' # Run all demos interactively
#' pep725_demo()
#'
#' # Run specific demos
#' pep725_demo(which = "class")
#' pep725_demo(which = c("filter", "plot"))
#'
#' # Run without pausing (for scripts)
#' pep725_demo(pause = FALSE)
#' }
#'
#' @seealso
#' \code{\link{pep_download}} for downloading synthetic data,
#' \code{\link{pheno_normals}} for calculating normals,
#' \code{\link{pheno_anomaly}} for anomaly detection,
#' \code{\link{pep_quality}} for quality assessment
#'
#' @author Matthias Templ
#' @export
pep725_demo <- function(which = "all", pause = interactive()) {

  # Validate input
  valid_demos <- c("all", "class", "filter", "plot", "analysis")
  if (!all(which %in% valid_demos)) {
    stop("Invalid demo selection. Choose from: ",
         paste(valid_demos, collapse = ", "))
  }

  if ("all" %in% which) {
    which <- c("class", "filter", "plot", "analysis")
  }

  results <- list()

  # Helper function for pausing
  wait <- function(msg = "Press [Enter] to continue...") {
    if (pause) {
      readline(prompt = msg)
    }
  }

  # Header
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("       PEP725 PACKAGE DEMONSTRATION\n")
  cat("       Pan-European Phenological Data Analysis\n")
  cat(strrep("=", 60), "\n\n")

  # Download synthetic data
  cat("Downloading synthetic PEP725 data...\n\n")
  pep <- pep_download()

  cat("Data loaded successfully!\n")
  cat(sprintf("  - %s observations\n", format(nrow(pep), big.mark = ",")))
  cat(sprintf("  - %d stations\n", length(unique(pep$s_id))))
  cat(sprintf("  - %d species\n", length(unique(pep$species))))
  cat(sprintf("  - Years: %d - %d\n\n", min(pep$year), max(pep$year)))

  wait("Press [Enter] to start the demonstration...")

  # =========================================================================
  # DEMO 1: Class features
  # =========================================================================
  if ("class" %in% which) {
    cat("\n")
    cat(strrep("-", 60), "\n")
    cat("DEMO 1: The pep Class - Smart Data Display\n")
    cat(strrep("-", 60), "\n\n")

    cat("The pep object has enhanced print/summary methods.\n")
    cat("Instead of dumping millions of rows, you get a useful overview:\n\n")

    cat(">>> print(pep)\n\n")
    print(pep)

    wait("\nPress [Enter] to see summary by species...")

    cat("\n>>> summary(pep, by = 'species', top = 5)\n\n")
    s1 <- summary(pep, by = "species", top = 5)

    wait("\nPress [Enter] to see summary by phase...")

    cat("\n>>> summary(pep, by = 'phase')\n\n")
    s2 <- summary(pep, by = "phase")

    wait("\nPress [Enter] to see data coverage...")

    cat("\n>>> coverage(pep)\n\n")
    cov <- coverage(pep)

    results$class <- list(summary_species = s1, summary_phase = s2, coverage = cov)

    wait("\nPress [Enter] to continue to filtering demo...")
  }

  # =========================================================================
  # DEMO 2: Data filtering
  # =========================================================================
  if ("filter" %in% which) {
    cat("\n")
    cat(strrep("-", 60), "\n")
    cat("DEMO 2: Data Filtering with data.table syntax\n")
    cat(strrep("-", 60), "\n\n")

    cat("The pep class inherits from data.table, so you can use\n")
    cat("all data.table operations directly:\n\n")

    cat(">>> # Filter wheat observations from 1990 onwards\n")
    cat(">>> wheat <- pep[species == 'Triticum aestivum' & year >= 1990]\n")
    cat(">>> wheat\n\n")

    wheat <- pep[species == "Triticum aestivum" & year >= 1990]
    print(wheat)

    wait("\nPress [Enter] to see subsetting preserves class...")

    cat("\n>>> # Check that class is preserved after subsetting\n")
    cat(">>> class(wheat)\n")
    cat(paste(class(wheat), collapse = ", "), "\n")

    cat("\n>>> # Aggregate by year and phase\n")
    cat(">>> wheat_agg <- wheat[, .(\n")
    cat(">>>   n_obs = .N,\n")
    cat(">>>   mean_doy = mean(day, na.rm = TRUE),\n")
    cat(">>>   sd_doy = sd(day, na.rm = TRUE)\n")
    cat(">>> ), by = .(year, phase_id)]\n\n")

    wheat_agg <- wheat[, .(
      n_obs = .N,
      mean_doy = mean(day, na.rm = TRUE),
      sd_doy = sd(day, na.rm = TRUE)
    ), by = .(year, phase_id)]

    cat("Result:\n")
    print(head(wheat_agg[order(year, phase_id)], 15))

    cat("\n>>> # Look up phase descriptions\n")
    cat(">>> bbch_description(c(60, 65, 100))\n")
    print(bbch_description(c(60, 65, 100)))

    results$filter <- list(wheat = wheat, wheat_agg = wheat_agg)

    wait("\nPress [Enter] to continue to plotting demo...")
  }

  # =========================================================================
  # DEMO 3: Visualization
  # =========================================================================
  if ("plot" %in% which) {
    cat("\n")
    cat(strrep("-", 60), "\n")
    cat("DEMO 3: Visualization with plot.pep()\n")
    cat(strrep("-", 60), "\n\n")

    cat("The pep class has built-in plot methods:\n\n")

    cat(">>> # Station map\n")
    cat(">>> plot(pep, type = 'map')\n\n")

    p1 <- plot(pep, type = "map")

    wait("Press [Enter] to see DOY histogram...")

    cat("\n>>> # DOY distribution\n")
    cat(">>> plot(pep, type = 'histogram')\n\n")

    p2 <- plot(pep, type = "histogram")

    wait("Press [Enter] to see time series...")

    cat("\n>>> # Time series of mean DOY\n")
    cat(">>> plot(pep, type = 'timeseries')\n\n")

    p3 <- plot(pep, type = "timeseries")

    results$plot <- list(map = p1, histogram = p2, timeseries = p3)

    wait("\nPress [Enter] to continue to analysis demo...")
  }

  # =========================================================================
  # DEMO 4: Analysis
  # =========================================================================
  if ("analysis" %in% which) {
    cat("\n")
    cat(strrep("-", 60), "\n")
    cat("DEMO 4: Phenological Analysis Functions\n")
    cat(strrep("-", 60), "\n\n")

    # Get wheat data for analysis
    wheat <- pep[species == "Triticum aestivum"]

    cat(">>> # Calculate phenological normals for wheat\n")
    cat(">>> normals <- pheno_normals(\n")
    cat(">>>   pep = wheat,\n")
    cat(">>>   period = 1990:2015,\n")
    cat(">>>   by = c('country', 'phase_id'),\n")
    cat(">>>   min_years = 10\n")
    cat(">>> )\n\n")

    normals <- pheno_normals(
      pep = wheat,
      period = 1990:2015,
      by = c("country", "phase_id"),
      min_years = 10
    )

    cat("Phenological normals:\n")
    print(normals)

    wait("\nPress [Enter] to see anomaly detection...")

    cat("\n>>> # Detect phenological anomalies\n")
    cat(">>> anomalies <- pheno_anomaly(\n")
    cat(">>>   pep = wheat,\n")
    cat(">>>   baseline_period = 1990:2010,\n")
    cat(">>>   by = c('country', 'phase_id'),\n")
    cat(">>>   min_years = 5\n")
    cat(">>> )\n\n")

    anomalies <- pheno_anomaly(
      pep = wheat,
      baseline_period = 1990:2010,
      by = c("country", "phase_id"),
      min_years = 5
    )

    cat("Anomaly detection results:\n")
    print(anomalies)

    wait("\nPress [Enter] to see quality assessment...")

    cat("\n>>> # Assess data quality\n")
    cat(">>> quality <- pep_quality(\n")
    cat(">>>   pep = wheat,\n")
    cat(">>>   by = c('s_id', 'phase_id')\n")
    cat(">>> )\n\n")

    quality <- pep_quality(
      pep = wheat,
      by = c("s_id", "phase_id")
    )

    cat("Quality assessment:\n")
    print(quality)

    cat("\n>>> summary(quality)\n\n")
    summary(quality)

    results$analysis <- list(
      normals = normals,
      anomalies = anomalies,
      quality = quality
    )

    cat("\n")
  }

  # =========================================================================
  # Summary
  # =========================================================================
  cat(strrep("=", 60), "\n")
  cat("DEMO COMPLETE\n")
  cat(strrep("=", 60), "\n\n")

  cat("Key functions demonstrated:\n")
  cat("
* pep_download()     - Download synthetic PEP725 data
* print(pep)         - Smart data overview
* summary(pep)       - Detailed summaries by species/phase/country
* coverage(pep)      - Assess data coverage
* plot(pep)          - Quick visualizations (map, histogram, timeseries)
* pheno_normals()    - Calculate climatological baselines
* pheno_anomaly()    - Detect deviations from normal
* pep_quality()      - Assess data quality
* bbch_description() - Look up BBCH phase codes
")

  cat("\nFor more information, see:\n")
  cat("* help(package = 'pep725')\n")
  cat("* vignette('getting-started', package = 'pep725')\n")
  cat("* vignette('phenological-analysis', package = 'pep725')\n")
  cat("* vignette('spatial-patterns', package = 'pep725')\n\n")

  invisible(results)
}
