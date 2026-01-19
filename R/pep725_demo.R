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
#'     \item \code{"analysis"} - Show analysis functions
#'     \item \code{"all"} - Run all demonstrations (default)
#'   }
#' @param pause Logical. If \code{TRUE} (default), pauses between demos
#'   waiting for user input. Set to \code{FALSE} for non-interactive use.
#'
#' @return Invisibly returns a list containing example outputs from each demo.
#'
#' @details
#' The demonstration uses the built-in \code{pep} dataset and walks through
#' typical workflows for phenological data analysis:
#'
#' \enumerate{
#'   \item \strong{Class demo}: Shows the enhanced print and summary methods
#'     for the pep class, demonstrating how to quickly understand the data.
#'   \item \strong{Filter demo}: Demonstrates selecting specific species,
#'     phases, and time periods using \code{select_phase()}.
#'   \item \strong{Plot demo}: Creates visualizations including station maps,
#'     time series, and DOY distributions.
#'   \item \strong{Analysis demo}: Shows climate sensitivity analysis and
#'     regional time series extraction.
#' }
#'
#' @examples
#' \dontrun{
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
#' \code{\link{pep}} for the main dataset,
#' \code{\link{select_phase}} for data filtering,
#' \code{\link{pheno_plot}} for visualization,
#' \code{\link{regional_box_ts}} for regional analysis
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

  # =========================================================================
 # DEMO 1: Class features
  # =========================================================================
  if ("class" %in% which) {
    cat(strrep("-", 60), "\n")
    cat("DEMO 1: The pep Class - Smart Data Display\n")
    cat(strrep("-", 60), "\n\n")

    cat("The pep object has enhanced print/summary methods.\n")
    cat("Instead of dumping millions of rows, you get a useful overview:\n\n")

    cat(">>> pep\n\n")
    print(pep)

    wait("\nPress [Enter] to see summary by species...")

    cat("\n>>> summary(pep, by = 'species', top = 5)\n\n")
    s1 <- summary(pep, by = "species", top = 5)

    wait("\nPress [Enter] to see summary by phase...")

    cat("\n>>> summary(pep, by = 'phase')\n\n")
    s2 <- summary(pep, by = "phase")

    results$class <- list(summary_species = s1, summary_phase = s2)

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
    cat(">>> wheat <- pep[genus == 'Triticum' & year >= 1990]\n")
    cat(">>> wheat\n\n")

    wheat <- pep[genus == "Triticum" & year >= 1990]
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
    cat("DEMO 4: Regional Analysis with regional_box_ts()\n")
    cat(strrep("-", 60), "\n\n")

    cat("Extract regional time series and merge with climate data:\n\n")

    cat(">>> # Get wheat phenology for Switzerland region\n")
    cat(">>> regional_data <- regional_box_ts(\n")
    cat(">>>   pep = pep,\n")
    cat(">>>   giss = giss,\n")
    cat(">>>   lon_min = 5.5, lon_max = 10.5,\n")
    cat(">>>   lat_min = 45.5, lat_max = 48,\n")
    cat(">>>   species_name = 'Triticum',\n")
    cat(">>>   year_min = 1960,\n")
    cat(">>>   phase = 60\n")
    cat(">>> )\n\n")

    regional_data <- regional_box_ts(
      pep = pep,
      giss = giss,
      lon_min = 5.5, lon_max = 10.5,
      lat_min = 45.5, lat_max = 48,
      species_name = "Triticum",
      year_min = 1960,
      phase = 60
    )

    cat("Aggregated time series:\n")
    print(head(regional_data$agg, 10))

    cat("\nMerged with GISS temperature anomalies:\n")
    print(head(regional_data$pep_giss, 10))

    results$analysis <- regional_data

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
* print(pep)         - Smart data overview
* summary(pep)        - Detailed summaries by species/phase/country
* plot(pep)           - Quick visualizations (map, histogram, timeseries)
* select_phase()      - Filter by species, phase, and time
* regional_box_ts()   - Extract regional time series with climate data
* bbch_description()  - Look up BBCH phase codes
")

  cat("\nFor more information, see:\n")
  cat("* help(package = 'pep725')
* vignette('pep725')  (if available)\n\n")

  invisible(results)
}
