# Prevent R CMD check notes for data.table columns
utils::globalVariables(c("phase_id", "total_obs", "n_present", "complete"))

#' Check for Expected Phenological Phases
#'
#' Validates that expected phenological phases (BBCH codes) are present in the
#' data. Issues informative warnings for missing phases without stopping
#' execution, making it useful for pipeline validation.
#'
#' @param pep A \code{pep} object or \code{data.table} containing phenological
#'   observations with a \code{phase_id} column.
#' @param expected Integer vector of expected BBCH phase codes. If \code{NULL}
#'   (default), checks for common phases: 10 (leaf), 60 (heading/flowering),
#'   65 (full flowering), and 100 (harvest).
#' @param species Optional character string to filter by species/genus before
#'   checking. If specified, only checks phases for this species.
#' @param year_min Optional integer. Only check phases for years >= this value.
#'   Useful for checking data availability within an analysis window.
#' @param year_max Optional integer. Only check phases for years <= this value.
#' @param warn Logical. If \code{TRUE} (default), issues warnings for missing
#'   phases. If \code{FALSE}, only returns the result silently.
#' @param label Character. Optional label to include in warning messages,
#'   useful when checking multiple datasets. Default is \code{"Data"}.
#'
#' @return A list with class \code{phase_check} containing:
#'   \describe{
#'     \item{expected}{The expected phase codes}
#'     \item{present}{Phase codes found in the data}
#'     \item{missing}{Phase codes not found in the data}
#'     \item{extra}{Phase codes in data but not in expected list}
#'     \item{complete}{Logical. TRUE if all expected phases are present}
#'     \item{n_obs}{Named integer vector of observation counts per present phase}
#'     \item{coverage}{Data.table with phase-level statistics}
#'   }
#'
#' @details
#' This function is designed to be used at the start of analysis pipelines
#' to validate that required phenological phases are available. It issues
#' warnings but does not stop execution, allowing users to be informed of
#' potential issues while still proceeding with available data.
#'
#' @section Common BBCH Phases:
#' \describe{
#'   \item{10}{Leaf development}
#'   \item{60}{Beginning of flowering / Heading}
#'   \item{65}{Full flowering / Anthesis}
#'   \item{69}{End of flowering}
#'   \item{87}{Hard dough (cereals)}
#'   \item{100}{Harvest}
#' }
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Use Swiss subset for faster checking
#' pep_ch <- pep[country == "Switzerland"]
#'
#' # Basic check for common phases
#' check <- pep_check_phases(pep_ch)
#'
#' # Check specific phases for grapevine (longest historical records)
#' check <- pep_check_phases(pep_ch,
#'                       expected = c(60, 65, 81),
#'                       species = "Vitis vinifera")
#'
#' # Check within analysis window
#' check <- pep_check_phases(pep_ch,
#'                       expected = c(60, 65),
#'                       year_min = 1991,
#'                       year_max = 2020)
#'
#' # Use in pipeline with custom label
#' vine_data <- pep_ch[species == "Vitis vinifera"]
#' if (nrow(vine_data) > 0) {
#'   pep_check_phases(vine_data, expected = c(60, 65),
#'                label = "Grapevine analysis")
#' }
#'
#' # Silent check (no warnings)
#' result <- pep_check_phases(pep_ch, warn = FALSE)
#' if (!result$complete) {
#'   # Handle missing phases programmatically
#' }
#' }
#'
#' @seealso
#' \code{\link{bbch_description}} for phase code descriptions,
#' \code{\link{pep_completeness}} for detailed completeness assessment,
#' \code{\link{pep_quality}} for quality grading
#'
#' @author Matthias Templ
#' @export
pep_check_phases <- function(pep,
                          expected = NULL,
                          species = NULL,
                          year_min = NULL,
                          year_max = NULL,
                          warn = TRUE,
                          label = "Data") {

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Check required columns
  if (!"phase_id" %in% names(pep)) {
    stop("'pep' must have a 'phase_id' column", call. = FALSE)
  }

  # Default expected phases (common ones)
  if (is.null(expected)) {
    expected <- c(10, 60, 65, 100)
  }

  # Make a copy for filtering
  dt <- data.table::copy(pep)

  # Apply species filter if specified
  if (!is.null(species)) {
    if ("genus" %in% names(dt)) {
      genus_match <- dt$genus == species
      species_match <- if ("species" %in% names(dt)) dt$species == species else FALSE
      dt <- dt[genus_match | species_match]
    } else if ("species" %in% names(dt)) {
      dt <- dt[dt$species == species]
    }

    if (nrow(dt) == 0) {
      if (warn) {
        warning(sprintf("%s: no observations found for species '%s'",
                        label, species), call. = FALSE)
      }
      return(structure(list(
        expected = expected,
        present = integer(0),
        missing = expected,
        extra = integer(0),
        complete = FALSE,
        n_obs = integer(0),
        coverage = NULL,
        species_filter = species,
        year_filter = c(year_min, year_max)
      ), class = "phase_check"))
    }
  }

  # Apply year filter if specified
  if (!is.null(year_min)) {
    if (!"year" %in% names(dt)) {
      warning("'year' column not found. Ignoring year_min filter.", call. = FALSE)
    } else {
      dt <- dt[year >= year_min]
    }
  }

  if (!is.null(year_max)) {
    if (!"year" %in% names(dt)) {
      warning("'year' column not found. Ignoring year_max filter.", call. = FALSE)
    } else {
      dt <- dt[year <= year_max]
    }
  }

  # Check if any data remains after filtering
  if (nrow(dt) == 0) {
    year_str <- ""
    if (!is.null(year_min) || !is.null(year_max)) {
      year_str <- sprintf(" (years %s-%s)",
                          if (is.null(year_min)) "?" else year_min,
                          if (is.null(year_max)) "?" else year_max)
    }
    if (warn) {
      warning(sprintf("%s: no observations found%s", label, year_str),
              call. = FALSE)
    }
    return(structure(list(
      expected = expected,
      present = integer(0),
      missing = expected,
      extra = integer(0),
      complete = FALSE,
      n_obs = integer(0),
      coverage = NULL,
      species_filter = species,
      year_filter = c(year_min, year_max)
    ), class = "phase_check"))
  }

  # Get phases present in the data
  present_phases <- sort(unique(dt$phase_id))

  # Compare with expected
  missing_phases <- setdiff(expected, present_phases)
  extra_phases <- setdiff(present_phases, expected)
  complete <- length(missing_phases) == 0

  # Count observations per phase
  phase_counts <- dt[, .N, by = phase_id][order(phase_id)]
  n_obs <- stats::setNames(phase_counts$N, phase_counts$phase_id)

  # Create coverage summary
  has_year <- "year" %in% names(dt)
  has_station <- "s_id" %in% names(dt)

  coverage <- dt[, {
    list(
      n_obs = .N,
      n_stations = if (has_station) data.table::uniqueN(s_id) else NA_integer_,
      n_years = if (has_year) data.table::uniqueN(year) else NA_integer_,
      year_min = if (has_year) min(year, na.rm = TRUE) else NA_integer_,
      year_max = if (has_year) max(year, na.rm = TRUE) else NA_integer_
    )
  }, by = phase_id][order(phase_id)]

  # Issue warnings for missing phases
  if (warn && length(missing_phases) > 0) {
    year_str <- ""
    if (!is.null(year_min) || !is.null(year_max)) {
      year_str <- sprintf(" (year %s-%s)",
                          if (is.null(year_min)) min(dt$year) else year_min,
                          if (is.null(year_max)) max(dt$year) else year_max)
    }
    warning(sprintf("%s%s: missing phase(s): %s",
                    label, year_str,
                    paste(missing_phases, collapse = ", ")),
            call. = FALSE)
  }

  # Return result
  result <- list(
    expected = expected,
    present = as.integer(present_phases),
    missing = as.integer(missing_phases),
    extra = as.integer(extra_phases),
    complete = complete,
    n_obs = n_obs,
    coverage = coverage,
    species_filter = species,
    year_filter = c(year_min, year_max)
  )

  class(result) <- "phase_check"
  result
}


#' Print Method for Phase Check Results
#'
#' @param x A \code{phase_check} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.phase_check <- function(x, ...) {
  cat("Phenological Phase Availability Check\n")
  cat(strrep("-", 45), "\n")

  # Filters applied
  if (!is.null(x$species_filter)) {
    cat(sprintf("Species filter: %s\n", x$species_filter))
  }
  if (!is.null(x$year_filter) && !all(is.na(x$year_filter))) {
    year_str <- sprintf("%s - %s",
                        if (is.na(x$year_filter[1])) "any" else x$year_filter[1],
                        if (is.na(x$year_filter[2])) "any" else x$year_filter[2])
    cat(sprintf("Year filter: %s\n", year_str))
  }

  # Expected phases
  cat(sprintf("\nExpected phases: %s\n", paste(x$expected, collapse = ", ")))

  # Status
  if (x$complete) {
    cat("Status: COMPLETE - all expected phases present\n")
  } else {
    cat("Status: INCOMPLETE - some phases missing\n")
  }

  # Missing phases
  if (length(x$missing) > 0) {
    cat(sprintf("\nMissing phases: %s\n", paste(x$missing, collapse = ", ")))
  }

  # Present phases with counts
  if (length(x$present) > 0) {
    cat("\nPhases found:\n")
    for (phase in x$present) {
      in_expected <- if (phase %in% x$expected) "" else " [extra]"
      count <- if (as.character(phase) %in% names(x$n_obs))
        x$n_obs[as.character(phase)] else 0
      cat(sprintf("  Phase %3d: %s observations%s\n",
                  phase, format(count, big.mark = ","), in_expected))
    }
  }

  # Coverage table if available
  if (!is.null(x$coverage) && nrow(x$coverage) > 0) {
    cat("\nDetailed coverage:\n")
    print(x$coverage)
  }

  invisible(x)
}


#' Check Multiple Phases Across Species
#'
#' A convenience function to check phase availability across multiple species
#' at once.
#'
#' @param pep A \code{pep} object or \code{data.table}
#' @param species_list Character vector of species/genus names to check
#' @param expected Integer vector of expected BBCH phase codes
#' @param year_min Optional minimum year filter
#' @param year_max Optional maximum year filter
#' @param warn Logical. Issue warnings for missing phases? Default TRUE.
#'
#' @return A \code{data.table} summarizing phase availability per species
#'
#' @examples
#' \donttest{
#' # Check multiple species (use subset for speed)
#' pep <- pep_download()
#' pep_subset <- pep[country %in% c("Switzerland", "Austria")]
#' result <- pep_check_phases_multi(pep_subset,
#'                              species_list = c("Malus domestica",
#'                                               "Vitis vinifera"),
#'                              expected = c(60, 65))
#' print(result)
#' }
#'
#' @seealso \code{\link{pep_check_phases}} for single-species checking
#'
#' @author Matthias Templ
#' @export
pep_check_phases_multi <- function(pep,
                                species_list,
                                expected = c(60, 65, 100),
                                year_min = NULL,
                                year_max = NULL,
                                warn = TRUE) {

  species_list <- as.character(species_list)

  results <- lapply(species_list, function(sp) {
    check <- pep_check_phases(pep,
                          expected = expected,
                          species = sp,
                          year_min = year_min,
                          year_max = year_max,
                          warn = warn,
                          label = sp)

    data.table::data.table(
      species = sp,
      complete = check$complete,
      n_expected = length(expected),
      n_present = sum(expected %in% check$present),
      n_missing = length(check$missing),
      missing_phases = if (length(check$missing) > 0)
        paste(check$missing, collapse = ",") else NA_character_,
      total_obs = sum(check$n_obs)
    )
  })

  result <- data.table::rbindlist(results)
  data.table::setorder(result, -complete, -n_present, -total_obs)

  class(result) <- c("phase_check_multi", class(result))
  attr(result, "expected") <- expected
  attr(result, "year_range") <- c(year_min, year_max)

  result
}


#' Print Method for Multi-Species Phase Check
#'
#' @param x A \code{phase_check_multi} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#' @author Matthias Templ
#' @export
print.phase_check_multi <- function(x, ...) {
  expected <- attr(x, "expected")
  year_range <- attr(x, "year_range")

  cat("Multi-Species Phase Availability Check\n")
  cat(strrep("=", 50), "\n")
  cat(sprintf("Expected phases: %s\n", paste(expected, collapse = ", ")))

  if (!is.null(year_range) && !all(is.na(year_range))) {
    cat(sprintf("Year range: %s - %s\n",
                if (is.na(year_range[1])) "any" else year_range[1],
                if (is.na(year_range[2])) "any" else year_range[2]))
  }

  n_complete <- sum(x$complete)
  cat(sprintf("\nSpecies with all phases: %d / %d (%.0f%%)\n",
              n_complete, nrow(x), 100 * n_complete / nrow(x)))

  cat(strrep("-", 50), "\n\n")
  print(data.table::as.data.table(x))

  invisible(x)
}
