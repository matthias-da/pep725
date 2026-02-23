#' Select and Label Phenophases from PEP Time Series
#'
#' Extracts a subset of phenological observations from a processed PEP time series (e.g., output from species-level aggregation),
#' filters by species, year, and phenophase ID, and attaches meaningful phase labels based on standard BBCH codes.
#'
#' @param dt A \code{data.table} containing columns \code{species}, \code{year}, \code{phase_id}, and \code{mean_day}.
#' Typically produced by aggregating PEP725 data (e.g., via \code{\link{pheno_regional}}).
#' @param label A character string used as identifier for source and site columns in the output.
#' @param sp Species name (as character string) to filter, e.g., \code{"Triticum aestivum"}.
#' @param yrmin Minimum year (inclusive) for filtering.
#' @param phases Integer vector of phase IDs to include (default: \code{c(60, 100)} corresponding to "Heading" and "Harvest").
#'
#' @return A \code{data.table} with columns:
#' \describe{
#'   \item{\code{year}}{Year of observation}
#'   \item{\code{phase}}{Mapped BBCH phenological stage name}
#'   \item{\code{DOY}}{Mean day-of-year for this phase, species, and year}
#'   \item{\code{source}}{Set to \code{label}}
#'   \item{\code{site}}{Set to \code{label}}
#' }
#'
#' @details
#' This function uses an internal lookup table to map numeric \code{phase_id} codes to descriptive BBCH phase names (e.g., \code{60 = "Heading"}, \code{100 = "Harvest"}).
#' Unmapped phase IDs will trigger a warning. If expected phases are missing from the result, a separate warning is issued.
#'
#' The function is particularly useful for visualizing specific growth stages or comparing phenological trends across datasets and locations.
#'
#' @seealso \code{\link{pheno_regional}}, \code{\link{pep_download}}, \code{\link{pheno_plot}}
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' agg <- pheno_regional(pep, species_name = "Triticum aestivum", phase = 60)$pep_agg
#' df <- select_phase(agg, label = "PEP725", sp = "Triticum aestivum", yrmin = 1961)
#' head(df)
#' }
#'
#' @author Matthias Templ
#' @export
select_phase <- function(dt, label, sp, yrmin, phases = c(60, 100)) {
  phase_lookup <- .bbch_lookup

  # Mapping function
  phase_name <- function(id) {
    res <- as.character(phase_lookup[as.character(id)])
    if (any(is.na(res))) {
      warning(sprintf("Unmapped phase_id(s): %s",
                      paste(setdiff(id, as.integer(names(phase_lookup))), collapse = ", ")),
              call. = FALSE)
    }
    res
  }

  # Core logic
  out <- dt[
    species == sp &
      year >= yrmin & phase_id %in% phases,
    .(year, phase = phase_name(phase_id), DOY = mean_day)
  ][order(year)][
    , `:=`(source = label, site = label)
  ][]

  # Optional: warn if expected phases are missing
  expected_phases <- phase_name(phases)
  observed_phases <- unique(out$phase)
  missing <- setdiff(expected_phases, observed_phases)
  if (length(missing) > 0L) {
    warning(sprintf(
      "%s (year >= %d): missing phase(s): %s",
      label, yrmin, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  return(out)
}
