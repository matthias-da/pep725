#' Compile Regional Phenological Time Series for Heading and Harvest
#'
#' Generates harmonized phenological time series for heading (BBCH 60) and
#' harvest (BBCH 100) phases of a specified species and spatial region.
#' Optionally integrates GISS climate data for climate-sensitivity studies.
#'
#' @param pep A data.table containing PEP725 phenological data.
#' @param giss Optional. A data.table with GISS global temperature anomalies
#'   (\code{year}, \code{dT}, \code{dT_sm}). If \code{NULL}, GISS-related
#'   outputs are omitted.
#' @param lon_min Minimum longitude for bounding box filter (default is 4.2)
#' @param lon_max Maximum longitude for bounding box filter (default is 8.1)
#' @param lat_min Minimum latitude for bounding box filter (default is 44.7)
#' @param lat_max Maximum latitude for bounding box filter (default is 48.1)
#' @param species_name Name of the species to filter (default is
#'   \code{"Triticum aestivum"})
#' @param year_min Minimum year for all time series outputs (default is 1961)
#' @param pep_for_giss Which PEP725 dataset to use for merging with GISS climate
#'   anomalies. Either \code{"aggregated"} (global) or \code{"near"} (spatially
#'   filtered). Ignored if \code{giss} is \code{NULL}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{ts_tidy}}{Long-format time series with consistent phase and source labeling}
#'   \item{\code{pep_agg}}{Aggregated PEP725 time series (global species-level)}
#'   \item{\code{pep_cgi}}{Spatially filtered PEP725 time series (based on
#'     lat/lon box)}
#'   \item{\code{giss}}{GISS global temperature anomalies (if provided)}
#'   \item{\code{pep_giss}}{PEP time series merged with GISS anomalies (if provided)}
#'   \item{\code{species}}{Species name used for filtering}
#' }
#'
#' @details
#' This function filters the PEP725 dataset by species and optionally by spatial
#' box. It aggregates phenological phases (Heading = 60, Harvest = 100) and
#' computes mean day-of-year (DOY) per year. If GISS data is provided,
#' temperature anomalies are merged with the selected PEP time series to support
#' phenology-climate analysis. Warnings are issued if phases are missing for
#' selected periods.
#'
#' @seealso \code{\link{pep_import}}, \code{\link{pheno_plot_hh}}
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#'
#' # PEP data only
#' out <- regional_box_ts_heading_harvest(pep,
#'          species_name = "Triticum aestivum")
#' pheno_plot_hh(out)
#'
#' # With GISS temperature anomalies
#' # out <- regional_box_ts_heading_harvest(pep, giss = giss)
#' }
#' @author Matthias Templ
#' @export
regional_box_ts_heading_harvest <- function(pep,
                            giss = NULL,
    lon_min = 4.2,  lon_max = 8.1,
    lat_min = 44.7, lat_max = 48.1,
    species_name = "Triticum aestivum",
    year_min   = 1961,
    pep_for_giss = c("aggregated","near")
) {
  pep_for_giss <- match.arg(pep_for_giss)

  # keep species first (faster), then build both aggregated tables
  stopifnot("species" %in% names(pep))
  pep_species <- pep[species %in% species_name]

  # Global PEP aggregation
  PEPData <- pep_species[
    , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
    by = .(species, year, phase_id)
  ][order(species, year, mean_day, phase_id)]

  # Box-filtered (near Changins) PEP aggregation
  pep_box <- pep_species[
    lat > lat_min & lat < lat_max & lon > lon_min & lon < lon_max
  ]
  PEPDataChangins <- pep_box[
    , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
    by = .(species, year, phase_id)
  ][order(species, year, mean_day, phase_id)]

  # --- smarter warning helper: only within analysis window ----------------
  check_phases <- function(dt, label, yr_min) {
    dt_win <- dt[year >= yr_min]
    if (nrow(dt_win) == 0L) {
      warning(sprintf("%s: no rows at or after year_min = %d.", label, yr_min), call. = FALSE)
      return(invisible())
    }
    miss <- setdiff(c("Heading", "Harvest"), unique(dt_win$phase))
    if (length(miss) > 0) {
      warning(sprintf("%s (year >= %d): missing phase(s): %s",
                      label, yr_min, paste(miss, collapse = ", ")), call. = FALSE)
    }
    invisible()
  }

  # --- PEP helper (explicit args; no ..) ---------------------------------
  phase_name <- function(id) ifelse(id == 60, "Heading", "Harvest")
  pepfun <- function(dt, label, sp, yrmin) {
    out <- dt[
      species == sp & year >= yrmin & phase_id %in% c(60, 100),
      .(year, phase = phase_name(phase_id), DOY = mean_day)
    ][order(year)][
      , `:=`(source = label, site = label)
    ][]
    check_phases(out, label, yrmin)  # warn only for analysis window
    out
  }

  pep_agg <- pepfun(PEPData,         "PEP725 (aggregated)",    species_name, year_min)
  pep_cgi <- pepfun(PEPDataChangins, "PEP725 (near Changins)", species_name, year_min)

  # --- One tidy dataset for both time-series panels ----------------------
  ts_tidy <- rbindlist(list(
    copy(pep_agg)[, panel := "PEP aggregated"],
    copy(pep_cgi)[, panel := "PEP near Changins"]
  ), use.names = TRUE)[
    year >= year_min
  ][]

  # --- GISS + merge with selected PEP (optional) -------------------------
  giss_out <- NULL
  pep_giss <- NULL
  if (!is.null(giss)) {
    giss_out <- giss[year >= year_min, .(year, dTgl = dT, Tgl = dT + 14)][order(year)]
    pep_sel <- switch(pep_for_giss, aggregated = pep_agg, near = pep_cgi)
    pep_giss <- merge(pep_sel, giss_out, by = "year")
    attr(pep_giss, "pep_source") <- pep_for_giss
  }

  list(
    ts_tidy  = ts_tidy,
    pep_agg  = pep_agg,
    pep_cgi  = pep_cgi,
    giss     = giss_out,
    pep_giss = pep_giss,
    species  = species_name
  )
}
