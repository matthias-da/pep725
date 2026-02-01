#' Compile Regional Phenological and Climate Time Series
#'
#' Generates a collection of harmonized phenological time series for a specified
#' species and spatial region, using data from multiple sources: PEP725
#' (aggregated and filtered), MeteoSwiss observations, and GISS global
#' temperature anomalies. This function supports comparative analysis across
#' data sources and climate-sensitivity studies.
#'
#' @param pep A data.table containing PEP725 phenological data. Defaults to
#'   object from parent environment.
#' @param giss A data.table with GISS global temperature anomalies. Defaults to
#'   object from parent environment.
#' @param meteoSwiss A data.table with MeteoSwiss phenological observations.
#'   Defaults to object from parent environment.
#' @param lon_min Minimum longitude for bounding box filter (default is 4.2)
#' @param lon_max Maximum longitude for bounding box filter (default is 8.1)
#' @param lat_min Minimum latitude for bounding box filter (default is 44.7)
#' @param lat_max Maximum latitude for bounding box filter (default is 48.1)
#' @param species_name Name of the species to filter (default is
#'   \code{"Triticum aestivum"})
#' @param year_min Minimum year for all time series outputs (default is 1961)
#' @param pep_for_giss Which PEP725 dataset to use for merging with GISS climate
#'   anomalies. Either \code{"aggregated"} (global) or \code{"near"} (spatially
#'   filtered near Changins)
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{ts_tidy}}{Long-format time series combining PEP725 and
#'     MeteoSwiss data, with consistent phase and source labeling}
#'   \item{\code{pep_agg}}{Aggregated PEP725 time series (global species-level)}
#'   \item{\code{pep_cgi}}{Spatially filtered PEP725 time series (based on
#'     lat/lon box)}
#'   \item{\code{giss}}{GISS global temperature anomalies for matching years}
#'   \item{\code{pep_giss}}{PEP time series (aggregated or filtered) merged with
#'     GISS anomalies}
#' }
#'
#' @details
#' This function filters the PEP725 dataset by species and optionally by spatial
#' box. It aggregates phenological phases (Heading = 60, Harvest = 100) and
#' computes mean day-of-year (DOY) per year. MeteoSwiss observations are
#' transformed to tidy format and included for regional comparison. GISS
#' temperature anomalies are merged with the selected PEP time series to support
#' phenology-climate analysis. Warnings are issued if phases are missing for
#' selected periods.
#'
#' @seealso \code{\link{pep_import}}, \code{\link{meteoSwiss}}, \code{\link{giss}}
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' out <- regional_box_ts_heading_harvest(pep, species_name = "Triticum aestivum")
#' str(out$ts_tidy)
#' gt <- pheno_plot_hh(out, type = "timeseries")
#' gt
#' pheno_plot_hh(out, type = "giss_smooth", smooth = "loess", se = TRUE)
#' gc <- pheno_plot_hh(out, type = "giss_sensitivity")
#' gt | gc
#'
#' # Example for other (selected) coordinates of the PEP data:
#' out <- regional_box_ts_heading_harvest(pep, pep_for_giss = "near",
#'                         lon_min = 4.2,  lon_max = 8.1,
#'                         lat_min = 44.7, lat_max = 48.1
#' )
#' str(out$ts_tidy)
#' pheno_plot_hh(out, type = "giss_smooth", smooth = "loess", se = TRUE)
#'
#' # Example for other year selection
#' out <- regional_box_ts_heading_harvest(pep, species_name = "Triticum aestivum",
#'                                        year_min = 1980)
#' gt <- pheno_plot_hh(out, type = "timeseries")
#' gc <- pheno_plot_hh(out, type = "giss_sensitivity")
#' gt | gc
#'
#' # Example to select only nearby stations (more useful that using all PEP station data)
#' # Better to select only stations near Changins and for the last 40 years:
#' d <- regional_box_ts_heading_harvest(pep, pep_for_giss = "near",
#'                         lon_min = 4.2,  lon_max = 8.1,
#'                         lat_min = 44.7, lat_max = 48.1,
#'                         year_min = 1980
#' )
#' pheno_plot_hh(d, type = "timeseries", alpha_lines = 0.6, linewidth = 0.7)
#' }
#' @author Matthias Templ
#' @export
regional_box_ts_heading_harvest <- function(pep = get("pep", envir = parent.frame()),
                            giss = get("giss", envir = parent.frame()),
                            meteoSwiss = get("meteoSwiss", envir = parent.frame()),
    lon_min = 4.2,  lon_max = 8.1, # default near Changins
    lat_min = 44.7, lat_max = 48.1, # default near Changins
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

  # --- MeteoCH, tidy long -----------------------------------------------
  obs_long <- rbindlist(list(
    meteoSwiss[, .(year = yearHd, phase = "Heading", DOY = DOYHd)],
    meteoSwiss[, .(year = yearHv, phase = "Harvest", DOY = DOYHv)]
  ))[order(year)][
    , `:=`(source = "Obs. Changins (MeteoCH)", site = "Changins")
  ][
    year >= year_min                                    # <- filter by year_min
  ][]

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

  # Warn for MeteoSwiss as well (within window)
  check_phases(obs_long, "MeteoCH data", year_min)

  # --- One tidy dataset for both time-series panels ----------------------
  obs_both <- rbindlist(list(
    copy(obs_long)[, panel := "PEP aggregated"],
    copy(obs_long)[, panel := "PEP near Changins"]
  ))
  pep_both <- rbindlist(list(
    copy(pep_agg)[, panel := "PEP aggregated"],
    copy(pep_cgi)[, panel := "PEP near Changins"]
  ))
  ts_tidy <- rbindlist(list(obs_both, pep_both), use.names = TRUE)[
    year >= year_min                                    # <- safety filter
  ][]

  # --- GISS + merge with selected PEP ------------------------------------
  giss <- giss[year >= year_min, .(year, dTgl = dT, Tgl = dT + 14)][order(year)]
  pep_sel <- switch(pep_for_giss, aggregated = pep_agg, near = pep_cgi)
  pep_giss <- merge(pep_sel, giss, by = "year")
  attr(pep_giss, "pep_source") <- pep_for_giss

  list(
    ts_tidy  = ts_tidy,
    pep_agg  = pep_agg,
    pep_cgi  = pep_cgi,
    giss     = giss,
    pep_giss = pep_giss,
    species = species_name
  )
}
