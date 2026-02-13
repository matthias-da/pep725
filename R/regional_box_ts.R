#' Compile Regional Phenology Data and Climate Sensitivity Inputs
#'
#' Extracts and aggregates phenological observations for a selected species and phenological phase from the PEP725 dataset (globally and spatially filtered), and links them with global temperature anomalies (GISS data). This function supports flexible phase selection and comparative regional-global climate impact studies.
#'
#' @param pep A data.table containing the full PEP725 dataset, typically loaded at package startup. Must include at least columns: \code{species}, \code{year}, \code{phase_id}, \code{day}, \code{lat}, \code{lon}. Defaults to a global object \code{pep} from the parent environment.
#' @param giss A data.table with GISS global temperature anomalies, containing columns
#'   \code{year}, \code{dT}, and \code{dT_sm}. Load from hail package: \code{data(giss, package = "hail")}.
#' @param lon_min Minimum longitude for spatial filtering of PEP data (default is \code{4.2})
#' @param lon_max Maximum longitude for spatial filtering of PEP data (default is \code{8.1})
#' @param lat_min Minimum latitude for spatial filtering of PEP data (default is \code{44.7})
#' @param lat_max Maximum latitude for spatial filtering of PEP data (default is \code{48.1})
#' @param species_name Character name of the species to be extracted (default is \code{"Triticum aestivum"})
#' @param functional_group Character. Optional functional group name to filter by (e.g., "C4_summer_cereals"). If provided, overwrites species_name filtering.
#' @param year_min Minimum year to include in all outputs (default is \code{1961})
#' @param pep_for_giss Selects which PEP data subset to use for merging with GISS data. Either \code{"near"} (box-filtered) or \code{"aggregated"} (global).
#' @param phase Integer or vector of phase_id(s) to extract (default is \code{60} = Heading). Named phase mappings are applied.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{pep_agg}}{Global (non-spatially filtered) PEP725 time series for the selected phase(s)}
#'   \item{\code{pep_cgi}}{Spatially filtered PEP725 time series based on lat/lon bounding box}
#'   \item{\code{giss}}{Processed GISS global temperature anomaly data from the given year onwards}
#'   \item{\code{pep_giss}}{Merged data frame of PEP phenology (from selected source) and GISS anomalies}
#'   \item{\code{species}}{Character species name used for filtering (echoed)}
#' }
#'
#' @details
#' The function aggregates PEP observations by species, year, and phase,
#' calculates mean day-of-year (DOY), applies spatial filtering if needed,
#' and attaches GISS climate anomalies. It also maps \code{phase_id}s to
#' descriptive names and warns if expected phenological phase are missing
#' in the selected data.
#'
#' For climate sensitivity analyses, this output is designed to be used in
#' downstream plotting functions or regression models linking phenology with
#' global warming trends.
#'
#' @seealso \code{\link{pheno_plot}}, \code{\link{pep_download}},
#'   \code{hail::giss}, \code{hail::plot_giss_smooth}, \code{hail::plot_giss_sensitivity}
#'
#' @examples
#' \dontrun{
#' # Typical usage for winter wheat heading phase:
#' pep <- pep_download()
#'
#' # Load GISS data from hail package
#' data(giss, package = "hail")
#'
#' out <- regional_box_ts(pep, giss, species_name = "Triticum aestivum", phase = 10)
#' str(out)
#'
#' # For climate sensitivity plots, use hail package:
#' # hail::plot_giss_smooth(out)
#' # hail::plot_giss_sensitivity(out)
#' }
#'
#' @author Matthias Templ
#' @export
regional_box_ts <- function(
    pep,
    giss,
    lon_min = 4.2, lon_max = 8.1,
    lat_min = 44.7, lat_max = 48.1,
    species_name = "Triticum aestivum",
    functional_group = NULL,
    year_min = 1961,
    pep_for_giss = c("near", "aggregated"),
    phase = 60  # <- default to Heading
) {

  if(!is.null(functional_group)) {
    warning("functional_group overwrites species_name.")
  }



  pep_for_giss <- match.arg(pep_for_giss)
  if(pep_for_giss == "near") {
    message("Using PEP725 data limited to the provided lon/lat coordinates")
  } else {
    message("Using ALL PEP725 data for GISS comparison.\nNote: lon/lat filtering is ignored in this case.")
  }

  # pep_species <- pep[species %in% species_name]

  if (!is.null(functional_group)) {
    functional_group <- as.character(functional_group)
    pep_filtered <- pep[as.character(functional_group) == functional_group]
    message("Filtering by functional group: ", functional_group)
    effective_label <- functional_group
  } else {
    species_name <- as.character(species_name)
    pep_filtered <- pep[as.character(species) == species_name]
    effective_label <- species_name
  }

  group_var <- if (!is.null(functional_group)) "functional_group" else "species"

  PEPData <- pep_filtered[
    , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
    by = c(group_var, "year", "phase_id")
  ][order(get(group_var), year, mean_day, phase_id)]

  # # Global Aggregated PEP
  # PEPData <- pep_filtered[
  #   , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
  #   by = .(species, year, phase_id)
  # ][order(species, year, mean_day, phase_id)]

  # Box-filtered (near Changins)
  pep_box <- pep_filtered[
    lat > lat_min & lat < lat_max & lon > lon_min & lon < lon_max
  ]
  # PEPDataChangins <- pep_box[
  #   , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
  #   by = .(species, year, phase_id)
  # ][order(species, year, mean_day, phase_id)]
  PEPDataChangins <- pep_box[
    , .(n = .N, mean_day = mean(day, na.rm = TRUE)),
    by = c(group_var, "year", "phase_id")
  ][order(get(group_var), year, mean_day, phase_id)]

  # --- Helper: flexible phase name mapping + warnings ---------------------
  phase_lookup <- c(
    `0`   = "Dry seed",
    `7`   = "Coleoptile emerged from caryopsis",
    `10`  = "Leaf development",
    `11`  = "First true leaf",
    `15`  = "True leaves",
    `30`  = "Stem elongation",
    `31`  = "Stem 10% of final length",
    `51`  = "Inflorescence or flower buds visible",
    `60`  = "Heading",
    `61`  = "Start of flowering: 10% of flowers open",
    `65`  = "Anthesis",
    `69`  = "End of flowering",
    `70`  = "Milk development",
    `75`  = "50% of fruits have reached final size",
    `80`  = "Dough development",
    `81`  = "Beginning of ripening or fruit colouration",
    `90`  = "Maturity",
    `95`  = "50% of leaves fallen",
    `100` = "Harvest",
    `111` = "First cut for silage winning",
    `131` = "First cut for hay winning",
    `151` = "Start of harvest for silage (corn, grass)",
    `205` = "Autumnal leaf colouring >= 50%"
  )
  phase_name <- function(id) {
    res <- as.character(phase_lookup[as.character(id)])
    if (any(is.na(res))) {
      warning(sprintf("Unmapped phase_id(s): %s",
                      paste(setdiff(id, as.integer(names(phase_lookup))), collapse = ", ")),
              call. = FALSE)
    }
    res
  }

  # pepfun_single <- function(dt, label, sp, yrmin, phase) {
  #   out <- dt[
  #     species == sp & year >= yrmin & phase_id %in% phase,
  #     .(year, phase = phase_name(phase_id), DOY = mean_day)
  #   ][order(year)][
  #     , `:=`(source = label, site = label)
  #   ][]
  #   expected_phase <- phase_name(phase)
  #   observed_phase <- unique(out$phase)
  #   mean_day <- dt$mean_day
  #   missing <- setdiff(expected_phase, observed_phase)
  #   if (length(missing) > 0L) {
  #     warning(sprintf(
  #       "%s (year >= %d): missing phase(s): %s",
  #       label, yrmin, paste(missing, collapse = ", ")
  #     ), call. = FALSE)
  #   }
  #   return(out)
  # }
  pepfun_single <- function(dt, label, sp, yrmin, phase, by_col = "species") {
    # Ensure the column to filter by exists
    if (!by_col %in% names(dt)) {
      stop(sprintf("Column '%s' not found in dataset.", by_col), call. = FALSE)
    }

    # Helper for dynamic filtering (species or functional_group)
    out <- dt[
      get(by_col) == sp & year >= yrmin & phase_id %in% phase,
      .(year, phase_id = phase_id, phase = phase_name(phase_id), DOY = mean_day)
    ][order(year)][
      , `:=`(source = label, site = label)
    ][]

    # --- Quality check and messaging ----------------------------------------
    expected_phase <- phase_name(phase)
    observed_phase <- unique(out$phase)
    mean_day <- dt$mean_day
    missing <- setdiff(expected_phase, observed_phase)

    if (length(missing) > 0L) {
      warning(sprintf(
        "%s (year >= %d): missing phase(s): %s",
        label, yrmin, paste(missing, collapse = ", ")
      ), call. = FALSE)
    }

    # Stamp the group label (species or functional_group) into the output
    out[, species := sp]
    out[, filter_type := by_col]

    return(out)
  }

  # --- Apply phase filtering and label ------------------------------------
  # pep_agg <- pepfun_single(PEPData, "PEP725 (aggregated)", species_name, year_min, phase)
  # pep_cgi <- pepfun_single(PEPDataChangins, "PEP725 (near Changins)", species_name, year_min, phase)
  # pep_agg <- pepfun_single(PEPData, "PEP725 (aggregated)", effective_label, year_min, phase)
  pep_agg <- pepfun_single(
    PEPData,
    label = "PEP725 (aggregated)",
    sp = effective_label,
    yrmin = year_min,
    phase = phase,
    by_col = if (!is.null(functional_group)) "functional_group" else "species"
  )
  # pep_cgi <- pepfun_single(PEPDataChangins, "PEP725 (near Changins)", effective_label, year_min, phase)
  pep_cgi <- pepfun_single(
    PEPDataChangins,
    label = "PEP725 (near Changins)",
    sp = effective_label,
    yrmin = year_min,
    phase = phase,
    by_col = if (!is.null(functional_group)) "functional_group" else "species"
  )

  if (nrow(pep_cgi) == 0L) {
    warning("No matching records found near Changins for functional group: ", functional_group, call. = FALSE)
  }

  # --- GISS subset and merge ----------------------------------------------
  giss <- giss[year >= year_min, .(year, dTgl = dT, Tgl = dT + 14)][order(year)]
  pep_sel <- switch(pep_for_giss, aggregated = pep_agg, near = pep_cgi)
  pep_giss <- merge(pep_sel, giss, by = "year")
  attr(pep_giss, "pep_source") <- pep_for_giss

  if (nrow(pep_giss) == 0L) {
    warning("No GISS merge possible (pep_giss is empty) -- check date/region/phase", call. = FALSE)
  }


  # Add species label and mean_day alias
  pep_agg[, species := if (!is.null(functional_group)) functional_group else species_name]
  pep_agg$mean_day <- pep_agg$DOY
  pep_cgi[, species := if (!is.null(functional_group)) functional_group else species_name]
  pep_cgi$mean_day <- pep_cgi$DOY

  # Return
  list(
    pep_agg  = pep_agg,
    pep_cgi  = pep_cgi,
    giss     = giss,
    pep_giss = pep_giss,
    species = if (is.null(functional_group)) species_name else NA_character_,
    functional_group = if (!is.null(functional_group)) functional_group else NA_character_,
    phase  = phase
  )
}
