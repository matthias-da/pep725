#' Compute Integrated Hail Exposure for CTRL and SCEN Scenarios
#'
#' This function computes the integrated hail probability over the
#' phenological window (BBCH phase1 to phase2) for both the climate
#' control period (CTRL) and the scenario (SCEN). Phenology can be
#' estimated by a simple shift model or a GDD-based model (TBD).
#'
#' @param hail A hail probability dataset containing columns
#'   \code{Country}, \code{Scenario}, \code{DOY}, and \code{mean}.
#' @param pep PEP725 phenology data frame.
#' @param phase1 Integer. First phenophase (BBCH).
#' @param phase2 Integer. Second phenophase (BBCH).
#' @param genus_name Character. Target genus (e.g., "Malus").
#' @param scen_warming Numeric. Warming (°C) for simple-shift phenology model.
#' @param shift_per_deg Numeric. DOY shift per °C.
#' @param phenology_method Either \code{"simple_shift"} or \code{"gdd_cmip6"}.
#' @param cmip6_tas Optional list of temperature time series for GDD phenology.
#' @param gdd_base, gdd_threshold1, gdd_threshold2 Numeric. GDD model parameters.
#' @author Matthias Templ
#'
#' @return A data frame with:
#'   \item{Country}{Country name}
#'   \item{s_ctrl}{Integrated hail probability for CTRL}
#'   \item{s_scen}{Integrated hail probability for SCEN}
#'
#' @examples
#' \dontrun{
#' data(hail)
#' data(pep)
#'
#' # Basic use (simple shift)
#' integral_hail(hail, pep, phase1 = 65, phase2 = 87, genus_name = "Malus")
#' }
#'
#' @export
integral_hail <- function(
    hail,
    pep,
    giss = NULL,
    phase1 = 65,
    phase2 = 87,
    genus_name = "Malus",
    scen_warming = 3,
    shift_per_deg = -4,
    phenology_method = c("robust_shift", "simple_shift", "gdd_cmip6"),
    cmip6_tas = NULL,
    gdd_base = 5,
    gdd_threshold1 = 150,
    gdd_threshold2 = 350
){

  phenology_method <- match.arg(phenology_method)

  # Compute CTRL and SCEN DOYs per country
  phen <- get_phenology_doys(
    pep = pep,
    giss_data = giss,
    phase1 = phase1, phase2 = phase2,
    genus_name = genus_name,
    phenology_method = phenology_method,
    scen_warming = scen_warming,
    shift_per_deg = shift_per_deg,
    cmip6_tas = cmip6_tas,
    gdd_base = gdd_base,
    gdd_threshold1 = gdd_threshold1,
    gdd_threshold2 = gdd_threshold2
  )

  # Join with hail
  hail2 <- dplyr::inner_join(hail, phen, by = "Country")

  # Compute CTRL
  result_ctrl <- hail2 %>%
    dplyr::filter(Scenario == "ctrl") %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      s_ctrl = sum(mean[first(doy_start_ctrl):first(doy_end_ctrl)]),
      .groups = "drop"
    )

  # Compute SCEN
  result_scen <- hail2 %>%
    dplyr::filter(Scenario == "scen") %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      s_scen = sum(mean[first(doy_start_scen):first(doy_end_scen)]),
      .groups = "drop"
    )

  dplyr::left_join(result_ctrl, result_scen, by = "Country")
}



#' Compute Integrated Hail Probability Over Phenological Windows
#'
#' This function calculates the integral (sum) of mean daily hail probabilities
#' over phenological intervals derived from PEP725 phenology data. The intervals
#' correspond to two phenophases (default: BBCH 65 and 87), with the mean
#' day-of-year (DOY) estimated for a reference period (2011–2021). For the
#' climate scenario period (SCEN) the function currently uses placeholder DOY
#' intervals and issues a warning.
#'
#' The function groups the hail probability data by `Country` and `Scenario`
#' and computes the sum of mean hail probabilities across the relevant DOY
#' window. It is intended for exploratory comparison of CTRL vs. scenario hail
#' exposure during key phenological periods.
#'
#' @param hail A data frame containing hail probability data, including a
#'   `mean` vector indexed by DOY, and variables `Country` and `Scenario`.
#' @param pep A data frame of PEP725 observations (optional). If not supplied,
#'   the function loads the `pep` dataset from the \pkg{pep725} package.
#' @param phase1 Integer. First phenophase ID defining the start of the
#'   phenological window (default: 65).
#' @param phase2 Integer. Second phenophase ID defining the end of the
#'   phenological window (default: 87).
#' @param genus_name Character. Genus name used to filter the PEP725 records
#'   (default: `"Malus"`).
#' @importFrom tidyr pivot_wider
#'
#' @details
#' The function estimates the mean DOY for `phase1` and `phase2` across the
#' reference period 2011–2021. These DOYs form the CTRL window.
#'
#' For the climate scenario period (SCEN), no equivalent phenological
#' projections are currently available; therefore the function uses a placeholder
#' interval (DOY 100–230) and emits a warning.
#'
#' Users may wish to replace this with species-specific phenology projections
#' under SCEN conditions.
#'
#' @return A tibble summarizing, for each `Country` × `Scenario`, the integral
#'   (sum) of hail probabilities across the CTRL phenological window.
#'
#' @note
#' The function currently assumes that `hail$mean` is indexed by DOY.
#' Regional averaging of phenology and hail data may produce more accurate
#' results; current implementation uses overall means.
#'
#' @examples
#' \dontrun{
#' library(pep725)
#' data(pep)
#' pep <- pep[pep$genus == genus_name, ]
#' data(hail)
#'
#' integral_hail(hail, pep, phase1 = 65, phase2 = 87, genus_name = "Malus")
#' integral_hail(hail, pep, phenology_method = "simple_shift")     # default
#' integral_hail(hail, pep, phenology_method = "gdd_cmip6")        # advanced (todo)
#' }
#'
#' @export
#'
#'

# integral_hail <- function(
#     hail,
#     pep,
#     phase1 = 65,
#     phase2 = 87,
#     genus_name = "Malus",
#     scen_warming = 3,
#     shift_per_deg = -4,
#     phenology_method = c("simple_shift", "gdd_cmip6"),
#     cmip6_tas = NULL,      # only used in GDD mode
#     gdd_base = 5,          # only used in GDD mode
#     gdd_threshold1 = 150,  # GDD threshold for BBCH phase1
#     gdd_threshold2 = 350   # GDD threshold for BBCH phase2
# ){
#
#   phenology_method <- match.arg(phenology_method)
#
#   # ------------------------------------------------------------
#   # 0. Load hail / pep data if missing
#   # ------------------------------------------------------------
#   if (!exists("hail")) data(hail)
#   if (!exists("pep"))  data(pep)
#
#   pep <- pep[pep$genus == genus_name, ]
#
#   # dynamic names
#   p1name <- paste0("phase_", phase1)
#   p2name <- paste0("phase_", phase2)
#
#   # ------------------------------------------------------------
#   # 1. CTRL phenology per country
#   # ------------------------------------------------------------
#   phenology_means_ctrl <- pep %>%
#     filter(year >= 2011, year <= 2021,
#            phase_id %in% c(phase1, phase2)) %>%
#     group_by(country, phase_id) %>%
#     summarize(d_mean = round(mean(day)), .groups = "drop") %>%
#     tidyr::pivot_wider(
#       names_from = phase_id,
#       values_from = d_mean,
#       names_prefix = "phase_"
#     ) %>%
#     rename(Country = country)
#
#   phenology_means_ctrl <- na.omit(phenology_means_ctrl)
#
#   # CTRL join
#   hail2 <- hail %>% inner_join(phenology_means_ctrl, by = "Country")
#
#   # ------------------------------------------------------------
#   # 2. CTRL result
#   # ------------------------------------------------------------
#   result_ctrl <- hail2 %>%
#     filter(Scenario == "ctrl") %>%
#     group_by(Country) %>%
#     summarize(
#       s_ctrl = sum(mean[first(.data[[p1name]]) : first(.data[[p2name]])]),
#       .groups = "drop"
#     )
#
#   # ------------------------------------------------------------
#   # 3. SCEN phenology shift (two modes)
#   # ------------------------------------------------------------
#
#   if (phenology_method == "simple_shift") {
#
#     # simple linear shift model
#     shift_days <- scen_warming * shift_per_deg
#
#     phenology_means_scen <- phenology_means_ctrl %>%
#       mutate(
#         phase1_scen = pmax(1,  .data[[p1name]] + shift_days),
#         phase2_scen = pmin(366, .data[[p2name]] + shift_days)
#       )
#
#   } else if (phenology_method == "gdd_cmip6") {
#
#     if (is.null(cmip6_tas)) {
#       warning("not fully implemented, use simpe_shift method instead")
#       stop("For phenology_method='gdd_cmip6', you must provide daily CMIP6 tas data.")
#     }
#
#     # ------------------------------------------------------------
#     # Placeholder GDD-based phenology model
#     # Extendable with real CMIP6 tas data per country
#     # ------------------------------------------------------------
#
#     compute_doy_gdd <- function(tas_vec, base, threshold) {
#       # GDD = max(T - base, 0)
#       gdd <- pmax(tas_vec - base, 0)
#       cum_gdd <- cumsum(gdd)
#       doy <- which(cum_gdd >= threshold)[1]
#       if (is.na(doy)) doy <- 366
#       return(doy)
#     }
#
#     phenology_means_scen <- phenology_means_ctrl %>%
#       rowwise() %>%
#       mutate(
#         phase1_scen =
#           compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold1),
#
#         phase2_scen =
#           compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold2)
#       ) %>%
#       ungroup()
#   }
#
#   # ------------------------------------------------------------
#   # 4. SCEN hail integration
#   # ------------------------------------------------------------
#   hail3 <- hail %>% inner_join(phenology_means_scen, by = "Country")
#
#   result_scen <- hail3 %>%
#     filter(Scenario == "scen") %>%
#     group_by(Country) %>%
#     summarize(
#       s_scen = sum(mean[first(phase1_scen) : first(phase2_scen)]),
#       .groups = "drop"
#     )
#
#   # ------------------------------------------------------------
#   # 5. Combine
#   # ------------------------------------------------------------
#   final <- result_ctrl %>%
#     left_join(result_scen, by = "Country")
#
#   return(final)
# }
# integral_hail <- function(
#     hail,
#     pep,
#     phase1 = 65,
#     phase2 = 87,
#     genus_name = "Malus",
#
#     # --- NEW ARGUMENTS ---
#     scen_warming = 3,          # degrees warming for PGW period
#     shift_per_deg = -4         # days change per +1C warming
# ){
#
#   # ------------------------------------------------------------
#   # 0. Load data if missing
#   # ------------------------------------------------------------
#   if(!exists("hail")){
#     data(hail)
#   }
#   if(!exists("pep")){
#     data(pep)
#   }
#
#   # Keep only target genus
#   pep <- pep[pep$genus == genus_name, ]
#
#   # ------------------------------------------------------------
#   # 1. CTRL phenology means per country (2011–2021)
#   # ------------------------------------------------------------
#   phenology_means_ctrl <- pep %>%
#     filter(year >= 2011, year <= 2021,
#            phase_id %in% c(phase1, phase2)) %>%
#     group_by(country, phase_id) %>%
#     summarize(
#       d_mean = round(mean(day)),
#       .groups = "drop"
#     ) %>%
#     tidyr::pivot_wider(
#       names_from = phase_id,
#       values_from = d_mean,
#       names_prefix = "phase_"
#     )
#
#   # rename for join
#   colnames(phenology_means_ctrl)[1] <- "Country"
#   phenology_means_ctrl <- na.omit(phenology_means_ctrl)
#
#   # ------------------------------------------------------------
#   # 2. Join hail with CTRL phenology
#   # ------------------------------------------------------------
#   hail2 <- hail %>%
#     inner_join(phenology_means_ctrl, by = "Country")
#
#   # ------------------------------------------------------------
#   # 3. Compute CTRL integrals per country/scenario
#   # ------------------------------------------------------------
#   # CTRL results only
#   result_ctrl <- hail2 %>%
#     filter(Scenario == "ctrl") %>%
#     group_by(Country) %>%
#     summarize(
#       s_ctrl = sum(mean[first(phase_65) : first(phase_87)]),
#       .groups = "drop"
#     )
#
#   # ------------------------------------------------------------
#   # 4. SCEN (PGW) phenology shift
#   # ------------------------------------------------------------
#   # phenomenological shift in days:
#   shift_days <- scen_warming * shift_per_deg
#   # e.g. 3°C * -4 days = -12 (12 days earlier)
#
#   # Build SCEN phenology windows
#   phenology_means_scen <- phenology_means_ctrl %>%
#     mutate(
#       phase1_scen = pmax(1, phase_65 + shift_days),
#       phase2_scen = pmin(366, phase_87 + shift_days)
#     )
#
#   # ------------------------------------------------------------
#   # 5. Join hail with SCEN phenology
#   # ------------------------------------------------------------
#   hail3 <- hail %>%
#     inner_join(phenology_means_scen, by = "Country")
#
#   # ------------------------------------------------------------
#   # 6. Compute SCEN integrals per country/scenario
#   # ------------------------------------------------------------
#   # SCEN results only
#   result_scen <- hail3 %>%
#     filter(Scenario == "scen") %>%
#     group_by(Country) %>%
#     summarize(
#       s_scen = sum(mean[first(phase1_scen) : first(phase2_scen)]),
#       .groups = "drop"
#     )
#
#   # ------------------------------------------------------------
#   # 7. Combine CTRL and SCEN output
#   # ------------------------------------------------------------
#   # Combine only by Country
#   final <- result_ctrl %>%
#     left_join(result_scen, by = "Country")
#
#   return(final)
# }


# integral_hail <- function(hail, pep, phase1 = 65, phase2 = 87, genus_name = "Malus"){
#   # remark: is this useful to estimate means for all apples in the pep data?
#   #         would it be not much better to estimate means per region
#   #         and also the hail data per region?
#   if(!exists("hail")){
#     data(hail)
#   }
#   if(!exists("pep")){
#     data(pep)
#     pep <- pep[pep$genus == genus_name, ]
#   }
#   # # estimate the mean doi of the phenophases (CTRL: 2011-2021)
#   # d1 <- round(mean(pep$day[pep$phase_id == phase1 & pep$year >= 2011 & pep$year <= 2021]))
#   # d2 <- round(mean(pep$day[pep$phase_id == phase2& pep$year >= 2011 & pep$year <= 2021]))
#   # Berechne d1 und d2 PRO COUNTRY
#   phenology_means <- pep %>%
#     filter(genus == genus_name,
#            year >= 2011, year <= 2021,
#            phase_id %in% c(phase1, phase2)) %>%
#     group_by(country, phase_id) %>%
#     summarize(d_mean = round(mean(day)), .groups = "drop") %>%
#     tidyr::pivot_wider(names_from = phase_id, values_from = d_mean,
#                        names_prefix = "phase_")
#   # merge hail with den phenologischen Means
#   colnames(phenology_means)[1] <- "Country"
#   phenology_means <- na.omit(phenology_means)
#   hail2 <- hail %>%
#     inner_join(phenology_means, by = "Country")
#   # hail2 <- hail2 %>%
#   #   mutate(d_ctrl = purrr::map2(phase_65, phase_87, ~ .x:.y))
#   result <- hail2 %>%
#     group_by(Country, Scenario) %>%
#     summarize(
#       s = sum(mean[first(phase_65):first(phase_87)]),
#       .groups = "drop"
#     )
#   # now for the SCEN period - the following contains old code, so rewrite:
#   # estimate the mean doi of the phenophases (SCEN: 2085-2095)
#   # ???
#   warning("did not know how to estimate the mean of the phenophases for SCEN,\n used 100 and 230 for phenophase 65 and 87 for SCEN")
#   d_scen <- 100:230
#   # estimate the integral over the hail probabilities
#   hail %>%
#     group_by(Country, Scenario) %>%
#     summarize(s = sum(mean[d_scen]))
# }
