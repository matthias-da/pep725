#' Compute CTRL and SCEN Phenology Day-of-Year Values
#'
#' This function computes the mean phenological day-of-year (DOY) for two
#' phenophases (e.g. BBCH 65 and 87) for each country, using either a simple
#' warming-shift model or a GDD-based model. Output is suitable for use in
#' both `integral_hail()` and `plot_hail_overlap()`.
#'
#' @param pep A data frame containing PEP725 phenology observations, including
#'   columns \code{country}, \code{day}, \code{year}, \code{phase_id}, and \code{genus}.
#' @param phase1 Integer. First phenophase (e.g. 65).
#' @param phase2 Integer. Second phenophase (e.g. 87).
#' @param genus_name Character. Genus to filter (e.g. "Malus").
#' @param phenology_method Either \code{"simple_shift"} or \code{"gdd_cmip6"}.
#' @param scen_warming Numeric. Warming in °C (simple shift model).
#' @param shift_per_deg Numeric. DOY shift per °C (simple shift).
#' @param cmip6_tas Optional list of daily temperature vectors—only required
#'   if using \code{"gdd_cmip6"}.
#' @param gdd_base Numeric. Base temperature for GDD model.
#' @param gdd_threshold1 Numeric. GDD threshold for phase1.
#' @param gdd_threshold2 Numeric. GDD threshold for phase2.
#' @author Matthias Templ
#'
#' @return A data frame containing:
#'   \item{Country}{Country name}
#'   \item{doy_start_ctrl}{Start DOY for CTRL period}
#'   \item{doy_end_ctrl}{End DOY for CTRL period}
#'   \item{doy_start_scen}{Start DOY for SCEN period}
#'   \item{doy_end_scen}{End DOY for SCEN period}
#'
#' Compute CTRL and SCEN Phenology Day-of-Year Values
#'
#' This function computes the mean phenological day-of-year (DOY) for two
#' phenophases (e.g., BBCH 65 and BBCH 87) for each country. The calculation
#' can use a simple warming‐shift model or a GDD-based model using CMIP6 daily
#' temperature time series.
#'
#' @param pep A data frame containing PEP725 phenology data, including columns
#'   \code{country}, \code{day}, \code{year}, \code{phase_id}, and \code{genus}.
#' @param phase1 Integer. First phenophase (e.g., 65).
#' @param phase2 Integer. Second phenophase (e.g., 87).
#' @param genus_name Character. Genus to filter (e.g., "Malus").
#' @param phenology_method Either \code{"simple_shift"} or \code{"gdd_cmip6"}.
#' @param scen_warming Numeric. Warming (°C) for simple shift model.
#' @param shift_per_deg Numeric. Shift in DOY per °C warming.
#' @param cmip6_tas Optional named list with daily temperature time series
#'   (one numeric vector per country) for GDD phenology.
#' @param gdd_base Numeric. Base temperature for GDD accumulation.
#' @param gdd_threshold1 Numeric. GDD threshold for phenophase 1.
#' @param gdd_threshold2 Numeric. GDD threshold for phenophase 2.
#'
#' @return A data frame with:
#'   \item{Country}{Country name}
#'   \item{doy_start_ctrl}{Phase1 DOY (CTRL)}
#'   \item{doy_end_ctrl}{Phase2 DOY (CTRL)}
#'   \item{doy_start_scen}{Phase1 DOY (SCEN)}
#'   \item{doy_end_scen}{Phase2 DOY (SCEN)}
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' data(giss)
#'
#' regions <- c("Austria", "Germany-North", "Germany-South", "Switzerland")
#'
#' # Simple shift method
#' d1 <- get_phenology_doys(
#'   pep = pep,
#'   phase1 = 65,
#'   phase2 = 87,
#'   genus_name = "Malus",
#'   regions = regions,
#'   phenology_method = 'simple_shift',
#' )
#' d1
#'
#' # robust shift
#' d2 <- get_phenology_doys(
#'   pep = pep,
#'   phase1 = 65,
#'   phase2 = 87,
#'   genus_name = "Malus",
#'   phenology_method = "robust_shift",
#'   giss_data = giss,
#'   regions = regions
#' )
#' d2
#' }
#'
#' @export
#'
get_phenology_doys <- function(
    pep,
    phase1,
    phase2,
    genus_name,
    phenology_method = c("robust_shift", "simple_shift", "gdd_cmip6"),
    ctrl_years = 2011:2021,       # CTRL für Tgl-Mittel (PGW-Setup)
    scen_years = 2085:2095,       # PGW-Periode
    calib_years = 1991:2020,       # Kalibrierzeitraum für Regression
    scen_warming = 3,
    shift_per_deg = -4,
    cmip6_tas = NULL,
    gdd_base = 5,
    gdd_threshold1 = 150,
    gdd_threshold2 = 350,
    regions = NULL,
    delta_Tgl = 2.09,        # numeric or "estimate"
    giss_data = NULL         # must be provided for robust_shift
){

  phenology_method <- match.arg(phenology_method)

  # ------------------------------------------------------------------
  # 1. Filter genus + regions
  # ------------------------------------------------------------------
  pep <- pep[pep$genus == genus_name, ]

  if (!is.null(regions)) {
    pep <- pep[pep$country %in% regions, ]
  }

  if (nrow(pep) == 0)
    stop("No data left after filtering for genus and selected regions.")

  p1name <- paste0("phase_", phase1)
  p2name <- paste0("phase_", phase2)

  # ================================================================
  # SIMPLE_SHIFT & GDD_CMIP6
  # ================================================================
  if (phenology_method %in% c("robust_shift", "simple_shift", "gdd_cmip6")) {

    ctrl <- pep %>%
      dplyr::filter(
        year >= min(ctrl_years), year <= max(ctrl_years),
        phase_id %in% c(phase1, phase2)
      ) %>%
      dplyr::group_by(country, phase_id) %>%
      dplyr::summarize(d_mean = round(mean(day)), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = phase_id,
        values_from = d_mean,
        names_prefix = "phase_"
      ) %>%
      dplyr::rename(Country = country) %>%
      stats::na.omit()

    # -----------------------------
    # simple shift
    # -----------------------------
    if (phenology_method == "simple_shift") {

      shift_days <- scen_warming * shift_per_deg

      ctrl <- ctrl %>%
        dplyr::mutate(
          doy_start_ctrl = .data[[p1name]],
          doy_end_ctrl   = .data[[p2name]],
          doy_start_scen = pmax(1, doy_start_ctrl + shift_days),
          doy_end_scen   = pmin(366, doy_end_ctrl + shift_days)
        )

      return(ctrl %>% dplyr::select(
        Country, doy_start_ctrl, doy_end_ctrl, doy_start_scen, doy_end_scen))
    }

    # -----------------------------
    # GDD CMIP6
    # -----------------------------
    if (phenology_method == "gdd_cmip6") {

      if (is.null(cmip6_tas))
        stop("cmip6_tas must be supplied for gdd_cmip6 method.")

      compute_doy_gdd <- function(tvec, base, thr) {
        gdd <- pmax(tvec - base, 0)
        cum <- cumsum(gdd)
        idx <- which(cum >= thr)[1]
        if (is.na(idx)) idx <- length(tvec)
        idx
      }

      ctrl <- ctrl %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          doy_start_ctrl = .data[[p1name]],
          doy_end_ctrl   = .data[[p2name]],
          doy_start_scen =
            compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold1),
          doy_end_scen   =
            compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold2)
        ) %>%
        dplyr::ungroup()

      return(ctrl %>% dplyr::select(
        Country, doy_start_ctrl, doy_end_ctrl, doy_start_scen, doy_end_scen))
    }
  }

  # ================================================================
  # ROBUST_SHIFT (DOY ~ Tgl from GISS)
  # ================================================================
  if (phenology_method == "robust_shift") {

    if (is.null(giss_data))
      stop("Please supply global temperature data via giss_data = ...")



    # --------------------------------------------------------------
    # Validate and automatically create Tgl if missing
    # --------------------------------------------------------------
    if (!("year" %in% names(giss_data)))
      stop("giss_data must contain a 'year' column.")

    if ("Tgl" %in% names(giss_data)) {
      giss_sub <- giss_data
    } else if ("dT" %in% names(giss_data)) {
      message("No column 'Tgl' found. Creating Tgl = dT + 14 (absolute global temp).")
      giss_sub <- giss_data %>%
        dplyr::mutate(Tgl = dT + 14)
    } else {
      stop("giss_data must contain either a 'Tgl' or 'dT' column.")
    }

    # Restrict to useful years
    giss_sub <- giss_sub[giss_sub$year >= 1961, ] %>%
      dplyr::arrange(year)

    Tgl_CTRL <- mean(giss_sub$Tgl[giss_sub$year %in% ctrl_years], na.rm = TRUE)

    # -------------------------
    # ΔTgl logic
    # -------------------------
    if (is.character(delta_Tgl) && delta_Tgl == "estimate") {

      Tgl_SCEN_est <- mean(giss_sub$Tgl[giss_sub$year %in% scen_years], na.rm = TRUE)
      delta_val <- Tgl_SCEN_est - Tgl_CTRL

      message(sprintf(
        "Estimated ΔTgl from GISS: %.3f K.", delta_val))

    } else if (is.numeric(delta_Tgl)) {
      delta_val <- delta_Tgl
    } else {
      stop('delta_Tgl must be numeric or "estimate".')
    }

    Tgl_SCEN <- Tgl_CTRL + delta_val

    # -------------------------
    # prep PEP aggregated DOYs
    # -------------------------
    agg <- pep %>%
      dplyr::filter(phase_id %in% c(phase1, phase2)) %>%
      dplyr::group_by(country, phase_id, year) %>%
      dplyr::summarize(mean_DOY = mean(day), .groups = "drop") %>%
      dplyr::left_join(giss_sub, by = "year")

    # -------------------------
    # Robust regression: DOY ~ Tgl (auf calib_years)
    # -------------------------
    robust_fit_Tgl <- function(df) {
      sub <- df %>%
        dplyr::filter(
          year >= min(calib_years), year <= max(calib_years),
          is.finite(mean_DOY),
          is.finite(Tgl)
        )

      if (nrow(sub) < 3)
        return(tibble::tibble(a = NA_real_, b = NA_real_))

      mod <- tryCatch(
        robustbase::lmrob(mean_DOY ~ Tgl, data = sub),
        error = function(e) NULL
      )

      if (is.null(mod))
        return(tibble::tibble(a = NA_real_, b = NA_real_))

      tibble::tibble(
        a = unname(coef(mod)[1]),
        b = unname(coef(mod)[2])
      )
    }

    fits <- agg %>%
      dplyr::group_by(country, phase_id) %>%
      dplyr::group_modify(~ robust_fit_Tgl(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(a))

    # Predictions
    fits <- fits %>%
      dplyr::mutate(
        doy_ctrl = a + b * Tgl_CTRL,
        doy_scen = a + b * Tgl_SCEN
      )

    safe_mean <- function(x)
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

    # -------------------------
    # reshape to start/end DOYs
    # -------------------------
    result <- fits %>%
      dplyr::mutate(
        doy_start_ctrl = ifelse(phase_id == phase1, doy_ctrl, NA_real_),
        doy_end_ctrl   = ifelse(phase_id == phase2, doy_ctrl, NA_real_),
        doy_start_scen = ifelse(phase_id == phase1, doy_scen, NA_real_),
        doy_end_scen   = ifelse(phase_id == phase2, doy_scen, NA_real_)
      ) %>%
      dplyr::group_by(country) %>%
      dplyr::summarize(
        doy_start_ctrl = round(safe_mean(doy_start_ctrl)),
        doy_end_ctrl   = round(safe_mean(doy_end_ctrl)),
        doy_start_scen = round(safe_mean(doy_start_scen)),
        doy_end_scen   = round(safe_mean(doy_end_scen)),
        .groups = "drop"
      ) %>%
      dplyr::rename(Country = country) %>%
      # erst jetzt Ordnung sichern und dann auf [1,366] beschränken
      dplyr::mutate(
        # CTRL
        tmp_start_ctrl = pmin(doy_start_ctrl, doy_end_ctrl),
        tmp_end_ctrl   = pmax(doy_start_ctrl, doy_end_ctrl),
        # SCEN
        tmp_start_scen = pmin(doy_start_scen, doy_end_scen),
        tmp_end_scen   = pmax(doy_start_scen, doy_end_scen)
      ) %>%
      dplyr::transmute(
        Country,
        doy_start_ctrl = pmin(366, pmax(1, tmp_start_ctrl)),
        doy_end_ctrl   = pmin(366, pmax(1, tmp_end_ctrl)),
        doy_start_scen = pmin(366, pmax(1, tmp_start_scen)),
        doy_end_scen   = pmin(366, pmax(1, tmp_end_scen))
      )

    return(result)
  }
}

# get_phenology_doys <- function(
#     pep,
#     phase1,
#     phase2,
#     genus_name,
#     phenology_method = c("simple_shift", "gdd_cmip6"),
#     scen_warming = 3,
#     shift_per_deg = -4,
#     cmip6_tas = NULL,
#     gdd_base = 5,
#     gdd_threshold1 = 150,
#     gdd_threshold2 = 350
# ){
#
#   phenology_method <- match.arg(phenology_method)
#
#   # Filter for genus
#   pep <- pep[pep$genus == genus_name, ]
#
#   p1name <- paste0("phase_", phase1)
#   p2name <- paste0("phase_", phase2)
#
#   # Compute CTRL mean DOYs per country
#   ctrl <- pep %>%
#     dplyr::filter(
#       year >= 2011, year <= 2021,
#       phase_id %in% c(phase1, phase2)
#     ) %>%
#     dplyr::group_by(country, phase_id) %>%
#     dplyr::summarize(d_mean = round(mean(day)), .groups = "drop") %>%
#     tidyr::pivot_wider(
#       names_from = phase_id,
#       values_from = d_mean,
#       names_prefix = "phase_"
#     ) %>%
#     dplyr::rename(Country = country) %>%
#     na.omit()
#
#   # Compute SCEN DOYs
#   if (phenology_method == "simple_shift") {
#
#     shift_days <- scen_warming * shift_per_deg
#
#     ctrl <- ctrl %>%
#       dplyr::mutate(
#         doy_start_ctrl = .data[[p1name]],
#         doy_end_ctrl   = .data[[p2name]],
#         doy_start_scen = pmax(1, doy_start_ctrl + shift_days),
#         doy_end_scen   = pmin(366, doy_end_ctrl + shift_days)
#       )
#
#   } else {
#
#     if (is.null(cmip6_tas)) stop("CMIP6 tas list is required for gdd_cmip6 method.")
#
#     compute_doy_gdd <- function(tvec, base, thr) {
#       gdd <- pmax(tvec - base, 0)
#       cum <- cumsum(gdd)
#       idx <- which(cum >= thr)[1]
#       if (is.na(idx)) idx <- length(tvec)
#       idx
#     }
#
#     ctrl <- ctrl %>%
#       dplyr::rowwise() %>%
#       dplyr::mutate(
#         doy_start_ctrl = .data[[p1name]],
#         doy_end_ctrl   = .data[[p2name]],
#         doy_start_scen =
#           compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold1),
#         doy_end_scen =
#           compute_doy_gdd(cmip6_tas[[Country]], gdd_base, gdd_threshold2)
#       ) %>%
#       dplyr::ungroup()
#   }
#
#   return(ctrl %>% dplyr::select(Country, doy_start_ctrl, doy_end_ctrl,
#                                 doy_start_scen, doy_end_scen))
# }


