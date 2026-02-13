utils::globalVariables(c("a"))

#' Plot Robust Phenological Trends
#'
#' This function visualizes long-term phenological trends for one genus or species,
#' using robust MM-regression (`robustbase::lmrob`) applied to mean annual DOYs.
#'
#' The function produces a faceted panel plot showing:
#' \itemize{
#'   \item annual mean DOY and interquartile range,
#'   \item robust trend lines for past and future periods,
#'   \item CTRL (1991-2020) and PGW (2066-2095) climate windows,
#'   \item optional layouts (country x phase or phase x country).
#' }
#'
#' @param pep A PEP725-style data frame with columns \code{year}, \code{DOY},
#'   \code{phase_id}, \code{country}, \code{s_id}, \code{genus}, \code{species}.
#' @param genus_name Character string specifying a genus to filter (optional).
#' @param species_name Character string specifying a species to filter (optional).
#'   If both genus and species are provided, the species filter is applied last.
#' @param subspecies_name Character string specifying a subspecies to filter (optional).
#'   Only one of genus_name, species_name, or subspecies_name can be specified.
#' @param phases Integer vector of phenological phase IDs to include
#'   (default: \code{c(65, 87)} for flowering and maturity of fruit).
#' @param common_stations Logical. If \code{TRUE} (default), only stations with observations
#'   for all selected phases are included. If \code{FALSE}, all stations with any of the selected phases are included.
#' @param combine_regions Logical. If \code{TRUE}, combines all regions into a single panel. Default is \code{FALSE}.
#' @param combine_layout Character. Layout for combined regions plot: either \code{"vertical"} or \code{"horizontal"}.
#' @param years Numeric vector of years to include in the analysis.
#'   Default is \code{1961:2024}.
#' @param calib_years Numeric vector specifying the calibration window for robust
#'   regression (default: \code{1991:2020}).
#' @param pred_years Numeric vector of years for projecting future phenology.
#'   Default is \code{1990:2090}.
#' @param subregions Character vector of country names to include
#'   (default: DACH region).
#' @param giss_data Deprecated. Previously used for climate data integration;
#'   now ignored.
#' @param layout Either \code{"country_phase"} (default) or \code{"phase_country"}
#'   to control facet arrangement.
#' @param title Optional plot title. If \code{NULL}, the function generates one
#'   from genus and species information.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filter PEP data by species, phases, regions, years.
#'   \item Aggregate DOYs by year-country-phase.
#'   \item Fit robust regression \eqn{DOY \sim year} over the calibration window.
#'   \item Predict past and future trends.
#'   \item Draw ribbon (IQR), annual means, trend lines, climate windows.
#'   \item Facet by country or by phase.
#' }
#'
#' The robust regression uses \code{robustbase::lmrob}, which is resistant to
#' outliers and non-normality, and therefore ideal for phenological time series.
#'
#' @return A \code{ggplot} object.
#'
#' @author Matthias Templ
#' @export
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#'
#' # Grapevine (Vitis vinifera) - longest historical records
#' # phases BBCH 65 (flowering) & 81 (veraison), DACH only:
#' plot_phenology_trends(
#'   pep,
#'   species_name = "Vitis vinifera",
#'   phases = c(65, 81),
#'   layout = "country_phase"
#' )
#'
#' # Same data, but facets flipped:
#' plot_phenology_trends(
#'   pep,
#'   species_name = "Vitis vinifera",
#'   phases = c(65, 81),
#'   layout = "phase_country"
#' )
#'
#' # Genus-level plot (all species of the genus Vitis)
#' plot_phenology_trends(
#'   pep,
#'   genus_name = "Vitis",
#'   phases = c(65, 81)
#' )
#' }
plot_phenology_trends <- function(
    pep,
    genus_name = NULL,
    species_name = NULL,
    subspecies_name = NULL,   # NEW
    phases = c(65, 87),
    common_stations = TRUE,
    combine_regions = FALSE,   # NEW ARGUMENT
    combine_layout = c("vertical", "horizontal"),
    years = 1961:2024,
    calib_years = 1991:2020,
    pred_years = NULL,
    subregions = c("Austria","Germany-North","Germany-South","Switzerland"),
    giss_data = NULL,
    layout = c("country_phase", "phase_country"),
    title = NULL
){

  layout <- match.arg(layout)
  combine_layout <- match.arg(combine_layout)

  # ------------------------------------------------------------------
  # 0. Validate taxonomy input (only one allowed)
  # ------------------------------------------------------------------
  taxo_args <- list(
    genus = genus_name,
    species = species_name,
    subspecies = subspecies_name
  )
  if (sum(!sapply(taxo_args, is.null)) > 1) {
    stop("Please specify only ONE of: genus_name, species_name, subspecies_name.")
  }

  # ------------------------------------------------------------------
  # 1. Filter PEP data
  # ------------------------------------------------------------------
  pep2 <- pep %>%
    dplyr::rename(DOY = dplyr::any_of("day")) %>%
    dplyr::filter(year %in% years)

  # TAXONOMY FILTER ---------------------------------------------------
  if (!is.null(subspecies_name)) {
    pep2 <- pep2 %>% dplyr::filter(!is.na(subspecies) & subspecies == subspecies_name)

  } else if (!is.null(species_name)) {
    pep2 <- pep2 %>% dplyr::filter(species == species_name)

  } else if (!is.null(genus_name)) {
    pep2 <- pep2 %>% dplyr::filter(genus == genus_name)
  }

  # Phase filtering
  pep2 <- pep2 %>% dplyr::filter(phase_id %in% phases)

  # Region filter
  pep2 <- pep2 %>% dplyr::filter(country %in% subregions)

  if(nrow(pep2) == 0){
    stop("No data available after filtering of years, phases and subregions. Please check your filters.")
  }

  # Keep only stations with all phases (optional)
  if(common_stations){
    common_ids <- Reduce(intersect,
                         lapply(phases, function(p) pep2$s_id[pep2$phase_id == p]))
    pep2 <- pep2 %>% dplyr::filter(s_id %in% common_ids)
  }

  if(nrow(pep2) == 0){
    if(!common_stations){
      stop("No data available after filtering. Try common_stations = FALSE.")
    } else {
      stop("No data available after filtering of years, phases, subregions and common_stations.")
    }
  }

  # ------------------------------------------------------------------
  # 2. Aggregate DOYs per country-year-phase
  # ------------------------------------------------------------------
  agg <- pep2 %>%
    dplyr::group_by(country, year, phase_id) %>%
    dplyr::summarise(
      mean_DOY = mean(DOY, na.rm=TRUE),
      q25      = quantile(DOY, 0.25, na.rm=TRUE),
      q75      = quantile(DOY, 0.75, na.rm=TRUE),
      .groups="drop"
    )


  # ------------------------------------------------------------------
  # 2b. NEW: combine all regions into one series
  # ------------------------------------------------------------------
  if (combine_regions) {

    message("Combining all regions into a single aggregated trend...")

    agg <- agg %>%
      dplyr::group_by(year, phase_id) %>%
      dplyr::summarise(
        mean_DOY = mean(mean_DOY, na.rm = TRUE),
        q25      = mean(q25, na.rm = TRUE),
        q75      = mean(q75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(country = "All regions")

    subregions <- "All regions"
  }


  # ------------------------------------------------------------------
  # 3. Robust regression: DOY ~ year (MM estimator)
  # ------------------------------------------------------------------
  robust_fit_year <- function(df){
    sub <- df %>%
      dplyr::filter(
        year >= min(calib_years),
        year <= max(calib_years),
        is.finite(mean_DOY)
      )

    if (nrow(sub) < 3)
      return(tibble::tibble(a = NA_real_, b = NA_real_))

    mod <- tryCatch(
      robustbase::lmrob(mean_DOY ~ year, data = sub),
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
    dplyr::group_by(phase_id, dplyr::across(dplyr::any_of("country"))) %>%
    dplyr::group_modify(~ robust_fit_year(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(a)) %>%
    dplyr::mutate(
      days_decade = b * 10,
      label = sprintf("%+.2f d/dec", days_decade)
    )


  # ------------------------------------------------------------------
  # 5. Predictions (regression line until PGW)
  # ------------------------------------------------------------------
  if (is.null(pred_years)) {
    pred_years <- seq(min(calib_years), 2095)
  }

  pred <- fits %>%
    dplyr::group_by(phase_id, dplyr::across(dplyr::any_of("country"))) %>%
    dplyr::do(tibble::tibble(
      year = pred_years,
      mean_DOY = .$a + .$b * pred_years
    )) %>%
    dplyr::ungroup()


  # ------------------------------------------------------------------
  # 6. Title construction
  # ------------------------------------------------------------------
  if (is.null(title)) {
    if (!is.null(subspecies_name))
      title <- subspecies_name
    else if (!is.null(species_name))
      title <- species_name
    else if (!is.null(genus_name))
      title <- genus_name
    else
      title <- "Phenology -- Robust Trend"
  }

  # Slope annotation at year = 2085
  label_x <- 2085
  fits <- fits %>% dplyr::mutate(label_y = a + b * label_x)


  # ------------------------------------------------------------------
  # 7. Build plot
  # ------------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = agg,
      ggplot2::aes(year, ymin = q25, ymax = q75),
      alpha = 0.18
    ) +
    ggplot2::geom_line(
      data = agg,
      ggplot2::aes(year, mean_DOY)
    ) +
    ggplot2::geom_line(
      data = pred,
      ggplot2::aes(year, mean_DOY),
      size = 0.7,
      linetype = "dashed",
      colour = "red"
    ) +
    ggplot2::geom_text(
      data = fits,
      ggplot2::aes(x = label_x, y = label_y, label = label),
      hjust = 0.7,
      vjust = -0.6,
      fontface = "italic",
      size = 2.5
    ) +
    ggplot2::geom_vline(
      xintercept = c(min(calib_years), max(calib_years)),
      linetype = 1,
      colour = "blue",
      size = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = c(2066, 2095),
      linetype = 5,
      colour = "red",
      size = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = mean(calib_years) + 5,
      y = min(agg$mean_DOY, na.rm = TRUE),
      label = "CTRL",
      size = 2.7,
      colour = "blue"
    ) +
    ggplot2::annotate(
      "text",
      x = 2080,
      y = min(agg$mean_DOY, na.rm = TRUE),
      label = "PGW",
      size = 2.7,
      colour = "red"
    ) +
    ggplot2::labs(
      title = title,
      x = "",
      y = "Day of Year (DOY)"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1900, 2100, 25)) +
    ggplot2::theme_bw(base_size = 12)


  # ------------------------------------------------------------------
  # 8. Faceting logic
  # ------------------------------------------------------------------
  if (!combine_regions) {

    # Original behavior
    if (layout == "country_phase") {
      p <- p + ggplot2::facet_grid(country ~ phase_id)
    } else {
      p <- p + ggplot2::facet_grid(phase_id ~ country)
    }

  } else {

    # Combined region plot: facet only by phase
    if (length(phases) > 1) {

      if (combine_layout == "vertical") {
        p <- p + ggplot2::facet_wrap(~ phase_id, ncol = 1)

      } else if (combine_layout == "horizontal") {
        p <- p + ggplot2::facet_wrap(~ phase_id, nrow = 1)

      }

    } else {
      message("Plotting combined-region trend for BBCH ", phases)
    }

  }

  return(p)
}
