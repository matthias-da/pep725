#' Comprehensive Subspecies Availability Report
#'
#' This function produces a complete report for phenological subspecies
#' availability, combining:
#'   (1) a tidy summary table,
#'   (2) a publication-ready wide table, and
#'   (3) a ggplot2 heatmap showing data presence/absence.
#'
#' Users may specify a \strong{genus}, \strong{species}, or one or more
#' \strong{subspecies}. The report evaluates BBCH phases, regions, years,
#' completeness, and several optional availability metrics.
#'
#' @param pep A PEP725-style data frame containing at least:
#'   `genus`, `species`, `subspecies`, `country`, `year`, `phase_id`,
#'   `s_id`, and `day` or `DOY`.
#' @param genus_name Character (optional). Genus to filter.
#' @param species_name Character (optional). Species to filter.
#' @param subspecies_name Character vector (optional). One or more subspecies.
#' @param phases Integer vector. BBCH phases to evaluate.
#' @param years Numeric vector. Years to include.
#' @param subregions Character vector of region/country names.
#' @param include_empty Logical. If TRUE, include subspecies x country rows
#'   even when no observations are present.
#' @param metric Character vector. Metrics to compute. Use `"all"` for:
#'   `"n_obs"`, `"n_stations"`, `"first_year"`, `"last_year"`,
#'   `"median_doy"`, `"completeness"`.
#' @param bbch_names Optional named list mapping BBCH phase numbers to labels,
#'   e.g. `list("65" = "Flowering", "87" = "Maturity")`.
#' @param return_results Logical. If TRUE, returns a list invisibly.
#'
#' @return Invisibly returns a list with:
#'   \describe{
#'     \item{summary_table}{Tidy long-format summary}
#'     \item{wide_table}{Publication-ready wide-format summary}
#'     \item{heatmap}{A ggplot2 object}
#'   }
#'
#' @seealso \code{\link{pheno_trend_turning}} for trend visualization
#'
#' @author Matthias Templ
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom tibble tibble
#' @importFrom purrr map
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#'
#' # Use Alpine subset for faster computation
#' pep_alpine <- pep[country %in% c("Switzerland", "Austria")]
#'
#' # Grapevine subspecies (wine varieties) - longest historical records
#' if (nrow(pep_alpine[species == "Vitis vinifera"]) > 0) {
#'   subspecies_report(
#'     pep_alpine,
#'     subspecies_name = c("Vitis vinifera Riesling",
#'                         "Vitis vinifera MUELLER THURGAU WEISS"),
#'     subregions = c("Switzerland", "Austria"),
#'     metric = "all",
#'     bbch_names = list("65"="Flowering", "81"="Veraison")
#'   )
#'
#'   subspecies_report(
#'     pep_alpine,
#'     species_name = "Vitis vinifera",
#'     subregions = c("Switzerland", "Austria"),
#'     metric = c("n_obs","median_doy","completeness")
#'   )
#'
#'   subspecies_report(
#'     pep_alpine,
#'     genus_name = "Vitis",
#'     subregions = c("Switzerland", "Austria"),
#'     metric = "all"
#'   )
#' }
#' }
subspecies_report <- function(
    pep,
    genus_name      = NULL,
    species_name    = NULL,
    subspecies_name = NULL,     # may be vector
    phases          = c(65, 87),
    years           = 1961:2024,
    subregions      = c("Austria","Germany-North","Germany-South","Switzerland"),
    include_empty   = TRUE,
    metric          = "all",
    bbch_names      = NULL,
    return_results  = TRUE      # return list invisibly
){
  # --------------------------------------------------------------
  # 1) RUN SUMMARY
  # --------------------------------------------------------------
  summary_tbl <- summarize_subspecies_availability(
    pep             = pep,
    genus_name      = genus_name,
    species_name    = species_name,
    subspecies_name = subspecies_name,
    phases          = phases,
    years           = years,
    subregions      = subregions,
    include_empty   = include_empty,
    metric          = metric
  )

  cat("\n========================\nSUBSPECIES SUMMARY TABLE\n========================\n")
  print(summary_tbl)


  # --------------------------------------------------------------
  # 2) WIDE TABLE
  # --------------------------------------------------------------
  wide_tbl <- format_subspecies_wide_table(
    df         = summary_tbl,
    phases     = phases,
    subregions = subregions,
    metric     = metric,
    bbch_names = bbch_names
  )

  cat("\n========================\nWIDE SUMMARY TABLE\n========================\n")
  print(wide_tbl)


  # --------------------------------------------------------------
  # 3) MISSINGNESS HEATMAP
  # --------------------------------------------------------------
  cat("\n========================\nMISSINGNESS HEATMAP\n========================\n")

  p_heat <- plot_subspecies_missingness_heatmap(
    pep             = pep,
    genus_name      = genus_name,
    species_name    = species_name,
    subspecies_name = subspecies_name,
    phases          = phases,
    subregions      = subregions,
    years           = years,
    bbch_names      = bbch_names
  )

  print(p_heat)

  # --------------------------------------------------------------
  # Return all outputs as a list (invisible)
  # --------------------------------------------------------------
  if (return_results) {
    return(invisible(list(
      summary_table = summary_tbl,
      wide_table    = wide_tbl,
      heatmap       = p_heat
    )))
  }
}


#' Summarize Subspecies Phenological Data Availability
#'
#' Computes data availability metrics for specified genus, species, or
#' subspecies across BBCH phases and regions. Metrics include numbers of
#' observations, station counts, first/last observation years, median DOY,
#' and completeness ratios.
#'
#' @inheritParams subspecies_report
#'
#' @param metric Character vector specifying which metrics to compute.
#'   Allowed values:
#'   `"n_obs"`, `"n_stations"`, `"first_year"`, `"last_year"`,
#'   `"median_doy"`, `"completeness"`, or `"all"`.
#'
#' @return A tibble in long format with one row per `subspecies x country`.
#'
#' @author Matthias Templ
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble
#' @importFrom stats median
#'
#' @examples
#' \donttest{
#' pep <- pep_download()
#' pep_alpine <- pep[country %in% c("Switzerland", "Austria")]
#'
#' # Grapevine species (longest historical records)
#' if (nrow(pep_alpine[species == "Vitis vinifera"]) > 0) {
#'   summarize_subspecies_availability(
#'     pep_alpine,
#'     species_name = "Vitis vinifera",
#'     subregions = c("Switzerland", "Austria"),
#'     metric = "all"
#'   )
#'
#'   summarize_subspecies_availability(
#'     pep_alpine,
#'     subspecies_name = c("Vitis vinifera Riesling",
#'                         "Vitis vinifera MUELLER THURGAU WEISS"),
#'     subregions = c("Switzerland", "Austria"),
#'     metric = c("n_obs","median_doy")
#'   )
#' }
#' }
summarize_subspecies_availability <- function(
    pep,
    genus_name      = NULL,
    species_name    = NULL,
    subspecies_name = NULL,   # may be vector
    phases          = c(65, 87),
    years           = 1961:2024,
    subregions      = c("Austria","Germany-North","Germany-South","Switzerland"),
    include_empty   = FALSE,
    metric          = c("n_obs", "n_stations", "first_year",
                        "last_year", "median_doy", "completeness")
){
  # ---------------------------------------------------------------
  # Normalize metric argument
  # ---------------------------------------------------------------
  if (identical(metric, "all")) {
    metric <- c("n_obs","n_stations","first_year","last_year","median_doy","completeness")
  }

  metric <- match.arg(
    metric,
    choices = c("n_obs","n_stations","first_year","last_year","median_doy","completeness"),
    several.ok = TRUE
  )

  # ---------------------------------------------------------------
  # 1) Filter PEP + rename DOY
  # ---------------------------------------------------------------
  pep2 <- pep %>%
    dplyr::rename(DOY = dplyr::any_of("day")) %>%
    dplyr::filter(
      year %in% years,
      country %in% subregions,
      phase_id %in% phases
    )

  # ---------------------------------------------------------------
  # 2) Taxonomy filtering (priority)
  # ---------------------------------------------------------------
  if (!is.null(subspecies_name)) {

    pep2 <- pep2 %>%
      dplyr::filter(!is.na(subspecies) & subspecies %in% subspecies_name)

  } else if (!is.null(species_name)) {

    pep2 <- pep2 %>%
      dplyr::filter(
        species == species_name |
          (!is.na(subspecies) & grepl(paste0("^", species_name), subspecies))
      )

  } else if (!is.null(genus_name)) {

    pep2 <- pep2 %>% dplyr::filter(genus == genus_name)
  }

  pep2 <- pep2 %>% dplyr::filter(!is.na(subspecies))

  # force character phase_id for pivot_wider stability
  pep2 <- pep2 %>% dplyr::mutate(phase_id = as.character(phase_id))

  # ---------------------------------------------------------------
  # 3) Phase-specific metrics (A–E)
  # ---------------------------------------------------------------
  phase_summary <- pep2 %>%
    dplyr::group_by(subspecies, country, phase_id) %>%
    dplyr::summarise(
      n_obs      = if ("n_obs"      %in% metric) dplyr::n()               else NA,
      n_stations = if ("n_stations" %in% metric) dplyr::n_distinct(s_id)  else NA,
      first_year = if ("first_year" %in% metric) min(year, na.rm=TRUE)    else NA,
      last_year  = if ("last_year"  %in% metric) max(year, na.rm=TRUE)    else NA,
      median_DOY = if ("median_doy" %in% metric) median(DOY, na.rm=TRUE)  else NA,
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = phase_id,
      values_from = c(n_obs, n_stations, first_year, last_year, median_DOY),
      names_glue  = "{.value}_{phase_id}"
    )

  # Ensure all expected columns exist
  expected_cols <- unlist(lapply(
    c("n_obs","n_stations","first_year","last_year","median_DOY"),
    function(prefix) paste0(prefix, "_", phases)
  ))
  for (cl in expected_cols) {
    if (!cl %in% names(phase_summary)) {
      if (grepl("n_obs|n_stations", cl)) phase_summary[[cl]] <- 0 else phase_summary[[cl]] <- NA
    }
  }

  # ---------------------------------------------------------------
  # 4) Completeness ratio (F)
  # ---------------------------------------------------------------
  if ("completeness" %in% metric) {

    completeness_phase <- pep2 %>%
      dplyr::group_by(subspecies, country, phase_id) %>%
      dplyr::summarise(
        years_present = dplyr::n_distinct(year),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from  = phase_id,
        values_from = years_present,
        names_glue  = "years_present_{phase_id}"
      )

    # Guarantee columns
    for (ph in phases) {
      nm <- paste0("years_present_", ph)
      if (!nm %in% names(completeness_phase)) completeness_phase[[nm]] <- 0
    }

    # Add completeness ratios
    completeness_phase <- completeness_phase %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::starts_with("years_present_"),
          ~ . / length(years),
          .names = "completeness_{.col}"
        )
      )

    phase_summary <- phase_summary %>%
      dplyr::left_join(completeness_phase, by = c("subspecies","country"))
  }

  # ---------------------------------------------------------------
  # 5) Optionally include requested subspecies × all countries
  # ---------------------------------------------------------------
  if (!is.null(subspecies_name) && include_empty) {

    full_grid <- expand.grid(
      subspecies = subspecies_name,
      country    = subregions,
      stringsAsFactors = FALSE
    )

    phase_summary <- full_grid %>%
      dplyr::left_join(phase_summary, by = c("subspecies","country"))

    # Replace NA values for numeric metrics with 0
    phase_summary <- phase_summary %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::matches("n_obs_|n_stations_"),
          ~ dplyr::coalesce(., 0)
        )
      )
  }

  # ---------------------------------------------------------------
  # Final ordering
  # ---------------------------------------------------------------
  phase_summary %>% dplyr::arrange(subspecies, country)
}

#' @keywords internal
#' @noRd
#'
#' @title Internal: Plot Subspecies × Country × Phase Presence Heatmap
#'
#' @description
#' Generates a ggplot2 heatmap showing presence/absence of BBCH phase
#' observations per subspecies and country.
#'
#' @inheritParams subspecies_report
#' @import dplyr
#' @import tidyr
#' @import ggplot2
plot_subspecies_missingness_heatmap <- function(
    pep,
    species_name = NULL,
    subspecies_name = NULL,
    genus_name = NULL,
    phases = c(65,87),
    subregions = c("Austria","Germany-North","Germany-South","Switzerland"),
    years = 1961:2024,
    bbch_names = NULL
) {

  # Filter
  pep2 <- pep %>%
    dplyr::filter(
      year %in% years,
      country %in% subregions,
      phase_id %in% phases
    )

  # Apply taxonomy filters
  if (!is.null(subspecies_name)) {
    pep2 <- pep2 %>% dplyr::filter(!is.na(subspecies) & subspecies %in% subspecies_name)

  } else if (!is.null(species_name)) {
    pep2 <- pep2 %>% dplyr::filter(
      species == species_name |
        (!is.na(subspecies) & grepl(paste0("^", species_name), subspecies))
    )

  } else if (!is.null(genus_name)) {
    pep2 <- pep2 %>% dplyr::filter(genus == genus_name)
  }

  # CRITICAL FIX: remove unused subspecies levels
  pep2 <- pep2 %>% dplyr::mutate(subspecies = droplevels(subspecies))

  # Summarize presence/absence
  heat <- pep2 %>%
    dplyr::group_by(subspecies, country, phase_id) %>%
    dplyr::summarise(
      present = as.integer(dplyr::n_distinct(year) > 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      present = factor(present, levels = c(0,1))
    )

  # Drop unused levels again after summarise
  heat <- heat %>% dplyr::mutate(subspecies = droplevels(subspecies))

  # Optional BBCH labels
  if (!is.null(bbch_names)) {
    heat <- heat %>%
      dplyr::mutate(phase_label = dplyr::recode(phase_id, !!!bbch_names))
  } else {
    heat <- heat %>% dplyr::mutate(phase_label = phase_id)
  }

  # Plot
  ggplot2::ggplot(
    heat,
    ggplot2::aes(x = country, y = subspecies, fill = present)
  ) +
    ggplot2::geom_tile(color = "grey30") +
    ggplot2::scale_fill_manual(
      values = c("0" = "white", "1" = "steelblue"),
      breaks = c("0","1"),
      labels = c("Absent","Present"),
      name = "Data"
    ) +
    ggplot2::facet_wrap(~ phase_label, nrow = 1) +
    ggplot2::theme_minimal(14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Subspecies x Country x BBCH Phase: Data Availability",
      x = "",
      y = ""
    )
}

#' @keywords internal
#' @noRd
#'
#' @title Internal: Format Subspecies Summary as Wide Table
#'
#' @description
#' Converts the output from summarize_subspecies_availability()
#' into a wide publication-ready table.
#'
#' @inheritParams subspecies_report
#' @import dplyr
#' @import tidyr
format_subspecies_wide_table <- function(
    df,
    phases = c(65,87),
    subregions = NULL,
    metric = "all",
    bbch_names = NULL  # optional: list mapping phase -> name
) {

  # If user provided BBCH names
  if (!is.null(bbch_names)) {
    names(bbch_names) <- phases
    rename_fun <- function(x) {
      for (ph in names(bbch_names)) {
        x <- gsub(paste0("_", ph), paste0("_", bbch_names[[ph]]), x)
      }
      x
    }
    names(df) <- rename_fun(names(df))
  }

  # If subregions not given, derive from the data
  if (is.null(subregions))
    subregions <- sort(unique(df$country))

  # reshape
  wide <- df %>%
    tidyr::pivot_longer(
      cols = -c(subspecies, country),
      names_to = "metric",
      values_to = "value"
    ) %>%
    tidyr::unite("colname", country, metric, remove = TRUE) %>%
    tidyr::pivot_wider(
      names_from = colname,
      values_from = value
    ) %>%
    dplyr::arrange(subspecies)

  return(wide)
}


