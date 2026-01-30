#' Plot Hail-Phenology Overlap for One or Multiple Countries and Scenarios
#'
#' This function visualizes the temporal overlap between daily hail probability
#' and the phenological window (between two BBCH phenophases) for a selected
#' country or for all available countries. Phenological day-of-year (DOY)
#' values for CTRL and SCEN scenarios are computed internally using
#' \code{get_phenology_doys()}, ensuring full consistency with
#' \code{integral_hail()}.
#'
#' The function supports multiple visualization modes:
#' \itemize{
#'   \item Plot a single scenario (\code{"ctrl"} or \code{"scen"}) for one country.
#'   \item Plot both scenarios (\code{"both"}) for one country in two facets.
#'   \item Facet over multiple countries for a single scenario.
#'   \item Facet over countries and scenarios in a \code{Country × Scenario} grid.
#'   \item \strong{NEW:} Overlay CTRL and SCEN curves for all countries
#'         (\code{overlay_both = TRUE}), producing one panel per country
#'         with both scenarios overlaid—including their phenological windows.
#' }
#'
#'
#' @param hail A hail dataset with columns \code{Country}, \code{Scenario},
#'   \code{DOY}, and \code{mean}. Must include both CTRL and SCEN where
#'   applicable.
#' @param pep A PEP phenology dataset from which phenological DOYs are estimated.
#' @param giss Data frame with GISS temperature data. Required for robust_shift method.
#' @param Country Character. Country to plot when \code{facet_countries = FALSE}.
#' @param regions Character vector. Countries/regions to include. If NULL, all are used.
#'   Ignored when \code{facet_countries = TRUE}.
#' @param phase1,phase2 Integer. BBCH phenophases defining the phenology window.
#' @param genus_name Character. Genus to filter from \code{pep}
#'   (default \code{"Malus"}).
#'
#' @param phenology_method Character. One of:
#'   \itemize{
#'     \item \code{"simple_shift"}: shifts phenology linearly by warming.
#'     \item \code{"gdd_cmip6"}: computes GDD-based phenology from CMIP6 temperatures.
#'   }
#'
#' @param scen One of \code{"ctrl"}, \code{"scen"}, or \code{"both"},
#'   controlling which scenario(s) to visualize.
#'
#' @param facet_countries Logical. If \code{FALSE}, only one country is plotted.
#'   If \code{TRUE}, panels for multiple countries are created.
#'   The behavior depends on \code{scen}:
#'   \itemize{
#'     \item \code{scen="ctrl"} or \code{"scen"}:
#'           facet panels for each country.
#'     \item \code{scen="both"}:
#'           either a Country × Scenario facet grid (default), or
#'           an overlay view per country if \code{overlay_both = TRUE}.
#'   }
#'
#' @param overlay_both Logical (default \code{FALSE}).
#'   When \code{facet_countries = TRUE} and \code{scen = "both"}, enables
#'   a new overlay mode:
#'   \itemize{
#'     \item One facet per country.
#'     \item CTRL and SCEN hail curves overlaid.
#'     \item Phenology windows for both scenarios drawn as semi-transparent bands.
#'   }
#'   This mode resembles a multi-country comparison of full CTRL-SCEN
#'   climatologies without splitting the panels by scenario.
#'
#'
#' @param show_integral Logical. If \code{TRUE}, the integrated hail exposure
#'   (computed by \code{integral_hail()}) is displayed in each panel.
#' @param integral_cex Numeric. Text size for integral labels (default is 3).
#'
#' @param scen_warming Numeric. Warming increment (°C) used in the
#'   simple-shift phenology model.
#' @param shift_per_deg Numeric. Linear shift in DOY per °C warming.
#'
#' @param cmip6_tas Optional named list of daily temperature vectors (per country).
#'   Required when using \code{phenology_method = "gdd_cmip6"}.
#'
#' @param gdd_base Numeric. Base temperature for GDD accumulation.
#' @param gdd_threshold1,gdd_threshold2 Numeric. GDD values defining the
#'   onset and end of the phenological window in the GDD method.
#'
#' @param color_ctrl Color for CTRL curves and shading.
#' @param color_scen Color for SCEN curves and shading.
#' @param title Optional custom plot title. Auto-generated titles are used
#'   in facet or overlay modes.
#'
#' @return A \code{ggplot} object visualizing hail frequency curves together
#' with phenology windows. The structure of the plot (single panel,
#' facets, or overlay panels) is determined by the combination of arguments
#' \code{scen}, \code{facet_countries}, and \code{overlay_both}.
#'
#'
#' @examples
#' \dontrun{
#' data(hail)
#' pep <- pep_download()
#' data(giss)
#'
#' # --- Single country -------------------------------------------------------
#'
#' # CTRL only
#' plot_hail_overlap(hail, pep, giss, Country = "Austria", scen = "ctrl")
#'
#' # SCEN only
#' plot_hail_overlap(hail, pep, giss, Country = "Austria", scen = "scen")
#'
#' # CTRL + SCEN in separate facets for one country
#' plot_hail_overlap(hail, pep, giss, Country = "Austria", scen = "both")
#'
#'
#' # --- Faceting by country --------------------------------------------------
#'
#' # All countries, CTRL only
#' plot_hail_overlap(hail, pep, giss, Country = "Austria",
#'                   scen = "ctrl", facet_countries = TRUE)
#'
#' # All countries, SCEN only
#' plot_hail_overlap(hail, pep, giss, Country = "Austria",
#'                   scen = "scen", facet_countries = TRUE)
#'
#'
#' # --- Both scenarios for all countries ------------------------------------
#'
#' # Country x Scenario facet grid (default)
#' plot_hail_overlap(hail, pep, Country = "Austria",
#'                   scen = "both", facet_countries = TRUE)
#'
#' # Overlay CTRL + SCEN inside each country panel
#' plot_hail_overlap(hail, pep, giss, Country = "Austria",
#'                   scen = "both", facet_countries = TRUE,
#'                   overlay_both = TRUE)
#' }
#'
#' @author Matthias Templ
#' @export

plot_hail_overlap <- function(
    hail,
    pep,
    giss,
    Country,
    regions = NULL,
    phase1 = 65,
    phase2 = 87,
    genus_name = "Malus",
    phenology_method = c("robust_shift", "simple_shift", "gdd_cmip6"),
    scen = c("ctrl", "scen", "both"),
    facet_countries = FALSE,
    overlay_both = FALSE,
    show_integral = TRUE,
    integral_cex = 3,
    scen_warming = 3,
    shift_per_deg = -4,
    cmip6_tas = NULL,
    gdd_base = 5,
    gdd_threshold1 = 150,
    gdd_threshold2 = 350,
    color_ctrl = "dodgerblue3",
    color_scen = "tomato3",
    title = NULL
) {

  scen <- match.arg(scen)
  phenology_method <- match.arg(phenology_method)

  # ------------------------------------------------------------
  # 0) Optional region filter
  # ------------------------------------------------------------
  if (!is.null(regions)) {
    hail <- dplyr::filter(hail, Country %in% regions)

    if ("country" %in% names(pep)) {
      pep <- dplyr::filter(pep, country %in% regions)
    } else if ("Country" %in% names(pep)) {
      pep <- dplyr::filter(pep, Country %in% regions)
    }

    if (is.null(Country)) {
      Country <- regions[1]
    }
  }

  # ------------------------------------------------------------
  # 1) Phenology DOYs for all countries
  # ------------------------------------------------------------

  phen_all <- get_phenology_doys(
    pep = pep,
    phase1 = phase1, phase2 = phase2,
    genus_name = genus_name,
    phenology_method = phenology_method,
    scen_warming = scen_warming,
    shift_per_deg = shift_per_deg,
    cmip6_tas = cmip6_tas,
    gdd_base = gdd_base,
    gdd_threshold1 = gdd_threshold1,
    gdd_threshold2 = gdd_threshold2,
    giss_data = giss
  )

  # ------------------------------------------------------------
  # 2) Restrict to countries present in BOTH hail + phenology
  # ------------------------------------------------------------
  valid_countries <- intersect(unique(hail$Country), unique(phen_all$Country))

  if (length(valid_countries) == 0)
    stop("No overlapping countries found in hail and phenology datasets.")

  hail     <- dplyr::filter(hail, Country %in% valid_countries)
  phen_all <- dplyr::filter(phen_all, Country %in% valid_countries)

  # ------------------------------------------------------------
  # 3) Integrals (CTRL + SCEN per country)
  # ------------------------------------------------------------
  integrals <- integral_hail(
    hail = hail,
    pep = pep,
    giss = giss,
    phase1 = phase1,
    phase2 = phase2,
    genus_name = genus_name,
    scen_warming = scen_warming,
    shift_per_deg = shift_per_deg,
    phenology_method = phenology_method,
    cmip6_tas = cmip6_tas,
    gdd_base = gdd_base,
    gdd_threshold1 = gdd_threshold1,
    gdd_threshold2 = gdd_threshold2
  ) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      s_ctrl = mean(s_ctrl, na.rm = TRUE),
      s_scen = mean(s_scen, na.rm = TRUE),
      .groups = "drop"
    )

  # ------------------------------------------------------------
  # Helper to attach DOY ranges + inside flag
  # ------------------------------------------------------------
  attach_doy_ranges <- function(df) {
    df %>%
      dplyr::left_join(phen_all, by = "Country") %>%
      dplyr::mutate(
        doy_start = dplyr::if_else(Scenario == "ctrl", doy_start_ctrl, doy_start_scen),
        doy_end   = dplyr::if_else(Scenario == "ctrl", doy_end_ctrl,   doy_end_scen),
        inside    = DOY >= doy_start & DOY <= doy_end
      )
  }

  # ------------------------------------------------------------
  # Helper: compute text placement (x_mid, y_mid)
  # ------------------------------------------------------------
  compute_label_position <- function(df) {
    d1 <- unique(df$doy_start)[1]
    d2 <- unique(df$doy_end)[1]
    x_mid <- (d1 + d2) / 2

    ymax_win <- suppressWarnings(max(df$mean[df$inside], na.rm = TRUE))
    if (!is.finite(ymax_win))
      ymax_win <- suppressWarnings(max(df$mean, na.rm = TRUE))
    if (!is.finite(ymax_win))
      ymax_win <- 0

    y_mid <- ymax_win * 0.8
    c(x_mid = x_mid, y_mid = y_mid)
  }

  # ============================================================
  # 4) CASE A – Single country (facet_countries = FALSE)
  # ============================================================
  if (!facet_countries) {

    phen <- dplyr::filter(phen_all, Country == !!Country)

    # ---- A1) One scenario (CTRL or SCEN) ----------------------
    if (scen != "both") {

      df <- hail %>%
        dplyr::filter(Country == !!Country, Scenario == scen) %>%
        attach_doy_ranges()

      if (nrow(df) == 0)
        stop("No hail data for selected country/scenario.")

      col <- if (scen == "ctrl") color_ctrl else color_scen
      scen_label <- paste(Country, toupper(scen))
      integral_value <- if (scen == "ctrl") {
        integrals$s_ctrl[integrals$Country == Country]
      } else {
        integrals$s_scen[integrals$Country == Country]
      }

      # compute label placement
      pos <- compute_label_position(df)
      label_df <- data.frame(x = pos["x_mid"], y = pos["y_mid"],
                             Integral = integral_value)

      p <- ggplot2::ggplot(df, ggplot2::aes(DOY, mean)) +
        ggplot2::geom_line(color = col, linewidth = 1.2) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = 0, ymax = ifelse(inside, mean, NA_real_)),
          fill = col, alpha = 0.18
        ) +
        ggplot2::geom_vline(xintercept = c(pos["x_mid"]*2 - unique(df$doy_end)[1],
                                           unique(df$doy_end)[1]),
                            linetype = 2) +
        ggplot2::theme_minimal(14) +
        ggplot2::labs(
          title = if (is.null(title)) scen_label else title,
          x = "DOY", y = "Hail Probability"
        )

      if (show_integral) {
        p <- p + ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = x, y = y,
                       label = sprintf("Integral = %.3f", Integral)),
          size = integral_cex,
          inherit.aes = FALSE
        )
      }

      return(p)
    }


    # ---- A2) BOTH scenarios (CTRL + SCEN) ---------------------
    df_ctrl <- hail %>%
      dplyr::filter(Country == !!Country, Scenario == "ctrl") %>%
      attach_doy_ranges() %>%
      dplyr::mutate(Scenario = "CTRL",
                    Integral = integrals$s_ctrl[integrals$Country == Country])

    df_scen <- hail %>%
      dplyr::filter(Country == !!Country, Scenario == "scen") %>%
      attach_doy_ranges() %>%
      dplyr::mutate(Scenario = "SCEN",
                    Integral = integrals$s_scen[integrals$Country == Country])

    df_both <- dplyr::bind_rows(df_ctrl, df_scen)

    # compute label position per panel
    pos_df <- df_both %>%
      dplyr::group_by(Scenario) %>%
      dplyr::summarise(
        x = compute_label_position(cur_data())["x_mid"],
        y = compute_label_position(cur_data())["y_mid"],
        Integral = unique(Integral),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(df_both, ggplot2::aes(DOY, mean)) +
      ggplot2::geom_line(ggplot2::aes(color = Scenario), linewidth = 1.2) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = 0,
          ymax = ifelse(inside, mean, NA_real_),
          fill = Scenario
        ),
        alpha = 0.20
      ) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
      ggplot2::scale_color_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
      ggplot2::scale_fill_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
      ggplot2::facet_wrap(~Scenario, ncol = 2) +
      ggplot2::theme_minimal(14) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(
        title = if (is.null(title))
          paste("Hail-Phenology Overlap:", Country) else title,
        x = "DOY", y = "Hail Probability"
      )

    if (show_integral) {
      p <- p +
        ggplot2::geom_text(
          data = pos_df,
          ggplot2::aes(
            x = x, y = y,
            label = sprintf("Integral = %.3f", Integral)
          ),
          size = integral_cex,
          inherit.aes = FALSE
        )
    }

    return(p)
  }


  # ============================================================
  # 5) CASE B – facet over countries (facet_countries = TRUE)
  # ============================================================

  hail_join <- hail %>%
    dplyr::inner_join(phen_all, by = "Country")

  # ------------------------------------------------------------
  # B1) ONE scenario, facet over countries
  # ------------------------------------------------------------
  if (scen != "both") {

    hail_sub <- hail_join %>%
      dplyr::filter(Scenario == scen) %>%
      dplyr::mutate(
        doy_start = dplyr::if_else(Scenario == "ctrl", doy_start_ctrl, doy_start_scen),
        doy_end   = dplyr::if_else(Scenario == "ctrl", doy_end_ctrl,   doy_end_scen),
        inside    = DOY >= doy_start & DOY <= doy_end
      ) %>%
      dplyr::left_join(
        integrals %>%
          dplyr::mutate(
            Integral = if (scen == "ctrl") s_ctrl else s_scen
          ) %>%
          dplyr::select(Country, Integral),
        by = "Country"
      )

    col_curve <- if (scen == "ctrl") color_ctrl else color_scen

    # compute per-country positions
    pos_df <- hail_sub %>%
      dplyr::group_by(Country) %>%
      dplyr::summarise(
        x = compute_label_position(cur_data())["x_mid"],
        y = compute_label_position(cur_data())["y_mid"],
        Integral = unique(Integral),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(hail_sub, ggplot2::aes(DOY, mean)) +
      ggplot2::geom_line(color = col_curve, linewidth = 1.1) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = 0, ymax = ifelse(inside, mean, NA_real_)),
        fill = col_curve, alpha = 0.20
      ) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
      ggplot2::facet_wrap(~Country, ncol = 2) +
      ggplot2::theme_minimal(14) +
      ggplot2::labs(
        title = paste0("Hail-Phenology Overlap (", scen, ")"),
        x = "DOY", y = "Hail Probability"
      )

    if (show_integral) {
      p <- p +
        ggplot2::geom_text(
          data = pos_df,
          ggplot2::aes(
            x = x, y = y,
            label = sprintf("Integral = %.3f", Integral)
          ),
          size = integral_cex,
          inherit.aes = FALSE
        )
    }

    return(p)
  }


  # ------------------------------------------------------------
  # B2) BOTH scenarios, overlay mode (with integrals)
  # ------------------------------------------------------------
  if (overlay_both) {

    # Phenology windows per country+scenario
    phen_long <- phen_all %>%
      dplyr::select(Country,
                    doy_start_ctrl, doy_end_ctrl,
                    doy_start_scen, doy_end_scen) %>%
      tidyr::pivot_longer(
        cols = starts_with("doy_"),
        names_to = "which",
        values_to = "DOY"
      ) %>%
      dplyr::mutate(
        Scenario = dplyr::if_else(grepl("scen", which), "scen", "ctrl"),
        bound    = dplyr::if_else(grepl("start", which), "start", "end")
      ) %>%
      dplyr::select(-which) %>%
      tidyr::pivot_wider(
        names_from  = bound,
        values_from = DOY,
        names_prefix = "doy_"
      )

    hail_use <- hail %>%
      dplyr::filter(Country %in% valid_countries,
                    Scenario %in% c("ctrl", "scen"))

    # integrals in long form
    ints_long <- integrals %>%
      tidyr::pivot_longer(
        cols = c(s_ctrl, s_scen),
        names_to = "Scenario",
        values_to = "Integral"
      ) %>%
      dplyr::mutate(
        Scenario = dplyr::recode(Scenario,
                                 "s_ctrl" = "ctrl",
                                 "s_scen" = "scen"
        )
      )

    # compute position per Country + Scenario
    pos_overlay <- hail_use %>%
      dplyr::left_join(
        phen_all %>% dplyr::select(Country,
                                   doy_start_ctrl, doy_end_ctrl,
                                   doy_start_scen, doy_end_scen),
        by = "Country"
      ) %>%
      dplyr::mutate(
        doy_start = dplyr::if_else(Scenario == "ctrl", doy_start_ctrl, doy_start_scen),
        doy_end   = dplyr::if_else(Scenario == "ctrl", doy_end_ctrl,   doy_end_scen),
        inside    = DOY >= doy_start & DOY <= doy_end
      ) %>%
      dplyr::group_by(Country, Scenario) %>%
      dplyr::summarise(
        x = compute_label_position(cur_data())["x_mid"],
        y = compute_label_position(cur_data())["y_mid"],
        .groups = "drop"
      ) %>%
      dplyr::left_join(ints_long, by = c("Country","Scenario"))

    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = phen_long,
        ggplot2::aes(
          xmin = doy_start, xmax = doy_end,
          ymin = 0, ymax = Inf,
          fill = Scenario
        ),
        alpha = 0.08
      ) +
      ggplot2::geom_line(
        data = hail_use,
        ggplot2::aes(x = DOY, y = mean, colour = Scenario),
        linewidth = 0.6
      ) +
      ggplot2::scale_color_manual(
        values = c(ctrl = color_ctrl, scen = color_scen),
        name = "Scenario"
      ) +
      ggplot2::scale_fill_manual(
        values = c(ctrl = color_ctrl, scen = color_scen),
        name  = "Phenology window"
      ) +
      ggplot2::facet_wrap(~Country, ncol = 2) +
      ggplot2::coord_cartesian(xlim = c(60, 300)) +
      ggplot2::theme_minimal(14) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box      = "horizontal"
      ) +
      ggplot2::labs(
        title = "CTRL vs SCEN hail climatology with phenology window",
        x = "DOY", y = "Mean daily hail frequency"
      )

    if (show_integral) {
      p <- p +
        ggplot2::geom_text(
          data = pos_overlay,
          ggplot2::aes(
            x = x, y = y,
            label = sprintf("%s: %.3f", toupper(Scenario), Integral)
          ),
          size = integral_cex,
          inherit.aes = FALSE
        )
    }

    return(p)
  }


  # ------------------------------------------------------------
  # B3) BOTH scenarios – Country × Scenario facet grid
  # ------------------------------------------------------------
  hail_sub <- hail_join %>%
    dplyr::mutate(
      Scenario = dplyr::recode(Scenario,
                               "ctrl" = "CTRL",
                               "scen" = "SCEN"),
      doy_start = dplyr::if_else(Scenario == "CTRL", doy_start_ctrl, doy_start_scen),
      doy_end   = dplyr::if_else(Scenario == "CTRL", doy_end_ctrl,   doy_end_scen),
      inside    = DOY >= doy_start & DOY <= doy_end
    ) %>%
    dplyr::left_join(
      integrals %>%
        tidyr::pivot_longer(
          cols = c(s_ctrl, s_scen),
          names_to = "Scenario",
          values_to = "Integral"
        ) %>%
        dplyr::mutate(
          Scenario = dplyr::recode(Scenario,
                                   "s_ctrl" = "CTRL",
                                   "s_scen" = "SCEN")
        ),
      by = c("Country", "Scenario")
    )

  pos_grid <- hail_sub %>%
    dplyr::group_by(Country, Scenario) %>%
    dplyr::summarise(
      x = compute_label_position(cur_data())["x_mid"],
      y = compute_label_position(cur_data())["y_mid"],
      Integral = unique(Integral),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(hail_sub, ggplot2::aes(DOY, mean)) +
    ggplot2::geom_line(ggplot2::aes(color = Scenario), linewidth = 1.1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = 0,
        ymax = ifelse(inside, mean, NA_real_),
        fill = Scenario
      ),
      alpha = 0.20
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
    ggplot2::scale_color_manual(
      values = c("CTRL" = color_ctrl, "SCEN" = color_scen)
    ) +
    ggplot2::scale_fill_manual(
      values = c("CTRL" = color_ctrl, "SCEN" = color_scen)
    ) +
    ggplot2::facet_grid(Country ~ Scenario) +
    ggplot2::theme_minimal(14) +
    ggplot2::labs(
      title = "Hail-Phenology Overlap by Country and Scenario",
      x = "DOY", y = "Hail Probability"
    ) +
    ggplot2::theme(legend.position = "none")

  if (show_integral) {
    p <- p +
      ggplot2::geom_text(
        data = pos_grid,
        ggplot2::aes(
          x = x, y = y,
          label = sprintf("Integral = %.3f", Integral)
        ),
        size = integral_cex,
        inherit.aes = FALSE
      )
  }

  return(p)
}

# plot_hail_overlap <- function(
#     hail,
#     pep,
#     Country,
#     phase1 = 65,
#     phase2 = 87,
#     genus_name = "Malus",
#     phenology_method = "simple_shift",
#     scen = c("ctrl", "scen", "both"),
#     facet_countries = FALSE,
#     overlay_both = FALSE,
#     show_integral = TRUE,
#     scen_warming = 3,
#     shift_per_deg = -4,
#     cmip6_tas = NULL,
#     gdd_base = 5,
#     gdd_threshold1 = 150,
#     gdd_threshold2 = 350,
#     color_ctrl = "dodgerblue3",
#     color_scen = "tomato3",
#     title = NULL
# ) {
#
#   scen <- match.arg(scen)
#
#   # ================================================================
#   # 1) Phenology for all countries
#   # ================================================================
#   phen_all <- get_phenology_doys(
#     pep = pep,
#     phase1 = phase1, phase2 = phase2,
#     genus_name = genus_name,
#     phenology_method = phenology_method,
#     scen_warming = scen_warming,
#     shift_per_deg = shift_per_deg,
#     cmip6_tas = cmip6_tas,
#     gdd_base = gdd_base,
#     gdd_threshold1 = gdd_threshold1,
#     gdd_threshold2 = gdd_threshold2
#   )
#
#   # ================================================================
#   # 2) Integrals (one value per country)
#   # ================================================================
#   integrals <- integral_hail(
#     hail = hail,
#     pep = pep,
#     phase1 = phase1,
#     phase2 = phase2,
#     genus_name = genus_name,
#     scen_warming = scen_warming,
#     shift_per_deg = shift_per_deg,
#     phenology_method = phenology_method,
#     cmip6_tas = cmip6_tas,
#     gdd_base = gdd_base,
#     gdd_threshold1 = gdd_threshold1,
#     gdd_threshold2 = gdd_threshold2
#   ) %>%
#     dplyr::group_by(Country) %>%
#     dplyr::summarize(
#       s_ctrl = mean(s_ctrl, na.rm = TRUE),
#       s_scen = mean(s_scen, na.rm = TRUE),
#       .groups = "drop"
#     )
#
#   # ================================================================
#   # 3) Merge hail with phenology
#   # ================================================================
#   hail_join <- hail %>%
#     dplyr::inner_join(phen_all, by = "Country")
#
#   # ================================================================
#   # Helper for scenario-specific DOY ranges
#   # ================================================================
#   attach_doy_ranges <- function(df) {
#     df %>%
#       dplyr::mutate(
#         ScenarioLabel = dplyr::recode(
#           Scenario,
#           "ctrl" = "CTRL",
#           "scen" = "SCEN"
#         ),
#         doy_start = ifelse(Scenario == "ctrl", doy_start_ctrl, doy_start_scen),
#         doy_end   = ifelse(Scenario == "ctrl", doy_end_ctrl,   doy_end_scen),
#         inside    = DOY >= doy_start & DOY <= doy_end
#       )
#   }
#
#   # ================================================================
#   # 4) Case A — Single country (facet_countries = FALSE)
#   # ================================================================
#   if (!facet_countries) {
#
#     phen <- phen_all %>% dplyr::filter(Country == !!Country)
#
#     # SINGLE SCENARIO -----------------------------------------------
#     if (scen != "both") {
#
#       df <- hail_join %>%
#         dplyr::filter(Country == !!Country, Scenario == scen) %>%
#         attach_doy_ranges()
#
#       integral_value <- if (scen == "ctrl") {
#         integrals$s_ctrl[integrals$Country == Country]
#       } else {
#         integrals$s_scen[integrals$Country == Country]
#       }
#
#       col <- if (scen == "ctrl") color_ctrl else color_scen
#       scen_label <- paste0(Country, " ", toupper(scen))
#
#       p <- ggplot2::ggplot(df, ggplot2::aes(DOY, mean)) +
#         ggplot2::geom_line(color = col, linewidth = 1.1) +
#         ggplot2::geom_ribbon(
#           ggplot2::aes(ymin = 0, ymax = ifelse(inside, mean, NA_real_)),
#           fill = col, alpha = 0.20
#         ) +
#         ggplot2::geom_vline(xintercept = c(df$doy_start[1], df$doy_end[1]),
#                             linetype = 2) +
#         ggplot2::labs(
#           title = scen_label,
#           x = "DOY", y = "Hail Probability"
#         ) +
#         ggplot2::theme_minimal(14)
#
#       if (show_integral) {
#         p <- p + ggplot2::annotate(
#           "text",
#           x = Inf, y = Inf,
#           label = sprintf("Integral = %.3f", integral_value),
#           hjust = 1.1, vjust = 1.5, size = 4
#         )
#       }
#
#       return(p)
#     }
#
#     # BOTH SCENARIOS -------------------------------------------------
#     df <- hail_join %>%
#       dplyr::filter(Country == !!Country) %>%
#       attach_doy_ranges() %>%
#       dplyr::left_join(
#         integrals %>%
#           tidyr::pivot_longer(
#             cols = c(s_ctrl, s_scen),
#             names_to = "ScenarioLabel2",
#             values_to = "Integral"
#           ) %>%
#           dplyr::mutate(
#             ScenarioLabel = dplyr::recode(
#               ScenarioLabel2,
#               "s_ctrl" = "CTRL",
#               "s_scen" = "SCEN"
#             )
#           ) %>%
#           dplyr::select(Country, ScenarioLabel, Integral),
#         by = c("Country", "ScenarioLabel")
#       )
#
#     p <- ggplot2::ggplot(df, ggplot2::aes(DOY, mean)) +
#       ggplot2::geom_line(ggplot2::aes(color = ScenarioLabel), linewidth = 1.1) +
#       ggplot2::geom_ribbon(
#         ggplot2::aes(
#           ymin = 0,
#           ymax = ifelse(inside, mean, NA_real_),
#           fill = ScenarioLabel
#         ),
#         alpha = 0.20
#       ) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#       ggplot2::facet_wrap(~ScenarioLabel, ncol = 2) +
#       ggplot2::scale_color_manual(values = c(CTRL = color_ctrl, SCEN = color_scen)) +
#       ggplot2::scale_fill_manual(values = c(CTRL = color_ctrl, SCEN = color_scen)) +
#       ggplot2::theme_minimal(14) +
#       ggplot2::labs(
#         title = paste("Hail-Phenology Overlap:", Country),
#         x = "DOY", y = "Hail Probability"
#       ) +
#       ggplot2::theme(legend.position = "none")
#
#     if (show_integral) {
#       p <- p + ggplot2::geom_text(
#         ggplot2::aes(
#           x = Inf, y = Inf,
#           label = sprintf("Integral = %.3f", Integral)
#         ),
#         hjust = 1.1, vjust = 1.5, size = 4
#       )
#     }
#
#     return(p)
#   }
#
#   # ================================================================
#   # 5) Case B — facet by countries, one scenario
#   # ================================================================
#   if (scen != "both") {
#
#     hail_sub <- hail_join %>%
#       dplyr::filter(Scenario == scen) %>%
#       attach_doy_ranges() %>%
#       dplyr::left_join(
#         integrals %>%
#           dplyr::mutate(
#             Integral = ifelse(scen == "ctrl", s_ctrl, s_scen)
#           ) %>%
#           dplyr::select(Country, Integral),
#         by = "Country"
#       )
#
#     col_curve <- if (scen == "ctrl") color_ctrl else color_scen
#
#     p <- ggplot2::ggplot(hail_sub, ggplot2::aes(DOY, mean)) +
#       ggplot2::geom_line(color = col_curve, linewidth = 1.1) +
#       ggplot2::geom_ribbon(
#         ggplot2::aes(
#           ymin = 0,
#           ymax = ifelse(inside, mean, NA_real_)
#         ),
#         fill = col_curve,
#         alpha = 0.20
#       ) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#       ggplot2::facet_wrap(~Country, ncol = 2) +
#       ggplot2::labs(
#         title = paste("Hail-Phenology Overlap (", scen, ")", sep = ""),
#         x = "DOY", y = "Hail Probability"
#       ) +
#       ggplot2::theme_minimal(14) +
#       ggplot2::theme(legend.position = "none")
#
#     return(p)
#   }
#
#   # ================================================================
#   # 6) Case C — BOTH scenarios, overlay OR facet grid
#   # ================================================================
#
#   hail_both <- hail_join %>%
#     attach_doy_ranges() %>%
#     dplyr::left_join(
#       integrals %>%
#         tidyr::pivot_longer(
#           cols = c(s_ctrl, s_scen),
#           names_to = "ScenarioLabel2",
#           values_to = "Integral"
#         ) %>%
#         dplyr::mutate(
#           ScenarioLabel = dplyr::recode(
#             ScenarioLabel2,
#             "s_ctrl" = "CTRL",
#             "s_scen" = "SCEN"
#           )
#         ) %>%
#         dplyr::select(Country, ScenarioLabel, Integral),
#       by = c("Country", "ScenarioLabel")
#     )
#
#   color_map <- c("CTRL" = color_ctrl, "SCEN" = color_scen)
#
#   # ------------------------------------------------------------
#   # NEW MODE: overlay for all countries × both scenarios
#   # ------------------------------------------------------------
#   if (overlay_both) {
#
#     # build window_long
#     window_long <- hail_both %>%
#       dplyr::distinct(
#         Country,
#         Scenario,
#         ScenarioLabel,
#         doy_start,
#         doy_end
#       )
#
#     p <- ggplot2::ggplot() +
#       ggplot2::geom_rect(
#         data = window_long,
#         ggplot2::aes(
#           xmin = doy_start, xmax = doy_end,
#           ymin = 0, ymax = Inf,
#           fill = Scenario
#         ),
#         alpha = 0.08
#       ) +
#       ggplot2::geom_line(
#         data = hail_both,
#         ggplot2::aes(DOY, mean, color = Scenario),
#         linewidth = 0.5
#       ) +
#       ggplot2::scale_color_manual(
#         values = c(ctrl = color_ctrl, scen = color_scen),
#         name = "Scenario"
#       ) +
#       ggplot2::scale_fill_manual(
#         values = c(ctrl = color_ctrl, scen = color_scen),
#         name = "Phenology Window"
#       ) +
#       ggplot2::facet_wrap(~Country, ncol = 2) +
#       ggplot2::coord_cartesian(xlim = c(60, 300)) +
#       ggplot2::labs(
#         title = "CTRL vs SCEN hail climatology with phenology window",
#         x = "Day of Year (DOY)",
#         y = "Mean daily hail frequency"
#       ) +
#       ggplot2::theme_minimal(14) +
#       ggplot2::theme(legend.position = "bottom")
#
#     return(p)
#   }
#
#   # ------------------------------------------------------------
#   # ORIGINAL facet grid version (Country × Scenario)
#   # ------------------------------------------------------------
#   p <- ggplot2::ggplot(hail_both, ggplot2::aes(DOY, mean)) +
#     ggplot2::geom_line(ggplot2::aes(color = ScenarioLabel), linewidth = 1.1) +
#     ggplot2::geom_ribbon(
#       ggplot2::aes(
#         ymin = 0,
#         ymax = ifelse(inside, mean, NA_real_),
#         fill = ScenarioLabel
#       ),
#       alpha = 0.20
#     ) +
#     ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#     ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#     ggplot2::scale_color_manual(values = color_map) +
#     ggplot2::scale_fill_manual(values = color_map) +
#     ggplot2::facet_grid(Country ~ ScenarioLabel) +
#     ggplot2::labs(
#       title = "Hail-Phenology Overlap by Country and Scenario",
#       x = "DOY", y = "Hail Probability"
#     ) +
#     ggplot2::theme_minimal(14) +
#     ggplot2::theme(legend.position = "none")
#
#   return(p)
# }

# plot_hail_overlap <- function(
#     hail,
#     pep,
#     Country,
#     phase1 = 65,
#     phase2 = 87,
#     genus_name = "Malus",
#     phenology_method = "simple_shift",
#     scen = c("ctrl", "scen", "both"),
#     facet_countries = FALSE,
#     show_integral = TRUE,
#     scen_warming = 3,
#     shift_per_deg = -4,
#     cmip6_tas = NULL,
#     gdd_base = 5,
#     gdd_threshold1 = 150,
#     gdd_threshold2 = 350,
#     color_ctrl = "dodgerblue",
#     color_scen = "tomato",
#     title = NULL
# ){
#
#   scen <- match.arg(scen)
#
#   # ------------------------------------------------------------------
#   # 1. Compute DOYs for ALL countries
#   # ------------------------------------------------------------------
#   phen_all <- get_phenology_doys(
#     pep = pep,
#     phase1 = phase1, phase2 = phase2,
#     genus_name = genus_name,
#     phenology_method = phenology_method,
#     scen_warming = scen_warming,
#     shift_per_deg = shift_per_deg,
#     cmip6_tas = cmip6_tas,
#     gdd_base = gdd_base,
#     gdd_threshold1 = gdd_threshold1,
#     gdd_threshold2 = gdd_threshold2
#   )
#
#   # ------------------------------------------------------------------
#   # 2. Compute integral hail exposure values (CTRL + SCEN)
#   # ------------------------------------------------------------------
#   integrals <- integral_hail(
#     hail = hail,
#     pep = pep,
#     phase1 = phase1,
#     phase2 = phase2,
#     genus_name = genus_name,
#     scen_warming = scen_warming,
#     shift_per_deg = shift_per_deg,
#     phenology_method = phenology_method,
#     cmip6_tas = cmip6_tas,
#     gdd_base = gdd_base,
#     gdd_threshold1 = gdd_threshold1,
#     gdd_threshold2 = gdd_threshold2
#   )
#
#   # ---- FIX: ensure one scalar value per country ----
#   integrals <- integrals %>%
#     dplyr::group_by(Country) %>%
#     dplyr::summarize(
#       s_ctrl = mean(s_ctrl, na.rm = TRUE),
#       s_scen = mean(s_scen, na.rm = TRUE),
#       .groups = "drop"
#     )
#
#   # ==================================================================
#   # == CASE A: Single country (facet_countries = FALSE)
#   # ==================================================================
#   if (!facet_countries) {
#
#     phen <- phen_all %>% dplyr::filter(Country == !!Country)
#
#     # ================================================================
#     # (1) SINGLE SCENARIO --------------------------------------------
#     # ================================================================
#     if (scen != "both") {
#
#       df <- hail %>%
#         dplyr::filter(Country == !!Country, Scenario == scen)
#
#       # DOYs and integral value
#       if (scen == "ctrl") {
#         d1 <- phen$doy_start_ctrl
#         d2 <- phen$doy_end_ctrl
#         col <- color_ctrl
#         scen_label <- paste(Country, "CTRL")
#         integral_value <- integrals$s_ctrl[integrals$Country == Country]
#       } else {
#         d1 <- phen$doy_start_scen
#         d2 <- phen$doy_end_scen
#         col <- color_scen
#         scen_label <- paste(Country, "SCEN")
#         integral_value <- integrals$s_scen[integrals$Country == Country]
#       }
#
#       df$inside <- df$DOY >= d1 & df$DOY <= d2
#
#       p <- ggplot2::ggplot(df, ggplot2::aes(DOY, mean)) +
#         ggplot2::geom_line(color = col, linewidth = 1.1) +
#         ggplot2::geom_ribbon(
#           ggplot2::aes(
#             ymin = 0,
#             ymax = ifelse(inside, mean, NA_real_)
#           ),
#           fill = col, alpha = 0.20
#         ) +
#         ggplot2::geom_vline(xintercept = c(d1, d2), linetype = 2) +
#         ggplot2::labs(
#           title = scen_label,
#           x = "DOY", y = "Hail Probability"
#         ) +
#         ggplot2::theme_minimal(14)
#
#       if (show_integral) {
#         p <- p + ggplot2::annotate(
#           "text",
#           x = Inf, y = Inf,
#           label = sprintf("Integral = %.3f", integral_value),
#           hjust = 1.1, vjust = 1.5, size = 4
#         )
#       }
#
#       return(p)
#     }
#
#     # ================================================================
#     # (2) TWO SCENARIOS (CTRL + SCEN) --------------------------------
#     # ================================================================
#     int_ctrl <- integrals$s_ctrl[integrals$Country == Country][1]
#     int_scen <- integrals$s_scen[integrals$Country == Country][1]
#
#     df_ctrl <- hail %>%
#       dplyr::filter(Country == !!Country, Scenario == "ctrl") %>%
#       dplyr::mutate(
#         doy_start = phen$doy_start_ctrl,
#         doy_end   = phen$doy_end_ctrl,
#         Scenario  = "CTRL",
#         Integral  = int_ctrl
#       )
#
#     df_scen <- hail %>%
#       dplyr::filter(Country == !!Country, Scenario == "scen") %>%
#       dplyr::mutate(
#         doy_start = phen$doy_start_scen,
#         doy_end   = phen$doy_end_scen,
#         Scenario  = "SCEN",
#         Integral  = int_scen
#       )
#
#     df_both <- dplyr::bind_rows(df_ctrl, df_scen) %>%
#       dplyr::mutate(
#         inside = DOY >= doy_start & DOY <= doy_end
#       )
#
#     p <- ggplot2::ggplot(df_both, ggplot2::aes(DOY, mean)) +
#       ggplot2::geom_line(ggplot2::aes(color = Scenario), linewidth = 1.1) +
#       ggplot2::geom_ribbon(
#         ggplot2::aes(
#           ymin = 0,
#           ymax = ifelse(inside, mean, NA_real_),
#           fill = Scenario
#         ),
#         alpha = 0.20
#       ) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#       ggplot2::scale_color_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
#       ggplot2::scale_fill_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
#       ggplot2::facet_wrap(~Scenario, ncol = 2, scales = "fixed") +
#       ggplot2::theme_minimal(14) +
#       ggplot2::labs(
#         title = paste("Hail-Phenology Overlap:", Country),
#         x = "DOY", y = "Hail Probability"
#       ) +
#       ggplot2::theme(legend.position = "none")
#
#     if (show_integral) {
#       p <- p + ggplot2::geom_text(
#         ggplot2::aes(
#           x = Inf, y = Inf,
#           label = sprintf("Integral = %.3f", Integral)
#         ),
#         hjust = 1.1, vjust = 1.5, size = 4
#       )
#     }
#
#     return(p)
#   }
#
#   # ==================================================================
#   # == CASE B: Facet by COUNTRY (facet_countries = TRUE)
#   # ==================================================================
#   hail_join <- hail %>% dplyr::inner_join(phen_all, by = "Country")
#
#   # ================================================================
#   # (1) ONE SCENARIO, facet by country
#   # ================================================================
#   if (scen != "both") {
#
#     hail_sub <- hail_join %>%
#       dplyr::filter(Scenario == scen) %>%
#       dplyr::mutate(
#         doy_start = ifelse(scen == "ctrl", doy_start_ctrl, doy_start_scen),
#         doy_end   = ifelse(scen == "ctrl", doy_end_ctrl,   doy_end_scen),
#         inside    = DOY >= doy_start & DOY <= doy_end
#       ) %>%
#       dplyr::left_join(
#         integrals %>%
#           dplyr::mutate(
#             Integral = dplyr::case_when(
#               scen == "ctrl" ~ s_ctrl,
#               scen == "scen" ~ s_scen
#             )
#           ) %>%
#           dplyr::select(Country, Integral),
#         by = "Country"
#       )
#
#     col_curve <- if (scen == "ctrl") color_ctrl else color_scen
#
#     p <- ggplot2::ggplot(hail_sub, ggplot2::aes(DOY, mean)) +
#       ggplot2::geom_line(color = col_curve, linewidth = 1.1) +
#       ggplot2::geom_ribbon(
#         ggplot2::aes(
#           ymin = 0,
#           ymax = ifelse(inside, mean, NA_real_)
#         ),
#         fill = col_curve,
#         alpha = 0.20
#       ) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#       ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#       ggplot2::facet_wrap(~Country, ncol = 2, scales = "fixed") +
#       ggplot2::theme_minimal(14) +
#       ggplot2::labs(
#         title = paste("Hail-Phenology Overlap (", scen, ")", sep = ""),
#         x = "DOY", y = "Hail Probability"
#       ) +
#       ggplot2::theme(legend.position = "none")
#
#     if (show_integral) {
#       p <- p + ggplot2::geom_text(
#         ggplot2::aes(
#           x = Inf, y = Inf,
#           label = sprintf("Integral = %.3f", Integral)
#         ),
#         hjust = 1.1, vjust = 1.5, size = 3
#       )
#     }
#
#     return(p)
#   }
#
#   # ================================================================
#   # (2) BOTH SCENARIOS, facet grid COUNTRY × SCENARIO
#   # ================================================================
#   hail_sub <- hail_join %>%
#     dplyr::mutate(
#       Scenario = dplyr::recode(Scenario,
#                                "ctrl" = "CTRL",
#                                "scen" = "SCEN"),
#       doy_start = ifelse(Scenario == "CTRL", doy_start_ctrl, doy_start_scen),
#       doy_end   = ifelse(Scenario == "CTRL", doy_end_ctrl,   doy_end_scen),
#       inside    = DOY >= doy_start & DOY <= doy_end
#     ) %>%
#     dplyr::left_join(
#       integrals %>%
#         tidyr::pivot_longer(
#           cols = c(s_ctrl, s_scen),
#           names_to = "Scenario",
#           values_to = "Integral"
#         ) %>%
#         dplyr::mutate(
#           Scenario = dplyr::recode(Scenario,
#                                    "s_ctrl" = "CTRL",
#                                    "s_scen" = "SCEN")
#         ),
#       by = c("Country", "Scenario")
#     )
#
#   p <- ggplot2::ggplot(hail_sub, ggplot2::aes(DOY, mean)) +
#     ggplot2::geom_line(ggplot2::aes(color = Scenario), linewidth = 1.1) +
#     ggplot2::geom_ribbon(
#       ggplot2::aes(
#         ymin = 0,
#         ymax = ifelse(inside, mean, NA_real_),
#         fill = Scenario
#       ),
#       alpha = 0.20
#     ) +
#     ggplot2::geom_vline(ggplot2::aes(xintercept = doy_start), linetype = 2) +
#     ggplot2::geom_vline(ggplot2::aes(xintercept = doy_end),   linetype = 2) +
#     ggplot2::scale_color_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
#     ggplot2::scale_fill_manual(values = c("CTRL" = color_ctrl, "SCEN" = color_scen)) +
#     ggplot2::facet_grid(Country ~ Scenario, scales = "fixed") +
#     ggplot2::theme_minimal(14) +
#     ggplot2::labs(
#       title = "Hail-Phenology Overlap by Country and Scenario",
#       x = "DOY", y = "Hail Probability"
#     ) +
#     ggplot2::theme(legend.position = "none")
#
#   if (show_integral) {
#     p <- p + ggplot2::geom_text(
#       ggplot2::aes(
#         x = Inf, y = Inf,
#         label = sprintf("Integral = %.3f", Integral)
#       ),
#       hjust = 1.1, vjust = 1.5, size = 3
#     )
#   }
#
#   return(p)
# }
