#' Plot Phenological Time Series and Climate Sensitivity
#'
#' Visualizes phenological time series and their relationship to climate using data from \code{\link{regional_box_ts}}.
#' Supports three plot types: year-wise time series across data sources, smoothed temperature–phenology relationships,
#' and robust linear sensitivity to global temperature anomalies.
#'
#' @param data_list A named list returned by \code{\link{regional_box_ts}}, containing at minimum \code{ts_tidy} and \code{pep_giss}.
#' @param type Character. Type of plot to produce. One of \code{"timeseries"} (default), \code{"giss_smooth"}, or \code{"giss_sensitivity"}.
#' @param phase_select Character. Optional filter for a specific phenological phase (e.g., "Heading", "Harvest"). If NULL, all phases are shown.
#' @param alpha_lines Numeric. Transparency level for lines (default is 0.6).
#' @param linewidth Numeric. Line width for time series plots (default is 0.7).
#' @param smooth Character. Smoother type for \code{"giss_smooth"} plots. Either \code{"gam"} (default) or \code{"loess"}.
#' @param span Numeric. Span parameter for LOESS smoothing (default is 0.3).
#' @param gam_bs Character. Basis type for GAM smoothing (default is \code{"tp"}). See \code{\link[mgcv]{s}}.
#' @param gam_k Integer (optional). Number of basis functions (\code{k}) for GAM smoothing. If \code{NULL}, defaults are used.
#' @param se Logical. Whether to display confidence intervals (shaded areas) around the smoother (default is \code{FALSE}).
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' \itemize{
#'   \item \code{type = "timeseries"}: Shows DOY trends by phase and data source across years.
#'   \item \code{type = "giss_smooth"}: Shows smoothed DOY vs. global temperature anomalies (\code{dTgl}) using either GAM or LOESS.
#'   \item \code{type = "giss_sensitivity"}: Fits robust linear models (via \code{robustbase::lmrob}) and visualizes phase-specific climate sensitivity.
#' }
#'
#' Global temperature data are taken from the \code{\link{giss}} dataset. Phenological data are drawn from the PEP725 database.
#' Aggregation and preparation is assumed to be done via \code{\link{regional_box_ts}}.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth facet_grid labs theme_bw theme scale_linetype_manual annotate
#' @importFrom mgcv gam s
#' @importFrom robustbase lmrob
#'
#' @seealso \code{\link{regional_box_ts}}, \code{\link[ggplot2]{ggplot}}, \code{\link[mgcv]{gam}}, \code{\link[robustbase]{lmrob}}
#'
#' @examples
#' \dontrun{
#' # Requires pep_download() data - see regional_box_ts_heading_harvest()
#' pep <- pep_download()
#' data(giss)
#' data(meteoSwiss)
#'
#' out <- regional_box_ts_heading_harvest(pep, giss, meteoSwiss,
#'                                        species_name = "Triticum aestivum")
#' str(out$ts_tidy)
#' gt <- pheno_plot_hh(out, type = "timeseries")
#' gt
#' pheno_plot_hh(out, type = "giss_smooth", smooth = "loess", se = TRUE)
#' gc <- pheno_plot_hh(out, type = "giss_sensitivity")
#' gt | gc
#'
#' # Select only nearby stations and recent years:
#' d <- regional_box_ts_heading_harvest(pep, giss, meteoSwiss, pep_for_giss = "near",
#'                         lon_min = 4.2,  lon_max = 8.1,
#'                         lat_min = 44.7, lat_max = 48.1,
#'                         year_min = 1980)
#' pheno_plot_hh(d, type = "timeseries", alpha_lines = 0.6, linewidth = 0.7)
#' }
#' @author Matthias Templ
#' @export
pheno_plot_hh <- function(
    data_list,
    type = c("timeseries", "giss_smooth", "giss_sensitivity"),
    phase_select = NULL,              # NEW: filter for a specific phase
    alpha_lines = 0.6,
    linewidth = 0.7,
    smooth = c("gam", "loess"),       # method for smoothing
    span = 0.3,                       # loess tuning
    gam_bs = "tp",                    # GAM basis
    gam_k = NULL,                     # optional GAM k
    se = FALSE                        # show confidence interval
) {
  type <- match.arg(type)
  smooth <- match.arg(smooth)

  # ── Helpers ────────────────────────────────────────────────────────────────────
  DOY1st  <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  DOYmid  <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  LABELS  <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  month_scale <- scale_y_continuous(
    limits = c(120, 260),
    breaks = DOYmid, labels = LABELS,
    minor_breaks = DOY1st
  )


  # ── Timeseries Plot ──────────────────────────────────────────────────────
  if (type == "timeseries") {
    ts_tidy <- data_list$ts_tidy

    p <- ggplot(ts_tidy, aes(year, DOY, color = source)) +
      geom_line(aes(linetype = source), linewidth = linewidth, alpha = alpha_lines, na.rm = TRUE) +
      facet_grid(phase ~ panel, scales = "free_y") +
      month_scale +
      labs(x = "", y = NULL, title = "Winter wheat phenology at/near Changins vs PEP725") +
      theme_bw() +
      theme(legend.position = "top") +
      scale_linetype_manual(values = c(
        "Obs. Changins (MeteoCH)"   = "dashed",
        "PEP725 (aggregated)"       = "solid",
        "PEP725 (near Changins)"    = "solid"
      ))
    return(p)
  }

  # ── GISS Smooth Plot ─────────────────────────────────────────────────────
  if (type == "giss_smooth") {
    pep_giss <- data_list$pep_giss
    if (!is.null(phase_select)) {
      pep_giss <- pep_giss[phase == phase_select]
    }

    # choose smoother
    layer_smooth <- switch(
      smooth,
      gam = {
        gam_formula <- if (is.null(gam_k)) {
          as.formula(sprintf("y ~ s(x, bs = '%s')", gam_bs))
        } else {
          as.formula(sprintf("y ~ s(x, bs = '%s', k = %d)", gam_bs, gam_k))
        }
        geom_smooth(method = mgcv::gam, formula = gam_formula, se = se)
      },
      loess = {
        geom_smooth(method = "loess", span = span, se = se)
      }
    )

    p <- ggplot(pep_giss, aes(dTgl, DOY, color = phase)) +
      geom_point(alpha = 0.8) +
      layer_smooth +
      month_scale +
      labs(
        x = expression(paste(Delta, "T"[gl], " [", degree, "C]")),
        y = NULL,
        title = sprintf("PEP725 (aggregated): %s vs global temperature (GISS)",
                        ifelse(is.null(phase_select), "Phenology", phase_select))
      ) +
      theme_bw() +
      theme(legend.position = "top")

    return(p)
  }

  # ── GISS Sensitivity Plot ────────────────────────────────────────────────
  if (type == "giss_sensitivity") {
    pep_giss <- data_list$pep_giss
    if (!is.null(phase_select)) {
      pep_giss <- pep_giss[phase == phase_select]
    }

    # robust slopes (days / degC)
    slopes <- pep_giss[, {
      fit <- robustbase::lmrob(DOY ~ dTgl)
      .(slope = unname(coef(fit)[2]))
    }, by = phase]

    lbls <- slopes[, sprintf("%s: robust sensitivity = %.1f days/degC", phase, slope)]

    p <- ggplot(pep_giss, aes(dTgl, DOY, color = phase)) +
      geom_point(alpha = 0.8) +
      geom_smooth(method = "lmrob", se = FALSE) +
      month_scale +
      labs(
        x = expression(paste(Delta, "T"[gl], " [", degree, "C]")),
        y = NULL,
        title = sprintf("Sensitivity of %s to global temperature",
                        ifelse(is.null(phase_select), "phenophases", phase_select))
      ) +
      theme_bw() +
      theme(legend.position = "top")

    if (is.null(phase_select)) {
      # Multi-phase: automatic label positioning
      y_range <- range(pep_giss$DOY, na.rm = TRUE)
      y_pos <- c(
        y_range[1] + 0.35 * diff(y_range),
        y_range[1] + 0.80 * diff(y_range)
      )
      p <- p + annotate("text",
                        x = max(pep_giss$dTgl, na.rm = TRUE),
                        y = y_pos, hjust = 1, size = 3.8,
                        label = lbls[match(c("Heading", "Harvest"), slopes$phase)])
    } else {
      # Single phase: add one label
      y_loc <- median(pep_giss$DOY, na.rm = TRUE)
      x_loc <- max(pep_giss$dTgl, na.rm = TRUE)
      p <- p + annotate("text",
                        x = x_loc,
                        y = y_loc,
                        hjust = 1, size = 4.2,
                        label = lbls)
    }

    return(p)
  }
}
