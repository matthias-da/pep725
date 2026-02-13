#' Plot Phenological Time Series for Heading and Harvest
#'
#' Visualizes phenological time series for heading and harvest phases,
#' comparing aggregated and spatially filtered PEP725 data.
#'
#' @param data_list A named list returned by \code{\link{regional_box_ts_heading_harvest}},
#'   containing \code{ts_tidy}.
#' @param phase_select Character. Optional filter for a specific phenological phase
#'   (e.g., "Heading", "Harvest"). If NULL, all phases are shown.
#' @param alpha_lines Numeric. Transparency level for lines (default is 0.6).
#' @param linewidth Numeric. Line width for time series plots (default is 0.7).
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' This function shows DOY trends by phase and data source across years,
#' with month labels on the y-axis for easier interpretation.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_grid labs theme_bw theme scale_linetype_manual scale_y_continuous
#'
#' @seealso \code{\link{regional_box_ts_heading_harvest}}
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' out <- regional_box_ts_heading_harvest(pep,
#'          species_name = "Triticum aestivum")
#' pheno_plot_hh(out)
#' }
#' @author Matthias Templ
#' @export
pheno_plot_hh <- function(
    data_list,
    phase_select = NULL,
    alpha_lines = 0.6,
    linewidth = 0.7
) {
  # Month scale helpers
  DOY1st  <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  DOYmid  <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  LABELS  <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  month_scale <- scale_y_continuous(
    limits = c(120, 260),
    breaks = DOYmid, labels = LABELS,
    minor_breaks = DOY1st
  )

  ts_tidy <- data_list$ts_tidy

  if (is.null(ts_tidy)) {
    stop("data_list must contain 'ts_tidy'. Use regional_box_ts_heading_harvest() to prepare data.")
  }

  # Filter by phase if specified
  if (!is.null(phase_select)) {
    ts_tidy <- ts_tidy[phase == phase_select]
  }

  p <- ggplot(ts_tidy, aes(year, DOY, color = source)) +
    geom_line(aes(linetype = source), linewidth = linewidth, alpha = alpha_lines, na.rm = TRUE) +
    facet_grid(phase ~ panel, scales = "free_y") +
    month_scale +
    labs(x = "", y = NULL, title = "Heading/Harvest phenology") +
    theme_bw() +
    theme(legend.position = "top")

  return(p)
}
