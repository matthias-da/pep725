#' Plot Phenological Time Series
#'
#' Visualizes phenological day-of-year (DOY) trends over time since 1961.
#' Designed for aggregated or site-level phenology data from PEP725.
#'
#' @param data A data.frame or data.table containing at least `year`, `DOY`, and one grouping variable
#'             (e.g. `species`, `phase`, or `functional_group`).
#' @param color_by Character. Column name used for color grouping (default = "species").
#' @param facet_by Character or NULL. Optional column name for faceting (e.g. "functional_group").
#' @param alpha_lines Numeric. Transparency level for time-series lines (default = 0.7).
#' @param linewidth Numeric. Line width (default = 0.8).
#' @param smooth Logical. If TRUE, adds a linear trend smoother (default = TRUE).
#' @param se Logical. Whether to display confidence interval for smoother (default = FALSE).
#' @param year_min Numeric. Minimum year to plot (default = 1961).
#' @param title Character. Optional plot title.
#'
#' @return A ggplot object.
#' @author Matthias Templ
#' @export
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' # Example: aggregated flowering DOY by species
#' pheno_plot_timeseries(data = pep, color_by = "functional_group",
#'                       facet_by = NULL, smooth = TRUE)
#'
#' # Example 1: all species, colored by functional group
#' pheno_plot_timeseries(
#'   data = pep[phase_id == 65],
#'   color_by = "functional_group",
#'   smooth = TRUE,
#'   title = "Flowering (BBCH 65) time series since 1961"
#' )
#'
#' # Example 2: single species, colored by site
#' pheno_plot_timeseries(
#'   data = pep[species == "Triticum aestivum" & phase_id == 65],
#'   color_by = "s_id",
#'   smooth = TRUE,
#'   title = "Winter wheat flowering (BBCH 65) across sites"
#' )
#'
#' # Example 3: facets by functional group
#' pheno_plot_timeseries(
#'   data = pep[phase_id == 65],
#'   color_by = "species",
#'   facet_by = "functional_group",
#'   smooth = TRUE
#' )
#' }
pheno_plot_timeseries <- function(
    data,
    color_by = "species",
    facet_by = NULL,
    alpha_lines = 0.7,
    linewidth = 0.8,
    smooth = TRUE,
    se = FALSE,
    year_min = 1961,
    title = "Phenological time series of flowering (BBCH 65)"
) {
  # ensure data.table and filter by year
  setDT(data)
  data <- data[year >= year_min]

  # --- base plot ---
  p <- ggplot(data, aes(x = year, y = day, color = .data[[color_by]])) +
    geom_line(alpha = alpha_lines, linewidth = linewidth, na.rm = TRUE) +
    theme_bw(base_size = 11) +
    labs(
      x = "Year",
      y = "Day of Year (DOY)",
      color = gsub("_", " ", color_by),
      title = title
    ) +
    theme(legend.position = "top")

  # --- add smoother if requested ---
  if (smooth) {
    p <- p + geom_smooth(method = "lm", se = se, linetype = "solid", linewidth = 0.8)
  }

  # --- optional facetting ---
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y")
  }

  return(p)
}


