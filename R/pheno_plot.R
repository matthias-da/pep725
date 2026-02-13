#' Plot Phenological Time Series
#'
#' Generates time series plots from processed phenology data. Shows DOY trends
#' with faceting by phenophase and spatial scope.
#'
#' @param data_list A named list of prepared data objects, typically output from
#'   \code{\link{regional_box_ts}}. Must contain element \code{ts_tidy}.
#' @param alpha_lines Alpha transparency for time series lines (default is \code{0.6}).
#' @param linewidth Line width for time series lines (default is \code{0.7}).
#'
#' @return A \code{ggplot} object for visual inspection and further customization.
#'
#' @details
#' This function visualizes DOY trends with faceting by phenophase and spatial scope.
#'
#' @seealso \code{\link{regional_box_ts}}
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' # Requires GISS temperature anomaly data
#' # out <- regional_box_ts(pep, giss, species_name = "Triticum aestivum", phase = 10)
#' # pheno_plot(out)
#' }
#'
#' @import ggplot2
#' @author Matthias Templ
#' @export
pheno_plot <- function(
    data_list,
    alpha_lines = 0.6,
    linewidth = 0.7
) {
  ts_tidy <- data_list$ts_tidy

  if (is.null(ts_tidy)) {
    stop("data_list must contain 'ts_tidy'. Use regional_box_ts() to prepare data.")
  }

  p <- ggplot(ts_tidy, aes(year, DOY, color = source)) +
    geom_line(aes(linetype = source), linewidth = linewidth, alpha = alpha_lines, na.rm = TRUE) +
    facet_grid(phase ~ panel, scales = "free_y") +
    labs(x = "", y = NULL, title = "Phenology time series") +
    theme_bw() +
    theme(legend.position = "top") +
    scale_linetype_manual(values = c(
      "PEP725 (aggregated)"       = "solid",
      "PEP725 (near Changins)"    = "solid"
    ))

  return(p)
}
