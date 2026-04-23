utils::globalVariables(c("lon", "lat", "s_id", "year", "phase_id", "day",
                         "is_outlier", "deviation", "n_obs", "n_outliers",
                         "max_abs_dev", "mean_abs_dev", "popup_html"))

#' Interactive Leaflet Map of Outlier-Flagged Stations
#'
#' Renders a Leaflet map of the stations in a \code{pep_outliers} object,
#' with each station plotted as a circle marker whose size encodes the
#' number of flagged observations and whose colour encodes the maximum
#' absolute residual (or Mahalanobis distance) for the station. Popups
#' list the worst-offending station-years and their residual values —
#' useful for interactive inspection during QC.
#'
#' Unlike \code{\link{pheno_leaflet}} (which is a selection gadget built
#' on Shiny + miniUI), this is a pure visualisation: it returns a
#' \code{leaflet} htmlwidget so you can embed it in an R Markdown
#' report, include it in a vignette, or call \code{print()} on it for
#' interactive exploration.
#'
#' @param x A \code{pep_outliers} object from \code{\link{pep_flag_outliers}}.
#'   Must contain \code{lon}, \code{lat}, \code{s_id}, \code{year},
#'   \code{phase_id}, \code{day}, \code{is_outlier}, and \code{deviation}
#'   columns.
#' @param outlier_only Logical. If \code{TRUE} (default), only stations
#'   with at least one flagged observation are shown. If \code{FALSE},
#'   every station appears (non-flagged stations are drawn small and
#'   grey for context).
#' @param top_n Integer. Maximum number of station-year records included
#'   in a station's popup. Default 10.
#' @param radius_range Numeric length-2. Minimum and maximum circle
#'   marker radius (in pixels) mapped to log1p(n_outliers). Default
#'   \code{c(4, 14)}.
#' @param palette Character. Name of a \code{RColorBrewer} or viridis
#'   palette for the colour scale. Default \code{"plasma"}.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @return A \code{leaflet} htmlwidget. Call \code{print()} to display
#'   in the RStudio Viewer or embed it in an Rmd file.
#'
#' @details
#' **Colour** encodes the station's maximum absolute residual
#' (for univariate methods: \code{max(|deviation|)} in days; for
#' \code{method = "mahalanobis"}: the maximum robust Mahalanobis
#' distance). **Size** encodes \code{log1p(n_outliers)} — a station
#' flagged once has a small marker, a station flagged 30 times has a
#' much larger one.
#'
#' For \code{method = "gam_residual"} and \code{"mahalanobis"} this is
#' the natural companion to the Schaber-Badeck-style text tables and
#' the static diagnostic figure, because spatial structure in outliers
#' (a local observer network, a regional climate anomaly) is often
#' only visible on a map.
#'
#' @examples
#' \dontrun{
#' pep <- pep_download()
#' pep_ch <- pep[country == "Switzerland" & species == "Malus domestica"]
#' flagged <- pep_flag_outliers(
#'   pep_ch,
#'   by = c("genus", "species", "phase_id"),
#'   method = "gam_residual"
#' )
#' pep_outliers_leaflet(flagged)
#' }
#'
#' @seealso \code{\link{pep_flag_outliers}} — detection,
#'   \code{\link{pep_plot_outliers}} — static diagnostic plots,
#'   \code{\link{pheno_leaflet}} — station-selection Shiny gadget.
#' @author Matthias Templ
#' @export
pep_outliers_leaflet <- function(x,
                                 outlier_only = TRUE,
                                 top_n = 10,
                                 radius_range = c(4, 14),
                                 palette = "plasma",
                                 ...) {
  if (!inherits(x, "pep_outliers")) {
    stop("'x' must be a pep_outliers object from pep_flag_outliers().",
         call. = FALSE)
  }
  required <- c("lon", "lat", "s_id", "is_outlier", "deviation")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("'x' is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for pep_outliers_leaflet(). ",
         "Install it with: install.packages('leaflet').",
         call. = FALSE)
  }

  method <- attr(x, "method")
  threshold <- attr(x, "threshold")

  dt <- data.table::as.data.table(x)
  # Station-level aggregates.
  st <- dt[, .(
    lon = lon[1],
    lat = lat[1],
    n_obs = .N,
    n_outliers = sum(is_outlier, na.rm = TRUE),
    max_abs_dev  = suppressWarnings(
      max(abs(deviation[is_outlier %in% TRUE]), na.rm = TRUE)),
    mean_abs_dev = mean(abs(deviation[is_outlier %in% TRUE]), na.rm = TRUE)
  ), by = s_id]
  # Replace -Inf / NaN (stations with no outliers) with zero so the colour
  # scale handles them.
  st[!is.finite(max_abs_dev),  max_abs_dev  := 0]
  st[is.na(mean_abs_dev),      mean_abs_dev := 0]

  if (outlier_only) {
    st <- st[n_outliers > 0L]
    if (nrow(st) == 0L) {
      message("No flagged stations to plot. Returning an empty map.")
    }
  }

  # Per-station popup: top_n worst offenders.
  .build_popup <- function(s) {
    sub <- dt[s_id == s & is_outlier %in% TRUE]
    if (nrow(sub) == 0L) {
      return(sprintf("<b>Station %s</b><br/>No flagged observations.", s))
    }
    sub <- sub[order(-abs(deviation))][seq_len(min(.N, top_n))]
    rows <- paste0(
      "<tr><td>", sub$year,
      "</td><td>", sub$phase_id,
      "</td><td>", round(sub$day, 0),
      "</td><td>", sprintf("%+.1f", sub$deviation),
      "</td></tr>",
      collapse = "")
    sprintf(paste0(
      "<b>Station %s</b><br/>",
      "<i>%d flagged of %d observations</i><br/>",
      "<table style='font-size:10px;border-collapse:collapse' ",
      "cellpadding='2'>",
      "<tr><th>Year</th><th>Phase</th><th>DOY</th><th>Dev</th></tr>",
      "%s</table>"),
      s,
      dt[s_id == s, sum(is_outlier, na.rm = TRUE)],
      dt[s_id == s, .N],
      rows)
  }
  st[, popup_html := vapply(s_id, .build_popup, character(1))]

  # Colour & size scales. Guard against empty or all-zero domains that
  # leaflet::colorNumeric refuses.
  max_dev <- if (nrow(st) == 0L) 1 else {
    m0 <- suppressWarnings(max(st$max_abs_dev, na.rm = TRUE))
    if (!is.finite(m0) || m0 <= 0) 1 else m0 + 1e-8
  }
  pal_fun <- leaflet::colorNumeric(palette = palette,
                                   domain = c(0, max_dev))
  radius <- if (nrow(st) == 0L) numeric(0) else {
    max_n <- max(st$n_outliers, na.rm = TRUE)
    if (max_n == 0L) rep(radius_range[1], nrow(st)) else {
      scale <- log1p(st$n_outliers) / log1p(max_n)
      radius_range[1] + scale * diff(radius_range)
    }
  }

  legend_title <- if (identical(method, "mahalanobis"))
    "Max robust MD" else "Max |residual| (d)"
  map_subtitle <- if (!is.null(method))
    sprintf("method = %s%s", method,
            if (!is.null(threshold) && is.finite(threshold))
              sprintf(", threshold = %s",
                      format(threshold, digits = 3)) else "") else ""

  m <- leaflet::leaflet(data = st) |>
    leaflet::addProviderTiles("CartoDB.Positron")
  if (nrow(st) > 0L) {
    m <- m |>
      leaflet::addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = radius,
        color = ~pal_fun(max_abs_dev),
        stroke = TRUE, weight = 1, opacity = 0.9,
        fillColor = ~pal_fun(max_abs_dev), fillOpacity = 0.65,
        popup = ~popup_html,
        label = ~sprintf("Station %s (%d outliers)", s_id, n_outliers)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal = pal_fun, values = ~max_abs_dev,
        title = legend_title,
        opacity = 0.8
      )
  }
  if (nzchar(map_subtitle)) {
    m <- m |>
      leaflet::addControl(
        html = sprintf(
          "<div style='background:white;padding:3px 6px;font-size:11px'>%s</div>",
          map_subtitle),
        position = "topleft"
      )
  }
  m
}
