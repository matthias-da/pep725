#' Plot Phenology Station Maps with Google Maps
#'
#' Generates a map of PEP725 phenological station locations with optional coloring
#' by number of observations or number of species per station.
#'
#' @param pep A `data.table` or `data.frame` with at least columns `lon`, `lat`, and `species`.
#' @param location Named vector with center longitude and latitude for the map.
#'        Defaults to near Changins: \code{c(lon = 6.233, lat = 46.400)}.
#' @param zoom Zoom level for the map (default: 4 for Europe-wide, 7 for regional).
#' @param color_by One of \code{"none"}, \code{"n_obs"}, or \code{"n_species"}:
#'        controls how points are colored.
#' @param point_size Size of station points (default: 0.8).
#' @param output_file Optional file path to export the plot (e.g. \code{"map.pdf"}).
#' @param key Google Maps API key (you can set globally via `register_google()` instead).
#'
#' @return A ggplot map object.
#' @import ggplot2
#' @import ggmap
#' @importFrom dplyr group_by summarise n n_distinct
#' @importFrom ggmap register_google get_map
#' @examples
#' \dontrun{
#' # Register API key (or provide via argument)
#' ggmap::register_google(key = "your_api_key_here")
#'
#' # Plot all stations (Europe-wide)
#' map_pep(pep, color_by = "none", zoom = 4)
#'
#' # Plot with number of observations
#' map_pep(pep, color_by = "n_obs", zoom = 4)
#'
#' # Plot near Changins with number of species per station
#' pep_sub <- pep[pep$lat > 44.7 & pep$lat < 48.1 & pep$lon > 4.2 & pep$lon < 8.1]
#' map_pep(pep_sub, color_by = "n_species", zoom = 7)
#' }
#' @author Matthias Templ
#' @export
map_pep <- function(
    pep,
    location = c(lon = 6.233, lat = 46.400),
    zoom = 4,
    color_by = c("none", "n_obs", "n_species"),
    point_size = 0.8,
    output_file = NULL,
    key = NULL
) {
  color_by <- match.arg(color_by)

  if (!is.null(key)) {
    register_google(key = key)
  }

  # Download map
  gmap <- get_map(location = location, zoom = zoom)
  g <- ggmap(gmap) +
    theme_minimal()

  if (color_by == "none") {
    # Just plot unique lon/lat
    unique_coords <- unique(pep[, c("lon", "lat")])
    g <- g + geom_point(data = unique_coords, aes(x = lon, y = lat),
                        size = point_size, color = "black")
  } else if (color_by == "n_obs") {
    nobs <- dplyr::group_by(pep, lon, lat) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
    g <- g + geom_point(data = nobs, aes(x = lon, y = lat, color = n),
                        size = point_size) +
      scale_color_viridis_c(name = "No. obs.")
  } else if (color_by == "n_species") {
    nsp <- dplyr::group_by(pep, lon, lat) %>%
      dplyr::summarise(n_species = n_distinct(species), .groups = "drop")
    g <- g + geom_point(data = nsp, aes(x = lon, y = lat, color = n_species),
                        size = point_size) +
      scale_color_viridis_c(name = "No. species")
  }

  g <- g + labs(title = "Phenology Stations") +
    theme(legend.position = "right")

  # Save if requested
  if (!is.null(output_file)) {
    ggsave(output_file, plot = g, width = 10, height = 8)
  }

  return(g)
}
