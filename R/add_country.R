#' Add Country Information to a Dataset Based on Latitude/Longitude
#'
#' This function adds a new column \code{country} by performing a fast spatial
#' join between point coordinates (\code{lat}, \code{lon}) and world country
#' polygons from \pkg{rnaturalearth}. Geometry is handled via \pkg{sf}.
#'
#' The input data must contain numeric columns named \code{lat} and \code{lon}
#' in WGS84 (EPSG:4326).
#'
#' After assigning countries, Germany is automatically split into
#' \code{"Germany-North"} and \code{"Germany-South"} using a latitude threshold:
#' \itemize{
#'   \item lat >= 50 → \code{"Germany-North"}
#'   \item lat  < 50 → \code{"Germany-South"}
#' }
#'
#' @param dt A \code{data.frame} or \code{data.table} containing columns
#'   \code{lat} and \code{lon}.
#' @param keep_geometry Logical. If \code{TRUE}, the sf geometry column is kept.
#'   Default is \code{FALSE}.
#'
#' @return A \code{data.table} with a new column \code{country}.
#' @author Matthias Templ
#'
#' @examples
#' \dontrun{
#' small <- data.table(
#'   lat = c(48.2, 51.0, 47.5),
#'   lon = c(16.3, 10.1, 8.5)
#' )
#'
#' pep_with_country <- add_country(small)
#' pep_with_country
#' }
#'
#' @export
add_country <- function(dt, keep_geometry = FALSE) {

  # Check input
  if (!("lat" %in% names(dt)) || !("lon" %in% names(dt))) {
    stop("Input data must contain columns 'lat' and 'lon'.")
  }

  suppressWarnings({
    message("Assigning countries based on coordinates ...")
  })

  # Load required packages
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required.")
  if (!requireNamespace("rnaturalearth", quietly = TRUE))
    stop("Package 'rnaturalearth' is required.")
  if (!requireNamespace("rnaturalearthdata", quietly = TRUE))
    stop("Package 'rnaturalearthdata' is required.")
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")

  # Convert to data.table
  dt <- data.table::as.data.table(dt)

  # -------------------------------------------------------------
  # Convert to sf POINT object with WGS84 coordinates
  # -------------------------------------------------------------
  pep_sf <- sf::st_as_sf(
    dt,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  # -------------------------------------------------------------
  # Load world polygons
  # -------------------------------------------------------------
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[, c("name")]
  world <- sf::st_make_valid(world)

  # Reproject both datasets (faster join in planar CRS)
  pep_sf <- sf::st_transform(pep_sf, 3857)
  world  <- sf::st_transform(world, 3857)

  sf::st_agr(world) <- "constant"

  # -------------------------------------------------------------
  # Spatial join
  # -------------------------------------------------------------
  pep_joined <- sf::st_join(pep_sf, world, left = TRUE)

  # Convert back to data.table
  dt2 <- data.table::as.data.table(pep_joined)

  # Remove geometry unless requested
  if (!keep_geometry) {
    dt2[, geometry := NULL]
  }

  data.table::setnames(dt2, "name", "country")

  # -------------------------------------------------------------
  # Germany split
  # -------------------------------------------------------------
  dt2[
    country == "Germany" & lat >= 50,
    country := "Germany-North"
  ]

  dt2[
    country == "Germany" & lat < 50,
    country := "Germany-South"
  ]

  return(dt2)
}
