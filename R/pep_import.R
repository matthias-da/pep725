#' Import and preprocess PEP725 phenological data
#'
#' This function imports all CSV files from the specified folder,
#' reads them efficiently using \pkg{data.table}, and combines them into a single
#' data table. It performs several preprocessing steps, including:
#' \itemize{
#'   \item Converting selected columns to factors (\code{provider_id}, \code{s_id}, \code{gss_id}, \code{genus}, \code{species}, \code{subspecies}).
#'   \item Replacing missing altitude values coded as \code{-9999} with \code{NA}.
#'   \item Creating a combined altitude variable \code{alt2} using \code{alt} and \code{alt_dem}.
#'   \item Converting the \code{date} column to \code{Date} class.
#'   \item Recoding cultivation season (\code{cult_season}) and quality control flags (\code{qc_ori_flag}) into labeled factors.
#'   \item Removing unused or problematic columns (\code{affected_flag}, \code{qc_flag}).
#' }
#'
#' This function is intended for importing raw PEP725 station data into a standardized
#' format suitable for further phenological analysis, such as sensitivity and trend estimation.
#'
#' @param path path to the folder containing PEP725 CSV files (default is \code{"data/Data_PEP725_all/"}).
#' @param flags Logical indicating whether the pep data contains quality control flags (default is \code{FALSE}).
#' @param add_country Logical indicating whether to add country information based on station coordinates (default is \code{TRUE}).
#' @return A \code{\link[=new_pep]{pep}} object (extends \code{data.table}) containing the combined and preprocessed PEP725 data.
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_make_valid st_as_sf st_transform st_join st_agr st_agr<-
#' @seealso \code{\link[data.table]{fread}}, \code{\link[data.table]{rbindlist}}
#' @author Matthias Templ (FHNW)
#' @examples
#' \dontrun{
#' # path to all csv files from pep725
#' mypath <- "~/data/Data_PEP725_all/"
#' pep_data <- pep_import(path = mypath)
#' str(pep_data)
#' }
#' @export
pep_import <- function(path = "data/Data_PEP725_all", flags = FALSE, add_country = TRUE){

  # Validate path
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path, call. = FALSE)
  }

  # Get all CSV file paths
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No CSV files found in: ", path, call. = FALSE)
  }

  # Read and bind them efficiently
  dt <- rbindlist(lapply(files, fread, sep = ";", fill = TRUE, quote = "",
                         encoding = "UTF-8", na.strings = c("", "NA")))

  if (nrow(dt) == 0) {
    stop("CSV files in '", path, "' contain no data rows", call. = FALSE)
  }

  dt$provider_id <- factor(dt$provider_id)
  dt$s_id <- factor(dt$s_id)
  dt$gss_id <- factor(dt$gss_id)
  dt$genus <- factor(dt$genus)
  dt$species <- factor(dt$species)
  dt$subspecies <- factor(dt$subspecies)
  dt[dt$alt == -9999, "alt"] <- NA
  dt[dt$alt_dem == -9999, "alt_dem"] <- NA
  # create a new altitude variable that combines both alt information
  dt$alt2 <- dt$alt
  dt$alt2[is.na(dt$alt2)] <- dt$alt_dem[is.na(dt$alt2)]
  dt[, date := as.Date(as.character(date))]
  dt$cult_season <- factor(dt$cult_season,
                           levels = 0:2,
                           labels = c("not applicable", "summer cereals - sowing in spring", "winter cereals - sowing in autumn"))
  if(flags){
    dt$affected_flag <- factor(dt$affected_flag,
                               levels = c(0, 1, 10),
                               labels = c("not affected", "affected", "unknown"))
    levels <- c(0L, 101007L, 1801000L, 1801001L, 1801002L)
    labels <- c("default", "spatial consistency check",
                "not tested", "no outlier", "outlier")

    dt[, qc_ori_flag := factor(qc_ori_flag, levels = levels, labels = labels)]
    dt[, c("affected_flag", "qc_flag") := NULL]
  }

  setcolorder(dt, c(
    head(names(dt), which(names(dt) == "alt_dem")),
    "alt2",
    setdiff(names(dt), c(head(names(dt), which(names(dt) == "alt_dem")), "alt2"))
  ))

  if(add_country){
    # Add country information to PEP data using fast spatial join
    message("Adding country information (add_country = TRUE). This may take 1-3 minutes...")

    # Convert to sf POINT object with WGS84 coordinates
    pep_sf <- st_as_sf(
      dt,
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )

    # Load world country polygons
    world <- ne_countries(scale = "medium", returnclass = "sf")[, c("name")]

    # Fix invalid geometries (important)
    world <- st_make_valid(world)

    # Transform both to projected CRS (3857 = fast for intersections)
    pep_sf   <- st_transform(pep_sf, 3857)
    world    <- st_transform(world, 3857)

    # Improve join performance (avoid warnings)
    st_agr(world) <- "constant"

    # Fast spatial join: assign each point the country polygon
    pep_sf_joined <- st_join(pep_sf, world, left = TRUE)

    # Convert back to data.table
    dt <- as.data.table(pep_sf_joined)

    # Remove geometry column (not needed anymore)
    dt[, geometry := NULL]

    # Rename the joined country column
    setnames(dt, "name", "country")

    # Split Germany into south and north
    dt[
      country == "Germany" & lat >= 50,
      country := "Germany-North"
    ]

    dt[
      country == "Germany" & lat < 50,
      country := "Germany-South"
    ]
  }

  # Return as pep class object (validate required columns)
  new_pep(dt)
}
