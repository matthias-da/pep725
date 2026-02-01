#' Interactive Leaflet Map to Select PEP725 Stations
#'
#' Launches a Shiny gadget that allows selection of PEP phenological stations
#' by drawing a bounding box or clicking individual points on the map.
#'
#' @param pep A data frame or data.table with columns \code{lon}, \code{lat}, and optionally \code{species}.
#' @param label_col Optional column name to show as label in popups (e.g. \code{"species"} or \code{"s_id"}).
#' @param quiet Logical. If \code{FALSE} (default), displays a message about expected loading time.
#' @return A `data.frame` with selected stations (columns: \code{lon}, \code{lat}, and metadata).
#'
#' @details
#' \strong{Note:} Loading time depends on the size of the dataset. For the full PEP725
#' dataset with millions of observations, expect 10-20 seconds for the map to fully
#' render. Consider filtering the data before calling this function to speed up loading:
#'
#' \preformatted{
#' # Filter to a specific species first
#' wheat <- pep[genus == "Triticum"]
#' selected <- leaflet_pep(wheat)
#' }
#'
#' @import leaflet leaflet.extras shiny miniUI
#' @examples
#' \donttest{
#' # Download synthetic data first
#' pep <- pep_download()
#'
#' # For faster loading, filter the data first (recommended)
#' pep_subset <- pep[species == "Triticum aestivum"]
#'
#' # Launch interactive selection gadget
#' selected <- leaflet_pep(pep_subset, label_col = "species")
#'
#' # Inspect selected subset
#' print(selected)
#' }
#' @author Matthias Templ
#' @export
leaflet_pep <- function(pep, label_col = NULL, quiet = FALSE) {
  requireNamespace("shiny")
  requireNamespace("miniUI")
  requireNamespace("leaflet")
  requireNamespace("leaflet.extras")

  if (!all(c("lon", "lat") %in% names(pep))) stop("pep must contain 'lon' and 'lat' columns.")


  # Inform user about loading time
  n_points <- nrow(pep)
  if (!quiet) {
    message(sprintf(
      "\nLaunching interactive map with %s points...",
      format(n_points, big.mark = ",")
    ))
    if (n_points > 100000) {
      message("Note: Large dataset detected. The map may take 10-20 seconds to fully load.")
      message("Tip: Filter your data first for faster loading (e.g., pep[genus == 'Triticum'])\n")
    } else if (n_points > 10000) {
      message("Note: Map loading may take a few seconds...\n")
    }
  }

  # Minimal label
  label <- if (!is.null(label_col) && label_col %in% names(pep)) {
    pep[[label_col]]
  } else {
    paste0("Lon: ", round(pep$lon, 3), ", Lat: ", round(pep$lat, 3))
  }

  # UI with loading indicator
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select PEP Stations"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .loading-overlay {
          position: absolute;
          top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(255,255,255,0.9);
          z-index: 1000;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          font-family: sans-serif;
        }
        .loading-spinner {
          border: 4px solid #f3f3f3;
          border-top: 4px solid #3498db;
          border-radius: 50%;
          width: 40px;
          height: 40px;
          animation: spin 1s linear infinite;
          margin-bottom: 15px;
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .loading-text {
          color: #666;
          font-size: 14px;
          text-align: center;
        }
        .loading-subtext {
          color: #999;
          font-size: 12px;
          margin-top: 5px;
        }
      "))
    ),
    miniUI::miniContentPanel(
      shiny::div(
        id = "loading-screen",
        class = "loading-overlay",
        shiny::div(class = "loading-spinner"),
        shiny::div(
          class = "loading-text",
          sprintf("Loading %s stations...", format(n_points, big.mark = ","))
        ),
        shiny::div(
          class = "loading-subtext",
          if (n_points > 100000) "This may take 10-20 seconds" else "Please wait..."
        )
      ),
      leaflet::leafletOutput("map", height = "100%")
    )
  )

  # Server
  server <- function(input, output, session) {
    values <- reactiveValues(
      selected_points = data.frame(),
      map_ready = FALSE
    )

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(pep) %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          lng = ~lon, lat = ~lat,
          radius = 4, stroke = FALSE, fillOpacity = 0.7,
          label = label,
          layerId = ~paste(lon, lat)
        ) %>%
        leaflet.extras::addDrawToolbar(
          targetGroup = "draw",
          polylineOptions = FALSE,
          polygonOptions = TRUE,
          rectangleOptions = TRUE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions(),
          singleFeature = FALSE
        )

      # Signal that map rendering is complete
      values$map_ready <- TRUE
      map
    })

    # Hide loading screen when map is ready
    observe({
      if (values$map_ready) {
        shiny::removeUI(selector = "#loading-screen", immediate = TRUE)
      }
    })

    # Individual point click
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      pt <- pep[abs(pep$lon - click$lng) < 1e-6 & abs(pep$lat - click$lat) < 1e-6, ]
      values$selected_points <- unique(rbind(values$selected_points, pt))
    })

    # Polygon/rectangle selection
    observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      if (feat$geometry$type %in% c("Polygon", "Rectangle")) {
        coords <- feat$geometry$coordinates[[1]]
        lng <- sapply(coords, `[[`, 1)
        lat <- sapply(coords, `[[`, 2)
        in_poly <- sp::point.in.polygon(
          point.x = pep$lon, point.y = pep$lat,
          pol.x = lng, pol.y = lat
        ) > 0
        values$selected_points <- unique(rbind(values$selected_points, pep[in_poly, ]))
      }
    })

    # Done button
    observeEvent(input$done, {
      stopApp(values$selected_points)
    })

    # Cancel button
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
  }

  # Run gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Select Stations", width = 900, height = 700))
}
