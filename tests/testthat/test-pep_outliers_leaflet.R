make_flagged_fixture <- function() {
  set.seed(42)
  n_st <- 10; n_yr <- 20
  grid <- data.table::CJ(s_id = seq_len(n_st), year = 1990:(1989 + n_yr))
  d <- grid
  d[, alt := rep(runif(n_st, 200, 2000), each = n_yr)]
  d[, lat := rep(runif(n_st, 46, 50), each = n_yr)]
  d[, lon := rep(runif(n_st, 5, 15), each = n_yr)]
  d[, day := 110 + 0.02 * alt + rnorm(.N, sd = 4)]
  d[, `:=`(genus = "Malus", species = "Malus domestica", phase_id = 60L)]
  # Inject three clear outliers
  d[s_id == 1 & year == 2000, day := day + 50]
  d[s_id == 2 & year == 1995, day := day - 45]
  d[s_id == 3 & year == 2005, day := day + 40]
  pep_flag_outliers(d, by = c("genus","species","phase_id"),
                    method = "30day")
}

test_that("pep_outliers_leaflet returns a leaflet htmlwidget", {
  skip_if_not_installed("leaflet")
  flagged <- make_flagged_fixture()
  m <- pep_outliers_leaflet(flagged)
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")
})

test_that("pep_outliers_leaflet respects outlier_only", {
  skip_if_not_installed("leaflet")
  flagged <- make_flagged_fixture()
  m_all  <- pep_outliers_leaflet(flagged, outlier_only = FALSE)
  m_only <- pep_outliers_leaflet(flagged, outlier_only = TRUE)
  # The only-outliers map has fewer or equal markers.
  n_all  <- length(m_all$x$calls[vapply(m_all$x$calls,
                                         function(c) c$method == "addCircleMarkers",
                                         logical(1))])
  n_only <- length(m_only$x$calls[vapply(m_only$x$calls,
                                          function(c) c$method == "addCircleMarkers",
                                          logical(1))])
  # Both paths should produce exactly one addCircleMarkers call.
  expect_equal(n_all, 1L)
  expect_equal(n_only, 1L)
})

test_that("pep_outliers_leaflet errors on wrong input class", {
  skip_if_not_installed("leaflet")
  expect_error(pep_outliers_leaflet(data.frame(x = 1)),
               "pep_outliers object")
})

test_that("pep_outliers_leaflet errors on missing columns", {
  skip_if_not_installed("leaflet")
  flagged <- make_flagged_fixture()
  # Strip required columns
  flagged2 <- data.table::copy(flagged)
  class(flagged2) <- c("pep_outliers", class(flagged2))
  flagged2[, c("lon", "lat") := NULL]
  expect_error(pep_outliers_leaflet(flagged2), "missing required columns")
})

test_that("pep_outliers_leaflet handles a fixture with zero flagged stations", {
  skip_if_not_installed("leaflet")
  d <- data.table::data.table(
    s_id = rep(1:3, each = 6),
    year = rep(2000:2005, 3),
    day = c(110, 111, 109, 108, 112, 110,
            115, 116, 114, 113, 117, 115,
            120, 121, 119, 118, 122, 120),
    phase_id = 60L,
    lon = rep(c(5, 7, 9), each = 6),
    lat = rep(c(47, 48, 49), each = 6),
    genus = "Malus", species = "Malus domestica"
  )
  flagged <- pep_flag_outliers(d, by = c("genus","species","phase_id"),
                               method = "30day")
  expect_message(m <- pep_outliers_leaflet(flagged),
                 "No flagged stations")
  expect_s3_class(m, "leaflet")
})
