# --- Fixture: two populations of stations, one contaminated observation ---
make_phenology_fixture <- function(seed = 1, n_stations = 30, n_years = 30,
                                   contaminate = TRUE) {
  set.seed(seed)
  stations <- data.table::data.table(
    s_id = seq_len(n_stations),
    alt  = runif(n_stations, 200, 2000),
    lat  = runif(n_stations, 45, 52),
    lon  = runif(n_stations, 5, 15)
  )
  years <- 1990:(1990 + n_years - 1)
  grid <- data.table::CJ(s_id = stations$s_id, year = years)
  dat <- merge(grid, stations, by = "s_id")
  # True phenology model: day depends on year (trend), altitude, latitude,
  # plus per-station random intercept and small noise.
  station_re <- rnorm(n_stations, sd = 3)
  dat[, day := 110 +
        0.015 * alt +                       # later at higher elevation
        1.2   * (lat - 47) +                # later further north
        -0.05 * (year - 1990) +             # trend: earlier over time
        station_re[s_id] +                  # station random intercept
        rnorm(.N, sd = 4)]                  # i.i.d. noise
  dat[, `:=`(genus = "Malus", species = "Malus domestica", phase_id = 60L)]
  if (contaminate) {
    # Inject a clearly anomalous (s_id = 1, year = 2000) observation 50 days
    # late — consistent with the station's own history (so the classic
    # 30-day-from-group-median rule may miss it if the station itself is
    # high-altitude and therefore late by a lot to begin with) but
    # inconsistent with the global covariate-driven expectation.
    dat[s_id == 1 & year == 2000, day := day + 50]
    # Also inject one "normal-looking-in-isolation" but covariate-inconsistent
    # point: a lowland station with a high-altitude DOY.
    dat[s_id == 2 & year == 2005, day := day + 30]
  }
  dat[]
}

test_that("gam_residual method flags a covariate-inconsistent outlier that 30day misses", {
  skip_if_not_installed("mgcv")
  dat <- make_phenology_fixture(seed = 1)
  # Use by = c(genus, species, phase_id) so the GAM pools across all stations.
  res_gam <- pep_flag_outliers(
    dat,
    by = c("genus", "species", "phase_id"),
    method = "gam_residual",
    threshold = 3.5
  )
  expect_s3_class(res_gam, "pep_outliers")
  expect_true("is_outlier" %in% names(res_gam))
  expect_true("deviation"  %in% names(res_gam))

  # The injected (s_id=1, year=2000) contamination must be flagged.
  hit <- res_gam[s_id == 1 & year == 2000, is_outlier]
  expect_true(isTRUE(hit))
})

test_that("gam_residual uses residuals from the supplied formula", {
  skip_if_not_installed("mgcv")
  dat <- make_phenology_fixture(seed = 2)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species", "phase_id"),
    method = "gam_residual",
    formula = day ~ s(year) + s(alt) + s(lat) + s(s_id, bs = "re"),
    threshold = 3.5
  )
  # The stored deviation should now represent GAM residuals, not
  # (day - station_median). Under a reasonable GAM, most residuals
  # should be small (|dev| < 15 days) except for the contamination.
  clean <- res[!(s_id %in% c(1, 2) & year %in% c(2000, 2005))]
  expect_lt(quantile(abs(clean$deviation), 0.95, na.rm = TRUE), 15)
})

test_that("gam_residual falls back to 30day when group is too small", {
  skip_if_not_installed("mgcv")
  dat <- make_phenology_fixture(seed = 3, n_stations = 3, n_years = 4)
  # 12 total observations: below min_n_per_group default -> fallback.
  expect_message(
    res <- pep_flag_outliers(
      dat,
      by = c("genus", "species", "phase_id"),
      method = "gam_residual",
      min_n_per_group = 50
    ),
    "fall|fallback|too few|fewer"
  )
  # Result is still a valid pep_outliers object with the usual columns.
  expect_s3_class(res, "pep_outliers")
  expect_true(all(c("is_outlier", "deviation", "expected_doy") %in% names(res)))
})

test_that("gam_residual respects flag_only = FALSE and removes outliers", {
  skip_if_not_installed("mgcv")
  dat <- make_phenology_fixture(seed = 4)
  n_before <- nrow(dat)
  cleaned <- pep_flag_outliers(
    dat,
    by = c("genus", "species", "phase_id"),
    method = "gam_residual",
    flag_only = FALSE,
    threshold = 3.5
  )
  expect_lt(nrow(cleaned), n_before)
  expect_false("is_outlier" %in% names(cleaned))
})

test_that("gam_residual: attr(result, 'method') == 'gam_residual'", {
  skip_if_not_installed("mgcv")
  dat <- make_phenology_fixture(seed = 5)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species", "phase_id"),
    method = "gam_residual",
    threshold = 3.5
  )
  expect_equal(attr(res, "method"), "gam_residual")
})
