# ---- kendall_tau: deprecated wrapper (kept for backwards compatibility) ----

test_that("kendall_tau is soft-deprecated and returns the old z-stat value", {
  # Pre-1.1.0 semantics: returned the Mann-Kendall Z-statistic, misnamed.
  expect_warning(
    x <- kendall_tau(c(100, 105, 110, 115, 120, 125, 130)),
    "deprecated"
  )
  # Old behavior must continue to work for existing callers.
  expect_true(x > 0)
  expect_warning(
    y <- kendall_tau(c(130, 125, 120, 115, 110, 105, 100)),
    "deprecated"
  )
  expect_true(y < 0)
})

test_that("kendall_tau returns NA for length < 3 (deprecated path)", {
  suppressWarnings({
    expect_true(is.na(kendall_tau(c(1, 2))))
    expect_true(is.na(kendall_tau(c(1))))
    expect_true(is.na(kendall_tau(numeric(0))))
  })
})

# ---- mann_kendall_z: the new, correctly-named Z-statistic ----

test_that("mann_kendall_z matches Kendall::MannKendall Z-statistic", {
  skip_if_not_installed("Kendall")
  set.seed(1)
  x <- c(100, 103, 98, 105, 110, 108, 115, 112, 120, 118)
  ref <- Kendall::MannKendall(x)
  # Kendall::MannKendall returns S and var(S); reconstruct Z with continuity
  # correction (matches our implementation).
  s <- as.numeric(ref$S)
  vs <- as.numeric(ref$varS)
  z_ref <- (s - sign(s)) / sqrt(vs)
  expect_equal(mann_kendall_z(x), z_ref, tolerance = 1e-8)
})

test_that("mann_kendall_z applies tie correction to variance", {
  skip_if_not_installed("Kendall")
  # Ties in phenology data are common (multiple stations same DOY).
  x_with_ties <- c(100, 100, 105, 105, 110, 115, 115, 120)
  ref <- Kendall::MannKendall(x_with_ties)
  vs <- as.numeric(ref$varS)
  s <- as.numeric(ref$S)
  z_ref <- (s - sign(s)) / sqrt(vs)
  expect_equal(mann_kendall_z(x_with_ties), z_ref, tolerance = 1e-6)
})

test_that("mann_kendall_z is sign-correct and NA-safe", {
  expect_true(mann_kendall_z(c(1, 2, 3, 4, 5, 6, 7)) > 0)
  expect_true(mann_kendall_z(c(7, 6, 5, 4, 3, 2, 1)) < 0)
  expect_true(is.na(mann_kendall_z(c(1, 2))))
  expect_true(is.na(mann_kendall_z(numeric(0))))
  # NAs are silently stripped
  expect_equal(
    mann_kendall_z(c(1, 2, NA, 3, 4, 5, 6, 7)),
    mann_kendall_z(c(1, 2, 3, 4, 5, 6, 7)),
    tolerance = 1e-10
  )
})
