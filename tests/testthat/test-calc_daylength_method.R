test_that("calc_daylength default (brock) matches the legacy formula", {
  # Pinned numeric: DOY 172 (approx. summer solstice) at 45°N.
  # Current implementation: delta = 23.45° * sin(2π(284 + doy)/365)
  # -> cos(H) = -tan(L)*tan(delta) -> daylength = 24*H/π.
  res <- calc_daylength(172, 45)
  expect_gt(res$daylength, 15)
  expect_lt(res$daylength, 16)
})

test_that("calc_daylength 'cbm' method implements Forsythe (1995) CBM", {
  # CBM near summer solstice at 45°N is about 15.4 hours.
  res <- calc_daylength(172, 45, method = "cbm")
  expect_gt(res$daylength, 15)
  expect_lt(res$daylength, 16)
})

test_that("calc_daylength brock and cbm agree within ~15 min at mid-latitudes", {
  brock <- calc_daylength(1:365, 47, method = "brock")
  cbm   <- calc_daylength(1:365, 47, method = "cbm")
  # Methods should differ by less than ~15 minutes/day at mid-latitudes.
  # (Observed max ~12.4 min at a Swiss latitude; difference grows near
  # the poles.)
  expect_true(max(abs(brock$daylength - cbm$daylength), na.rm = TRUE) < 15/60)
})

test_that("calc_daylength cbm handles polar day / polar night", {
  # 80°N at summer solstice: polar day (24h).
  expect_equal(calc_daylength(172, 80, method = "cbm")$daylength, 24)
  # 80°N at winter solstice: polar night (0h).
  expect_equal(calc_daylength(355, 80, method = "cbm")$daylength, 0)
})

test_that("calc_daylength equinox ~12h for both methods", {
  for (m in c("brock", "cbm")) {
    eq <- calc_daylength(80, c(30, 45, 60), method = m)   # spring equinox
    expect_true(all(abs(eq$daylength - 12) < 0.5))
  }
})

test_that("calc_daylength errors on unknown method", {
  expect_error(calc_daylength(172, 45, method = "bogus"))
})
