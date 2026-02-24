test_that("calc_daylength returns list for scalar inputs", {
  result <- calc_daylength(80, 50)
  expect_type(result, "list")
  expect_named(result, c("daylength", "declination"))
  expect_true(result$daylength > 0 && result$daylength < 24)
})

test_that("calc_daylength returns data.frame for vector inputs", {
  result <- calc_daylength(1:365, 45)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 365)
  expect_true(all(c("doy", "lat", "daylength", "declination") %in% names(result)))
})

test_that("calc_daylength equinox is approximately 12h", {
  result <- calc_daylength(80, 0)
  expect_true(abs(result$daylength - 12) < 1)
})

test_that("calc_daylength polar day at high latitude in summer", {
  result <- calc_daylength(172, 70)
  expect_equal(result$daylength, 24)
})

test_that("calc_daylength errors on non-numeric", {
  expect_error(calc_daylength("a", 50), "must be numeric")
})

test_that("calc_daylength errors on lat > 90", {
  expect_error(calc_daylength(100, 95), "between -90 and 90")
})

test_that("calc_max_daylength returns longer days at higher latitudes", {
  low <- calc_max_daylength(30)
  high <- calc_max_daylength(60)
  expect_true(high > low)
})
