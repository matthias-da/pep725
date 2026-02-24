test_that("calc_thermal_units average method produces non-negative values", {
  tmin <- c(5, 7, 8, 10, 12)
  tmax <- c(15, 18, 20, 22, 25)
  result <- calc_thermal_units(tmin, tmax, t_base = 10, cumulative = FALSE)
  expect_true(all(result >= 0))
})

test_that("calc_thermal_units cumulative returns monotonically increasing values", {
  tmin <- c(5, 7, 8, 10, 12)
  tmax <- c(15, 18, 20, 22, 25)
  result <- calc_thermal_units(tmin, tmax, t_base = 10, cumulative = TRUE)
  expect_true(all(diff(result) >= 0))
})

test_that("calc_thermal_units daily vs cumulative consistency", {
  tmin <- c(5, 7, 8, 10, 12)
  tmax <- c(15, 18, 20, 22, 25)
  daily <- calc_thermal_units(tmin, tmax, t_base = 10, cumulative = FALSE)
  cum <- calc_thermal_units(tmin, tmax, t_base = 10, cumulative = TRUE)
  expect_equal(cumsum(daily), cum)
})

test_that("calc_thermal_units t_cap limits temperature effect", {
  tmin <- c(20, 20)
  tmax <- c(40, 40)
  without_cap <- calc_thermal_units(tmin, tmax, t_base = 10, t_cap = NULL, cumulative = FALSE)
  with_cap <- calc_thermal_units(tmin, tmax, t_base = 10, t_cap = 30, cumulative = FALSE)
  expect_true(all(with_cap <= without_cap))
})

test_that("calc_thermal_units errors on non-numeric tmin", {
  expect_error(calc_thermal_units("a", c(15, 18)), "must be numeric")
})

test_that("calc_thermal_units single_sine method works", {
  tmin <- c(5, 7, 8, 10, 12)
  tmax <- c(15, 18, 20, 22, 25)
  result <- calc_thermal_units(tmin, tmax, t_base = 10, method = "single_sine", cumulative = FALSE)
  expect_length(result, 5)
  expect_true(all(result >= 0))
})
