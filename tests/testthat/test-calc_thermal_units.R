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

test_that("calc_thermal_units single_sine matches Baskerville-Emin formula", {
  # Pinned numerics against the canonical Baskerville-Emin (1969) /
  # Snyder (1985) / UC-IPM single-sine formula for the mixed case
  # (Tmin < Tbase < Tmax), computed by hand as:
  #   GDD = (1/pi) * [ (Tavg - Tbase) * theta + Tamp * sin(theta) ]
  # where
  #   Tavg  = (Tmin + Tmax) / 2
  #   Tamp  = (Tmax - Tmin) / 2
  #   theta = acos( (Tbase - Tavg) / Tamp )

  # Case A: symmetric around base (Tmin=0, Tmax=10, Tbase=5)
  # Tavg=5, Tamp=5, (Tbase-Tavg)/Tamp=0, theta=pi/2
  # GDD = (1/pi) * [0*pi/2 + 5*sin(pi/2)] = 5/pi
  expect_equal(
    calc_thermal_units(tmin = 0, tmax = 10, t_base = 5,
                       method = "single_sine", cumulative = FALSE),
    5 / pi,
    tolerance = 1e-8
  )

  # Case B: asymmetric (Tmin=2, Tmax=12, Tbase=5)
  # Tavg=7, Tamp=5, (Tbase-Tavg)/Tamp=-0.4, theta=acos(-0.4) ~ 1.9823
  # GDD = (1/pi) * [2 * 1.9823... + 5 * sin(1.9823...)]
  theta_B <- acos(-0.4)
  expected_B <- (1 / pi) * (2 * theta_B + 5 * sin(theta_B))
  expect_equal(
    calc_thermal_units(tmin = 2, tmax = 12, t_base = 5,
                       method = "single_sine", cumulative = FALSE),
    expected_B,
    tolerance = 1e-8
  )

  # Case C: Tmin >= Tbase -> whole day above base -> GDD = Tavg - Tbase
  expect_equal(
    calc_thermal_units(tmin = 10, tmax = 20, t_base = 5,
                       method = "single_sine", cumulative = FALSE),
    (10 + 20) / 2 - 5,
    tolerance = 1e-10
  )

  # Case D: Tmax <= Tbase -> whole day below base -> GDD = 0
  expect_equal(
    calc_thermal_units(tmin = -5, tmax = 2, t_base = 5,
                       method = "single_sine", cumulative = FALSE),
    0,
    tolerance = 1e-10
  )
})

test_that("calc_thermal_units single_sine is bounded by modified method", {
  # Single-sine GDD is always <= the "modified" method GDD, because the
  # modified method clips daily temperatures up to Tbase (never below)
  # and averages, which is the maximum conceivable GDD under a sinusoid
  # that dips below Tbase for part of the day.
  set.seed(1)
  n <- 50
  tmin <- runif(n, -5, 10)
  tmax <- tmin + runif(n, 5, 20)
  t_base <- 5
  ss  <- calc_thermal_units(tmin, tmax, t_base = t_base,
                            method = "single_sine", cumulative = FALSE)
  mod <- calc_thermal_units(tmin, tmax, t_base = t_base,
                            method = "modified", cumulative = FALSE)
  expect_true(all(ss <= mod + 1e-10))
})
