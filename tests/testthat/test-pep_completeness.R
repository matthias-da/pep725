test_that("pep_completeness returns pep_completeness class", {
  data(pep_seed)
  result <- pep_completeness(pep_seed)
  expect_s3_class(result, "pep_completeness")
})

test_that("pep_completeness has expected columns", {
  data(pep_seed)
  result <- pep_completeness(pep_seed)
  expected_cols <- c("n_obs", "n_stations", "n_years", "year_min", "year_max",
                     "year_span", "completeness_pct", "median_doy", "iqr_doy")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("pep_completeness year_range filter works", {
  data(pep_seed)
  result <- pep_completeness(pep_seed, year_range = c(2000, 2010))
  expect_true(all(result$year_min >= 2000))
  expect_true(all(result$year_max <= 2010))
})

test_that("pep_completeness min_obs filter works", {
  data(pep_seed)
  result <- pep_completeness(pep_seed, min_obs = 50)
  expect_true(all(result$n_obs >= 50))
})

test_that("pep_completeness include_years adds year-level detail", {
  data(pep_seed)
  result <- pep_completeness(pep_seed, include_years = TRUE)
  expect_true("year" %in% names(result))
})
