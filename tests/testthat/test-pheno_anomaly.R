test_that("pheno_anomaly() returns correct class", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  expect_s3_class(result, "pheno_anomaly")
  expect_s3_class(result, "data.table")
})

test_that("pheno_anomaly() has expected columns", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  expected_cols <- c("observed_doy", "baseline_doy", "anomaly_days",
                     "z_score", "is_extreme", "direction")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("anomaly_days equals observed_doy minus baseline_doy", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  valid <- result[!is.na(anomaly_days)]
  expect_equal(valid$anomaly_days, valid$observed_doy - valid$baseline_doy)
})

test_that("direction values are valid", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  valid_dirs <- result$direction[!is.na(result$direction)]
  expect_true(all(valid_dirs %in% c("early", "late", "normal")))
})

test_that("is_extreme is logical", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  expect_type(result$is_extreme, "logical")
})

test_that("robust vs classical method both work", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  robust_result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10, robust = TRUE)
  )
  classical_result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10, robust = FALSE)
  )
  expect_true(attr(robust_result, "robust"))
  expect_false(attr(classical_result, "robust"))
})

test_that("target_years filter works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  target_years = 2010:2015,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  expect_true(all(result$year >= 2010 & result$year <= 2015))
})

test_that("pheno_anomaly() stores attributes", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10,
                  extreme_threshold = 2, robust = TRUE)
  )
  expect_equal(attr(result, "baseline_period"), 1990:2005)
  expect_equal(attr(result, "extreme_threshold"), 2)
  expect_true(attr(result, "robust"))
})

test_that("percentile is between 0 and 100", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- suppressMessages(
    pheno_anomaly(p, baseline_period = 1990:2005,
                  by = c("genus", "phase_id"), min_years = 10)
  )
  valid_pct <- result$percentile[!is.na(result$percentile)]
  expect_true(all(valid_pct >= 0 & valid_pct <= 100))
})

test_that("pheno_anomaly() errors on invalid input", {
  expect_error(pheno_anomaly("not a df"), "must be a data.frame")
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_error(
    pheno_anomaly(p, baseline_period = "bad"),
    "numeric vector"
  )
  expect_error(
    pheno_anomaly(p, extreme_threshold = -1),
    "positive number"
  )
})
