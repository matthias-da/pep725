test_that("pheno_normals() returns correct class", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10)
  expect_s3_class(result, "pheno_normals")
  expect_s3_class(result, "data.table")
})

test_that("pheno_normals() has expected output columns", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10)
  expected_cols <- c("n_years", "mean_doy", "median_doy", "sd_doy",
                     "iqr_doy", "mad_doy", "q25", "q75", "period")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("pheno_normals() min_years threshold returns NA for insufficient data", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  # pep_seed has 26 years (1990-2015), so min_years = 30 should cause NAs
  result <- suppressMessages(
    pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                  min_years = 30)
  )
  expect_true(all(is.na(result$mean_doy)))
})

test_that("pheno_normals() species filter works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, species = "Malus",
                          by = c("phase_id"), min_years = 10)
  expect_gt(nrow(result), 0)
})

test_that("pheno_normals() phase_id filter works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, phase_id = 60,
                          by = c("genus"), min_years = 10)
  expect_gt(nrow(result), 0)
})

test_that("pheno_normals() stores attributes", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, species = "Malus",
                          phase_id = 60, by = c("country"), min_years = 10)
  expect_equal(attr(result, "period"), 1990:2015)
  expect_equal(attr(result, "min_years"), 10)
  expect_equal(attr(result, "species_filter"), "Malus")
  expect_equal(attr(result, "phase_filter"), 60)
})

test_that("pheno_normals() period column is correctly formatted", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10)
  expect_equal(unique(result$period), "1990-2015")
})

test_that("pheno_normals() custom by grouping works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus"),
                          min_years = 10)
  expect_true("genus" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("pheno_normals() statistics are plausible", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10)
  valid <- result[!is.na(mean_doy)]
  expect_true(all(valid$mean_doy >= 1 & valid$mean_doy <= 365))
  expect_true(all(valid$sd_doy >= 0))
  expect_true(all(valid$q25 <= valid$q75))
})

test_that("pheno_normals() default probs produce q05/q10/q25/q75/q90/q95", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10)
  expect_true(all(c("q05", "q10", "q25", "q75", "q90", "q95") %in% names(result)))
})

test_that("pheno_normals() custom probs generate correctly named columns", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  # Non-default probs must produce column names derived from the levels,
  # not silently re-use q05..q95.
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10,
                          probs = c(0.025, 0.25, 0.75, 0.975))
  expect_true("q02_5"  %in% names(result))
  expect_true("q25"    %in% names(result))
  expect_true("q75"    %in% names(result))
  expect_true("q97_5"  %in% names(result))
  # The old hardcoded names must not be present when the user did not ask.
  expect_false("q05" %in% names(result))
  expect_false("q95" %in% names(result))
  # Values must correspond to actual quantile levels.
  valid <- result[!is.na(q25)]
  expect_true(all(valid$q02_5 <= valid$q25))
  expect_true(all(valid$q75   <= valid$q97_5))
})

test_that("pheno_normals() stores probs as attribute", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                          min_years = 10,
                          probs = c(0.1, 0.9))
  expect_equal(attr(result, "probs"), c(0.1, 0.9))
})

test_that("pheno_normals() accepts a two-element probs without error", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_no_error(
    pheno_normals(p, period = 1990:2015, by = c("genus", "phase_id"),
                  min_years = 10, probs = c(0.1, 0.9))
  )
})

test_that("pheno_normals() errors on invalid input", {
  expect_error(pheno_normals("not a df"), "must be a data.frame")
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_error(pheno_normals(p, period = "bad"), "numeric vector")
  expect_error(pheno_normals(p, by = c("nonexistent")), "not found")
  expect_error(
    pheno_normals(p, species = "ZZZnonexistent", period = 1990:2015,
                  by = c("phase_id")),
    "No observations"
  )
})
