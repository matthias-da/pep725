test_that("pep_quality() returns correct class", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expect_s3_class(result, "pep_quality")
  expect_s3_class(result, "data.table")
})

test_that("pep_quality() has expected output columns", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expected_cols <- c("n_obs", "completeness_pct", "quality_grade",
                     "year_min", "year_max", "year_span", "n_years",
                     "max_gap_years", "n_outliers", "outlier_pct")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("quality grades are valid A/B/C/D", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expect_true(all(result$quality_grade %in% c("A", "B", "C", "D")))
})

test_that("completeness_pct is between 0 and 100", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expect_true(all(result$completeness_pct >= 0 & result$completeness_pct <= 100))
})

test_that("year_span equals year_max - year_min + 1", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expect_equal(result$year_span, result$year_max - result$year_min + 1L)
})

test_that("n_years <= year_span", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  expect_true(all(result$n_years <= result$year_span))
})

test_that("Grade A criteria verified", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, by = c("s_id", "genus", "phase_id"))
  grade_a <- result[quality_grade == "A"]
  if (nrow(grade_a) > 0) {
    expect_true(all(grade_a$completeness_pct >= 80))
    expect_true(all(grade_a$max_gap_years <= 2))
    expect_true(all(grade_a$outlier_pct < 2))
  }
})

test_that("pep_quality() species and phase_id filters work", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, species = "Malus", phase_id = 60,
                        by = c("s_id"))
  expect_gt(nrow(result), 0)
})

test_that("pep_quality() year_range filter works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, year_range = c(2000, 2015),
                        by = c("s_id", "genus", "phase_id"))
  expect_true(all(result$year_min >= 2000))
  expect_true(all(result$year_max <= 2015))
})

test_that("both outlier methods (tukey, zscore) work", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  tukey <- pep_quality(p, outlier_method = "tukey", by = c("s_id", "genus", "phase_id"))
  zscore <- pep_quality(p, outlier_method = "zscore", by = c("s_id", "genus", "phase_id"))
  expect_equal(attr(tukey, "outlier_method"), "tukey")
  expect_equal(attr(zscore, "outlier_method"), "zscore")
})

test_that("pep_quality() stores attributes", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pep_quality(p, species = "Malus", phase_id = 60,
                        year_range = c(2000, 2015),
                        by = c("s_id"))
  expect_equal(attr(result, "year_range"), c(2000, 2015))
  expect_equal(attr(result, "species_filter"), "Malus")
  expect_equal(attr(result, "phase_filter"), 60)
})

test_that("pep_quality() errors on invalid input", {
  expect_error(pep_quality("not a df"), "must be a data.frame")
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_error(pep_quality(p, by = c("nonexistent")), "not found")
  expect_error(pep_quality(p, year_range = c(2000)), "numeric vector of length 2")
})
