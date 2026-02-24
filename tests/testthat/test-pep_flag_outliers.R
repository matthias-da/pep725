test_that("pep_flag_outliers returns pep_outliers class with flag columns", {
  data(pep_seed)
  result <- pep_flag_outliers(pep_seed)
  expect_s3_class(result, "pep_outliers")
  expect_true(all(c("is_outlier", "deviation", "expected_doy") %in% names(result)))
})

test_that("pep_flag_outliers 30day method flags large deviations", {
  data(pep_seed)
  result <- pep_flag_outliers(pep_seed, method = "30day")
  outliers <- result[result$is_outlier == TRUE & !is.na(result$is_outlier), ]
  if (nrow(outliers) > 0) {
    expect_true(all(abs(outliers$deviation) > 30))
  }
})

test_that("pep_flag_outliers mad method works", {
  data(pep_seed)
  result <- pep_flag_outliers(pep_seed, method = "mad")
  expect_s3_class(result, "pep_outliers")
  expect_equal(attr(result, "method"), "mad")
})

test_that("pep_flag_outliers iqr method works", {
  data(pep_seed)
  result <- pep_flag_outliers(pep_seed, method = "iqr")
  expect_s3_class(result, "pep_outliers")
  expect_equal(attr(result, "method"), "iqr")
})

test_that("pep_flag_outliers flag_only=FALSE removes outliers", {
  data(pep_seed)
  flagged <- pep_flag_outliers(pep_seed, method = "30day")
  cleaned <- pep_flag_outliers(pep_seed, method = "30day", flag_only = FALSE)
  n_outliers <- sum(flagged$is_outlier, na.rm = TRUE)
  if (n_outliers > 0) {
    expect_true(nrow(cleaned) < nrow(flagged))
  }
  expect_false("is_outlier" %in% names(cleaned))
})

test_that("pep_flag_outliers errors on non-data.frame", {
  expect_error(pep_flag_outliers("not a df"), "must be a data.frame")
})
