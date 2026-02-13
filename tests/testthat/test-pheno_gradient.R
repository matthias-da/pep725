test_that("pheno_gradient() returns correct class", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus", phase_id = 60)
  expect_s3_class(result, "pheno_gradient")
  expect_true(is.list(result))
})

test_that("pheno_gradient() result has expected structure", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus", phase_id = 60)
  expect_true(all(c("summary", "model", "data", "variable", "method") %in% names(result)))
})

test_that("pheno_gradient() summary has expected columns", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus", phase_id = 60)
  expected_cols <- c("slope", "intercept", "r_squared", "rmse",
                     "n_points", "interpretation")
  expect_true(all(expected_cols %in% names(result$summary)))
})

test_that("pheno_gradient() latitude gradient works", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "lat", species = "Malus", phase_id = 60)
  expect_equal(result$variable, "lat")
  expect_false(is.na(result$summary$slope))
})

test_that("pheno_gradient() OLS method returns lm model", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus",
                           phase_id = 60, method = "ols")
  expect_s3_class(result$model, "lm")
})

test_that("pheno_gradient() robust method returns lmrob model", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus",
                           phase_id = 60, method = "robust")
  expect_s3_class(result$model, "lmrob")
})

test_that("pheno_gradient() by = 'country' produces multiple summary rows", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus",
                           phase_id = 60, by = "country")
  expect_gt(nrow(result$summary), 0)
  expect_true("country" %in% names(result$summary))
})

test_that("pheno_gradient() r_squared is between 0 and 1", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus", phase_id = 60)
  r2 <- result$summary$r_squared
  expect_true(!is.na(r2) && r2 >= 0 && r2 <= 1)
})

test_that("pheno_gradient() interpretation text is populated", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  result <- pheno_gradient(p, variable = "alt", species = "Malus", phase_id = 60)
  expect_true(nchar(result$summary$interpretation[1]) > 0)
})

test_that("pheno_gradient() errors on invalid input", {
  expect_error(pheno_gradient("not a df"), "must be a data.frame")
  data(pep_seed, package = "pep725")
  # Remove alt column to trigger altitude error
  dt <- data.table::copy(pep_seed)
  dt[, alt := NULL]
  expect_error(pheno_gradient(dt, variable = "alt"), "Altitude column not found")
})
