test_that("pep_check_phases returns phase_check class", {
  data(pep_seed)
  result <- pep_check_phases(pep_seed, warn = FALSE)
  expect_s3_class(result, "phase_check")
  expect_true("expected" %in% names(result))
  expect_true("present" %in% names(result))
  expect_true("missing" %in% names(result))
  expect_true("complete" %in% names(result))
})

test_that("pep_check_phases identifies present phases correctly", {
  data(pep_seed)
  phases_in_data <- sort(unique(pep_seed$phase_id))
  result <- pep_check_phases(pep_seed, expected = phases_in_data, warn = FALSE)
  expect_true(result$complete)
  expect_length(result$missing, 0)
})

test_that("pep_check_phases detects missing phases", {
  data(pep_seed)
  result <- pep_check_phases(pep_seed, expected = c(999), warn = FALSE)
  expect_false(result$complete)
  expect_equal(result$missing, 999L)
})

test_that("pep_check_phases species filter works", {
  data(pep_seed)
  result <- pep_check_phases(pep_seed, species = "Vitis vinifera", warn = FALSE)
  expect_s3_class(result, "phase_check")
})

test_that("pep_check_phases_multi returns correct structure", {
  data(pep_seed)
  species_list <- unique(pep_seed$species)[1:2]
  result <- pep_check_phases_multi(pep_seed,
                                   species_list = species_list,
                                   expected = c(60, 65),
                                   warn = FALSE)
  expect_s3_class(result, "phase_check_multi")
  expect_equal(nrow(result), length(species_list))
  expect_true("complete" %in% names(result))
})
