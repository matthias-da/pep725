test_that("new_pep() creates correct class from pep_seed", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_s3_class(p, "pep")
  expect_s3_class(p, "data.table")
  expect_s3_class(p, "data.frame")
  expect_equal(class(p), c("pep", "data.table", "data.frame"))
})

test_that("new_pep() converts plain data.frame to data.table", {
  data(pep_seed, package = "pep725")
  df <- as.data.frame(pep_seed)
  expect_false(inherits(df, "data.table"))
  p <- new_pep(df)
  expect_s3_class(p, "pep")
  expect_s3_class(p, "data.table")
})

test_that("new_pep() errors on missing required columns", {
  dt <- data.table::data.table(s_id = 1, lon = 8, lat = 47)
  expect_error(new_pep(dt), "Missing required columns")
})

test_that("new_pep() errors on non-numeric lon/lat", {
  dt <- data.table::data.table(
    s_id = 1, lon = "a", lat = "b", genus = "Malus",
    species = "Malus domestica", phase_id = 60, year = 2000, day = 100
  )
  expect_error(new_pep(dt), "lon.*lat.*must be numeric")
})

test_that("new_pep() errors on non-numeric year/day", {
  dt <- data.table::data.table(
    s_id = 1, lon = 8, lat = 47, genus = "Malus",
    species = "Malus domestica", phase_id = 60, year = "x", day = "y"
  )
  expect_error(new_pep(dt), "year.*day.*must be numeric")
})

test_that("new_pep() errors on non-numeric phase_id", {
  dt <- data.table::data.table(
    s_id = 1, lon = 8, lat = 47, genus = "Malus",
    species = "Malus domestica", phase_id = "abc", year = 2000, day = 100
  )
  expect_error(new_pep(dt), "phase_id.*must be numeric")
})

test_that("new_pep() with validate = FALSE skips validation", {
  dt <- data.table::data.table(x = 1)
  p <- new_pep(dt, validate = FALSE)
  expect_s3_class(p, "pep")
})

test_that("is.pep() returns TRUE for pep objects", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_true(is.pep(p))
})

test_that("is.pep() returns FALSE for non-pep objects", {
  expect_false(is.pep(data.frame(x = 1)))
  expect_false(is.pep(42))
})

test_that("as.pep() works as alias for new_pep()", {
  data(pep_seed, package = "pep725")
  p <- as.pep(pep_seed)
  expect_s3_class(p, "pep")
})

test_that("[.pep preserves class when required columns present", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  sub <- p[year >= 2000]
  expect_s3_class(sub, "pep")
})

test_that("[.pep drops class when required columns removed", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  sub <- p[, .(s_id, year)]
  expect_false(inherits(sub, "pep"))
})

test_that("[.pep drops class for zero-row result", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  sub <- p[year > 9999]
  expect_false(inherits(sub, "pep"))
})

test_that("bbch_description() returns data.frame with correct columns", {
  result <- bbch_description(c(60, 65, 100))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("phase_id", "description") %in% names(result)))
  expect_equal(nrow(result), 3)
})

test_that("bbch_description() handles NA codes", {
  result <- bbch_description(c(60, NA, 65), na.rm = TRUE)
  expect_false(any(is.na(result$phase_id)))
})

test_that("print.pep() produces output", {
  data(pep_seed, package = "pep725")
  p <- new_pep(pep_seed)
  expect_output(print(p), "PEP725 Phenological Data")
})
