test_that("pep_coverage returns pep_coverage object", {
  data(pep_seed)
  result <- pep_coverage(pep_seed)
  expect_s3_class(result, "pep_coverage")
})

test_that("pep_coverage kind=species works", {
  data(pep_seed)
  result <- pep_coverage(pep_seed, kind = "species")
  expect_s3_class(result, "pep_coverage")
})

test_that("pep_coverage kind=temporal works", {
  data(pep_seed)
  result <- pep_coverage(pep_seed, kind = "temporal")
  expect_s3_class(result, "pep_coverage")
})
