test_that("summary.pep produces output", {
  data(pep_seed)
  pep <- new_pep(pep_seed)
  expect_output(summary(pep))
})

test_that("summary.pep by=species produces output", {
  data(pep_seed)
  pep <- new_pep(pep_seed)
  expect_output(summary(pep, by = "species"))
})

test_that("summary.pep by=phase produces output", {
  data(pep_seed)
  pep <- new_pep(pep_seed)
  expect_output(summary(pep, by = "phase"))
})

test_that("print.pheno_normals produces output", {
  data(pep_seed)
  pep <- new_pep(pep_seed)
  normals <- pheno_normals(pep, period = 2000:2015, by = "phase_id")
  expect_output(print(normals))
})

test_that("print.pep_quality produces output", {
  data(pep_seed)
  pep <- new_pep(pep_seed)
  quality <- pep_quality(pep, by = c("s_id", "phase_id"))
  expect_output(print(quality))
})
