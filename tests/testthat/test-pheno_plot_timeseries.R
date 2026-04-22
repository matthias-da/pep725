test_that("pheno_plot_timeseries does not mutate the caller's data", {
  # Regression test for the setDT(data) caller-mutation bug.
  # A data.frame passed in must remain a data.frame after the call.

  df <- data.frame(
    year = 1990:2020,
    day = 100 + rnorm(31, 0, 3),
    species = "Malus domestica"
  )
  class_before <- class(df)

  p <- pheno_plot_timeseries(df)

  expect_identical(class(df), class_before)
  expect_false("data.table" %in% class(df))
  expect_s3_class(p, "ggplot")
})

test_that("pheno_plot_timeseries accepts a data.table without side-effects", {
  dt <- data.table::data.table(
    year = 1990:2020,
    day = 100 + rnorm(31, 0, 3),
    species = "Malus domestica"
  )
  # data.table already has the right class; we just want to make sure the
  # function doesn't error and returns a ggplot.
  p <- pheno_plot_timeseries(dt)
  expect_s3_class(p, "ggplot")
})
