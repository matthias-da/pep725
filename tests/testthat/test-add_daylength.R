test_that("add_daylength adds daylength column", {
  data(pep_seed)
  result <- add_daylength(pep_seed)
  expect_true("daylength" %in% names(result))
  expect_true(all(result$daylength > 0 & result$daylength <= 24, na.rm = TRUE))
})

test_that("add_daylength errors when columns missing", {
  df <- data.frame(x = 1:5)
  expect_error(add_daylength(df), "day.*lat")
})
