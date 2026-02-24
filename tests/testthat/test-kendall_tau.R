test_that("kendall_tau positive for increasing sequence", {
  result <- kendall_tau(c(100, 105, 110, 115, 120, 125, 130))
  expect_true(result > 0)
})

test_that("kendall_tau negative for decreasing sequence", {
  result <- kendall_tau(c(130, 125, 120, 115, 110, 105, 100))
  expect_true(result < 0)
})

test_that("kendall_tau returns NA for length < 3", {
  expect_true(is.na(kendall_tau(c(1, 2))))
  expect_true(is.na(kendall_tau(c(1))))
  expect_true(is.na(kendall_tau(numeric(0))))
})
