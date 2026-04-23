make_multiphase_fixture <- function(seed = 1, n_stations = 40, n_years = 25,
                                    contaminate = TRUE) {
  set.seed(seed)
  stations <- data.table::data.table(
    s_id = seq_len(n_stations),
    alt  = runif(n_stations, 200, 2000),
    lat  = runif(n_stations, 45, 52),
    lon  = runif(n_stations, 5, 15)
  )
  years <- 1990:(1990 + n_years - 1)
  phases <- c(60L, 65L, 89L)   # three BBCH phases: start/peak/senescence
  grid <- data.table::CJ(s_id = stations$s_id, year = years, phase_id = phases)
  dat  <- merge(grid, stations, by = "s_id")

  # True phenology: each phase has a different baseline, with a small
  # correlated shift per station-year (shared weather signal) and i.i.d.
  # per-phase noise. The correlation across phases is the multivariate
  # signal that Mahalanobis should exploit.
  ref_by_phase <- c(`60` = 110, `65` = 120, `89` = 250)
  sy <- data.table::CJ(s_id = stations$s_id, year = years)
  sy[, shift := rnorm(.N, sd = 4)]
  dat <- merge(dat, sy, by = c("s_id", "year"))
  dat[, day := ref_by_phase[as.character(phase_id)] +
        0.02 * alt + shift + rnorm(.N, sd = 2)]
  dat[, `:=`(genus = "Malus", species = "Malus domestica", shift = NULL)]

  if (contaminate) {
    # Contamination target: station 1 in year 2000 has BBCH 60 = normal
    # (early spring), but BBCH 65 appears only 1 day later (impossible -
    # full-flowering is typically ~10 days after first-flowering). The
    # *marginal* DOY for phase 60 is fine; for phase 65 it's unusually
    # *early*. Neither is extreme on its own scale but the JOINT (60, 65)
    # vector is an outlier after accounting for the natural correlation.
    dat[s_id == 1 & year == 2000 & phase_id == 65L,
        day := dat[s_id == 1 & year == 2000 & phase_id == 60L, day] + 1]
  }
  dat[]
}

test_that("mahalanobis method flags a multivariate-but-marginal-OK outlier", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 1)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis",
    threshold = sqrt(qchisq(0.975, df = 3))
  )
  expect_s3_class(res, "pep_outliers")
  # The contaminated station-year must be flagged on all its phase rows
  # (Mahalanobis flags the whole station-year vector, so every
  # observation inside that station-year inherits the flag).
  hits <- res[s_id == 1 & year == 2000, is_outlier]
  expect_true(all(hits))
  # A randomly-chosen clean station-year should not be flagged.
  clean <- res[s_id == 10 & year == 2005, is_outlier]
  expect_false(any(clean))
})

test_that("mahalanobis stores per-observation MD in deviation column", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 2)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis",
    threshold = sqrt(qchisq(0.975, df = 3))
  )
  # Every row in the same station-year shares the same MD value.
  sy_mds <- res[, .(u = data.table::uniqueN(deviation)), by = .(s_id, year)]
  expect_true(all(sy_mds$u == 1))
  # MDs are non-negative and approximately chi-distributed in their bulk.
  expect_true(all(res$deviation >= 0, na.rm = TRUE))
  expect_lt(stats::median(res$deviation, na.rm = TRUE),
            sqrt(qchisq(0.5, df = 3)) * 3)
})

test_that("mahalanobis falls back gracefully on incomplete / degenerate groups", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 3, n_stations = 5, n_years = 5)
  # 75 observations across 3 phases -> 25 station-years; covMcd will
  # usually fit, but the group is too small per-phase for a meaningful
  # robust covariance. Should return a valid pep_outliers object without
  # erroring.
  expect_no_error(
    res <- pep_flag_outliers(
      dat,
      by = c("genus", "species"),
      method = "mahalanobis"
    )
  )
  expect_s3_class(res, "pep_outliers")
})

test_that("mahalanobis respects flag_only = FALSE", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 4)
  n_before <- nrow(dat)
  cleaned <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis",
    flag_only = FALSE
  )
  expect_lt(nrow(cleaned), n_before)
  expect_false("is_outlier" %in% names(cleaned))
})

test_that("pep_plot_outliers(type = 'profile') renders a ggplot for Mahalanobis", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 7)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis"
  )
  p <- pep_plot_outliers(res, type = "profile")
  expect_s3_class(p, "ggplot")
})

test_that("pep_plot_outliers(type = 'diagnostic') handles Mahalanobis without expected_doy", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 8)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis"
  )
  # Must not error even though expected_doy is all NA.
  expect_no_error(p <- pep_plot_outliers(res, type = "diagnostic"))
  # The diagnostic is a patchwork composition.
  expect_s3_class(p, "patchwork")
})

test_that("attr(result, 'threshold') is populated for Mahalanobis", {
  skip_if_not_installed("robustbase")
  dat <- make_multiphase_fixture(seed = 9)
  res <- pep_flag_outliers(
    dat,
    by = c("genus", "species"),
    method = "mahalanobis"
  )
  expect_true(is.finite(attr(res, "threshold")))
  # With 3 phases, the default cut-off is sqrt(qchisq(0.975, df = 3)).
  expect_equal(
    attr(res, "threshold"),
    sqrt(qchisq(0.975, df = 3)),
    tolerance = 1e-6
  )
})
