test_that("sequential_mk progressive u(t) has E[t] = i(i-1)/4 and Var[t] = i(i-1)(2i+5)/72", {
  # Under H0 (no trend), the rank-count-based statistic t_i has
  #   E[t_i]   = i(i-1)/4
  #   Var[t_i] = i(i-1)(2i+5)/72
  # (Sneyers 1975; Mann-Kendall under the null).
  # For the monotone series 1, 2, ..., n we know n_i = i - 1, so
  #   t_i = sum_{k=1}^{i} (k - 1) = i(i-1)/2
  # which gives u(t_i) = (t_i - E[t_i]) / sqrt(Var[t_i]).
  x <- 1:20
  result <- pep725:::sequential_mk(x)

  i <- seq_along(x)
  t_expected <- i * (i - 1) / 2
  E_t <- i * (i - 1) / 4
  V_t <- i * (i - 1) * (2 * i + 5) / 72
  u_expected <- c(0, (t_expected[-1] - E_t[-1]) / sqrt(V_t[-1]))

  expect_equal(result$progressive, u_expected, tolerance = 1e-8)
})

test_that("sequential_mk final progressive u matches Mann-Kendall Z (without continuity correction)", {
  # At i = n, the Sneyers u(t_n) under no ties equals S / sqrt(Var(S))
  # — i.e. the standard Mann-Kendall Z WITHOUT continuity correction.
  # (mann_kendall_z() applies the (S - sign(S)) continuity correction,
  # so we reconstruct the uncorrected Z by hand here.)
  set.seed(7)
  x <- rnorm(30) + seq_len(30) * 0.05
  result <- pep725:::sequential_mk(x)
  d <- outer(x, x, "-")
  s  <- sum(sign(d[lower.tri(d)]))
  vs <- length(x) * (length(x) - 1) * (2 * length(x) + 5) / 18
  z_uncorr <- s / sqrt(vs)
  expect_equal(result$progressive[length(x)], z_uncorr, tolerance = 1e-8)
})

test_that("sequential_mk retrograde is the forward u on the reversed (then reversed back)", {
  # Moraes et al. 1998 convention: u'(t) = rev(u(rev(x))), no negation.
  # Under this convention, prog and retr fluctuate around zero under H0
  # and diverge symmetrically under a monotone trend.
  set.seed(11)
  x <- rnorm(25)
  r <- pep725:::sequential_mk(x)
  r_rev <- pep725:::sequential_mk(rev(x))
  expect_equal(r$retrograde, rev(r_rev$progressive), tolerance = 1e-8)
})

test_that("sequential_mk detects an injected change point near the correct position", {
  # Flat-with-noise for the first 20 years, then a gentle positive drift
  # in the last 10 years. Under Sneyers, u and u' should cross inside the
  # 95% non-significance band near the change. The signal is deliberately
  # weak so the crossing stays within the band (a strong trend would
  # produce the divergence Sneyers expects, outside the band).
  set.seed(42)
  flat <- 100 + rnorm(20, sd = 2)
  rise <- 100 + seq_len(10) * 0.5 + rnorm(10, sd = 2)
  x <- c(flat, rise)
  result <- pep725:::sequential_mk(x)
  tp_idx <- which(result$turning_points)
  # At least one turning point must be flagged, and it must be within
  # 10 years of the injected change (index 20).
  expect_true(length(tp_idx) >= 1)
  expect_true(any(abs(tp_idx - 20) <= 10))
})

test_that("sequential_mk on a pure monotone trend produces no in-band crossings", {
  # A strong monotone trend must not flag a Sneyers turning point: the
  # curves diverge symmetrically and do not cross inside the band.
  x <- 1:30
  result <- pep725:::sequential_mk(x)
  expect_false(any(result$turning_points))
})

test_that("sequential_mk handles short series by returning NA vectors", {
  r <- pep725:::sequential_mk(c(1, 2, 3))
  expect_true(all(is.na(r$progressive)))
  expect_true(all(is.na(r$retrograde)))
  expect_false(any(r$turning_points))
})
