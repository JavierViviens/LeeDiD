test_that("cic_bounds widens (weakly) as the AO shares shrink", {
  # More aggressive trimming (smaller pi) should not produce a narrower
  # identified interval, on average across the quantile grid.
  set.seed(4)
  df <- data.frame(
    y = rnorm(400),
    time = rep(1:2, each = 200),
    group = rep(rep(0:1, each = 100), 2)
  )
  epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
                         t = 2, tmin1 = 1)
  q <- seq(0.1, 0.9, 0.1)

  width_full <- cic_bounds(epc, q, pi_0 = 1, pi_1 = 1)
  width_trimmed <- cic_bounds(epc, q, pi_0 = 0.7, pi_1 = 0.7)

  expect_true(mean(width_trimmed$ub - width_trimmed$lb) >=
                mean(width_full$ub - width_full$lb) - 1e-8)
})

test_that("cic_bounds always orders lb <= ub for pi in (0, 1]", {
  set.seed(3)
  df <- data.frame(
    y = rnorm(300),
    time = rep(1:2, each = 150),
    group = rep(rep(0:1, each = 75), 2)
  )
  epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
                         t = 2, tmin1 = 1)
  q <- seq(0.05, 0.95, 0.1)

  for (pi_1 in c(0.5, 0.75, 1)) {
    for (pi_0 in c(0.5, 0.75, 1)) {
      bounds <- cic_bounds(epc, q, pi_0 = pi_0, pi_1 = pi_1)
      expect_true(all(bounds$lb <= bounds$ub + 1e-8))
    }
  }
})
