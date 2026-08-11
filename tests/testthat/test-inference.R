test_that("imbens_manski_ci reduces to the standard normal CI when lb == ub", {
  # With a degenerate (point-identified) interval, the Imbens-Manski
  # critical value solves pnorm(Z) - pnorm(-Z) = coverage, i.e. the usual
  # two-sided normal critical value.
  out <- imbens_manski_ci(lb = 2, ub = 2, se_lb = 1, se_ub = 1,
                           coverage = 0.95)
  z <- stats::qnorm(0.975)
  expect_equal(out$ci_lb, 2 - z * 1, tolerance = 1e-4)
  expect_equal(out$ci_ub, 2 + z * 1, tolerance = 1e-4)
})

test_that("imbens_manski_ci is vectorized and always contains [lb, ub]", {
  out <- imbens_manski_ci(
    lb = c(0, -1, 2), ub = c(1, 0, 2.5),
    se_lb = c(0.1, 0.2, 0.05), se_ub = c(0.1, 0.15, 0.05)
  )
  expect_equal(nrow(out), 3)
  expect_true(all(out$ci_lb <= c(0, -1, 2)))
  expect_true(all(out$ci_ub >= c(1, 0, 2.5)))
})

test_that("imbens_manski_ci errors on mismatched lengths", {
  expect_error(
    imbens_manski_ci(lb = c(0, 1), ub = 1, se_lb = 1, se_ub = 1),
    "same length"
  )
})
