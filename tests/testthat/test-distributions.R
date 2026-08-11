test_that("empirical_cdfs recovers a simple DiD-style ATT when pre distributions coincide", {
  # When the treated and control pre-period distributions are identical,
  # the CiC counterfactual reduces to the control post-period distribution
  # (up to a relabeling by rank), so the ATT should equal the simple
  # difference in post-period means.
  df <- data.frame(
    y = c(1, 2, 3, 2, 3, 4, 1, 2, 3, 3, 4, 5),
    time = rep(c(1, 1, 1, 2, 2, 2), 2),
    group = rep(c(0, 1), each = 6)
  )

  epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
                         t = 2, tmin1 = 1)

  expect_equal(epc$att, mean(c(3, 4, 5)) - mean(c(2, 3, 4)))
})

test_that("empirical_cdfs returns ecdf objects for all four cells", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(80),
    time = rep(1:2, each = 40),
    group = rep(rep(0:1, each = 20), 2)
  )
  epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
                         t = 2, tmin1 = 1)
  expect_s3_class(epc$F_pre_treated, "ecdf")
  expect_s3_class(epc$F_post_treated, "ecdf")
  expect_s3_class(epc$F_pre_control, "ecdf")
  expect_s3_class(epc$F_post_control, "ecdf")
  expect_s3_class(epc$F_cf, "ecdf")
  expect_true(is.numeric(epc$att))
})
