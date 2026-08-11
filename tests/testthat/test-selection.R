test_that("compute_ao_share matches a hand-computed example", {
  # Group 0: ids 1-4, id 4 fails source s1 in period 2 (s2 always persists).
  # Group 1: ids 5-8, id 8 fails source s2 in period 2 (s1 always persists).
  # By construction: pre_s1 = pre_s2 = 1 in both groups;
  #   obs_s1_g0 = 3/4, obs_s2_g0 = 4/4, obs_s1_g1 = 4/4, obs_s2_g1 = 3/4;
  #   mis_s1_g0 = obs_s1_g1 = 1,   mis_s2_g0 = obs_s2_g1 = 0.75
  #   mis_s1_g1 = obs_s1_g0 = 0.75, mis_s2_g1 = obs_s2_g0 = 1
  #   loss_g0 = (1 - min(.75,1)) + (1 - min(1,.75)) = 0.5
  #   loss_g1 = (1 - min(1,.75)) + (1 - min(.75,1)) = 0.5
  # Joint S = s1 * s2 persists for ids {1,2,3} in group 0 and {5,6,7} in
  # group 1, so observed_g0 = observed_g1 = 3/4, giving
  #   pi_0 = pi_1 = (1 - 0.5) / 0.75 = 2/3.
  df <- data.frame(
    id = rep(1:8, each = 2),
    time = rep(1:2, 8),
    group = rep(c(0, 1), each = 8),
    s1 = c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1),
    s2 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  )

  out <- compute_ao_share(df, idname = "id", tname = "time", gname = "group",
                           selection_vars = c("s1", "s2"), t = 2, tmin1 = 1)

  expect_equal(out$pi_0, 2 / 3)
  expect_equal(out$pi_1, 2 / 3)
})

test_that("compute_ao_share clamps out-of-range shares and warns", {
  # Group 0 (ids 1-4): only id 1 persists on either source (obs = 0.25 for
  # both s1 and s2), but pre-period rates are 1 for both. Group 1 (ids
  # 5-8): ids 5-6 persist on both sources, 7-8 on neither (pre = obs = 0.5,
  # so the extrapolation ratio is 1). This drives the extrapolated `mis`
  # share for group 0 up to 1 for both sources while `obs` is only 0.25,
  # pushing the raw (pre-clamp) pi_0 to 1 - 1.5 = -0.5 (and pi_1 negative
  # too), which should be clamped to 0 with a warning.
  df <- data.frame(
    id = rep(1:8, each = 2),
    time = rep(1:2, 8),
    group = rep(c(0, 1), each = 8),
    s1 = c(1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0),
    s2 = c(1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  )
  expect_warning(
    out <- compute_ao_share(df, idname = "id", tname = "time",
                             gname = "group", selection_vars = c("s1", "s2"),
                             t = 2, tmin1 = 1),
    "negative"
  )
  expect_equal(out$pi_0, 0)
  expect_equal(out$pi_1, 0)
})

test_that("compute_ao_share requires at least two selection sources", {
  df <- data.frame(id = 1:4, time = rep(1:2, 2), group = rep(0:1, 2),
                    s1 = 1)
  expect_error(
    compute_ao_share(df, idname = "id", tname = "time", gname = "group",
                      selection_vars = "s1", t = 2, tmin1 = 1),
    "at least two"
  )
})
