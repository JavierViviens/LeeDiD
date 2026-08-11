test_that("leedid() runs end to end and returns internally consistent output", {
  df <- .make_test_panel()
  fit <- leedid(df, yname = "y", tname = "time", idname = "id",
                gname = "group", selection_vars = c("employed", "responded"),
                t = 2, tmin1 = 1, boot = FALSE)

  expect_s3_class(fit, "leedid")
  expect_true(fit$pi_0 >= 0 && fit$pi_0 <= 1)
  expect_true(fit$pi_1 >= 0 && fit$pi_1 <= 1)
  expect_true(all(fit$qtt_bounds$lb <= fit$qtt_bounds$ub + 1e-8))
  expect_true(fit$att_bounds["lb"] <= fit$att_bounds["ub"] + 1e-8)
})

test_that("leedid() with bootstrap produces positive SEs and CIs containing the bounds", {
  skip_on_cran()
  df <- .make_test_panel()
  fit <- leedid(df, yname = "y", tname = "time", idname = "id",
                gname = "group", selection_vars = c("employed", "responded"),
                t = 2, tmin1 = 1, boot = TRUE, biters = 30, seed = 1,
                quiet = TRUE)

  expect_true(fit$se$att > 0)
  expect_true(fit$se$att_lb > 0)
  expect_true(fit$se$att_ub > 0)
  expect_true(fit$ci$att_bounds$ci_lb <= fit$att_bounds["lb"] + 1e-8)
  expect_true(fit$ci$att_bounds$ci_ub >= fit$att_bounds["ub"] - 1e-8)
})

test_that("leedid() validates its inputs", {
  df <- .make_test_panel()
  expect_error(
    leedid(df, yname = "not_a_column", tname = "time", idname = "id",
           gname = "group", selection_vars = c("employed", "responded"),
           t = 2, tmin1 = 1, boot = FALSE),
    "not found"
  )
  expect_error(
    leedid(df, yname = "y", tname = "time", idname = "id", gname = "group",
           selection_vars = "employed", t = 2, tmin1 = 1, boot = FALSE),
    "at least two"
  )
  expect_error(
    leedid(df, yname = "y", tname = "time", idname = "id", gname = "group",
           selection_vars = c("employed", "responded"), t = 3, tmin1 = 1,
           boot = FALSE),
    "values of"
  )
})

test_that("bootstrap_se recomputes AO shares from the resampled data (regression guard)", {
  # Historical bug in the original research script: the AO-share
  # computation inside the bootstrap loop referenced a global `paneldata`
  # object instead of the function's own (resampled) data argument, so the
  # bootstrap silently never re-estimated the AO shares. Guard against ever
  # reintroducing that pattern.
  src <- paste(deparse(body(bootstrap_se)), collapse = "\n")
  expect_false(grepl("paneldata", src, fixed = TRUE))
  expect_true(grepl("compute_ao_share(boot_data", src, fixed = TRUE))
})

test_that("bootstrap_se is sensitive to the resampled data (not frozen on the original sample)", {
  skip_on_cran()
  df <- .make_test_panel()
  out1 <- bootstrap_se(df, yname = "y", tname = "time", idname = "id",
                        gname = "group",
                        selection_vars = c("employed", "responded"),
                        t = 2, tmin1 = 1, quantiles = c(0.5), biters = 25,
                        seed = 1, quiet = TRUE)
  out2 <- bootstrap_se(df, yname = "y", tname = "time", idname = "id",
                        gname = "group",
                        selection_vars = c("employed", "responded"),
                        t = 2, tmin1 = 1, quantiles = c(0.5), biters = 25,
                        seed = 2, quiet = TRUE)
  # Different seeds must not produce bit-identical SEs (a frozen/unresampled
  # AO-share computation would make several downstream quantities constant
  # across bootstrap replications regardless of the seed).
  expect_false(isTRUE(all.equal(out1$se, out2$se)))
})
