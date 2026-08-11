test_that("print, summary, and plot methods work on a leedid object", {
  df <- .make_test_panel()
  fit <- leedid(df, yname = "y", tname = "time", idname = "id",
                gname = "group", selection_vars = c("employed", "responded"),
                t = 2, tmin1 = 1, boot = FALSE)

  expect_output(print(fit), "LeeDiD")
  expect_output(summary(fit), "Quantile treatment effect")

  p <- plot(fit)
  expect_s3_class(p, "ggplot")
})
