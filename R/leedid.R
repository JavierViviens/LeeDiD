#' Validate the inputs to `leedid()`
#' @noRd
.check_leedid_data <- function(data, yname, tname, idname, gname,
                                selection_vars, t, tmin1) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  required <- c(yname, tname, idname, gname, selection_vars)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "))
  }
  if (length(selection_vars) < 2) {
    stop("`selection_vars` must contain at least two selection sources.")
  }
  if (!all(c(t, tmin1) %in% data[[tname]])) {
    stop("Both `t` and `tmin1` must be values of the `", tname, "` column.")
  }
  if (!all(data[[gname]] %in% c(0, 1))) {
    stop("`", gname, "` must be coded 0 (control) / 1 (treated).")
  }
  n_periods <- table(data[[idname]])
  if (!all(n_periods == 2)) {
    stop("`leedid()` requires a balanced two-period panel: every `",
         idname, "` must appear exactly twice (", tmin1, " and ", t, ").")
  }
  invisible(TRUE)
}

#' Estimate the intensive margin treatment effect under joint selection
#'
#' End-to-end estimator for the average and quantile intensive-margin
#' treatment effect on the treated, partially identified among
#' "Always-Observed" units by adapting the Horowitz-Manski-Lee bounds to
#' the Changes-in-Changes framework, using multiple jointly required
#' sources of sample selection (Viviens, 2026).
#'
#' @inheritParams empirical_cdfs
#' @inheritParams compute_ao_share
#' @param quantiles Numeric vector of quantiles in (0, 1) at which to
#'   estimate the intensive margin QTT (bounds). Default
#'   `seq(0.05, 0.95, 0.05)`.
#' @param boot Logical; compute bootstrap standard errors and confidence
#'   intervals? Default `TRUE`.
#' @param biters Number of bootstrap replications, if `boot = TRUE`.
#' @param coverage Desired coverage probability for the Imbens-Manski
#'   confidence intervals. Default `0.95`.
#' @param seed Optional integer seed for the bootstrap.
#' @param quiet Suppress bootstrap progress messages? Default `FALSE`.
#'
#' @return An object of class `"leedid"`: a list with the AO shares
#'   (`pi_0`, `pi_1`), the unconditional CiC ATT and QTT, the AO att/QTT
#'   bounds, the classical two-way fixed-effects DiD point/bound
#'   comparison estimates, standard errors and confidence intervals (if
#'   `boot = TRUE`), and the matched call.
#' @examples
#' data(leedid_sim)
#' fit <- leedid(
#'   leedid_sim,
#'   yname = "y", tname = "time", idname = "id", gname = "group",
#'   selection_vars = c("employed", "responded"),
#'   t = 2, tmin1 = 1, boot = FALSE
#' )
#' fit
#' @export
leedid <- function(data, yname, tname, idname, gname, selection_vars,
                    t, tmin1, quantiles = seq(0.05, 0.95, 0.05),
                    boot = TRUE, biters = 999, coverage = 0.95,
                    seed = NULL, quiet = FALSE) {
  .check_leedid_data(data, yname, tname, idname, gname, selection_vars,
                      t, tmin1)

  pis <- compute_ao_share(data, idname, tname, gname, selection_vars,
                           t, tmin1)
  trimmed <- .trim_samples(data, idname, tname, gname, yname, t, tmin1,
                            pis$pi_0, pis$pi_1)
  epc <- empirical_cdfs(trimmed$finaldata, yname, tname, gname, t, tmin1)
  qtt <- cic_qtt(epc, quantiles)
  bounds <- cic_bounds(epc, quantiles, pis$pi_0, pis$pi_1)

  did <- .fit_did(trimmed, yname, tname, idname, gname, t)

  out <- list(
    call = match.call(),
    pi_0 = pis$pi_0,
    pi_1 = pis$pi_1,
    att = epc$att,
    qtt = data.frame(quantile = quantiles, estimate = qtt),
    att_bounds = c(lb = mean(bounds$lb), ub = mean(bounds$ub)),
    qtt_bounds = bounds,
    did = did,
    n = list(
      control = sum(trimmed$finaldata[[gname]] == 0 &
                      trimmed$finaldata[[tname]] == t),
      treated = sum(trimmed$finaldata[[gname]] == 1 &
                      trimmed$finaldata[[tname]] == t)
    ),
    se = NULL,
    ci = NULL,
    coverage = coverage
  )

  if (boot) {
    boot_out <- bootstrap_se(data, yname, tname, idname, gname,
                              selection_vars, t, tmin1, quantiles,
                              biters = biters, seed = seed, quiet = quiet)
    out$se <- boot_out$se
    out$ci <- list(
      att = imbens_manski_ci(out$att, out$att, boot_out$se$att,
                              boot_out$se$att, coverage),
      att_bounds = imbens_manski_ci(out$att_bounds["lb"],
                                    out$att_bounds["ub"],
                                    boot_out$se$att_lb, boot_out$se$att_ub,
                                    coverage),
      qtt_bounds = cbind(
        quantile = quantiles,
        imbens_manski_ci(bounds$lb, bounds$ub, boot_out$se_qtt$se_lb,
                          boot_out$se_qtt$se_ub, coverage)
      )
    )
  }

  class(out) <- "leedid"
  out
}

#' Classical two-way fixed-effects DiD, unconditional and AO-trimmed
#'
#' `gname` is time-invariant, so it cannot itself be used as the regressor
#' in a two-way fixed-effects model (it would be collinear with the unit
#' fixed effect). We construct the usual time-varying treatment dummy
#' `W = group * 1(time == t)` internally instead.
#' @noRd
.fit_did <- function(trimmed, yname, tname, idname, gname, t) {
  add_W <- function(df) {
    df$.leedid_W <- df[[gname]] * (df[[tname]] == t)
    df
  }
  fml <- stats::as.formula(
    paste(yname, "~ .leedid_W |", idname, "+", tname)
  )
  fit_all <- fixest::feols(fml, data = add_W(trimmed$finaldata), notes = FALSE)
  fit_lb <- fixest::feols(fml, data = add_W(trimmed$lb_data), notes = FALSE)
  fit_ub <- fixest::feols(fml, data = add_W(trimmed$ub_data), notes = FALSE)

  list(
    estimate = unname(stats::coef(fit_all)[1]),
    lb = unname(stats::coef(fit_lb)[1]),
    ub = unname(stats::coef(fit_ub)[1])
  )
}
