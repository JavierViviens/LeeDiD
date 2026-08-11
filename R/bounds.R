#' Changes-in-Changes quantile treatment effect on the treated
#'
#' The unconditional (no sample-selection correction) CiC quantile
#' treatment effect on the treated, at each quantile in `quantiles`.
#'
#' @param epc A list as returned by [empirical_cdfs()].
#' @param quantiles Numeric vector of quantiles in (0, 1) at which to
#'   evaluate the effect.
#' @return A numeric vector of the same length as `quantiles`.
#' @seealso [cic_bounds()] for the Always-Observed bounds version.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(200),
#'   time = rep(1:2, each = 100),
#'   group = rep(rep(0:1, each = 50), 2)
#' )
#' epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
#'                       t = 2, tmin1 = 1)
#' cic_qtt(epc, quantiles = c(0.25, 0.5, 0.75))
#' @export
cic_qtt <- function(epc, quantiles) {
  Q1 <- stats::quantile(epc$F_post_treated, probs = quantiles, type = 1)
  Q0 <- stats::quantile(epc$F_cf, probs = quantiles, type = 1)
  unname(Q1 - Q0)
}

#' Bounds on the intensive margin quantile treatment effect
#'
#' Lower and upper bounds on the quantile treatment effect on the treated
#' among Always-Observed units, combining the Horowitz-Manski-Lee trimming
#' argument with the Changes-in-Changes counterfactual distribution.
#'
#' @inheritParams cic_qtt
#' @param pi_0,pi_1 Estimated AO shares for the control and treated groups
#'   (see [compute_ao_share()]).
#'
#' @return A data frame with columns `quantile`, `lb`, and `ub`.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(200),
#'   time = rep(1:2, each = 100),
#'   group = rep(rep(0:1, each = 50), 2)
#' )
#' epc <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
#'                       t = 2, tmin1 = 1)
#' cic_bounds(epc, quantiles = c(0.25, 0.5, 0.75), pi_0 = 0.9, pi_1 = 0.85)
#' @export
cic_bounds <- function(epc, quantiles, pi_0, pi_1) {
  # Lower bound: bottom of the treated distribution vs. top of control.
  Q1_lb <- stats::quantile(epc$F_post_treated, probs = quantiles * pi_1,
                            type = 1)
  probs_lb <- epc$F_pre_control(
    stats::quantile(epc$F_pre_treated,
                     probs = quantiles * pi_1 + 1 - pi_1, type = 1)
  ) + 1 - pi_0
  probs_lb[probs_lb > 1] <- 1
  Q0_lb <- stats::quantile(epc$F_post_control, probs = probs_lb, type = 1)
  lb <- unname(Q1_lb - Q0_lb)

  # Upper bound: top of the treated distribution vs. bottom of control.
  Q1_ub <- stats::quantile(epc$F_post_treated,
                            probs = quantiles * pi_1 + 1 - pi_1, type = 1)
  probs_ub <- epc$F_pre_control(
    stats::quantile(epc$F_pre_treated, probs = quantiles * pi_1, type = 1)
  ) - 1 + pi_0
  probs_ub[probs_ub < 0] <- 0
  Q0_ub <- stats::quantile(epc$F_post_control, probs = probs_ub, type = 1)
  ub <- unname(Q1_ub - Q0_ub)

  data.frame(quantile = quantiles, lb = lb, ub = ub)
}
