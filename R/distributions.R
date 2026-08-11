#' Subset a panel to one group/period cell
#'
#' @param data A data frame.
#' @param gname Name of the group column.
#' @param tname Name of the time column.
#' @param group Group value to keep.
#' @param t Time value to keep.
#' @return A subset of `data`.
#' @noRd
.gt_subset <- function(data, gname, tname, group, t) {
  data[data[[gname]] == group & data[[tname]] == t, , drop = FALSE]
}

#' Empirical CDFs and the Changes-in-Changes counterfactual
#'
#' Computes the empirical distribution functions of the outcome in each of
#' the four group/period cells of a two-period panel, together with the
#' Changes-in-Changes (CiC) counterfactual distribution for the treated
#' group in the post-treatment period (Athey and Imbens 2006) and the
#' resulting average treatment effect on the treated (ATT).
#'
#' @param data A data frame containing a two-period panel.
#' @param yname Name of the outcome column.
#' @param tname Name of the time column.
#' @param gname Name of the (time-invariant) group column, coded 0
#'   (control) / 1 (treated).
#' @param t Value of `tname` identifying the post-treatment period.
#' @param tmin1 Value of `tname` identifying the pre-treatment period.
#'
#' @return A list with components:
#'   \describe{
#'     \item{F_pre_treated, F_post_treated}{`ecdf` objects for the treated
#'       group, pre- and post-treatment.}
#'     \item{F_pre_control, F_post_control}{`ecdf` objects for the control
#'       group, pre- and post-treatment.}
#'     \item{F_cf}{`ecdf` of the CiC counterfactual outcome for the treated
#'       group in the post-treatment period.}
#'     \item{att}{The CiC estimate of the average treatment effect on the
#'       treated.}
#'   }
#' @references Athey, S. and Imbens, G. W. (2006), "Identification and
#'   Inference in Nonlinear Difference-in-Differences Models,"
#'   *Econometrica*, 74(2), 431-497.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y = rnorm(200),
#'   time = rep(1:2, each = 100),
#'   group = rep(rep(0:1, each = 50), 2)
#' )
#' cdfs <- empirical_cdfs(df, yname = "y", tname = "time", gname = "group",
#'                        t = 2, tmin1 = 1)
#' cdfs$att
#' @export
empirical_cdfs <- function(data, yname, tname, gname, t, tmin1) {
  pre_treated <- .gt_subset(data, gname, tname, 1, tmin1)
  post_treated <- .gt_subset(data, gname, tname, 1, t)
  pre_control <- .gt_subset(data, gname, tname, 0, tmin1)
  post_control <- .gt_subset(data, gname, tname, 0, t)

  F_pre_treated <- stats::ecdf(pre_treated[[yname]])
  F_post_treated <- stats::ecdf(post_treated[[yname]])
  F_pre_control <- stats::ecdf(pre_control[[yname]])
  F_post_control <- stats::ecdf(post_control[[yname]])

  cf_sample <- stats::quantile(
    F_post_control,
    probs = F_pre_control(pre_treated[[yname]]),
    type = 1
  )
  F_cf <- stats::ecdf(cf_sample)

  att <- mean(post_treated[[yname]]) - mean(cf_sample)

  list(
    F_pre_treated = F_pre_treated,
    F_post_treated = F_post_treated,
    F_pre_control = F_pre_control,
    F_post_control = F_post_control,
    F_cf = F_cf,
    att = att
  )
}
