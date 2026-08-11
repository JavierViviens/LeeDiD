#' Rank-trim a panel into the AO lower- and upper-bound samples
#'
#' Restricts to units with a defined long difference in the outcome (i.e.
#' observed at both `tmin1` and `t`), ranks them within group by that long
#' difference, and builds the two trimmed samples used to bound the
#' intensive margin DiD estimate: `lb_data` keeps the top of the control
#' distribution and the bottom `pi_1` share of the treated distribution;
#' `ub_data` keeps the bottom of the control distribution and the top
#' `pi_0`/`pi_1`-implied share of the treated distribution.
#'
#' @inheritParams empirical_cdfs
#' @param idname Name of the unit id column.
#' @param pi_0,pi_1 Estimated AO shares for the control and treated groups.
#' @return A list with `finaldata` (all units with a defined long
#'   difference), `lb_data`, and `ub_data`.
#' @noRd
.trim_samples <- function(data, idname, tname, gname, yname, t, tmin1,
                           pi_0, pi_1) {
  wide_diff <- data %>%
    dplyr::group_by(.data[[idname]]) %>%
    dplyr::mutate(
      .diff_y = .data[[yname]][.data[[tname]] == t] -
        .data[[yname]][.data[[tname]] == tmin1]
    ) %>%
    dplyr::ungroup()

  finaldata <- wide_diff[!is.na(wide_diff$.diff_y), ]
  finaldata <- finaldata %>%
    dplyr::group_by(.data[[gname]], .data[[tname]]) %>%
    dplyr::mutate(.rank = rank(.data$.diff_y, ties.method = "first")) %>%
    dplyr::ungroup()

  n_control <- sum(finaldata[[gname]] == 0 & finaldata[[tname]] == t)
  n_treated <- sum(finaldata[[gname]] == 1 & finaldata[[tname]] == t)

  q_ub_0 <- round(n_control * pi_0)
  q_ub_1 <- round(n_treated * pi_1)
  q_lb_0 <- n_control - q_ub_0 + 1
  q_lb_1 <- n_treated - q_ub_1 + 1

  lb_data <- finaldata[
    (finaldata[[gname]] == 0 & finaldata$.rank >= q_lb_0) |
      (finaldata[[gname]] == 1 & finaldata$.rank <= q_ub_1),
  ]
  ub_data <- finaldata[
    (finaldata[[gname]] == 0 & finaldata$.rank <= q_ub_0) |
      (finaldata[[gname]] == 1 & finaldata$.rank >= q_lb_1),
  ]

  list(finaldata = finaldata, lb_data = lb_data, ub_data = ub_data)
}
