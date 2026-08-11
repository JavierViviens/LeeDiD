#' Share of persistently selected units on one indicator, within a group
#'
#' @param data A data frame already filtered to a single group.
#' @param idname Name of the unit id column.
#' @param tname Name of the time column.
#' @param varname Name of the (0/1) selection-indicator column.
#' @param t Post-treatment period value.
#' @param tmin1 Pre-treatment period value.
#' @return A single number: the share of ids with `varname == 1` in both
#'   `tmin1` and `t`.
#' @noRd
.persistence_rate <- function(data, idname, tname, varname, t, tmin1) {
  data %>%
    dplyr::group_by(.data[[idname]]) %>%
    dplyr::summarise(
      .persistent = any(.data[[tname]] == tmin1 & .data[[varname]] == 1) &
        any(.data[[tname]] == t & .data[[varname]] == 1),
      .groups = "drop"
    ) %>%
    dplyr::pull(.data$.persistent) %>%
    mean()
}

#' Share of "Always Observed" units in each group
#'
#' Estimates the proportion of units in each group (`pi_0` for the control
#' group, `pi_1` for the treated group) that would be observed in the
#' estimation sample under *either* treatment arm ("Always Observed", AO
#' units), following the Horowitz-Manski-Lee bounds approach adapted to
#' allow for **multiple, jointly required sources of sample selection**
#' (e.g., employment status *and* survey response).
#'
#' A unit is treated as observed at a given period if and only if all of
#' `selection_vars` equal 1 in that period (their indicator product). For
#' each individual selection source, the share of units that would have
#' been selected under the counterfactual treatment arm is extrapolated
#' from the other group's observed-to-pre-period selection ratio, in the
#' same spirit as the Changes-in-Changes counterfactual construction. This
#' requires a strictly positive pre-period selection rate on every source,
#' in both groups, and at least one unit per group observed on all sources
#' in both periods.
#'
#' @inheritParams empirical_cdfs
#' @param idname Name of the unit id column.
#' @param selection_vars Character vector of at least two (0/1)
#'   selection-indicator column names. A unit is "observed" at a period iff
#'   all of these equal 1.
#'
#' @return A list with components `pi_0`, `pi_1` (the estimated AO shares
#'   for the control and treated groups) and `detail`, a data frame with
#'   the per-source rates used to construct them.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   id = rep(1:200, each = 2),
#'   time = rep(1:2, 200),
#'   group = rep(rep(0:1, each = 2), 100),
#'   employed = rbinom(400, 1, 0.8),
#'   responded = rbinom(400, 1, 0.9)
#' )
#' compute_ao_share(df, idname = "id", tname = "time", gname = "group",
#'                   selection_vars = c("employed", "responded"),
#'                   t = 2, tmin1 = 1)
#' @export
compute_ao_share <- function(data, idname, tname, gname, selection_vars,
                              t, tmin1) {
  if (length(selection_vars) < 2) {
    stop("`selection_vars` must contain at least two selection sources.")
  }

  data[[".leedid_S"]] <- Reduce(`*`, data[selection_vars])

  detail <- lapply(selection_vars, function(sv) {
    pre <- vapply(0:1, function(g) {
      rows <- data[data[[gname]] == g & data[[tname]] == tmin1, ]
      mean(rows[[sv]])
    }, numeric(1))

    obs <- vapply(0:1, function(g) {
      rows <- data[data[[gname]] == g, ]
      .persistence_rate(rows, idname, tname, sv, t, tmin1)
    }, numeric(1))

    # Extrapolate the counterfactual selection rate for group g from the
    # other group's observed-to-pre-period ratio (CiC-style scaling).
    mis <- vapply(0:1, function(g) {
      other <- 1 - g
      (obs[other + 1] / pre[other + 1]) * pre[g + 1]
    }, numeric(1))

    data.frame(source = sv, group = 0:1, pre = pre, obs = obs, mis = mis)
  })
  detail <- do.call(rbind, detail)

  observed <- vapply(0:1, function(g) {
    rows <- data[data[[gname]] == g, ]
    .persistence_rate(rows, idname, tname, ".leedid_S", t, tmin1)
  }, numeric(1))

  if (any(observed == 0)) {
    stop("No unit in at least one group is observed (selected on all of ",
         "`selection_vars`) in both periods, so the AO share is undefined ",
         "for that group.")
  }

  loss <- vapply(0:1, function(g) {
    sub <- detail[detail$group == g, ]
    sum(1 - pmin(sub$obs, sub$mis))
  }, numeric(1))

  pi <- (1 - loss) / observed
  names(pi) <- c("pi_0", "pi_1")

  if (any(pi < 0)) {
    warning("Estimated AO share is negative for at least one group; ",
            "the joint selection/monotonicity assumption may be violated. ",
            "Clamping to 0.")
  }
  pi[pi < 0] <- 0
  pi[pi > 1] <- 1

  list(pi_0 = unname(pi["pi_0"]), pi_1 = unname(pi["pi_1"]), detail = detail)
}
