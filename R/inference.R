#' Solve for the Imbens-Manski critical value at one identified-set boundary
#' @noRd
.im_root <- function(lb, ub, se_lb, se_ub, coverage) {
  if (se_lb == 0) se_lb <- 1e-100
  if (se_ub == 0) se_ub <- 1e-100
  f <- function(Z) {
    stats::pnorm(Z + (ub - lb) / max(se_lb, se_ub)) - stats::pnorm(-Z) - coverage
  }
  stats::uniroot(f, lower = 0, upper = 6)$root
}

#' Imbens-Manski confidence interval for a partially identified parameter
#'
#' Confidence interval for a parameter known only to lie in an estimated
#' interval `[lb, ub]`, following Imbens and Manski (2004). Vectorized: pass
#' vectors of bounds/standard errors (e.g. one pair per quantile) to get one
#' interval per element. For a point-identified parameter, pass `lb == ub`.
#'
#' @param lb,ub Estimates of the lower and upper bound of the identified set.
#' @param se_lb,se_ub Standard errors of `lb` and `ub`.
#' @param coverage Desired coverage probability of the identified set (not
#'   a significance level in the usual "alpha" sense) — e.g. `0.95` for a
#'   95% confidence interval. Default `0.95`.
#'
#' @return A data frame with columns `ci_lb` and `ci_ub`.
#' @references Imbens, G. W. and Manski, C. F. (2004), "Confidence Intervals
#'   for Partially Identified Parameters," *Econometrica*, 72(6), 1845-1857.
#' @examples
#' imbens_manski_ci(lb = 0.9, ub = 1.4, se_lb = 0.1, se_ub = 0.12)
#' @export
imbens_manski_ci <- function(lb, ub, se_lb, se_ub, coverage = 0.95) {
  n <- length(lb)
  if (length(ub) != n || length(se_lb) != n || length(se_ub) != n) {
    stop("`lb`, `ub`, `se_lb`, and `se_ub` must have the same length.")
  }
  z <- vapply(seq_len(n), function(i) {
    .im_root(lb[i], ub[i], se_lb[i], se_ub[i], coverage)
  }, numeric(1))
  data.frame(ci_lb = lb - z * se_lb, ci_ub = ub + z * se_ub)
}

#' Bootstrap standard errors for the LeeDiD estimates
#'
#' Resamples units (with replacement, keeping both periods of a unit
#' together) and recomputes the AO shares, the CiC point estimate, the AO
#' bounds, and (if `fixest` regressions are requested) the classical DiD
#' point/bound estimators, on each replicate. Standard errors are the
#' empirical standard deviations of the resampled statistics.
#'
#' @inheritParams empirical_cdfs
#' @inheritParams compute_ao_share
#' @param quantiles Numeric vector of quantiles at which to bootstrap the
#'   QTT bounds curve.
#' @param biters Number of bootstrap replications.
#' @param seed Optional integer seed, set once before resampling begins.
#' @param quiet If `FALSE` (default), prints one progress message every
#'   ~10% of replications.
#'
#' @return A list with `se` (a one-row data frame of standard errors for
#'   `att`, `att_lb`, `att_ub`) and `se_qtt` (a data frame with columns
#'   `quantile`, `se_lb`, `se_ub`).
#' @export
bootstrap_se <- function(data, yname, tname, idname, gname, selection_vars,
                          t, tmin1, quantiles, biters = 999, seed = NULL,
                          quiet = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  ids <- unique(data[[idname]])
  boot_att <- numeric(biters)
  boot_lb <- numeric(biters)
  boot_ub <- numeric(biters)
  boot_qtt_lb <- matrix(NA_real_, nrow = length(quantiles), ncol = biters)
  boot_qtt_ub <- matrix(NA_real_, nrow = length(quantiles), ncol = biters)

  next_report <- ceiling(biters / 10)

  for (b in seq_len(biters)) {
    repeat {
      boot_ids <- sample(ids, length(ids), replace = TRUE)
      boot_key <- data.frame(.boot_id = boot_ids, .draw = seq_along(boot_ids))
      names(boot_key)[1] <- idname
      boot_data <- merge(boot_key, data, by = idname, all.x = TRUE)
      # Re-key so resampled duplicate ids don't collide downstream.
      boot_data[[idname]] <- boot_data$.draw

      n_groups <- length(unique(boot_data[[gname]][
        boot_data[[tname]] == t
      ]))
      if (!is.na(n_groups) && n_groups == 2) break
    }

    # Recompute AO shares on the *resampled* data (this is the correctness
    # fix relative to the original research script, which mistakenly read
    # the AO shares off the original, unresampled data on every iteration).
    pis <- compute_ao_share(boot_data, idname, tname, gname, selection_vars,
                             t, tmin1)

    trimmed <- .trim_samples(boot_data, idname, tname, gname, yname, t,
                              tmin1, pis$pi_0, pis$pi_1)
    epc <- empirical_cdfs(trimmed$finaldata, yname, tname, gname, t, tmin1)
    bounds <- cic_bounds(epc, quantiles, pis$pi_0, pis$pi_1)

    boot_att[b] <- epc$att
    boot_lb[b] <- mean(bounds$lb)
    boot_ub[b] <- mean(bounds$ub)
    boot_qtt_lb[, b] <- bounds$lb
    boot_qtt_ub[, b] <- bounds$ub

    if (!quiet && b %% next_report == 0) {
      message(sprintf("Bootstrap: %d / %d replications done", b, biters))
    }
  }

  se <- data.frame(att = stats::sd(boot_att), att_lb = stats::sd(boot_lb),
                    att_ub = stats::sd(boot_ub))
  se_qtt <- data.frame(
    quantile = quantiles,
    se_lb = apply(boot_qtt_lb, 1, stats::sd),
    se_ub = apply(boot_qtt_ub, 1, stats::sd)
  )

  list(se = se, se_qtt = se_qtt)
}
