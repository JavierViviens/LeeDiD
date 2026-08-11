.make_test_panel <- function(n = 200, seed = 42) {
  set.seed(seed)
  group <- rep(c(0, 1), each = n / 2)
  u <- stats::rnorm(n)
  tau <- stats::rnorm(n, mean = 0.3, sd = 0.15)
  id <- rep(seq_len(n), each = 2)
  time <- rep(1:2, times = n)
  post <- as.integer(time == 2)
  group_long <- rep(group, each = 2)
  u_long <- rep(u, each = 2)
  tau_long <- rep(tau, each = 2)
  y_star <- u_long + 0.2 * post + group_long * post * tau_long +
    stats::rnorm(2 * n, sd = 0.4)
  p_employed <- stats::plogis(2.0 + 0.4 * u_long)
  p_responded <- stats::plogis(2.0 + 0.25 * u_long)
  employed <- stats::rbinom(2 * n, 1, p_employed)
  responded <- stats::rbinom(2 * n, 1, p_responded)
  y <- ifelse(employed == 1 & responded == 1, y_star, NA_real_)
  data.frame(id = id, time = time, group = group_long, y = y,
             employed = employed, responded = responded)
}
