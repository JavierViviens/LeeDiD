## Simulate a small panel matching the shape LeeDiD expects, for use in
## examples, tests, and the introductory vignette. Not derived from any
## real data. Selection intercepts are tuned so the estimated
## Always-Observed shares land in a realistic, moderate range
## (roughly 0.8-0.95) rather than near-degenerate (~0 or ~1) or negative
## (which would signal a monotonicity violation).
library(dplyr)

set.seed(20260717)

n <- 800
group <- rep(c(0, 1), each = n / 2)
u <- rnorm(n) # permanent unit-level latent skill/productivity
tau <- rnorm(n, mean = 0.30, sd = 0.15) # heterogeneous treatment effect

panel <- tibble(
  id = rep(seq_len(n), each = 2),
  time = rep(1:2, times = n),
  group = rep(group, each = 2),
  u = rep(u, each = 2),
  tau = rep(tau, each = 2)
) %>%
  mutate(
    post = as.integer(time == 2),
    y_star = u + 0.2 * post + group * post * tau + rnorm(dplyr::n(), sd = 0.4),
    p_employed = plogis(2.0 + 0.4 * u),
    p_responded = plogis(2.0 + 0.25 * u),
    employed = rbinom(dplyr::n(), 1, p_employed),
    responded = rbinom(dplyr::n(), 1, p_responded),
    y = ifelse(employed == 1 & responded == 1, y_star, NA_real_)
  ) %>%
  select(id, time, group, y, employed, responded)

leedid_sim <- as.data.frame(panel)

usethis::use_data(leedid_sim, overwrite = TRUE)
