#' Print a `leedid` object
#'
#' @param x A `leedid` object, as returned by [leedid()].
#' @param digits Number of digits to round to.
#' @param ... Unused, present for S3 consistency.
#' @return `x`, invisibly.
#' @export
print.leedid <- function(x, digits = 3, ...) {
  cat("LeeDiD: Intensive Margin Treatment Effect (Always-Observed units)\n")
  cat(sprintf("AO shares: pi_0 = %s, pi_1 = %s\n",
              round(x$pi_0, digits), round(x$pi_1, digits)))
  cat(sprintf("N (post-period): control = %d, treated = %d\n",
              x$n$control, x$n$treated))
  cat("\n")
  cat(sprintf("CiC ATT (unconditional): %s\n", round(x$att, digits)))
  cat(sprintf("AO ATT bounds:           [%s, %s]\n",
              round(x$att_bounds["lb"], digits),
              round(x$att_bounds["ub"], digits)))
  if (!is.null(x$se)) {
    cat(sprintf("  bootstrap SE:          %s\n", round(x$se$att, digits)))
    ci <- x$ci$att_bounds
    cat(sprintf("  %.0f%% CI:               [%s, %s]\n",
                100 * x$coverage,
                round(ci$ci_lb, digits), round(ci$ci_ub, digits)))
  }
  cat(sprintf("Classical DiD estimate:  %s [%s, %s] (AO bounds)\n",
              round(x$did$estimate, digits), round(x$did$lb, digits),
              round(x$did$ub, digits)))
  invisible(x)
}

#' Summarize a `leedid` object
#'
#' @param object A `leedid` object, as returned by [leedid()].
#' @param digits Number of digits to round to.
#' @param ... Unused, present for S3 consistency.
#' @return A list (invisibly) with the printed tables; also printed to the
#'   console.
#' @export
summary.leedid <- function(object, digits = 3, ...) {
  print(object, digits = digits)
  cat("\nQuantile treatment effect on the treated, AO bounds:\n")
  qtt_tab <- object$qtt_bounds
  if (!is.null(object$ci$qtt_bounds)) {
    qtt_tab <- cbind(qtt_tab, object$ci$qtt_bounds[, c("ci_lb", "ci_ub")])
  }
  print(round(qtt_tab, digits))
  invisible(list(att = object$att, att_bounds = object$att_bounds,
                 qtt_bounds = qtt_tab, did = object$did))
}

#' Plot the intensive margin QTT bounds
#'
#' Plots the estimated Always-Observed bounds on the quantile treatment
#' effect on the treated across the quantile grid, with the Imbens-Manski
#' confidence bands if available.
#'
#' @param x A `leedid` object, as returned by [leedid()].
#' @param ... Unused, present for S3 consistency.
#' @return A `ggplot` object.
#' @export
plot.leedid <- function(x, ...) {
  df <- x$qtt_bounds
  if (!is.null(x$ci$qtt_bounds)) {
    df$ci_lb <- x$ci$qtt_bounds[, "ci_lb"]
    df$ci_ub <- x$ci$qtt_bounds[, "ci_ub"]
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$quantile)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.5) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lb, ymax = .data$ub),
      alpha = 0.4
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$lb)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$ub)) +
    ggplot2::labs(
      x = "Quantile", y = "Intensive margin QTT (AO bounds)",
      title = "Bounds on the intensive margin quantile treatment effect"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(x$ci$qtt_bounds)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$ci_lb), linetype = "dotted") +
      ggplot2::geom_line(ggplot2::aes(y = .data$ci_ub), linetype = "dotted")
  }

  p
}
