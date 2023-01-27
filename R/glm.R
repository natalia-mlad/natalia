#' P__disp
#'
#' @param x x
#'
#' @export
P__disp <- function(x) {
  pr <- sum(stats::residuals(x, type = "pearson")^2)
  dispersion <- pr / x$df.residual
  cat("\n Pearson Chi2 = ", pr, "\n Dispersion = ", dispersion, "\n")
}

#' invlogit
#' see [arm::invlogit]
#' @param x x
invlogit <- function(x) {
  1 / (1 + exp(-x))
}

#' lm_eqn
#'
#' @param df df
#'
#' @export
lm_eqn <- function(df) {
  m <- stats::lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~ ~italic(r)^2 ~ "=" ~ r2, list(a = format(unname(stats::coef(m)[1]), digits = 2), b = format(unname(stats::coef(m)[2]), digits = 2), r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}
