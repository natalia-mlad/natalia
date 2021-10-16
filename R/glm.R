#' bootReg
#' @export
bootReg <- function(formula, data, indices) {
  d <- data
  fit <- glm(formula, data = d, weights = SumOpportunitiesToPay, family = binomial(link = "logit"))
  return(coef(fit))
}

#' P__disp
#' @export
P__disp <- function(x) {
  pr <- sum(residuals(x, type = "pearson")^2)
  dispersion <- pr / x$df.residual
  cat("\n Pearson Chi2 = ", pr, "\n Dispersion = ", dispersion, "\n")
}

#' invlogit
#' @export
invlogit <- function(x) {
  1 / (1 + exp(-x))
}

#' lm_eqn
#' @export
lm_eqn <- function(df) {
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~ ~italic(r)^2 ~ "=" ~ r2, list(a = format(unname(coef(m)[1]), digits = 2), b = format(unname(coef(m)[2]), digits = 2), r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}
