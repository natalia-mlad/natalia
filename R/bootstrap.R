#' fold_incr
#' @export
fold_incr <- function(split, ...) {
  dat <- analysis(split)
  quants <- quantile(dat$VO2, probs = c(.1, .9))
  tibble(
    term = "fold increase",
    estimate = unname(quants[2]/quants[1]),
    # We don't know the analytical formula for this
    std.error = NA_real_
  )
}

#' Will be used to fit the models to different bootstrap data sets
#' @export
fit_fun <- function(split, ...) {
  # We could check for convergence, make new parameters, etc.
  nls(nonlin_form, data = analysis(split), ...) %>%
    tidy()
}

#' corr.boot
#' @export
corr.boot <- function(data, i) {
  cor(data10[i, "V1"], data10[i, "V2"], method='pearson')
}
