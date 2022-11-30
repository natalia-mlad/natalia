#----Numeric Tricks----

#' Polytomize Data
#' polytomize data to, for example, demo ordinal models
#'
#' @param df a dataframe where all numeric
#' @param thresholds a numeric vector of cutoff points for the
#' category thresholds; provide one fewer than the required number
#' of categories (e.g., c(-0.5, 1) for three categories)
#' @export
polytomize_data <- function(df, thresholds) {
  # Source:
  # OneDrive/PhD Psychology/01 - R Project/01 - SEM/Extending Structural Equation Models of Generalizability Theory/SEM4GT.R
  # TODO: check if all numeric?
  all_values <- purrr::flatten_dbl(df)

  # TODO: check if categories are sensible?
  stopifnot(min(all_values) < min(thresholds))
  stopifnot(max(all_values) > max(thresholds))

  n <- length(thresholds) + 1
  usethis::ui_info("Turning data into {n} categories.")

  breaks <- c(-Inf, thresholds, Inf)
  out <- data.frame(lapply(df, function(x) {
    cut(x, breaks = breaks, labels = FALSE)
  }))
  # out$ID <- 1:nrow(out)
  return(out)
}


#' Change Range
#' Adjust the range of variables to the desired minimum and maximum using a simple formula.
#'
#' @param data dataset
#' @param col_names names of the columns that need to be adjusted
#' @param min numeric; the minimum range; 0 by default.
#' @param max numeric; the maximum range; 100 by default.
#'
#' @return the original dataset with the desired columns adjusted
#' @export
change_range_simple <- function(data, col_names, min = 0, max = 100) {
  # stopifnot(min != max)
  stopifnot(min < max)
  stopifnot(is.character(col_names))
  stopifnot(is.numeric(data[, col_names]))

  min.new <- min
  max.new <- max
  new.range <- max.new - min.new

  min.old <- min(data[, col_names], na.rm = TRUE)
  max.old <- max(data[, col_names], na.rm = TRUE)
  old.range <- max.old - min.old

  data[, col_names] <- ((new.range * (data[, col_names] - min.old)) / old.range) + min.new
  # plot(density(y)); plot(density(data[, col_names]))
  # new_col_names <- paste0(col_names, "_new_range")
  return(data)
}

#----Formatting----
#' comma
#' @param x x
#' @export
comma <- function(x) {
  format(x, digits = 2, big.mark = ",")
}

#' capwords
#' @param s string
#' @param strict TF
#' @export
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' removes leading zero before decimals and rounds number to two decimals (by default)
#' @param x numeric vector
#' @param k number of decimals
#' @returns character vector
#' @export
nozero <- function(x, k = 2) {
  sub('^(-)?0[.]', '\\1.', round(x, k))
  # sub('^(-)?0[.]', '\\1.', format(round(x, k), nsmall = k))
}

#' formats numbers to no more or less than two decimals
#' @param x x
#' @param k number of decimals
#' @export
zero <- function(x, k = 2) {
  format(round(x, k), nsmall = k)
}
