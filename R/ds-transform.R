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


#' Letters to Numbers
#' Takes a character vector of letters from the Roman alphabet
#' and transforms them into their numeric equivalent value.
#' The case (lower vs upper) is irrelevant.
#' Returns `NA`, if the letter is not part of the Roman alphabet.
#' Useful little tool for basic cipher challenges.
#'
#' @param x a character vector
#' @return interger vector
#' @export
#'
#' @examples
#' letters_to_numbers("a")
#' letters_to_numbers(c("p", "n"))
letters_to_numbers <- function(x) {
  # stopifnot(length(x) > 0)
  stopifnot(is.character(x))
  letter <- tolower(x)
  myLetters <- letters[1:26]
  return(match(letter, myLetters))
}


#----Formatting----
#' Format a long number with the comma(s)
#' @param x the number (or vector of numbers) to be formatted
#' @param digits how many significant digits are to be used; 2 by default. see [base::format()].
#' @returns a character (or vector of characters)
#' @export
comma <- function(x, digits = 2) {
  if(!is.numeric(x)) x <- as.numeric(x)
  if(is.na(x)) stop("Please provide a number to be formatted.")
  format(x, digits = digits, big.mark = ",")
}

#' Capitalise the first letter of words in a string
#' @param x string
#' @param strict logical; FALSE by default. If TRUE, makes all the letters lowercase first before capitalising.
#' @export
capwords <- function(x, strict = FALSE) {
  stopifnot(is.character(x))
  stopifnot(length(x) == 1) #TODO: make it so it works with character vectors
  cap <- function(x) {
    paste(toupper(substring(x, 1, 1)),
          {x <- substring(x, 2); if(strict) tolower(x) else x},
          sep = "", collapse = " ")
  }
  sapply(strsplit(x, split = " "), cap, USE.NAMES = !is.null(names(x)))
}

#' nozero
#' Removes leading zero before decimals and rounds number to two decimals (by default)
#' @param x numeric vector
#' @param k number of decimals (2 by default)
#' @returns character vector
#' @export
nozero <- function(x, k = 2) {
  if(!is.numeric(x)) x <- as.numeric(x)
  if(is.na(x)) stop("Please provide a number to be formatted.")
  sub('^(-)?0[.]', '\\1.', round(x, k))
  # sub('^(-)?0[.]', '\\1.', format(round(x, k), nsmall = k))
}

#' zero
#' Rounds to two decimal places (by default) and adds trailing zero(es) which are normally omitted
#' @param x x
#' @param k number of decimals (2 by default)
#' @returns character vector
#' @export
zero <- function(x, k = 2) {
  if(!is.numeric(x)) x <- as.numeric(x)
  if(is.na(x)) stop("Please provide a number to be formatted.")
  format(round(x, k), nsmall = k)
}
