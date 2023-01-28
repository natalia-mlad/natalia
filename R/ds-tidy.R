# Do ----------------------------------------------------------------------

#' Remove Identical Cols
#' @details Tested to be the fastest out of other possible methods! aka fun4
#' @param data dataframe
#' @return a tidier dataframe
#' @export
remove_identical_cols <- function(data) {
  data[!duplicated(as.list(data))]
}

#' Tidy my Data
#' @param data dataframe
#' @param na.rm T/F
#' @param quiet T/F
#' @return a tidier dataframe
#' @export
tidy_my_data <- function(data, na.rm = TRUE, quiet = FALSE) {
  data <- data %>%
    janitor::remove_empty(which = c("rows", "cols"), quiet = quiet) %>%
    janitor::remove_constant(na.rm = na.rm, quiet = quiet)
  remove_identical_cols(data)
}

#' Force Bind two Dataframes
#'
#' Force bind (row-wise) two dataframes By forcing their columns to be the same
#'
#' @param df1 dataframe 1
#' @param df2 dataframe 2
#'
#' @return a dataframe
#' @export
force_bind <- function(df1, df2) {
  colnames(df2) <- colnames(df1)
  dplyr::bind_rows(df1, df2)
}


# Identify ----------------------------------------------------------------

#' Identify ordinal variables
#'
#' @param data dataset
#' @param n max number of levels allowed for the variable before it stops being
#' considered ordinal and becomes continuous. Default is 7.
#'
#' @export
isOrd <- function(data, n = 7) {
  # for(i in 1:ncol(data)) {length(unique(data[, i])) <= 7}
  #map_chr(data, class) %>% keep(~ .x != "numeric") %>% length
  if (purrr::map(data, ~ class(.x) != "numeric") %>% purrr::flatten_lgl() %>% any(isTRUE(.))) {
    data <- data[, sapply(data, is.numeric)]
  }
  isOrd <- sapply(1:ncol(data), function(i) {
    isInt <- is.integer(data[[i]]) | all(data[[i]] %% 1 == 0, na.rm = T)
    nLevel <- length(unique(data[[i]]))
    return(isInt & nLevel <= n)
  })
  out <- names(data)[isOrd]
  if(purrr::is_empty(out)) return(NULL)
  else return(out)
}

#' check if count
#'
#' @param x a vector
#' @param tol tolerance (e.g., 0.000000015); is .Machine$double.eps^0.5 by default
#'
#' @return logical value (TRUE/FALSE)
#' @export
is.count <- function(x, tol = .Machine$double.eps^0.5) {
  ifelse((is.numeric(x) & !anyNA(x)), all(abs(x - round(x)) < tol), FALSE)
}


#' Test Nest
#'
#' Check whether variable `y` is nested in variable `x`.
#' For instance, variable `x` could be phone brands and variable `y` is phone models of users who completed a questionnaire.
#' Or variable `x` could be country and variable `y` is region or city.
#' Most relevant for factor/character vectors, rather than numeric ones.
#' Useful for figuring out redundant variables or when trying to avoid multicollinearity.
#'
#' @param x a vector
#' @param y another vector
#'
#' @return a print statement whether the variables are nested or not
#'
#' @export
test_nest <- function(x, y) {
  stopifnot(length(x) == length(y))
  M <- stats::xtabs(~ x + y, sparse = T)
  result <- all(Matrix::colSums(M > 0) == 1L)
  if (result == TRUE) {
    print(paste(deparse(substitute(y)),
                "is nested in",
                deparse(substitute(x))))
  } else if (result == FALSE) {
    print(paste(
      deparse(substitute(y)),
      "is NOT nested in",
      deparse(substitute(x))
    ))
  }
}


#' Identify Redundant ID Columns in a Dataset
#'
#' Great for real-life business data where the dataset comes from database where there are
#' a lot of ids/keys being used alongside character explanation of the said key.
#' Thus, most of the time it's completely irrelevant column clutter to you as a data-scientist
#' and you want it gone easliy. E.g., 'ApplicationStatusPrimaryID' & 'ApplicationStatusPrimary'
#' in the vwApps dataset ("03 - Oakam Work/vwApps-from-2007-05-to-2021-05.RData") are redundant
#' in such a way. The numeric id column then gets flagged for removal. The character similarity
#' between the column names and subsequent 'nesting test' control which columns are identified
#' as redundant.
#'
#' I know it's a pretty specific pattern to identify, but it's quite handy for me. For the vwApps
#' dataset, for example, 9% of the columns (out of 68) gets removed in such a way.
#'
#' @param data the data.frame/tibble
#'
#' @param char.dist numeric. The maximum distance allowed between column names for them to
#' be considered as candidates for redundant ids. The character distance is
#' calculated by the adist function (cases ignored and using bytes). The default value of 4
#' seems about right since it will match column names StpId' & 'UserId' (adist = 4), and
#' 'ProviderTypeID' & 'ProviderType' (adist = 2), but not 'Name' & 'Salary' or
#' 'ApplicationKey' & ApplicationMonth' which both have the character adist value of 5.
#'
#' @returns character vector of names of the redundant id columns to remove from the dataset
#'
#' @export
identify_redundant_ids <- function(data, char.dist = 4) {
  data <- janitor::remove_constant(data, quiet = FALSE)
  x <- data %>% purrr::keep(is.numeric) %>% names()
  y <- names(data)[!(names(data) %in% x)]
  # TODO: say: "creating combin_mat"
  # TODO: this part is too slow!!
  combin_mat <- expand.grid(ids = x, factors = y, stringsAsFactors = F) %>%
    dplyr::mutate(dist = adist(ids, factors, ignore.case = T, useBytes = T) %>% diag()) %>%
    dplyr::filter(dist <= char.dist)
  # TODO: say: ...
  combin_mat %>%
    dplyr::mutate(is_nested = purrr::map2_lgl(combin_mat$ids, combin_mat$factors, function(x, y) {
      if(length(unique(data[, x])) == 1) return(NA)
      if(length(unique(data[, y])) == 1) return(NA)
      M <- stats::xtabs(~ data[, x] + data[, y], sparse = T)
      all(Matrix::colSums(M > 0) == 1L)
      #is_nested <- all(Matrix::colSums(M > 0) == 1L)
      # if(is_nested == FALSE & all(Matrix::rowSums(M > 0) == 1L) == TRUE) {
      #   m <- round(apply(M, 1, function(x) x / Matrix::colSums(M)), 2) %>% t()
      #   if(m[m > 0.03 & m < 0.97] %>% length() == 0) return(TRUE)
      #   else return(FALSE)
      # } else return(is_nested)
    })) %>%
    dplyr::filter(is_nested == TRUE) %>%
    dplyr::pull(ids)
}


#----Fuzzy Matching----
#' Join two tables using fuzzy (aka approximate) matching
#'
#' Uses [base::agrep()] and loops through each row of `df.x` identifying the closest value match in `df.y`, given parameters set.
#' It's a bit of a work in progress for the most part.
#' Sometimes works really well with the default parameters and other times requires quite a bit of fiddling around.
#' So remember to always check the results!
#' I've successfully used it for matching customer names across databases when other (more robust) variables were unavailable or returned conflicting results.
#'
#' @param df.x dataframe 1
#' @param df.y dataframe 2
#' @param by join by argument (e.g., by = c("x" = "y")); same usage as in [dplyr::left_join()]
#' @param max.distance numeric; 0.3 by default; see [base::agrep()] for details
#' @param ignore.case logical; TRUE by default; see [base::agrep()] for details
#' @param useBytes logical; TRUE by default; see [base::agrep()] for details
#'
#' @return modified df.x with instructions on how to join with df.y
#'
#' @export
fuzzy_join <- function(df.x, df.y, by = NULL, max.distance = 0.3,
                       ignore.case = TRUE, useBytes = TRUE) {
  # TODO: not the most accurate implementation for some cases tbh
  # + auto join based on what type of join is desired [left, right, inner, full]
  # + remove the JOIN column at the end
  x_names <- names(df.x)
  y_names <- names(df.y)

  # Parsing the `by` argument (e.g., by = c("x" = "y")):
  if (is.list(by)) {
    by <- by[c("x", "y")]
  }
  else if (is.character(by)) {
    by_x <- names(by)
    by_y <- unname(by)
    by_x[by_x == ""] <- by_y[by_x == ""]
    by <- list(x = by_x, y = by_y)
  } else {
    stop("by must be a (named) character vector, list, or NULL")
    # abort("by must be a (named) character vector, list, or NULL")
    # warning(""); return(NULL)
  }
  # check_join_vars(by$x, x_names)
  stopifnot(by$x %in% x_names)
  stopifnot(by$y %in% y_names)

  # Create named integers:
  x_by <- magrittr::set_names(match(by$x, x_names), by$x)
  y_by <- magrittr::set_names(match(by$y, y_names), by$y)
  stopifnot(class(df.x[[x_by]]) == "character")
  stopifnot(class(df.y[[y_by]]) == "character")

  # x$adist <- adist(x$FullName.x, x$FullName.y, ignore.case = T, useBytes = T, partial = T) %>% diag()
  df.x$JOIN <- ""
  for (i in 1:nrow(df.x)) {
    # skip the loop if there's already a perfect match
    if(df.x[[x_by]][i] %in% df.y[[y_by]]) {
      df.x$JOIN[i] <- df.x[[x_by]][i]
    } else {
      temp <- agrep(df.x[[x_by]][i], df.y[[y_by]], value = TRUE, max.distance = max.distance,
                    ignore.case = ignore.case, useBytes = useBytes)
      if (length(temp) > 1) {
        temp_value <- agrep(df.x[[x_by]][i], df.y[[y_by]], value = FALSE, max.distance = max.distance,
                            ignore.case = ignore.case, useBytes = useBytes)
        temp <- temp[which(temp_value == max(temp_value))]
      }
      temp <- paste0(temp, "") # what does this do again?
      df.x$JOIN[i] <- temp
    }
  }

  # Output:
  percent_coverage <- sum(is.na(df.x$JOIN))/nrow(df.x)
  if(percent_coverage > 0.33) {
    warning(paste0("More than a third of the variables (", round(percent_coverage * 100),
                   "%) were unmatched! Consider increasing the max.distance or changing other agrep options."))
  }
  cat(crayon::cyan(crayon::bold(
    paste0(
      "Fuzzy matching is completed.\nYou can now join datasets \`",
      match.call()[2],
      "\` (the output of this function) and \`",
      match.call()[3],
      "\` on \"JOIN\" = \"",
      names(y_by),
      "\" \n"
    )
  )))
  return(df.x)
}
