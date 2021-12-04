#' Tidy my Data
#'
#' @param data dataframe
#' @param na.rm T/F
#' @param quiet T/F
#'
#' @return a tidier dataframe
#'
#' @export
#'
tidy_my_data <- function(data, na.rm = TRUE, quiet = FALSE) {
  data %>%
    janitor::remove_empty(which = c("rows", "cols"), quiet = quiet) %>%
    janitor::remove_constant(na.rm = na.rm, quiet = quiet)
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
#'
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}

#' Identify ordinary variables
#'
#' @param data dataset
#' @param n max number of levels allowed for the variable before it stops being
#' considered ordinal and becomes continuous. Default is 7.
#'
#' @export
#'
isOrd <- function(data, n = 7) {
  # for(i in 1:ncol(data)) {length(unique(data[, i])) <= 7}
  #map_chr(data, class) %>% keep(~ .x != "numeric") %>% length
  if (map(data, ~ class(.x) != "numeric") %>% flatten_lgl() %>% any(isTRUE(.))) {
    data <- data[, sapply(data, is.numeric)]
  }
  isOrd <- sapply(1:ncol(data), function(i) {
    isInt <- is.integer(data[[i]]) | all(data[[i]] %% 1 == 0, na.rm = T)
    nLevel <- length(unique(data[[i]]))
    return(isInt & nLevel <= n)
  })
  out <- names(data)[isOrd]
  if(is_empty(out)) return(NULL)
  else return(out)
}

#' is.count
#'
#' @param x x
#' @param tol tol
#'
#' @return T/F
#' @export
#'
is.count <- function(x, tol = .Machine$double.eps^0.5) {
  ifelse((is.numeric(x) & !anyNA(x)), all(abs(x - round(x)) < tol), FALSE)
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
#'
identify_redundant_ids <- function(data, char.dist = 4) {
  data <- janitor::remove_constant(data, quiet = F)
  x <- data %>% keep(is.numeric) %>% names()
  y <- names(data)[!(names(data) %in% x)]
  # y <- names(data)[names(data) %notin% x]
  combin_mat <- expand.grid(ids = x, factors = y, stringsAsFactors = F) %>%
    mutate(dist = adist(ids, factors, ignore.case = T, useBytes = T) %>% diag()) %>%
    filter(dist <= char.dist)
  combin_mat %>%
    mutate(is_nested = map2_lgl(combin_mat$ids, combin_mat$factors, function(x, y) {
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
    })) %>% filter(is_nested == TRUE) %>%
    pull(ids)
}
