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


#' identify.redundant.ids
#' @export
identify.redundant.ids <- function(data, char.dist = 4) {
  data <- janitor::remove_constant(data, quiet = F)
  x <- data %>% keep(is.numeric) %>% names()
  y <- names(data)[names(data) %notin% x]
  combin_mat <- expand.grid(ids = x, factors = y, stringsAsFactors = F) %>%
    mutate(dist = adist(ids, factors, ignore.case = T, useBytes = T) %>% diag()) %>%
    filter(dist < char.dist)
  combin_mat %>%
    mutate(is_nested = map2_lgl(combin_mat$ids, combin_mat$factors, function(x, y) {
      if(length(unique(data[, x])) == 1) return(NA)
      if(length(unique(data[, y])) == 1) return(NA)
      M <- xtabs(~ data[, x] + data[, y], sparse = T)
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
