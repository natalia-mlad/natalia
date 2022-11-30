#' Simple formatting of a correlation matrix
#'
#' This function provides a simple formatting of a correlation matrix into a table with 4 columns containing:
#'
#' * Column 1: row names (variable 1 for the correlation test)
#' * Column 2: column names (variable 2 for the correlation test)
#' * Column 3: the correlation coefficients
#' * Column 4: the p-values of the correlations
#'
#' Useful for conditional reporting
#'
#' @param cor_r correlation matrix with r values
#' @param cor_p (optional) matrix of correlation p values
#'
#' @returns
#' a table with 4 columns containing:
#'
#' * Column 1: row names (variable 1 for the correlation test)
#' * Column 2: column names (variable 2 for the correlation test)
#' * Column 3: the correlation coefficients
#' * Column 4: the p-values of the correlations
#'
#' @export
flat_cor_mat <- function(cor_r, cor_p = NA) {
  cor_r <- tibble::rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- tidyr::gather(cor_r, column, cor, -1)
  if (!is.na(cor_p)) {
    cor_p <- tibble::rownames_to_column(as.data.frame(cor_p), var = "row")
    cor_p <- tidyr::gather(cor_p, column, p, -1)
    cor_p_matrix <- dplyr::left_join(cor_r, cor_p, by = c("row", "column"))
    return(cor_p_matrix)
  } else {
    return(cor_r)
  }
}


#' corr_simple
#'
#' A small basic function useful for quickly seeing notable correlations.
#' Good for quick exploration/checking.
#' Prints a table and produces a plot.
#'
#' @param data a dataframe
#' @param sig numeric; the absolute correlation value above which the correlation is considered notable (.5 by default)
#' @param ... further arguments for [corrplot::corrplot()]
#'
#' @export
corr_simple <- function(data, sig = 0.5, ...) {
  stopifnot(is.numeric(sig))
  stopifnot(sig <= 1)

  # 1. Data Prep:
  # convert data to numeric in order to run correlations
  # convert to factor first to keep the integrity of the data
  # (each value will become a number rather than turn into NA)
  df_cor <- data %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_if(is.factor, as.numeric)

  # 2. Run a correlation and drop the 'insignificant' ones:
  corr <- stats::cor(df_cor)
  corr[lower.tri(corr, diag = TRUE)] <- NA # drop duplicates
  corr[corr == 1] <- NA # drop perfect correlations
  corr <- as.data.frame(as.table(corr)) # turn into a 3-column table
  corr <- stats::na.omit(corr) # remove the NA values (as introduced above)
  corr <- subset(corr, abs(Freq) > sig) # select significant values
  corr <- corr[order(-abs(corr$Freq)), ] # sort by highest correlation

  # 3. Outputs:
  print(dplyr::rename(corr, "Corr" = "Freq")) # print table
  # turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1 ~ Var2, value.var = "Freq")
  # plot correlations visually
  corrplot::corrplot(
    mtx_corr,
    is.corr = FALSE,
    tl.col = "black",
    na.label = " ",
    ...
  )
}
