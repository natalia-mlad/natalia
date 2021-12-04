#' Simple formatting of a correlation matrix
#'
#' This function provides a simple formatting of a correlation matrix into a table with 4 columns containing:
#' Column 1 : row names (variable 1 for the correlation test)
#' Column 2 : column names (variable 2 for the correlation test)
#' Column 3 : the correlation coefficients
#' Column 4 : the p-values of the correlations
#' @param cor_r correlation matrix with r values
#' @param cor_p matrix of correlation p values
#'
#' @returns
#' Column 1 : row names (variable 1 for the correlation test)
#' Column 2 : column names (variable 2 for the correlation test)
#' Column 3 : the correlation coefficients
#' Column 4 : the p-values of the correlations
#'
#' @export
flat_cor_mat <- function(cor_r, cor_p){
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}

#' corr_simple
#' @export
corr_simple <- function(data = df, sig = 0.5) {
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)  #run a correlation and drop the insignificant ones
  corr <- stats::cor(df_cor)
  #prepare to drop duplicates and correlations of 1
  corr[lower.tri(corr, diag = TRUE)] <- NA
  #drop perfect correlations
  corr[corr == 1] <- NA   #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above
  corr <- stats::na.omit(corr)   #select significant values
  corr <- subset(corr, abs(Freq) > sig)
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)), ]   #print table
  print(corr)  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1 ~ Var2, value.var = "Freq")
  #plot correlations visually
  corrplot::corrplot(mtx_corr,
           is.corr = FALSE,
           tl.col = "black",
           na.label = " ")
}
