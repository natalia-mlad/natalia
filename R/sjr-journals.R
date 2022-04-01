#' clean journal tables
#'
#' see fs::path_home("OneDrive/PhD Psychology/
#' 01 - Investigation - Developing Creditworthiness Measure/
#' 01 - Study 1a - Systematic Review/journals.R")
#' @param df df
#' @return cleaned df
#' @export
clean_journal_tables <- function(df) {
  output <- strsplit(df$Categories, "; ")
  output <- sapply(output, '[', seq(max(sapply(output, length)))) #lapply or map is better?
  output <- as.data.frame(t(output))
  output$id <- as.numeric(row.names(output))
  df$id <- as.numeric(row.names(df))
  y <- output[, c(1, 9)]
  n <- ncol(output) - 1
  for (i in 2:n) {
    x <- output[, c(i, 9)]
    names(x) <- names(y)
    y <- rbind(y, x)
  }
  y <- y %>% filter(!is.na(V1)) %>% arrange(V1)
  y$n <- rep(1, nrow(y))
  y <- y %>% pivot_wider(names_from = V1, values_from = n, values_fill = 0)
  df <- df %>%
    full_join(y, by = "id") %>%
    mutate(
      SJR = as.numeric(gsub(",", ".", SJR)),
      `Cites / Doc. (2years)` = as.numeric(gsub(",", ".", `Cites / Doc. (2years)`)),
      `Ref. / Doc.` = as.numeric(gsub(",", ".", `Ref. / Doc.`))
    )
  # clean_names(df)
  return(df)
}


