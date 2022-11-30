# Count Summaries ---------------------------------------------------------

#' Quick Summary
#'
#' The quick way of doing the group_by(), summarise(), arrange() routine
#' that I always seem to do.
#'
#' @param df dataframe
#' @param variable the variable that you want to summarise by
#' @param desc logical; Order by descending order? Default is TRUE.
#'
#' @return summarised df
#' @export
quick_summary <- function(df, variable, desc = TRUE) {
  out <- df %>%
    dplyr::group_by({{variable}}) %>%
    dplyr::summarise(n = dplyr::n())
  if (desc) {
    return(dplyr::arrange(out, dplyr::desc(n)))
  } else {
    return(dplyr::arrange(out, n))
  }
}


#' create summary [janitor::tabyl] table
#'
#' Produces counts, totals, and percentages across all the variables in a dataframe;
#' with an option to present the variables with different names.
#'
#' Useful for quickly counting and presenting answers to multiple-choice questions
#' in a report.
#'
#' @param df dataframe
#' @param new_names (optional) a character vector of labels to replace the
#' column names with as necessary (qs)
#'
#' @return a pretty [janitor::tabyl()] table
#' @export
create.summary.table <- function(df, new_names = NULL) {
  summary.table <- df %>% purrr::map(~ janitor::tabyl(.x))
  if(!is.null(new_names)){
    for(i in 1:length(summary.table)) {
      names(summary.table[[i]])[1] <- new_names[i]
    }
  }
  out <- summary.table %>%
    janitor::adorn_totals("row") %>%
    janitor::adorn_pct_formatting(digits = 2, rounding = "half up")
  return(out)
}
