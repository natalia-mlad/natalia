#' create.summary.table
#'
#' @param df dataframe
#' @param new_names (optional) a character vector of labels to replace the
#' column names with as necessary (qs)
#'
#' @return a pretty tabyl
#' @export
#'
create.summary.table <- function(df, new_names = NULL) {
  summary.table <- df %>% map(~ tabyl(.x))
  if(!is.null(new_names)){
    for(i in 1:length(summary.table)) {
      names(summary.table[[i]])[1] <- new_names[i]
    }
  }
  out <- summary.table %>%
    adorn_totals("row") %>%
    adorn_pct_formatting(digits = 2, rounding = "half up")
  return(out)
}

#' quick_summary
#'
#' @param df the dataframe
#' @param variable the var
#' @param desc logical. default TRUE
#'
#' @return summarised df
#' @export
#'
quick_summary <- function(df, variable, desc = TRUE) {
  out <- df %>%
    group_by({{variable}}) %>%
    summarise(n = n())
  if(isTRUE(desc)){
    return(arrange(out, desc(n)))
  } else {
    return(arrange(out, n))
  }
}


# Convert Chapters to Articles --------------------------------------------

#' Make Article from Chapter
#'
#' @param chapter chapter
#'
#' @export
#'
make_article <- function(chapter){

  text <- readLines(here::here(chapter)) %>%
    # up one header level
    stringr::str_replace_all("subsection", "section") %>%
    # inspect for chapter refs
    #.[doc %>% str_detect("(C|c)hapter")] %>%
    # change chapter refs
    stringr::str_replace_all("this chapter", "this paper") %>%
    stringr::str_replace_all("This chapter", "This paper") %>%
    stringr::str_replace_all("(In the |The) previous chapter(s|)", "Elsewhere") %>%
    stringr::str_replace_all("(in )the previous chapter(s|)", "elsewhere") %>%
    stringr::str_replace_all("Chapter .ref", "\\\\citet")

  # save article version of tex file
  writeLines(text, article)

  # compile tex
  tinytex::xelatex(article)

  # YAML: ####

  # ---
  # knit: ( function(input, ...){
  #         rmarkdown::render(input)
  #
  #         make_article(
  #           paste0(xfun::sans_ext(input), '.tex')
  #           )
  #         }
  #       )
  # title: The Blah Blah Blha
  # ---

}
