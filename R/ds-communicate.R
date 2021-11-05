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
}
