#' duckduckgo search
#' @param searchTerms a character vector
#' @return charracter vector
#' @export
duckduckgo <- function(searchTerms) {
  duckBase <- "https://duckduckgo.com/html/"
  searchQuery <- paste0("?q=", paste(searchTerms, collapse = "+"))
  link <- utils::URLencode(
    paste0(duckBase, searchQuery, "&kp=-2")
    # see https://duckduckgo.com/params
  )
  page <- read_html(link)
  page %>%
    html_elements(".results") %>%
    html_children() %>%
    html_text2()
}
