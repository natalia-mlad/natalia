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
  page <- rvest::read_html(link)
  page %>%
    rvest::html_elements(".results") %>%
    rvest::html_children() %>%
    rvest::html_text2()
}
