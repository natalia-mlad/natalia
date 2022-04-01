
#' scrape url
#'
#' @param my_url url of the head/start page
#' @param path dir path where to save the html files
#'
#' @return a character vector of all the urls scraped
#' @export
#'
#' @examples
#' # scrapeurl("https://loal.app/", path_home("OneDrive/PhD Psychology/loal-website-scrape"))
#' # "https://www.oakam.com/" %>% scrapeurl(path_home("OneDrive/PhD Psychology/oakam-website-scrape"))
#'
scrapeurl <- function(my_url, path) {
  x <- grab_links(my_url)
  links_in <- unique(c(my_url, x)) %>%
    stringr::str_subset(my_url)

  # Find all the urls:
  out <- tibble(links_in) %>%
    mutate(links_out = map(links_in, grab_links)) %>%
    unnest(links_out) %>%
    filter(str_detect(links_out, my_url)) %>%
    unique()
  new_links <- unique(out$links_out[!out$links_out %in% out$links_in])
  while (length(new_links) > 0) {
    out <- tibble(links_in = new_links) %>%
      mutate(links_out = map(links_in, grab_links)) %>%
      unnest(links_out) %>%
      filter(str_detect(links_out, my_url)) %>%
      rbind(out) %>%
      unique()
    new_links <- unique(out$links_out[!out$links_out %in% out$links_in])
  }

  # Save the htmls:
  all_pages <- unique(c(out$links_out, out$links_out))
  all_filenames <- paste0(path, "/page", 1:length(all_pages), ".html")

  usethis::ui_done("Found {length(all_pages)} pages.\n")
  usethis::ui_todo("Saving them to {ui_path(path)}.\n")

  dir_create(path)
  walk2(all_pages, all_filenames, save_pages)
  cat(all_pages, file = paste0(path, "/all_pages.txt"), sep = "\n")

  return(all_pages)
}


#' @keywords internal
grab_links <- function(my_url) {
  # find all links on a page
  rvest::read_html(my_url) %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    unique()
    # internal # stringr::str_subset(my_url) %>%
}

#' @keywords internal
save_pages <- function(my_url, filename) {
  rvest::read_html(my_url) %>%
    xml2::write_xml(file = filename)
}

# recurse_links <- function(){}



# duckduckgo search -------------------------------------------------------

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
