#' Grab Wikipedia links
#'
#' Scrape all the internal links from a given [Wikipedia](https://en.wikipedia.org/) page.
#' This will exclude any external links, links with colons, and the main page.
#' Useful for various network analyses and quick scrapping from [Wikipedia](https://en.wikipedia.org/).
#'
#' @param url character. the url of the wiki page
#' @param keep_duplicates logical. FALSE by default
#'
#' @return a character vector
#' @examples
#' \dontrun{
#' base_url = "https://en.wikipedia.org"
#' page_name = "List_of_filename_extensions"
#' url <- httr::modify_url(base_url, path = c("wiki", page_name))
#' wiki_grab_links(url) %>%
#'   paste0(base_url, .) %>%
#'   str_subset(page_name) %>%  #keep pages that are part of this page
#'   str_subset(paste0("^", url, "$"), negate = T) %>% #but remove identical to the url
#'   str_remove_all("#.*") %>% #remove anchors
#'   curl::curl_unescape() %>%
#'   suna()
#' }
#'
#' @export
wiki_grab_links <- function(url, keep_duplicates = FALSE) {
  links <- rvest::read_html(url) %>% # read page
    rvest::html_elements("a") %>% # link nodes
    rvest::html_attr("href") %>% # links
    stringr::str_subset("^/wiki/") %>% # only keep links to wiki pages
    stringr::str_subset(":", negate = TRUE) %>% # remove links with colons
    stringr::str_subset("^#", negate = TRUE) %>% # remove anchor links for this page
    stringr::str_subset("/wiki/Main_Page", negate = TRUE) # exclude main page

  if (keep_duplicates) {
    return(links)
  } else {
    return(unique(stringr::str_remove_all(links, "#.*")))
  }
}


#' Recurse through links of pages (of links)^{n} of a Wikipedia page
#'
#' Great for creating a dataset for network analysis.
#' Depth of 1 means just the links on the existing page are found; depth of 2 means the links of the links; and so on.
#'
#' Please note, this function gets expensive quickly (i.e., several hours) at depth 3 for pages with lots of links (i.e., 300+).
#' Also, while some care has been taken to optimise it, it currently considers pages that redirect to each other as different, thus resulting in unnecessary repetion (TODO)
#' (e.g., "/wiki/Principle_of_Compositionality" and "wiki/Principle_of_compositionality")
#'
#' @param url a wikipedia url (absolute or relative)
#' @param depth numeric, default is 2. Depth of 1 means just the links on the existing page are found; depth of 2 means the links of the links; and so on.
#'
#' @return a nested tibble with columns page and links
#' @export
#'
wiki_recursive_links <- function(url, depth = 2) {
  stopifnot(is.numeric(depth))

  out <- stringr::str_replace(url, ".*/wiki/", "/wiki/") %>% #if it's an absolute url
    link_to_df(.)
  # TODO: if nrow(out) > 300 and depth >= 3, let the user know that this will take a while

  if(depth > 1) {
    depth <- depth - 1
    for(i in 1:depth) out <- df_to_df(out)
  }

  return(tidyr::nest(out, links = c(link, link_name)))
}


# internals ---------------------------------------------------------------
#' Given a relative wiki link, output a tibble with the pages it links to
#' @keywords internal
link_to_df <- function(relative_link) {
  page <- wiki_link_to_title(relative_link)
  link <- relative_link %>%
    paste0("https://en.wikipedia.org", .) %>%
    wiki_grab_links()
  link_name <- wiki_link_to_title(link)
  return(dplyr::tibble(page, link, link_name))
}

#' Given a tibble of pages given page(s) link(s) to, find their recursive links
#' @keywords internal
df_to_df <- function(first_df) {
  # TODO: sort out where some of the new_links actually redirect to already existing pages
  new_links <- first_df$link[!first_df$link_name %in% first_df$page]
  second_df <- purrr::map_df(new_links, link_to_df)
  out <- rbind(first_df, second_df)
  return(out)
}

#' Convert a wikipedia link to the title
#' @keywords internal
wiki_link_to_title <- function(wiki_link) {
  stringr::str_replace(wiki_link, ".*/wiki/", "")
  # '.*' ensures it will work with relative or absolute links
}
