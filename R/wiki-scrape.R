#' grab_wiki_links
#'
#' @param url character. the url of the wiki page
#' @param keep_duplicates logical.
#'
#' @return a character vector
#' @export
wiki_grab_links <- function(url, keep_duplicates = FALSE){
  links <- rvest::read_html(url) %>% # read page
    rvest::html_elements("a") %>% # link nodes
    rvest::html_attr("href") %>% # links
    stringr::str_subset("^/wiki/") %>% # only keep links to wiki pages
    stringr::str_subset(":", negate = TRUE) %>% # remove links with colons
    # stringr::str_subset("^#", negate = TRUE) %>% # remove anchor links for this page
    stringr::str_subset("/wiki/Main_Page", negate = TRUE) # exclude main page

  if(keep_duplicates){
    return(links)
  } else {
    return(unique(links))
  }
}
# wiki_grab_links(url) %>%
#   wiki_rel_to_abs_link() %>%
#   str_subset(paste0("^", url, "$")) %>% #remove identical to
#   str_subset(page_name)


link_to_row <- function(relative_link){
  page <- link_to_title(relative_link)
  link <- relative_link %>%
    wiki_rel_to_abs_link() %>%
    wiki_grab_links()
  link_name = link_to_title(link)

  tibble(page = page,
         link = link,
         link_name = link_name) %>%
    nest(links = c(link, link_name))
  # return(out)
}


df_to_df <- function(first_df) {
  links <- first_df %>%
    unnest(links) %>%
    pull(link)

  keep <- links %>%
    map_lgl(~ !already_done(.x, first_df))

  second_df <- links[keep] %>%
    map_df(link_to_row)
  #Joining, by = c("page", "links")
  return(full_join(first_df, second_df))
}

# df_to_df <- function(first_df){
#   second_df <- first_df %>%
#     unnest(links) %>%
#     pull(link) %>%
#     map_df(link_to_row)
#   return(full_join(first_df, second_df))
# }


page_to_df_recursive <- function(url, depth = 3) {
  # to make sure we can plug in an absolute url...
  out <- str_replace(url, ".*/wiki/", "/wiki/") %>%
    link_to_row(.)
  for(layer in 1:depth) out <- df_to_df(out)
  return(out)
}


# utils -------------------------------------------------------------------
wiki_rel_to_abs_link <- function(relative_link){
  base_url <- "https://en.wikipedia.org"
  paste0(base_url, relative_link)
}

link_to_title <- function(relative_link){
  stringr::str_replace(relative_link, ".*/wiki/", "")
  # '.*' ensures it will also work with absolute links
}

already_done <- function(link, first_df){
  link_to_title(link) %in% first_df$page
}
