#' Quick Git
#' Stage all, commit, and push to github all with one function.
#' @param commit_msg Git Commit Message
#' @export
quick_git <- function(commit_msg = NULL){
  changes <- gert::git_status()
  n <- nrow(changes) # if(n == 0) stop("No")
  usethis::ui_info("{n} unstaged files")
  if(n > 0){
    # files <- paste(changes$file, collapse = "', '")
    usethis::ui_info("{usethis::ui_path(changes$file)}")
    # git_add()/git_rm() #e.g., git_add(c("dev/", "man/"))
    gert::git_add(".")
    usethis::ui_done("Files staged")
  }

  if(is.null(commit_msg))
    stop("You need to include a commit message with your commit!")
  usethis::ui_info("Commit Message: {usethis::ui_field(commit_msg)}")
  gert::git_commit(commit_msg) #now commit
  usethis::ui_done("Git commit")

  gert::git_push() #and push!
  usethis::ui_done("Changes pushed!")

  usethis::ui_info("Here's a log of your most recent commits")
  return(gert::git_log())
}


# Search Github -----------------------------------------------------------
#' search_github
#' @param my_query chr query; see https://docs.github.com/en/search-github/searching-on-github/searching-code
#' filename: path: language: size: extension:
#' @param max number of results; 100 default
#' @param wait how many seconds minimum to wait between requests; default 10
#' @return tibble
#' @export
search_github <- function(my_query, max = 100, wait = 10) {
  ## Pagination:
  rate_lim <- gh::gh("/rate_limit")$resources$search
  total <- gh::gh("/search/code?q={query}", query = my_query)$total_count
  usethis::ui_info("{total} results found. Limiting to the first {max} results.")
  if(total > max) total <- max
  pages <- ceiling(total / rate_lim$limit)

  ## Searching:
  results <- list()
  for (i in seq_len(pages)) {
    usethis::ui_info("Fetching page {i}/{pages}.")
    g <- gh::gh(
      "/search/code?q={query}&page={page}&per_page={per_page}",
      query = my_query,
      page = i,
      per_page = rate_lim$limit
    )
    # ^ TODO make it so if it throws error to wait or something
    # Error in gh_process_response(raw) : GitHub API error (403):
    # Message: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.
    # Read more at https://docs.github.com/en/free-pro-team@latest/rest/overview/resources-in-the-rest-api#secondary-rate-limits
    # stop -> throw -> gh_error -> gh_process_response(raw) -> gh
    ##
    # Secondary rate limits are another way we ensure the API's availability. To avoid hitting this limit, you should ensure your application follows the guidelines below:
    # Make authenticated requests, or use your application's client ID and secret. Unauthenticated requests are subject to more aggressive secondary rate limiting.
    # Make requests for a single user or client ID serially. Do not make requests for a single user or client ID concurrently.
    # When you have been limited, use the Retry-After response header to slow down. The value of the Retry-After header will always be an integer, representing the number of seconds you should wait before making requests again. For example, Retry-After: 30 means you should wait 30 seconds before sending more requests.
    results <- c(results, g$items)

    ## Pause between searches:
    if (pages > 1) {
      # x <- gh("/rate_limit")$resources$search$used
      # while (x > 0) {
      #   Sys.sleep(10)
      #   x <- gh("/rate_limit")$resources$search$used
      # }
      while (TRUE) {
        Sys.sleep(wait) #12,15,20 is fine
        x <- gh::gh("/rate_limit")$resources$search$remaining
        if (x >= rate_lim$limit) {
          break
        }
      }
    }
  }

  ## Results:
  results <- dplyr::tibble(jsonlite::fromJSON(jsonlite::toJSON(results)))
  return(results)
}
# search_limit <- function()
# debugonce(search_github)


# fetch ------------------------------------------------------------------
#' fetch_repos
#' @param username github username chr
#' @param .limit default Inf
#' @export
fetch_repos <- function(username, .limit = Inf) {
  gh::gh("/users/{username}/repos", username = username, .limit = .limit) %>%
    # , type = "sources"
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    dplyr::tibble() %>%
    # TODO: sources only
    # 1:10; 41:64; 66:74 # -"mirror_url"
    dplyr::pull(full_name) %>%
    purrr::flatten_chr()
  # Parameters
  # type	string	query
  # Specifies the types of repositories you want returned.
  # Can be one of all, public, private, forks, sources, member, internal.
  # Note: For GitHub AE, can be one of all, private, forks, sources, member, internal. Default: all.
  # If your organization is associated with an enterprise account using GitHub Enterprise Cloud or GitHub Enterprise Server 2.20+, type can also be internal. However, the internal value is not yet supported when a GitHub App calls this API with an installation access token.
}

#' fetch_repos_contents
#' @param repo_path chr
#' @param .limit Inf is defaul
#' @export
fetch_repos_contents <- function(repo_path, .limit = Inf) {
  paste0("GET /repos/", repo_path, "/contents/") %>%
    # head(2) %>%
    purrr::map_df(~ gh::gh(.x, .limit = .limit) %>% jsonlite::toJSON() %>% jsonlite::fromJSON()) %>%
    dplyr::as_tibble() %>%
    dplyr::select(name, path, type, size, url, html_url, git_url) %>%
    tidyr::unnest() # %>%
  # mutate(pkg = map_chr(html_url, ~parse_github_url(.x) %>% .$repo))
}

