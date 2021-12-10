#' quick git stage -> commit -> push
#' @param commit_msg Git Commit Message
#' @export
quick_git <- function(commit_msg = NULL){
  changes <- git_status()
  n <- nrow(changes) # if(n == 0) stop("No")
  usethis::ui_info("{n} unstaged files")
  if(n > 0){
    # files <- paste(changes$file, collapse = "', '")
    usethis::ui_info("{usethis::ui_path(changes$file)}")
    # git_add()/git_rm() #e.g., git_add(c("dev/", "man/"))
    git_add(".")
    usethis::ui_done("Files staged")
  }

  if(is.null(commit_msg))
    stop("You need to include a commit message with your commit!")
  usethis::ui_info("Commit Message: {usethis::ui_field(commit_msg)}")
  git_commit(commit_msg) #now commit
  usethis::ui_done("Git commit")

  git_push() #and push!
  usethis::ui_done("Changes pushed!")

  usethis::ui_info("Here's a log of your most recent commits")
  return(git_log())
}


# git_setup ---------------------------------------------------------------
#' git_setup (quite experimental atm)
#'
#' @param repo_name character vector
#' @param private T/F
#' @param user default uses gh::gh_whoami()$"login"
#' @param git_ignore additional folders or files to pass onto .gitignore.
#'
#' @export
git_setup <- function(repo_name = NULL, private = TRUE,
                      user = gh::gh_whoami()$"login", git_ignore = NULL) {
  stopifnot(!is.null(repo_name))
  # also do a quick regex check for illegal repo names (e.g., no .?)
  # Don't Commit & Restart
  usethis::use_git()

  # 2. Control what we commit + do a first commit!
  usethis::git_vaccinate()
  # check regarding files over the 100Mb limit!
  usethis::use_git_ignore(c(git_ignore, "sessionInfoLog", "/history/"))
  # Then, commit some changes locally

  # 3. Create the origin on github:
  new_repo <- paste0(user, "/", repo_name)
  ghee::gh_repos_create("natalia-mlad/relevant-references", private = private)

  # 4. Do some quick checks:
  # First, check that there isn't one already with usethis:::check_no_origin()
  # If there's one, then wipe it off with:
  usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
  # Then, check again, if necessary, with usethis:::check_no_origin()
  # Also, check that:
  stopifnot(identical(usethis::git_default_branch(), git_branch()))

  # 5. Add this origin as the remote + do first push!
  my_repo <- gh::gh("GET /user/repos", type = "owner", sort = "created") %>%
    jsonlite::toJSON(.) %>% jsonlite::fromJSON(.) %>% tibble() %>%
    select(name, html_url, clone_url, svn_url, ssh_url, git_url) %>%
    unnest(c(name, html_url, clone_url, svn_url, ssh_url, git_url)) %>%
    slice(1)
  # origin_url <- my_repo$clone_url; use_git_remote("origin", origin_url)
  usethis::use_git_remote("origin", my_repo$clone_url)
  # ^ because usethis::git_protocol() is "https"
  git_push(
    remote = "origin",
    set_upstream = TRUE,
    repo = path_wd(),
    verbose = TRUE
  )
  # git_branch_list(local = TRUE, repo = repo) %>% nrow() #check its 1

  # 6. All done! Check out the repo on github!
  usethis::ui_done("All done! Check out the repo on GitHub!")
  return(utils::browseURL(my_repo$html_url))

  # ##
  # # 7. Bonus - Can also add some links/descriptions/etc:
  # repo_desc <- if(usethis:::is_package()) usethis:::package_data()$Title %||% "" else ""
  # repo_desc <- gsub("\n", " ", repo_desc)
  # repo_spec <- glue::glue("{owner}/{repo_name}")
  # # visibility <- "private"
  # # visibility_string <- if(visibility == "public") "" else glue::glue("{visibility} ")
  # use_github_links()
  # # √ Setting URL field in DESCRIPTION to 'https://github.com/natalia-mlad/relevant-references'
  # # √ Setting BugReports field in DESCRIPTION to 'https://github.com/natalia-mlad/relevant-references/issues'
  # # There is 1 uncommitted file:
  # #   * 'DESCRIPTION'
  # # Is it ok to commit it?
  # # 1: Not now
  # ##
}

# Work in progress: -------------------------------------------------------

#' natalia_use_r
#' @param name NULL
#' @param open rlang::is_interactive()
natalia_use_r <- function(name = NULL, open = rlang::is_interactive()) {
  name <- name %||% get_active_r_file(path = "tests/testthat")
  name <- gsub("^test-", "", name)
  name <- slug(name, "R")
  check_file_name(name)
  usethis::use_directory("R")
  usethis::edit_file(usethis::proj_path("R", name), open = open)
  test_path <- usethis::proj_path("tests", "testthat", paste0("test-", name, ".R"))
  if (!file_exists(test_path)) {
    usethis::ui_todo("Call {usethis::ui_code('use_test()')} to create a matching test file")
  }
  invisible(TRUE)
}

#' slug
#' @param x x
#' @param ext ext
slug <- function (x, ext) {
  x_base <- path_ext_remove(x)
  x_ext <- path_ext(x)
  ext <- if(identical(tolower(x_ext), tolower(ext))) x_ext else ext
  as.character(path_ext_set(x_base, ext))
}

#' check_file_name
#' @param name name
check_file_name <- function (name) {
  if (!rlang::is_string(name)) {
    usethis::ui_stop("Name must be a single string")
  }
  if (!valid_file_name(path_ext_remove(name))) {
    usethis::ui_stop(c("{ui_value(name)} is not a valid file name. It should:",
              "* Contain only ASCII letters, numbers, '-', and '_'."))
  }
  name
}

#' get_active_r_file
#' @param path "R"
get_active_r_file <- function (path = "R") {
  if (!rstudioapi::isAvailable()) {
    usethis::ui_stop("Argument {ui_code('name')} must be specified.")
  }
  active_file <- rstudioapi::getSourceEditorContext()$path
  active_file <- proj_path_prep(path_expand_r(active_file))
  rel_path <- proj_rel_path(active_file)
  if (path_dir(rel_path) != path) {
    usethis::ui_stop(
      c("Open file must be in the {ui_path(path)} directory of the active package.",
              "  * Actual path: {ui_path(rel_path)}")
    )
  }
  ext <- path_ext(active_file)
  if (toupper(ext) != "R") {
    usethis::ui_stop("Open file must have {ui_value('.R')} or {ui_value('.r')} as extension,\\\n      not {ui_value(ext)}.")
  }
  path_file(active_file)
}

#' proj_path_prep
#' @param path path
proj_path_prep <- function(path){
  if(is.null(path)) return(path)
  path <- path_abs(path)
  if(file_exists(path)) path_real(path) else path
}

#' proj_rel_path
#' @param path path
proj_rel_path <- function(path) {
  if (is_in_proj(path)) {
    as.character(path_rel(path, start = proj_get())) #usethis::: !
  }
  else {
    path
  }
}
