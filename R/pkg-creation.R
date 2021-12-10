#' quick git stage -> commit -> push
#' @param commit_msg Git Commit Message
#' @export
quick_git <- function(commit_msg = NULL){
  n <- nrow(git_status()) # if(n == 0) stop("No")
  usethis::ui_info("{n} new unstaged files")
  if(n > 0){
    # git_add()/git_rm() #e.g., git_add(c("dev/", "man/"))
    git_add(".")
    usethis::ui_done("Files staged")
  }

  if(is.null(commit_msg))
    stop("You need to include a commit message with your commit!")
  git_commit(commit_msg) #now commit
  usethis::ui_done("Git commit")

  git_push() #and push!
  usethis::ui_done("Changes pushed!")

  usethis::ui_info("Here's a log of your most recent commits")
  return(git_log())
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
  if (!is_string(name)) {
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
