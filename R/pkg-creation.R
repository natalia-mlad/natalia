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
  ext <- if (identical(tolower(x_ext), tolower(ext))) x_ext
  else ext
  as.character(path_ext_set(x_base, ext))
}

#' check_file_name
#' @param name name
check_file_name <- function (name) {
  if (!is_string(name)) {
    ui_stop("Name must be a single string")
  }
  if (!valid_file_name(path_ext_remove(name))) {
    ui_stop(c("{ui_value(name)} is not a valid file name. It should:",
              "* Contain only ASCII letters, numbers, '-', and '_'."))
  }
  name
}

#' get_active_r_file
#' @param path "R"
get_active_r_file <- function (path = "R") {
  if (!rstudio_available()) {
    ui_stop("Argument {ui_code('name')} must be specified.")
  }
  active_file <- rstudioapi::getSourceEditorContext()$path
  active_file <- proj_path_prep(path_expand_r(active_file))
  rel_path <- proj_rel_path(active_file)
  if (path_dir(rel_path) != path) {
    ui_stop(c("Open file must be in the {ui_path(path)} directory of the active package.",
              "  * Actual path: {ui_path(rel_path)}"))
  }
  ext <- path_ext(active_file)
  if (toupper(ext) != "R") {
    ui_stop("Open file must have {ui_value('.R')} or {ui_value('.r')} as extension,\\\n      not {ui_value(ext)}.")
  }
  path_file(active_file)
}
