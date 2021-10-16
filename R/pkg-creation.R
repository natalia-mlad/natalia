natalia_use_r <- function(name = NULL, open = rlang::is_interactive()) {
  name <- name %||% get_active_r_file(path = "tests/testthat")
  name <- gsub("^test-", "", name)
  name <- slug(name, "R")
  check_file_name(name)
  use_directory("R")
  edit_file(proj_path("R", name), open = open)
  test_path <- proj_path("tests", "testthat", paste0("test-", name, ".R"))
  if (!file_exists(test_path)) {
    ui_todo("Call {ui_code('use_test()')} to create a matching test file")
  }
  invisible(TRUE)
}
