#'
#' #help(".onLoad")
#'
#' .onLoad <- function(libname, pkgname){
#'   x <- rnorm(10)   ## dummy example
#' }
#'
#' # There is usually a "processing function" (traditionally called zzz.R) with tasks to be performed when the package is loaded, such as loading libraries and compiled code. For example you can create a zzz.R file where you create this function:
#' #
#' #   .onLoad <- function(libname, pkgname){
#' #'
#' #' No, the answer is simply wrong; this code never worked. But your code is also not correct, don’t write the data into the global environment. You want to write the data into the package environment. To do this you need to use topenv() instead of .GlobalEnv. See stackoverflow.com/a/67664852/1968. –
#' #' Konrad Rudolph
#' #' May 24 at 9:25
#' #' @KonradRudolph What do you mean by the answer is wrong ? –
#' #' agstudy
#' #' May 25 at 18:07
#' #' @agstudy I mean that it doesn’t work: <- performs local assignment, which isn’t useful here (I realise this is a toy example, but one of the top use-cases of .onLoad is to set a package variable). Incidentally, using <<- wouldn’t fix this. See the linked answer for a proper fix. –
#' #' Konrad Rudolph
#' #' May 25 at 18:44
#'
#' ###
#' # dplyr:
#' # https://github.com/tidyverse/dplyr
#' ###
#' .onLoad <- function(libname, pkgname) {
#'   op <- options()
#'   op.dplyr <- list(
#'     dplyr.show_progress = TRUE
#'   )
#'   toset <- !(names(op.dplyr) %in% names(op))
#'   if (any(toset)) options(op.dplyr[toset])
#'
#'   .Call(dplyr_init_library, ns_env("dplyr"), ns_env("vctrs"), ns_env("rlang"))
#'
#'   has_dbplyr <- is_installed("dbplyr")
#'   if (!has_dbplyr || !exists("count.tbl_sql", ns_env("dbplyr"))) {
#'     s3_register("dplyr::count", "tbl_sql")
#'     s3_register("dplyr::tally", "tbl_sql")
#'   }
#'
#'   run_on_load()
#'
#'   invisible()
#' }
#'
#' .onAttach <- function(libname, pkgname) {
#'   setHook(packageEvent("plyr", "attach"), function(...) {
#'     packageStartupMessage(rule())
#'     packageStartupMessage(
#'       "You have loaded plyr after dplyr - this is likely ",
#'       "to cause problems.\nIf you need functions from both plyr and dplyr, ",
#'       "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)"
#'     )
#'     packageStartupMessage(rule())
#'   })
#' }
#'
#' .onDetach <- function(libpath) {
#'   setHook(packageEvent("plyr", "attach"), NULL, "replace")
#' }
#'

##
# .onLoad <- function(...) {
#   holding <- suffix_load()
#   assign("is_wildcard", holding$is_wildcard, envir = urltools_env)
#   assign("cleaned_suffixes", holding$cleaned_suffixes, envir = urltools_env)
#   assign("suff_trie", holding$suff_trie, envir = urltools_env)
# }
##
